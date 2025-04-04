## Global Setting ####################################################

# BEFORE: run setting.R
setwd(wrkpath)
getwd()
# Data Path with all Nexus_Indicators
datpath <- wrkpath %+% "data/"


## Load Libraries ############################################################# 
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(mdendro)
library(purrr)
library(ape)
library(gplots)
library(RColorBrewer)

complete_bee_phylo_tree <- read.tree(file = "BEE_mat7_IQTufbs_tplo_1001bin.nwk")
str(complete_bee_phylo_tree)

species_list <- read.csv(paste0(datpath,"species_list.csv"))


######## trimming the tree

modified_trees <- list()

# Loop through each tree in complete_bee_phylo_tree
for (i in seq_along(complete_bee_phylo_tree)) {
  
  # Find the tips to keep (those in species_list)
  tips_to_keep <- intersect(complete_bee_phylo_tree[[i]]$tip.label, species_list$species)
  
  # Remove the tips not in species_list from the tree
  modified_tree_i <- drop.tip(complete_bee_phylo_tree[[i]], tip = setdiff(complete_bee_phylo_tree[[i]]$tip.label, tips_to_keep), trim.internal = TRUE)
  
  # Add the modified tree to the list
  modified_trees[[i]] <- modified_tree_i
}

# Combine the modified trees into a consensus tree
consensus_tree <- consensus(modified_trees)
plot(consensus_tree)
write.tree(phy=consensus_tree, file="beetree_phylogeny_trimed.newick")



######## getting the distance matrix from the consensus_tree

# transform the tree to a distance matrix
distance_matrix <- cophenetic(consensus_tree)

# Get the alphabetical order of species names
species_order <- order(rownames(distance_matrix))

# Sort the distance matrix based on alphabetical order
sorted_distance_matrix <- distance_matrix[species_order, species_order]

write.csv(sorted_distance_matrix, paste0(datpath,"phylo_distance_matrix.csv"))




######## complete phylo tree with added species 

dis.matrix <- read.csv2(paste0(datpath,"phylo_matrix_complete.csv"))

# Set the first column as row names
rownames(dis.matrix) <- dis.matrix$X

# Remove the first column
dis.matrix <- dis.matrix[, -1]

# Convert all values to numeric
dis.matrix <- as.data.frame(lapply(dis.matrix, as.numeric))


# Assuming dis.matrix is your dissimilarity matrix
min_value <- min(dis.matrix)
max_value <- max(dis.matrix)

# Min-Max scaling
scaled_matrix <- (dis.matrix - min_value) / (max_value - min_value)

# Check the head of the scaled matrix
str(scaled_matrix)


##### agnes

# Agglomerative coefficient - vector of methods to compare
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(scaled_matrix, method = x)$ac
}
map_dbl(m, ac)  

# -> Ward's method gets us the highest agglomerative coefficient

clustering2 <- agnes(scaled_matrix, method = "ward")
plot(clustering2, labels = FALSE)

# safe as NEWICK
class(clustering2) # must be hclust class
clustering2 <- as.hclust(clustering2)
agnes_tree <- as.phylo(clustering2) 

str(agnes_tree)

# Assigning species names as tip labels
agnes_tree$tip.label <- colnames(scaled_matrix)

write.tree(phy=agnes_tree, file="phylo_tree_complete.newick")
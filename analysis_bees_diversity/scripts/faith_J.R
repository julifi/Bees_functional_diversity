## Global Setting ####################################################

# BEFORE: run setting.R
setwd(wrkpath)
getwd()
# Data Path with all Nexus_Indicators
datpath <- wrkpath %+% "data/"


## Load Libraries #############################################################  
library(phytools)

# Read the trait-based tree
trait_tree <- read.newick(file = paste0(datpath,"phylo_tree_complete.newick"))

# Read the community matrix for the site
m_days <- read.csv(paste0(datpath,"m_rarefied_by_days.csv"))

# Remove columns not related to species abundance
m_days_community <- m_days[, -c(1:4)]

# Initialize vector to store Faith's PD values
faith_values <- numeric()

# Loop through each row (trap) in the dataframe
for (i in 1:nrow(m_days_community)) {
  # Get species abundance for the current trap
  trap_abundance <- m_days_community[i, ]
  
  # Get species present in the current trap
  species_names <- names(trap_abundance)[trap_abundance > 0]
  
  # Trim the tree to the present species
  pruned.tree <- drop.tip(trait_tree, trait_tree$tip.label[!trait_tree$tip.label %in% species_names])
  
  # Calculate Faith's PD for the current trap
  faith_value <- sum(pruned.tree$edge.length) / sum(trait_tree$edge.length)
  
  # Store Faith's PD value
  faith_values <- c(faith_values, faith_value)
}

# Add Faith's PD values to the dataframe
faith_values <- as.data.frame(faith_values)
faith_values <- cbind(site = m_days$site, trap = m_days$trap, year = m_days$year, 
                      faith = faith_values)
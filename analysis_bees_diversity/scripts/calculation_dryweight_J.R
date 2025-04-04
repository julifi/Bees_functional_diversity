## Global Setting ####################################################

# BEFORE: run setting.R
setwd(wrkpath)
getwd()
# Data Path with all Nexus_Indicators
datpath <- wrkpath %+% "data/"


## Load Libraries #############################################################  
library(pollimetry)
library(devtools)
library(PerformanceAnalytics)

# body size (dryweight in mg) based on the ITD

x <- read.csv2(paste0(datpath,"IT.csv"))
taxa <- c("bee")
type <- c("taxo")
bodysize_IT <- bodysize(x, taxa, type)
IT_bodysize <- bodysize_IT$Est.Weight

# body size (dryweight in mg) based on the body length

bodylength <- read.csv2(paste0(datpath,"BL.csv"))
BL <- bodylength$BL
BL_bodysize <- lengthsize(BL, Eq = "HYM")

# mean body size from the ITD-bodysize and the length-bodysize

bodysize <- data.frame(BL_bodysize)
bodysize$IT <- IT_bodysize

chart.Correlation(bodysize, histogram = T, pch = 19, method = "pearson")
# all good correlations expect GR84H (Anfangsbuchstaben Autor, Jahr, Taxagruppe)
# -> W13H am aktuellsten und auf Hymenoptera spezialisiert

mean_TI_W13H <- ifelse(is.na(bodysize$IT), bodysize$W13H, rowMeans(bodysize[c("IT", "W13H")], na.rm = TRUE))
mean_TI_W13H <- data.frame(species = x$Species, mean_TI_W13H = mean_TI_W13H)
print(mean_TI_W13H)
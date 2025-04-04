# FileInfo ##################################################################
# File: Settings.R
# checks for nodename (or username) and sets directories accordingly
# We define a temporary folder which is outside of (but close to) our 
# repository to avoid that data to be pushed or pulled
# 

cu <- Sys.info()["user"]
cn <- Sys.info()["nodename"]


# Explanatiomn of Environment-Variable:
# wrkpath is supposed to be to point to each individual subfolder with the 
#   scripts
# outpath is supposed to point to a subfolder where outputs are saved, somewhere 
#   outside the repository to reduce sync-data

if (cn == "UFZ542535" && cu == "fischjul")     # Julia
{ 
  wrkpath <- "Y:/Home/fischjul/Arbeit_aktuell/Bees_diversity/analysis_bees_diversity/"
  figpath <- "Y:/Home/fischjul/Arbeit_aktuell/Bees_diversity/analysis_output/"
  tmppath <- "./Rtemp"

} else if (cn == "MSG9" && cu == "fischjul")     # Julia
{ 
  wrkpath <- "Y:/Home/fischjul/Arbeit_aktuell/Bees_diversity/analysis_bees_diversity/"
  figpath <- "Y:/Home/fischjul/Arbeit_aktuell/Bees_diversity/analysis_output/"
  tmppath <- "./Rtemp"
} 

# Further misc utils
"%+%" <- function(x,y)paste(x,y,sep="")


################################################################################
# List of Packages needed, 
listOfPackages <- c("ape",
                    "caret", "cluster",
                    "data.table", "dendextend ", "devtools", "dplyr", 
                    "factoextra",
                    "gawdis", "ggmap", "ggplot2", "giscoR", "gmodels", "gplots", "gridExtra",
                    "ipred", 
                    "ks",
                    "MASS", "mdendro",
                    "PerformanceAnalytics", "phytools", "plot3D", "plot3Drgl", "plyr", "pollimetry", 
                    "Polychrome", "purrr",
                    "raster", "RColorBrewer", "readr", "reshape2",  "rgl", "rnaturalearth", "rpart", 
                    "rpart.plot", "rsample", 
                    "scatterplot3d", "sf", "stringr", 
                    "terra", "tibble", "tidyr","tidyterra", "tidyverse", "TPD",
                    "vegan"
                  )

# Moving forward from now (2022) most of the packages that support sp (rgdal, 
# rgeos, maptools) are being retired from the end of 2023, and everyone is being 
# advised to move to sf. raster library (which has many of the same dependents 
# as sp) is transitioning to terra.

################################################################################
# Install Packages, if needed
for (i in listOfPackages){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
  }
}
devtools::install_github("mattflor/chorddiag")

################################################################################
# For the Data Management Report, we need to memorize with which libraries
# the entire analysis was done, and in case, we need to redo the analysis
# and library changed the functionaly substantially, we shgould simpply
# get back to the original labrary version, this is, what generates a textfile
# that contains all library version used for that analysis
sink("LibraryVersions.txt")
sessionInfo()
sink()

## Global Setting ####################################################

# BEFORE: run setting.R
setwd(wrkpath)
getwd()
# Data Path with all Nexus_Indicators
datpath <- wrkpath %+% "data/"


## Load Libraries #############################################################  
library(ggmap)
library(sf)
library(giscoR)
library(ggplot2)
library(rnaturalearth)

register_stadiamaps(key = "e547caee-c3d8-4f00-985f-ac12612d99d6")

# Get map tiles from Google Maps for a specific location and zoom level
map <- get_stadiamap(
  bbox = c(bottom = 51.3, left = 10.9, top = 52.15, right = 11.85),
  zoom = 11,
  maptype = "stamen_terrain",
  crop = TRUE)

# stamen_terrain, stamen_toner, stamen_toner_lite, stamen_watercolor, stamen_terrain_background, 
# stamen_toner_background, stamen_terrain_lines, stamen_terrain_labels, stamen_toner_lines, 
# stamen_toner_labels

# Create a ggplot2 plot with the map tiles
m1 <- ggmap(map)

# Read coordinates data
cor <- read.csv2(paste0(datpath,"trap_coordinates.csv"))

# Load the shapefile for Bundesländer borders
bundeslander <- st_read(paste0(datpath,"vg2500_bld.shp"))("")

# Check the structure and attributes of the shapefile
print(bundeslander)

# Define the palette
palette <- c("#070751", "#8F7C00", "#F99414", "#740AFF", "#4CE5E5", "#E538B1")
names(palette) <- c("FBG", "GFH", "HAR", "SIP", "SST", "WAN")

palette <- c("mediumorchid", "#F9C114", "#4C3006", "mediumblue", "grey70", "#A5EB0E")
names(palette) <- c("FBG", "GFH", "HAR", "SIP", "SST", "WAN")

m1 +
  geom_point(data = cor, aes(x = long, y = lat, fill = site), color = "black", alpha = 1,
             size = 1.7, shape = 21) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = "black") +
  xlab("Longitude") +  
  ylab("Latitude") +  
  theme_minimal()
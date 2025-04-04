## Global Setting ####################################################

# BEFORE: run setting.R
setwd(wrkpath)
getwd()
# Data Path with all Nexus_Indicators
datpath <- wrkpath %+% "data/"


## Load Libraries #############################################################  
library(sf)
library(giscoR)
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE
# Proof with map
library(ggplot2)



deu <- giscoR::gisco_get_countries(country = "Germany")
deu_gg <- ggplot(deu) +
  geom_sf()
# Map stored

# create df
cor <- read.csv2(paste0(datpath,"trap_coordinates.csv"))

lat <- cor$lat
long <- cor$long
df <- data.frame(lat, long)
df$coords <- data.frame(longitude = df$long, latitude = df$lat)

# First to EPSG:4326
df$coords_sf <- st_as_sf(df$coords,
                         coords = c("longitude", "latitude"),
                         crs = 4326
)

df$coords_sf

# Proof
deu_gg +
  geom_sf(data = df$coords_sf) +
  coord_sf(crs = 4326)

# Mercator
df$mercator <- st_transform(df$coords_sf, 3857)
df$mercator

# Proof
deu_gg +
  geom_sf(data = df$mercator) +
  coord_sf(crs = 3857)

# LAEA
df$eters_laea <- st_transform(df$coords_sf, 3035)
df$eters_laea

# Proof
deu_gg +
  geom_sf(data = df$eters_laea) +
  coord_sf(crs = 3035)

# extract the needed coordinates

cor_laea <- df$eters_laea
all_cor_formats <- df

head(cor_laea)
# Separate X and Y coordinates into new columns
cor_laea <- st_coordinates(cor_laea)

# Rename the columns to 'X' and 'Y'
colnames(cor_laea) <- c('X', 'Y')

write.csv(cor_laea, paste0(datpath, "cor_laea.csv"))
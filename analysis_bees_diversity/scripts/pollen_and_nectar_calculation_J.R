## Global Setting ####################################################

# BEFORE: run setting.R
setwd(wrkpath)
getwd()
# Data Path with all Nexus_Indicators
datpath <- wrkpath %+% "data/"


############################################################# 

pol_nec <- read.csv2(paste0(datpath,"pollen_and_nectar.csv"))
thunen <- read.csv2(paste0(datpath,"crop_thunen.csv"))
sebastian <- read.csv2(paste0(datpath,"crop_sebastian.csv"))

str(pol_nec)
str(thunen)


##### thunen #####

# Initialize an empty dataframe to store the results
pol_and_nec_thunen <- data.frame(site = character(),
                                 trap = character(),
                                 year = integer(),
                                 radius = integer(),
                                 pollen_g_buffer_area = numeric(),
                                 nectar_l_buffer_area = numeric(),
                                 stringsAsFactors = FALSE)

# Get the crop names from the thunen dataframe
crop_names <- names(thunen)[grep("^[a-z]", names(thunen))][-c(1:4)]
str(crop_names)

# Loop through each row of the thunen dataframe
for (i in 1:nrow(thunen)) {
  # Extract relevant information from thunen dataframe
  site <- thunen[i, "site"]
  trap <- thunen[i, "trap"]
  year <- thunen[i, "year"]
  radius <- thunen[i, "radius"]
  
  # Multiply crop values with corresponding pollen and nectar values from pol_nec dataframe
  pollen_g_buffer_area <- sum(thunen[i, crop_names] * pol_nec$pollen_g_m2[match(crop_names, pol_nec$crop)])
  nectar_l_buffer_area <- sum(thunen[i, crop_names] * pol_nec$nectar_l_m2[match(crop_names, pol_nec$crop)])
  
  # Add the calculated values to pol_and_nec_thunen dataframe
  pol_and_nec_thunen <- rbind(pol_and_nec_thunen, 
                              data.frame(site = site,
                                         trap = trap,
                                         year = year,
                                         radius = radius,
                                         pollen_g_buffer_area = pollen_g_buffer_area,
                                         nectar_l_buffer_area = nectar_l_buffer_area))
}

# Print the resulting dataframe
print(pol_and_nec_thunen)

write.csv(pol_and_nec_thunen, paste0(datpath,"pol_and_nec_thunen.csv"))


##### sebastian #####

# Initialize an empty dataframe to store the results
pol_and_nec_sebastian <- data.frame(site = character(),
                                    trap = character(),
                                    year = integer(),
                                    radius = integer(),
                                    pollen_g_buffer_area = numeric(),
                                    nectar_l_buffer_area = numeric(),
                                    stringsAsFactors = FALSE)

# Get the crop names from the sebastian dataframe
crop_names <- names(sebastian)[grep("^[a-z]", names(sebastian))][-c(1:4)]
str(crop_names)

# Loop through each row of the sebastian dataframe
for (i in 1:nrow(sebastian)) {
  # Extract relevant information from sebastian dataframe
  trap <- sebastian[i, "trap"]
  year <- sebastian[i, "year"]
  radius <- sebastian[i, "radius"]
  
  # Multiply crop values with corresponding pollen and nectar values from pol_nec dataframe
  pollen_g_buffer_area <- sum(sebastian[i, crop_names] * pol_nec$pollen_g_m2[match(crop_names, pol_nec$crop)])
  nectar_l_buffer_area <- sum(sebastian[i, crop_names] * pol_nec$nectar_l_m2[match(crop_names, pol_nec$crop)])
  
  # Add the calculated values to pol_and_nec_sebastian dataframe
  pol_and_nec_sebastian <- rbind(pol_and_nec_sebastian, 
                                 data.frame(trap = trap,
                                            year = year,
                                            radius = radius,
                                            pollen_g_buffer_area = pollen_g_buffer_area,
                                            nectar_l_buffer_area = nectar_l_buffer_area))
}

# Print the resulting dataframe
print(pol_and_nec_sebastian)

write.csv(pol_and_nec_sebastian, paste0(datpath,"pol_and_nec_sebastian.csv"))
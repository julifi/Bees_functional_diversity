## Global Setting ####################################################

# BEFORE: run setting.R
setwd(wrkpath)
getwd()
# Data Path with all Nexus_Indicators
datpath <- wrkpath %+% "data/"


## Load Libraries ############################################################# 
library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(purrr)
library(countrycode)
library(readxl)
library(terra)
library(sf)
library(exactextractr)
library(raster)
library(rlang)
library(openxlsx)

########## A) Data preparation #########

### 1) read in data
# raw data: 2019-2021
data_19_21 <- read_excel('analysis_bees_diversity/data/data_raw/Bee_Data2019-2021_2024-08-16.xlsx', sheet = "BeeData_2019-21")

# raw data: 2010-2019
data_10_19 <- read_excel("analysis_bees_diversity/data/data_raw/TERENO_bees_2010-19.xlsx", sheet = "Query_Bexis_work_variables_Bees")

# refine columns in 2010-2019 data 
data_10_19 <- data_10_19[c("LocName", "LocTrap","YearValue", "StartDate", "EndDate", "DaysExposure","GenSpec", "Males", "Females", "SumIndividuals")]

# trait data
traits <- read_excel("analysis_bees_diversity/data/data_raw/all_traits.xlsx")
#rename species name (replace '.' with ' '):
traits$species <- gsub(".", " ", traits$species, fixed = TRUE)

# traits_raw <- read_excel("analysis_bees_diversity/data/data_raw/Traits_TerenoBees_20210913_updated.xlsx", skip=1)
# 
# # rename colnames in traits_raw data
# colnames(traits_raw)[c(1,5,6,9,10)]<- c("species", "ITD_mean_f_[mm]", "foraging range_ff_[km]", "habitat_specialisation", "sociality")

### 2) Synchronize colnames and Location names
# rename Location:
data_10_19$LocName <- str_extract(data_10_19$LocName, "\\(.*\\)")
data_10_19$LocName <- str_remove_all(data_10_19$LocName, "[\\(\\)]")

# rename colnames in 19-21 data
colnames(data_19_21)[c(2:4,6:8)]<- c("Males", "Females","LocName","StartDate", "EndDate","LocTrap")

### 3) Synchronize species names in the two data sets
for(i in 1:nrow(data_19_21)){pos<-which(strsplit(data_19_21$fullGenSpec[i], "")[[1]]==" ")
                             data_19_21$fullGenSpec[i]<-substr(data_19_21$fullGenSpec[i],0,pos[2]-1)}

# check whether all species names in 19-21 are also there in the file before
x<-unique(data_19_21$fullGenSpec); y<-unique(data_10_19$GenSpec)
x<-x[order(x)]; y<-y[order(y)]
x<-as.data.frame(x); y<-as.data.frame(y)

# correct mistakes
data_19_21$fullGenSpec[which(data_19_21$fullGenSpec==x$x[127])]<- 'Lasioglossum glabriusculum'
data_19_21$fullGenSpec[which(data_19_21$fullGenSpec==x$x[159])]<- 'Macropis europaea'
data_19_21$fullGenSpec[which(data_19_21$fullGenSpec==x$x[229])]<- 'Stelis odontopyga'

data_10_19$GenSpec[which(data_10_19$GenSpec==y$y[13])]<- 'Andrena coitana'
data_10_19$GenSpec[which(data_10_19$GenSpec==y$y[122])]<- 'Halictus leucaheneus'
data_10_19$GenSpec[which(data_10_19$GenSpec==y$y[277])]<- 'Stelis odontopyga'

# check whether remaining mismatches are really new species in 2019-21
x<-unique(data_19_21$fullGenSpec); y<-unique(data_10_19$GenSpec)
x<-x[order(x)]; y<-y[order(y)]
x<-as.data.frame(x); y<-as.data.frame(y)
z<-c(y$y, x$x[which(is.na(match(x$x, y$y)))])
z<-z[order(z)]; z<-as.data.frame(z)

x$x[which(is.na(match(x$x, y$y)))]
# yes, seems to be the case

spec.list<- z$z
rm(pos,x,y,z)

### 4) merge the two data files with bee data
# note: for now, the 2019 data from the old file is deleted. But we should still check once whether the two 
# data sets are identical

# create year and month column
data_10_19$year<-year(data_10_19$StartDate);data_10_19$month<-month(data_10_19$StartDate)
data_19_21$year<-year(data_19_21$StartDate);data_19_21$month<-month(data_19_21$StartDate)

length(which(data_10_19$year==2019)); length(which(data_19_21$year==2019))
# data is not the same - there are some columns in the old files missing...

# delete the 2019 data in the old file
data_10_19<-data_10_19[which(data_10_19$year!=2019),]

# synchronise last column name
colnames(data_19_21)[1]<-'GenSpec'

# create one data file
dat_all<- rbind(data_10_19[c("LocName", "LocTrap","year","month", "StartDate", "EndDate", "GenSpec", "Males", "Females")],
                data_19_21[c("LocName", "LocTrap","year","month", "StartDate", "EndDate", "GenSpec", "Males", "Females")])

# there is one sample that is associated to the wrong year, we need to correct that
dat_all$uniqueID<-paste0(dat_all$LocTrap,dat_all$year)
dat_all$year[which(dat_all$uniqueID=='HAR152020' & dat_all$EndDate== dat_all$EndDate[55953]
                   & dat_all$StartDate== dat_all$StartDate[55953])]<-2021

### 5) take all social bees - I would exclude the male bees as those are probably not contributing to pollination.
communal<-c(traits$species[which(traits$sociality=='communal')],'Apis mellifera')

# merging parameter showing whether males should be used or not
dat_all$sum<-1
dat_all$sum[which(is.na(match(dat_all$GenSpec,communal))==F)]<-0

rm(communal)

### 6) create meta-data containing all location-year combinations
# create and trim the meta-data
meta<- aggregate(list(dat_all$Males), by=list(dat_all$LocName, dat_all$LocTrap,dat_all$year), function(x){mean(x, na.rm=T)})
meta<-meta[,1:3]; colnames(meta)<-c("LocName", "LocTrap","year")

# create a unique ID that can be used for the species data
meta$uniqueID<-paste0(meta$LocTrap,meta$year)

### 7) add sampling time and season to meta-data
# (i) first add season to dat_all
dat_all$start.day<-yday(dat_all$StartDate)
hist(dat_all$start.day) #180 is a good separation between seasons

dat_all$season<-'spring'
dat_all$season[dat_all$start.day>=180]<-'summer'

# (ii) now create a season meta data:
x<-which(dat_all$season=='spring')
meta.spring<- aggregate(list(dat_all$Males[x]), 
                        by=list(dat_all$LocName[x], dat_all$LocTrap[x], dat_all$year[x],
                                dat_all$StartDate[x], dat_all$EndDate[x]), function(x){mean(x, na.rm=T)})
colnames(meta.spring)<-c('LocName', 'LocTrap', 'year', 'StartDate', 'EndDate')
meta.spring<-meta.spring[,-6]
# this data frame contains now all trap collection periods - those are multiple per season (i.e spring or summer season)

# (iii) we can now add a starting date and an end date of the spring season to the meta data
# start date
y<- aggregate(list(dat_all$StartDate[x]), by=list(dat_all$LocName[x], dat_all$LocTrap[x], dat_all$year[x]),
                                                  function(x){min(x, na.rm=T)})
# end date
y.2<- aggregate(list(dat_all$EndDate[x]), by=list(dat_all$LocName[x], dat_all$LocTrap[x], dat_all$year[x]),
              function(x){max(x, na.rm=T)})
colnames(y)<-c('LocName', 'LocTrap', 'year', 'StartDate')
# bring both together and add exposure time
y$EndDate<-y.2[,4]
y$exposure<- yday(y$EndDate) - yday(y$StartDate)

# (iv) now let's check whether exposure time is the same if the we sum up individual exposure times instead
# if it is not, we have gaps in our sampling season
meta.spring$exposure<- yday(meta.spring$EndDate) - yday(meta.spring$StartDate)

y.3<- aggregate(list(meta.spring$exposure), 
                by=list(meta.spring$LocName, meta.spring$LocTrap, meta.spring$year),
              function(x){sum(x, na.rm=T)})
colnames(y.3)<-c('LocName', 'LocTrap', 'year', 'exposure.true')

gaps<-which((y$exposure==y.3$exposure.true)==F)
# no, we do have quite a number of gaps in our sampling seasons (in 129 cases...)

# (v) next identify the gaps in sampling seasons
# first create a unique ID:
y.3$uniqueID<-paste0(y.3$LocTrap,y.3$year); meta.spring$uniqueID<-paste0(meta.spring$LocTrap,meta.spring$year)

# check how many gaps we have and save the first gap in two vectors
gaps<-which((y$exposure==y.3$exposure.true)==F)
no.of.gaps<-c(); start.gap<-as.Date(c()); end.gap<-as.Date(c())
for(i in gaps){
  starts<-unique(meta.spring$StartDate[which(meta.spring$uniqueID==y.3$uniqueID[i])])
  ends<-unique(meta.spring$EndDate[which(meta.spring$uniqueID==y.3$uniqueID[i])])
  starts<-starts[2:length(starts)]
  ends<-ends[1:(length(ends)-1)]
  no.of.gaps<-c(no.of.gaps, length(which(ends!=starts)))
# if there is a gap, there is always only one gap - take makes life easier...
  start.gap<-c(start.gap, ends[which(ends!=starts)]) 
# this might first seems odd, but using ends here is correct
  end.gap<-c(end.gap, starts[which(ends!=starts)])
}

# correct the exposure time in the y data frame (this now takes gaps into account)
y$exposure<-y.3$exposure.true

# add the information of gap start and end to the data frame
y$spring.gap.start<-as.Date(NA); y$spring.gap.end<-as.Date(NA)
y$spring.gap.start[gaps]<-start.gap; y$spring.gap.end[gaps]<-end.gap

# (iv) now we can add spring exposure time and info about spring gap timing to the meta-data
# get unique ID to the data frame with the info
y$uniqueID<-paste0(y$LocTrap,y$year)

# add the missing info
meta$spring.start<- y$StartDate[match(meta$uniqueID, y$uniqueID)]
meta$spring.end<- y$EndDate[match(meta$uniqueID, y$uniqueID)]
meta$spring.gap.start<- y$spring.gap.start[match(meta$uniqueID, y$uniqueID)]
meta$spring.gap.end<- y$spring.gap.end[match(meta$uniqueID, y$uniqueID)]
meta$spring.exposure<- y$exposure[match(meta$uniqueID, y$uniqueID)]

# (v) Now repeat this for the summer season...
# create a season meta data:
  x<-which(dat_all$season=='summer')
meta.summer<- aggregate(list(dat_all$Males[x]), 
                        by=list(dat_all$LocName[x], dat_all$LocTrap[x], dat_all$year[x],
                                dat_all$StartDate[x], dat_all$EndDate[x]), function(x){mean(x, na.rm=T)})
colnames(meta.summer)<-c('LocName', 'LocTrap', 'year', 'StartDate', 'EndDate')
meta.summer<-meta.summer[,-6]

# (vi) we can now add a starting date and an end date of the spring season to the meta data
# start date
y<- aggregate(list(dat_all$StartDate[x]), by=list(dat_all$LocName[x], dat_all$LocTrap[x], dat_all$year[x]),
              function(x){min(x, na.rm=T)})
# end date
y.2<- aggregate(list(dat_all$EndDate[x]), by=list(dat_all$LocName[x], dat_all$LocTrap[x], dat_all$year[x]),
                function(x){max(x, na.rm=T)})
colnames(y)<-c('LocName', 'LocTrap', 'year', 'StartDate')
# bring both together and add exposure time
y$EndDate<-y.2[,4]
y$exposure<- yday(y$EndDate) - yday(y$StartDate)

# (vii) now let's check whether exposure time is the same if the we sum up individual exposure times instead
# if it is not, we have gaps in our sampling season
meta.summer$exposure<- yday(meta.summer$EndDate) - yday(meta.summer$StartDate)

y.3<- aggregate(list(meta.summer$exposure), 
                by=list(meta.summer$LocName, meta.summer$LocTrap, meta.summer$year),
                function(x){sum(x, na.rm=T)})
colnames(y.3)<-c('LocName', 'LocTrap', 'year', 'exposure.true')

gaps<-which((y$exposure==y.3$exposure.true)==F)
# no, we do have quite a number of gaps again (in 73 cases...)

# (viii) next identify the gaps in sampling seasons
# first create a unique ID:
y.3$uniqueID<-paste0(y.3$LocTrap,y.3$year); meta.summer$uniqueID<-paste0(meta.summer$LocTrap,meta.summer$year)

# check how many gaps we have and save the first gap in two vectors
gaps<-which((y$exposure==y.3$exposure.true)==F)
no.of.gaps<-c(); start.gap<-as.Date(c()); end.gap<-as.Date(c())
for(i in gaps[]){
  starts<-unique(meta.summer$StartDate[which(meta.summer$uniqueID==y.3$uniqueID[i])])
  ends<-unique(meta.summer$EndDate[which(meta.summer$uniqueID==y.3$uniqueID[i])])
  starts<-starts[2:length(starts)]
  ends<-ends[1:(length(ends)-1)]
  no.of.gaps<-c(no.of.gaps, length(which(ends!=starts)))
  # again, only max one gap per trap in summer... that's good
  start.gap<-c(start.gap, ends[which(ends!=starts)]) 
  # this might first seems odd, but using ends here is correct
  end.gap<-c(end.gap, starts[which(ends!=starts)])
}

# correct the exposure time in the y data frame (this now takes gaps into account)
y$exposure<-y.3$exposure.true

# add the information of gap start and end to the data frame
y$summer.gap.start<-as.Date(NA); y$summer.gap.end<-as.Date(NA)
y$summer.gap.start[gaps]<-start.gap; y$summer.gap.end[gaps]<-end.gap

# (ix) now we can add spring exposure time and info about summer gap timing to the meta-data
# get unique ID to the data frame with the info
y$uniqueID<-paste0(y$LocTrap,y$year)

# add the missing info
meta$summer.start<- y$StartDate[match(meta$uniqueID, y$uniqueID)]
meta$summer.end<- y$EndDate[match(meta$uniqueID, y$uniqueID)]
meta$summer.gap.start<- y$summer.gap.start[match(meta$uniqueID, y$uniqueID)]
meta$summer.gap.end<- y$summer.gap.end[match(meta$uniqueID, y$uniqueID)]
meta$summer.exposure<- y$exposure[match(meta$uniqueID, y$uniqueID)]

# (x) compute total exposure time for each trap-year combination and check its distribution
meta$total.exposure<-meta$spring.exposure+meta$summer.exposure
hist(meta$total.exposure, breaks = 40)
# options for rarefracation thresholds if only days of exposure are considered (in my opinion)
length(which(meta$total.exposure<64))
length(which(meta$total.exposure<69))

rm(meta.spring, meta.summer, y, y.2, y.3, end.gap, ends, gaps, no.of.gaps, start.gap, starts)

### 8) start creating species matrix

cm.females<-matrix(nrow = nrow(meta), ncol=length(spec.list))

# now fill the species matrix
# create one for males, one for females, one for total abundance combined (f and m), one for total biomass



## Script Lili:
m <- read.csv2(paste0(datpath,"data_raw/community_matrix_female.csv"))
m$site <- as.factor(m$site)
m$trap <- as.factor(m$trap)
m$year <- as.factor(m$year)
str(m)

# trait data
a_traits <- read.csv2(paste0(datpath,"activity_traits.csv"))
m_traits <- read.csv2(paste0(datpath,"morphological_traits.csv"))
s_traits <- read.csv2(paste0(datpath,"social_traits.csv"))
s_traits$sociality <- as.factor(s_traits$sociality)
s_traits$lecty <- as.factor(s_traits$lecty)
s_traits$nesting_type <- as.factor(s_traits$nesting_type)
s_traits$nesting_habitat <- as.factor(s_traits$nesting_habitat)


####### rarefaction (based on days of exposure for each trap) #######
### histogram of days of exposure
hist(m$days_of_exposure, 
     breaks = 30,
     ylim = c(0, 400), 
     main = "Histogram of Total Days of Exposure",
     xlab = "Days of Exposure",
     ylab = "Frequency")

### good cut at which day? -> setting the threshold
less_then_50_days_of_exposure <- m[m$days_of_exposure < 50, ] # 27 traps would get lost
less_then_60_days_of_exposure <- m[m$days_of_exposure < 60, ] # 87 traps would get lost
less_then_54_days_of_exposure <- m[m$days_of_exposure < 54, ] # 29 traps would get lost
less_then_55_days_of_exposure <- m[m$days_of_exposure < 55, ] # 31 traps would get lost
less_then_56_days_of_exposure <- m[m$days_of_exposure < 56, ] # 43 traps would get lost 
# -> cut at 55 days of exposure 
# traps with <55 days of exposure are excluded -> 31 traps out of 949 traps  

### save excluded traps
excluded_traps <- m[m$days_of_exposure < 55, ]
excluded_traps <- excluded_traps[,c(1:3)]

write.csv(excluded_traps, paste0(datpath,"excluded_traps.csv"))

# -> HAR 2017 has most traps with <55 days of exposure (02, 03, 09, 13, 14) 
# -> 5 traps is max. missing per site per year -> min number of traps = 11 = 16 - 5
min_number_traps <- 11

### m2 = matrix with only traps >54 days of exposure
m2 <- m[m$days_of_exposure > 54, ]
str(m2)

### loop for rarefaction
# IMPORTANT: species must be cols and samples rows!

# remove site, trap, year and days_of_exposure
m3 <- m2[, -c(1:4)]
# define the min_days_of_exposure
a <- min(m2$days_of_exposure) # -> 55 days
# loop
m3_rarefied <- m3
for (k in 1:ncol(m3)) {
  for (i in 1:nrow(m3)) {
    m3_rarefied[i, k] <- m3[i, k] * (a / m2$days_of_exposure[i])
  }
}
m3_rarefied_rd <- trunc(m3_rarefied)
commas <- m3_rarefied - m3_rarefied_rd
upround <- rowSums(commas)
rank <- as.data.frame(t(apply(-commas, 1, order)))
for (k in 1:nrow(m3)){
  for (i in 1:round(upround[k])) {if(round(upround[k])>0){
    m3_rarefied[k, rank[k, i]] <- m3_rarefied_rd[k, rank[k, i]] + 1}
  }}
m3 <- trunc(m3_rarefied)
m_rarefied_by_days <- cbind(m2[, 1:3], m3)

# With this code above: 
# 1) all decimal places are rounded down to whole numbers 
# 2) with rowSums, the total amount of missing individuals per trap is checked (essentially 
# how much was deducted too much) 
# 3) rank arranges all species of a sample according to the height of the decimal place -> the 
# highest decimal place is at rank #1. -> now, if according to upround e.g. 30 individuals were 
# deducted too much, +1 is added for the first 30 species in the rank (simply rounding up and 
# down at 0.5 would not work because then the proportions/relations of the rarefaction are no 
# longer correct)

write.csv(m_rarefied_by_days, paste0(datpath,"m_rarefied_by_days.csv"))

# remove unnecessary stuff
rm(less_then_50_days_of_exposure, less_then_60_days_of_exposure, less_then_54_days_of_exposure,
   less_then_55_days_of_exposure, less_then_56_days_of_exposure, commas, m3_rarefied, 
   m3_rarefied_rd, rank, m2, m3, excluded_traps)


####### 1. total abundance #######

m_rarefied_by_days <- read.csv(paste0(datpath,"matrix_rarefied_by_days.csv"))

#### a) on SITE level ####
# m_rarefied_by_days with only community
rarefied_community <- m_rarefied_by_days[,-c(1:3)]

# create empty vectors
year_out <- c()
site_out <- c()
total_abundance <- c()

# loop through unique combinations of site and year
for (site in unique(m_rarefied_by_days$site)) {
  
  for (year in unique(m_rarefied_by_days$year[m_rarefied_by_days$site == site])) {
    year_out <- c(year_out, year)
    site_out <- c(site_out, site)
    abundance_sum <- 0
    community_matrix <- rarefied_community[which(m_rarefied_by_days$site == site & m_rarefied_by_days$year == year), ]
    
    for (row in 1:nrow(community_matrix)) {
      abundance_sum <- abundance_sum + sum(community_matrix[row, ])
    }
    total_abundance <- c(total_abundance, abundance_sum)
  }
}

output_site_level <- data.frame(year = as.numeric(year_out), site = site_out, total_abundance_bootstrapped = total_abundance)

# colour palette
palette <- c("#156B52","#A8773C","#FF803C","#CE2220","#804E99","#6C9FD4")
# green=FBG; brown=GFH; orange=HAR; red=SIP; violett=SST; blue=WAN

# plot species richness
plot <- ggplot(output_site_level, aes(x = year, y = total_abundance_bootstrapped, color = site)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = palette) + 
  labs(x = "Year", y = "Bootstrapped Total Abundance", title = "Bootstrapped Total Abundance on Site Level (rarefied by days of exposure)", color = "Site") + # color = "Site" = legend title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(output_site_level$year), labels = format(unique(output_site_level$year), scientific = FALSE))
print(plot)

#### b) on TRAP level ####

# community_data
community_data <- m_rarefied_by_days[, 5:ncol(m_rarefied_by_days)]

# Calculate total abundance for each trap
total_abundance <- apply(community_data, 1, sum)

output_trap_level <- data.frame(year = m_rarefied_by_days$year, 
                                site = m_rarefied_by_days$site, 
                                trap = m_rarefied_by_days$trap, 
                                total_abundance = total_abundance)

# Plot for total abundance
ggplot(output_trap_level, aes(x = as.factor(year), y = total_abundance, fill = site)) +
  geom_boxplot() +
  scale_fill_manual(values = palette) + 
  facet_wrap(~site, scales = "fixed", ncol = 3) +  # arrange in 2 rows, 3 columns
  labs(title = "Total Abundance on Trap Level (rarefied by days of exposure)",
       x = "Year",
       y = "Total Abundance") +
  theme_minimal() +
  theme(legend.position = "none",  # remove legend
        axis.text.x = element_text(angle = 45, hjust = 1),  # rotate x-axis labels
        plot.title = element_text(hjust = 0.5))  # center title


####### 2. species richness #######
#### a) on SITE level ####

# create empty vectors
year_out <- c()
site_out <- c()
richness <- c()

### loop
for (site in unique(m_rarefied_by_days$site)) {
  
  # loop through each year for the current site
  for (year in unique(m_rarefied_by_days$year[m_rarefied_by_days$site == site])) {
    year_out <- c(year_out, year)
    site_out <- c(site_out, site)
    richness_boot <- c()
    community_matrix <- rarefied_community[which(m_rarefied_by_days$site==site & m_rarefied_by_days$year==year), ]
    for (round in 1:num_iterations) {
      community_sub <- community_matrix[sample(1:nrow(community_matrix), min_number_traps, replace = F), ]
      x <- colSums(community_sub)
      x <- length(which(x>0))
      richness_boot <- c(richness_boot, x)
      x <- sum(colSums(community_sub))
    }
    richness <- c(richness, round(mean(richness_boot)))
  }}

# create a dataframe 
species_richness <- data.frame(year = as.numeric(year_out), site = site_out, species_richness_bootstrapped = richness)

# merge dataframes based on "year" and "site"
output_site_level <- merge(output_site_level, species_richness, by = c("year", "site"))

# colour palette
palette <- c("#156B52","#A8773C","#FF803C","#CE2220","#804E99","#6C9FD4")
# green=FBG; brown=GFH; orange=HAR; red=SIP; violett=SST; blue=WAN

# plot species richness
plot <- ggplot(output_site_level, aes(x = year, y = richness, color = site)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = palette) + 
  labs(x = "Year", y = "Bootstrapped Species Richness", title = "Bootstrapped Species Richness on Site Level (rarefied by days of exposure)", color = "Site") + # color = "Site" = legend title
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(output_site_level$year), labels = format(unique(output_site_level$year), scientific = FALSE))
print(plot)


#### b) on TRAP level ####

# Calculate species richness for each trap
richness <- apply(community_data, 1, function(row) specnumber(row > 0))

richness_trap_level <- data.frame(year = m_rarefied_by_days$year, site = m_rarefied_by_days$site, 
                                trap = m_rarefied_by_days$trap, species_richness = richness)

output_trap_level <- merge(output_trap_level, richness_trap_level, by = c("year", "site", "trap"))

# plot for species richness
ggplot(output_trap_level, aes(x = as.factor(year), y = species_richness, fill = site)) +
  geom_boxplot() +
  scale_fill_manual(values = palette) + 
  facet_wrap(~site, scales = "fixed", ncol = 3) +  # arrange in 2 rows, 3 columns
  labs(title = "Species Richness on Trap Level (rarefied by days of exposure)",
       x = "Year",
       y = "Species Richness") +
  theme_minimal() +
  theme(legend.position = "none",  # remove legend
        axis.text.x = element_text(angle = 45, hjust = 1),  # rotate x-axis labels
        plot.title = element_text(hjust = 0.5))  # center title



####### 3. Pielou evenness (with the un-rarefied community matrix) #######

# To compare the Pielou evenness of the different sites it is not as important that they had the 
# same days of exposure as for the species richness (according to Alfred). That is why we use the 
# un-rarefied community matrix with more data here.

#### a) on SITE level ####

# sum abundances for each species by site and year
abundance_sum <- aggregate(. ~ site + year, data = m, sum)
# remove site and year
abundance_sum_community <- abundance_sum[,-c(1:3)]
# shannon
shannon <- apply(abundance_sum_community, 1, function(row) diversity(row, index = "shannon"))
# Pielou evenness based on shannon
pielou_evenness <- shannon / log(length(abundance_sum_community))
# create a dataframe 
pielou_df <- data.frame(site = as.character(abundance_sum$site), year = abundance_sum$year,
                        pielou_evenness = pielou_evenness)

# merge dataframes based on "year" and "site"
output_site_level <- merge(output_site_level, pielou_df, by = c("year", "site"))

# plot
plot <- ggplot(output_site_level, aes(x = year, y = pielou_evenness, color = site)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = palette) + 
  labs(x = "Year", y = "Pielou Evenness", title = "Pielou Evenness on Site Level", color = "Site") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = unique(output_site_level$year), labels = format(unique(output_site_level$year), scientific = FALSE))
print(plot)


#### b) on TRAP level ####

# sum abundances for each species by trap and year
abundance_sum <- aggregate(. ~ trap + site + year, data = m, sum)
# remove site and year
abundance_sum_community <- abundance_sum[,-c(1:4)]
# shannon
shannon <- apply(abundance_sum_community, 1, function(row) diversity(row, index = "shannon"))
# Pielou evenness based on shannon
pielou_evenness <- shannon / log(length(abundance_sum_community))
# create a dataframe 
pielou_df <- data.frame(trap = as.character(abundance_sum$trap), site = as.character(abundance_sum$site),
                        year = abundance_sum$year,
                        pielou_evenness = pielou_evenness)

# merge dataframes based on "year" and "site"
output_trap_level <- merge(output_trap_level, pielou_df, by = c("year", "site", "trap"))

# plot
ggplot(pielou_df, aes(x = as.factor(year), y = pielou_evenness, fill = site)) +
  geom_boxplot() +
  scale_fill_manual(values = palette) + 
  facet_wrap(~site, scales = "fixed", ncol = 3) +  # arrange in 2 rows, 3 columns
  labs(title = "Pielou Evenness on Trap Level",
       x = "Year",
       y = "Pielou Evenness") +
  theme_minimal() +
  theme(legend.position = "none",  # remove legend
        axis.text.x = element_text(angle = 45, hjust = 1),  # rotate x-axis labels
        plot.title = element_text(hjust = 0.5))  # center title

# remove unnecessary stuff
rm(abundance_sum, abundance_sum_community, community_matrix,community_sub, pielou_df, rarefied_community,
   richness, pielou_evenness, richness_boot, shannon, site, site_out, upround, x, year, year_out, community_data)


####### 4. Gawdis #######

# scaling 0-1
a_traits$voltinism <- (a_traits$voltinism - min(a_traits$voltinism, na.rm = TRUE)) / 
  (max(a_traits$voltinism, na.rm = TRUE) - min(a_traits$voltinism, na.rm = TRUE))
m_traits$body_length <- (m_traits$body_length - min(m_traits$body_length, na.rm = TRUE)) / 
  (max(m_traits$body_length, na.rm = TRUE) - min(m_traits$body_length, na.rm = TRUE))
m_traits$ITD <- (m_traits$ITD - min(m_traits$ITD, na.rm = TRUE)) / 
  (max(m_traits$ITD, na.rm = TRUE) - min(m_traits$ITD, na.rm = TRUE))
m_traits$foraging_range <- (m_traits$foraging_range - min(m_traits$foraging_range, na.rm = TRUE)) / 
  (max(m_traits$foraging_range, na.rm = TRUE) - min(m_traits$foraging_range, na.rm = TRUE))

# gawdis a_traits
a_t <- a_traits[,-c(1:2)]
gawdis_a_t <- gawdis(a_t, w.type = "optimized", groups = c(1,2,2,2,2,2,2,2,2,2,2,2,2), fuzzy = c(2))
gawdis_activity <- t(as.matrix(gawdis_a_t))
colnames(gawdis_activity) <- a_traits$species
rownames(gawdis_activity) <- a_traits$species
write.csv(gawdis_activity, paste0(datpath,"gawdis_activity"))

# gawdis m_traits
m_t <- m_traits[,-c(1:2)]
gawdis_m_t <- gawdis(m_t, w.type = "optimized", groups = c(1,2,3))
gawdis_morphology <- t(as.matrix(gawdis_m_t))
colnames(gawdis_morphology) <- m_traits$species
rownames(gawdis_morphology) <- m_traits$species
write.csv(gawdis_morphology, paste0(datpath,"gawdis_morphology.csv"))

# gawdis s_traits
s_t <- s_traits[,-c(1:2)]
gawdis_s_t <- gawdis(s_t, w.type = "optimized", groups = c(1,2,3,4,4))
gawdis_s_traits <- t(as.matrix(gawdis_s_t))
gawdis_lifestyle <- t(gawdis_s_traits)
colnames(gawdis_lifestyle) <- s_traits$species
rownames(gawdis_lifestyle) <- s_traits$species
write.csv(gawdis_lifestyle, paste0(datpath,"gawdis_lifestyle.csv"))

# remove unnecessary stuff
rm(a_t, m_t, s_t, gawdis_a_t, gawdis_m_t, gawdis_s_t, gawdis_a_traits,
   gawdis_m_traits, gawdis_s_traits)


####### 5. NMDS #######

NMDS_activity <- metaMDS(gawdis_activity, k = 2)
NMDS_morphology <- metaMDS(gawdis_morphology, k = 2)
NMDS_lifestyle <- metaMDS(gawdis_lifestyle, k = 2)

# Shepards test/goodness of fit
stressplot(NMDS_activity) 
stressplot(NMDS_morphology)
stressplot(NMDS_lifestyle)

# extract NMDS values
NMDS_activity <- NMDS_activity$points[, c(1,2)]
NMDS_morphology <- NMDS_morphology$points[, c(1,2)]
NMDS_lifestyle <- NMDS_lifestyle$points[, c(1,2)]

NMDS_axis <- data.frame(activity = NMDS_activity, morphology = NMDS_morphology,
                        lifestyle = NMDS_lifestyle)

### scaling 

## handling the negatives

# find the minimum values in activity, morphology and lifestyle
min_activity.MDS1 <- min(NMDS_axis$activity.MDS1)
min_activity.MDS2 <- min(NMDS_axis$activity.MDS2)
min_morphology.MDS1 <- min(NMDS_axis$morphology.MDS1)
min_morphology.MDS2 <- min(NMDS_axis$morphology.MDS2)
min_lifestyle.MDS1 <- min(NMDS_axis$lifestyle.MDS1)
min_lifestyle.MDS2 <- min(NMDS_axis$lifestyle.MDS2)

# add the minimum values to activity, morphology and lifestyle
NMDS_axis$activity.MDS1 <- NMDS_axis$activity.MDS1 + abs(min_activity.MDS1)
NMDS_axis$morphology.MDS1 <- NMDS_axis$morphology.MDS1 + abs(min_morphology.MDS1)
NMDS_axis$lifestyle.MDS1 <- NMDS_axis$lifestyle.MDS1 + abs(min_lifestyle.MDS1)
NMDS_axis$activity.MDS2 <- NMDS_axis$activity.MDS2 + abs(min_activity.MDS2)
NMDS_axis$morphology.MDS2 <- NMDS_axis$morphology.MDS2 + abs(min_morphology.MDS2)
NMDS_axis$lifestyle.MDS2 <- NMDS_axis$lifestyle.MDS2 + abs(min_lifestyle.MDS2)

# apply the scaling function
NMDS_axis$activity.MDS1 <- (NMDS_axis$activity.MDS1 - min(NMDS_axis$activity.MDS1, na.rm = TRUE)) / 
  (max(NMDS_axis$activity.MDS1, na.rm = TRUE) - min(NMDS_axis$activity.MDS1, na.rm = TRUE))
NMDS_axis$activity.MDS2 <- (NMDS_axis$activity.MDS2 - min(NMDS_axis$activity.MDS2, na.rm = TRUE)) / 
  (max(NMDS_axis$activity.MDS2, na.rm = TRUE) - min(NMDS_axis$activity.MDS2, na.rm = TRUE))
NMDS_axis$morphology.MDS1 <- (NMDS_axis$morphology.MDS1 - min(NMDS_axis$morphology.MDS1, na.rm = TRUE)) / 
  (max(NMDS_axis$morphology.MDS1, na.rm = TRUE) - min(NMDS_axis$morphology.MDS1, na.rm = TRUE))
NMDS_axis$morphology.MDS2 <- (NMDS_axis$morphology.MDS2 - min(NMDS_axis$morphology.MDS2, na.rm = TRUE)) / 
  (max(NMDS_axis$morphology.MDS2, na.rm = TRUE) - min(NMDS_axis$morphology.MDS2, na.rm = TRUE))
NMDS_axis$lifestyle.MDS1 <- (NMDS_axis$lifestyle.MDS1 - min(NMDS_axis$lifestyle.MDS1, na.rm = TRUE)) / 
  (max(NMDS_axis$lifestyle.MDS1, na.rm = TRUE) - min(NMDS_axis$lifestyle.MDS1, na.rm = TRUE))
NMDS_axis$lifestyle.MDS2 <- (NMDS_axis$lifestyle.MDS2 - min(NMDS_axis$lifestyle.MDS2, na.rm = TRUE)) / 
  (max(NMDS_axis$lifestyle.MDS2, na.rm = TRUE) - min(NMDS_axis$lifestyle.MDS2, na.rm = TRUE))

NMDS_axis <- cbind(activity = NMDS_axis$activity.MDS1+NMDS_axis$activity.MDS2,
                   morphology = NMDS_axis$morphology.MDS1+NMDS_axis$morphology.MDS2,
                   lifestyle = NMDS_axis$lifestyle.MDS1+NMDS_axis$lifestyle.MDS2)
NMDS_axis <- as.data.frame(NMDS_axis)

# apply the scaling function
NMDS_axis$activity <- (NMDS_axis$activity - min(NMDS_axis$activity, na.rm = TRUE)) / 
  (max(NMDS_axis$activity, na.rm = TRUE) - min(NMDS_axis$activity, na.rm = TRUE))
NMDS_axis$morphology <- (NMDS_axis$morphology - min(NMDS_axis$morphology, na.rm = TRUE)) / 
  (max(NMDS_axis$morphology, na.rm = TRUE) - min(NMDS_axis$morphology, na.rm = TRUE))
NMDS_axis$lifestyle <- (NMDS_axis$lifestyle - min(NMDS_axis$lifestyle, na.rm = TRUE)) / 
  (max(NMDS_axis$lifestyle, na.rm = TRUE) - min(NMDS_axis$lifestyle, na.rm = TRUE))
rownames(NMDS_axis) <- a_traits$species

### correlations 
NMDS_values <- NMDS_axis
NMDS_values$species <- rownames(NMDS_axis) # extract species names from NMDS_axis as column names

# activity
a_traits$nr_of_active_months <- rowSums(a_traits[, 4:15]) # add the number of active month
merged_activity <- merge(a_traits, NMDS_values, by = "species", all.x = TRUE)
merged_activity <- merged_activity[, c(3,16,17)]
chart.Correlation(merged_activity, histogram = F, pch = 19, method = "pearson")

# morphology
merged_morphology <- merge(m_traits, NMDS_values, by = "species", all.x = TRUE)
merged_morphology <- merged_morphology[, c(3,4,5,7)]
chart.Correlation(merged_morphology, histogram = F, pch = 19, method = "pearson")

# lifestyle
merged_lifestyle <- merge(s_traits, NMDS_values, by = "species", all.x = TRUE)
merged_lifestyle <- merged_lifestyle[, c(3,4,5,6,7,10)]
habitat_specialisation <- merged_lifestyle[, c(1,6)]
chart.Correlation(habitat_specialisation, histogram = F, pch = 19, method = "pearson")
# list of factor variables
factor_vars <- c("sociality", "lecty", "nesting_type", "nesting_habitat")
# loop through each factor variable and perform chi-square test
for (var in factor_vars) {
  contingency_table <- table(merged_lifestyle$lifestyle, merged_lifestyle[[var]])
  chi_square_result <- chisq.test(contingency_table)
  cat("Chi-square test between lifestyle and", var, ":\n")
  print(chi_square_result)
}

write.csv(NMDS_axis, paste0(datpath,"NMDS_axis.csv"))

rm(NMDS_activity, NMDS_morphology, NMDS_lifestyle, min_activity.MDS1, min_activity.MDS2,
   min_lifestyle.MDS1, min_lifestyle.MDS2, min_morphology.MDS1, min_morphology.MDS2, 
   merged_activity, merged_lifestyle, merged_morphology)


####### 6. TPD #######

### estimating the SD for each trait axis 
means <- NMDS_axis
sd <- sqrt(diag(Hpi.diag(means))) # axes_bandwidths
sds <- data.frame(
  activity = rep(sd[1], nrow(means)),
  morphology = rep(sd[2], nrow(means)),
  lifestyle = rep(sd[3], nrow(means))
)

# (code from Patrick: 'I got this from looking at the code from 
# https://www.science.org/doi/10.1126/sciadv.abf2675 - which is led my Carlos Carmona 
# the person who developed TPDs as a method and he also recommended this method to me 
# when I emailed him. I'm not entirely convinced by it by at least it has prior form.')

# The Hpi.diag() function calculates the squared Euclidean distances between points in 
# the NMDS space. The diag() function extracts the diagonal elements of the resulting 
# distance matrix, which represent the squared distances of each point from the origin 
# (i.e., the center of the coordinate system).
# Then, sqrt() is applied to these diagonal elements to obtain the square root of each 
# distance, which effectively gives the distance of each point from the origin in the 
# NMDS space. This distance represents the spread or dispersion of the points along each 
# NMDS axis.
# So, sd <- sqrt(diag(Hpi.diag(means))) calculates the standard deviations of the NMDS 
# axes by taking the square root of the squared distances of each point from the origin 
# in the NMDS space.

### TPDs
species <- as.factor(rownames(NMDS_axis))
TPDs <- TPDsMean(species, means, sds)

# WARNING FROM THE PACKAGE: "When TPDs are calculated using the TPDsMean function, 
# Evenness and Divergence are meaningless!!" -> Therefore I will use Richness and Redundancy!

#### a) TPDc on SITE level ####

#### FBG ####
# empty list to store the results
output_list <- list()
# loop for the specified number of iterations
for (i in 1:num_iterations) {
  # group by site and year
  grouped_data <- split(m_rarefied_by_days, list(m_rarefied_by_days$site, m_rarefied_by_days$year)) 
  # sample 11 traps for each group
  sampled_data <- lapply(grouped_data, function(group) {
    if (length(group) > min_number_traps) {
      group[sample(seq_along(group$trap), size = min_number_traps), ]
    } else {
      group  # keep all traps if there are only 11 traps
    }})
  # combine the sampled groups back into a dataframe
  m_rarefied_by_days_reduced_trap_level <- do.call(rbind, sampled_data)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_trap_level[,-2]
  m_rarefied_by_days_reduced_site_level <- aggregate(. ~ site + year, data = m_rarefied_by_days_reduced_trap_level, sum)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_site_level[,-3]
  # TPDc
  sampUnit <- subset(m_rarefied_by_days_reduced_site_level, site == "FBG")
  rownames(sampUnit) <- sampUnit$year
  sampUnit <- sampUnit[,-c(1:2)]
  TPDc_FBG <- TPDc(TPDs, sampUnit)
  # REND
  REND <- REND(TPDc_FBG, TPDs)
  functional_diversity_community_level <- as.data.frame(REND$communities)
  output_FBG <- cbind(functional_diversity_community_level)
  # redundancy
  redundancy <- redundancy(TPDc_FBG)
  output_FBG <- cbind(output_FBG, total_functional_redundancy = redundancy$redundancy, 
                      relative_functional_redundancy = redundancy$redundancyRelative)
  # append the result to the list
  output_list[[i]] <- output_FBG
}
# calculate the mean over all runs
mean_TPDc_output_FBG <- Reduce(`+`, output_list) / num_iterations
mean_TPDc_output_FBG$site <- "FBG"
mean_TPDc_output_FBG <- mean_TPDc_output_FBG[, c("site", names(mean_TPDc_output_FBG)[-ncol(mean_TPDc_output_FBG)])] # reorder the columns
mean_TPDc_output_FBG <- mean_TPDc_output_FBG[,-c(3,4)]
mean_TPDc_output_FBG <- rownames_to_column(mean_TPDc_output_FBG, var = "year")
mean_TPDc_output_FBG <- mean_TPDc_output_FBG[, c(2,1,3,4,5)]

#### GFH ####
output_list <- list()
for (i in 1:num_iterations) {
  grouped_data <- split(m_rarefied_by_days, list(m_rarefied_by_days$site, m_rarefied_by_days$year))
  sampled_data <- lapply(grouped_data, function(group) {
    if (length(group) > min_number_traps) {
      group[sample(seq_along(group$trap), size = min_number_traps), ]
    } else {group}})
  m_rarefied_by_days_reduced_trap_level <- do.call(rbind, sampled_data)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_trap_level[,-2]
  m_rarefied_by_days_reduced_site_level <- aggregate(. ~ site + year, data = m_rarefied_by_days_reduced_trap_level, sum)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_site_level[,-3]
  sampUnit <- subset(m_rarefied_by_days_reduced_site_level, site == "GFH")
  rownames(sampUnit) <- sampUnit$year
  sampUnit <- sampUnit[,-c(1:2)]
  TPDc_GFH <- TPDc(TPDs, sampUnit)
  REND <- REND(TPDc_GFH, TPDs)
  functional_diversity_community_level <- as.data.frame(REND$communities)
  output_GFH <- cbind(functional_diversity_community_level)
  redundancy <- redundancy(TPDc_GFH)
  output_GFH <- cbind(output_GFH, total_functional_redundancy = redundancy$redundancy, 
                      relative_functional_redundancy = redundancy$redundancyRelative)
  output_list[[i]] <- output_GFH
}
mean_TPDc_output_GFH <- Reduce(`+`, output_list) / num_iterations
mean_TPDc_output_GFH$site <- "GFH"
mean_TPDc_output_GFH <- mean_TPDc_output_GFH[, c("site", names(mean_TPDc_output_GFH)[-ncol(mean_TPDc_output_GFH)])] # reorder the columns
mean_TPDc_output_GFH <- mean_TPDc_output_GFH[,-c(3,4)]
mean_TPDc_output_GFH <- rownames_to_column(mean_TPDc_output_GFH, var = "year")
mean_TPDc_output_GFH <- mean_TPDc_output_GFH[, c(2,1,3,4,5)]

#### HAR ####
output_list <- list()
for (i in 1:num_iterations) {
  grouped_data <- split(m_rarefied_by_days, list(m_rarefied_by_days$site, m_rarefied_by_days$year))
  sampled_data <- lapply(grouped_data, function(group) {
    if (length(group) > min_number_traps) {
      group[sample(seq_along(group$trap), size = min_number_traps), ]
    } else {group}})
  m_rarefied_by_days_reduced_trap_level <- do.call(rbind, sampled_data)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_trap_level[,-2]
  m_rarefied_by_days_reduced_site_level <- aggregate(. ~ site + year, data = m_rarefied_by_days_reduced_trap_level, sum)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_site_level[,-3]
  sampUnit <- subset(m_rarefied_by_days_reduced_site_level, site == "HAR")
  rownames(sampUnit) <- sampUnit$year
  sampUnit <- sampUnit[,-c(1:2)]
  TPDc_HAR <- TPDc(TPDs, sampUnit)
  REND <- REND(TPDc_HAR, TPDs)
  functional_diversity_community_level <- as.data.frame(REND$communities)
  output_HAR <- cbind(functional_diversity_community_level)
  redundancy <- redundancy(TPDc_HAR)
  output_HAR <- cbind(output_HAR, total_functional_redundancy = redundancy$redundancy, 
                      relative_functional_redundancy = redundancy$redundancyRelative)
  output_list[[i]] <- output_HAR
}
mean_TPDc_output_HAR <- Reduce(`+`, output_list) / num_iterations
mean_TPDc_output_HAR$site <- "HAR"
mean_TPDc_output_HAR <- mean_TPDc_output_HAR[, c("site", names(mean_TPDc_output_HAR)[-ncol(mean_TPDc_output_HAR)])] # reorder the columns
mean_TPDc_output_HAR <- mean_TPDc_output_HAR[,-c(3,4)]
mean_TPDc_output_HAR <- rownames_to_column(mean_TPDc_output_HAR, var = "year")
mean_TPDc_output_HAR <- mean_TPDc_output_HAR[, c(2,1,3,4,5)]

#### SIP ####
output_list <- list()
for (i in 1:num_iterations) {
  grouped_data <- split(m_rarefied_by_days, list(m_rarefied_by_days$site, m_rarefied_by_days$year))
  sampled_data <- lapply(grouped_data, function(group) {
    if (length(group) > min_number_traps) {
      group[sample(seq_along(group$trap), size = min_number_traps), ]
    } else {group}})
  m_rarefied_by_days_reduced_trap_level <- do.call(rbind, sampled_data)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_trap_level[,-2]
  m_rarefied_by_days_reduced_site_level <- aggregate(. ~ site + year, data = m_rarefied_by_days_reduced_trap_level, sum)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_site_level[,-3]
  sampUnit <- subset(m_rarefied_by_days_reduced_site_level, site == "SIP")
  rownames(sampUnit) <- sampUnit$year
  sampUnit <- sampUnit[,-c(1:2)]
  TPDc_SIP <- TPDc(TPDs, sampUnit)
  REND <- REND(TPDc_SIP, TPDs)
  functional_diversity_community_level <- as.data.frame(REND$communities)
  output_SIP <- cbind(functional_diversity_community_level)
  redundancy <- redundancy(TPDc_SIP)
  output_SIP <- cbind(output_SIP, total_functional_redundancy = redundancy$redundancy, 
                      relative_functional_redundancy = redundancy$redundancyRelative)
  output_list[[i]] <- output_SIP
}
mean_TPDc_output_SIP <- Reduce(`+`, output_list) / num_iterations
mean_TPDc_output_SIP$site <- "SIP"
mean_TPDc_output_SIP <- mean_TPDc_output_SIP[, c("site", names(mean_TPDc_output_SIP)[-ncol(mean_TPDc_output_SIP)])] # reorder the columns
mean_TPDc_output_SIP <- mean_TPDc_output_SIP[,-c(3,4)]
mean_TPDc_output_SIP <- rownames_to_column(mean_TPDc_output_SIP, var = "year")
mean_TPDc_output_SIP <- mean_TPDc_output_SIP[, c(2,1,3,4,5)]


#### SST ####
output_list <- list()
for (i in 1:num_iterations) {
  grouped_data <- split(m_rarefied_by_days, list(m_rarefied_by_days$site, m_rarefied_by_days$year))
  sampled_data <- lapply(grouped_data, function(group) {
    if (length(group) > min_number_traps) {
      group[sample(seq_along(group$trap), size = min_number_traps), ]
    } else {group}})
  m_rarefied_by_days_reduced_trap_level <- do.call(rbind, sampled_data)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_trap_level[,-2]
  m_rarefied_by_days_reduced_site_level <- aggregate(. ~ site + year, data = m_rarefied_by_days_reduced_trap_level, sum)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_site_level[,-3]
  sampUnit <- subset(m_rarefied_by_days_reduced_site_level, site == "SST")
  rownames(sampUnit) <- sampUnit$year
  sampUnit <- sampUnit[,-c(1:2)]
  TPDc_SST <- TPDc(TPDs, sampUnit)
  REND <- REND(TPDc_SST, TPDs)
  functional_diversity_community_level <- as.data.frame(REND$communities)
  output_SST <- cbind(functional_diversity_community_level)
  redundancy <- redundancy(TPDc_SST)
  output_SST <- cbind(output_SST, total_functional_redundancy = redundancy$redundancy, 
                      relative_functional_redundancy = redundancy$redundancyRelative)
  output_list[[i]] <- output_SST
}
mean_TPDc_output_SST <- Reduce(`+`, output_list) / num_iterations
mean_TPDc_output_SST$site <- "SST"
mean_TPDc_output_SST <- mean_TPDc_output_SST[, c("site", names(mean_TPDc_output_SST)[-ncol(mean_TPDc_output_SST)])] # reorder the columns
mean_TPDc_output_SST <- mean_TPDc_output_SST[,-c(3,4)]
mean_TPDc_output_SST <- rownames_to_column(mean_TPDc_output_SST, var = "year")
mean_TPDc_output_SST <- mean_TPDc_output_SST[, c(2,1,3,4,5)]

#### WAN ####
output_list <- list()
for (i in 1:num_iterations) {
  grouped_data <- split(m_rarefied_by_days, list(m_rarefied_by_days$site, m_rarefied_by_days$year))
  sampled_data <- lapply(grouped_data, function(group) {
    if (length(group) > min_number_traps) {
      group[sample(seq_along(group$trap), size = min_number_traps), ]
    } else {group}})
  m_rarefied_by_days_reduced_trap_level <- do.call(rbind, sampled_data)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_trap_level[,-2]
  m_rarefied_by_days_reduced_site_level <- aggregate(. ~ site + year, data = m_rarefied_by_days_reduced_trap_level, sum)
  m_rarefied_by_days_reduced_site_level <- m_rarefied_by_days_reduced_site_level[,-3]
  sampUnit <- subset(m_rarefied_by_days_reduced_site_level, site == "WAN")
  rownames(sampUnit) <- sampUnit$year
  sampUnit <- sampUnit[,-c(1:2)]
  TPDc_WAN <- TPDc(TPDs, sampUnit)
  REND <- REND(TPDc_WAN, TPDs)
  functional_diversity_community_level <- as.data.frame(REND$communities)
  output_WAN <- cbind(functional_diversity_community_level)
  redundancy <- redundancy(TPDc_WAN)
  output_WAN <- cbind(output_WAN, total_functional_redundancy = redundancy$redundancy, 
                      relative_functional_redundancy = redundancy$redundancyRelative)
  output_list[[i]] <- output_WAN
}
mean_TPDc_output_WAN <- Reduce(`+`, output_list) / num_iterations
mean_TPDc_output_WAN$site <- "WAN"
mean_TPDc_output_WAN <- mean_TPDc_output_WAN[, c("site", names(mean_TPDc_output_WAN)[-ncol(mean_TPDc_output_WAN)])] # reorder the columns
mean_TPDc_output_WAN <- mean_TPDc_output_WAN[,-c(3,4)]
mean_TPDc_output_WAN <- rownames_to_column(mean_TPDc_output_WAN, var = "year")
mean_TPDc_output_WAN <- mean_TPDc_output_WAN[, c(2,1,3,4,5)]

# Combine the two data frames
mean_TPDc_parameters_site_level <- rbind(mean_TPDc_output_FBG, mean_TPDc_output_GFH,
                                         mean_TPDc_output_HAR, mean_TPDc_output_SIP,
                                         mean_TPDc_output_SST, mean_TPDc_output_WAN)

# merge dataframes based on "year" and "site"
output_site_level <- merge(output_site_level, mean_TPDc_parameters_site_level, by = c("year", "site"))
output_site_level$functional_richness <- output_site_level$FRichness
output_site_level <- output_site_level[,-5]

write.csv(mean_TPDc_output_WAN, paste0(datpath,"mean_TPDc_output_WAN.csv"))

# remove unneccessary stuff
rm(mean_TPDc_output_FBG, mean_TPDc_output_GFH, mean_TPDc_output_HAR, mean_TPDc_output_SIP,
   mean_TPDc_output_SST, mean_TPDc_output_WAN, output_FBG, output_GFH, output_HAR, output_SIP, 
   output_SST, output_WAN, m_rarefied_by_days_reduced_site_level, m_rarefied_by_days_reduced_trap_level,
   output_list, redundancy, REND)

write.csv(output_site_level, paste0(datpath,"output_site_level.csv"))
write.csv(output_trap_level, paste0(datpath,"output_trap_level.csv"))

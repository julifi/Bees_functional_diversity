## Global Setting ####################################################
# to un-comit the last changes put in the terminal: git reset HEAD~

# BEFORE: run setting.R
setwd(wrkpath)
getwd()
# Data Path with all Nexus_Indicators
datpath <- wrkpath %+% "data/"


## Load Libraries ############################################################# 
library(readr); library(plyr); library(dplyr); library(stringr); library(tidyr); library(data.table); library(purrr)
library(countrycode); library(readxl); library(terra); library(sf); library(exactextractr); library(raster)
library(rlang); library(openxlsx); library(ggplot2); library(ggridges); library(vegan)

# load in colour palette
cbPalette6 <- c( "#D0B541",  "#7EB875", "#57A2AC", "#4E78C4",  "#CE2220","#E67F33")
rainbow10<-c(rgb(82,25,19,maxColorValue=255),"#CE2220","#E67F33", "#D0B541",  "#7EB875", "#57A2AC", "#4E78C4",  
             rgb(130,77,153,maxColorValue=255),rgb(185,151,198,maxColorValue=255),rgb(231,235,250,maxColorValue=255))

########## A) Data preparation #########

### A1) read in data #####
# raw data: 2019-2021
dat_all <- read.csv('analysis_bees_diversity/data/data_raw/Bee_Data2010-2021_with_quality_scores.csv',sep = ';', dec = ',')

# correct species spelling mistakes
dat_all$GenSpec[which(dat_all$GenSpec=="Sphaecodes miniatus")]<-"Sphecodes miniatus"
dat_all$GenSpec[which(dat_all$GenSpec=="Andrena coitana\xff(Kirby")]<-"Andrena coitana"

# there are some dates that are not read in correctly - correct that
x<-substr(dat_all$Start, 0,4)
y<-2010:2021
other.format<-which(is.element(x,y)==F)

z<-as.Date(dat_all$Start)
z[(other.format)]<- as.Date(dat_all$Start[(other.format)], format = "%d.%m.%Y")
dat_all$Start<-z

z<-as.Date(dat_all$End)
z[(other.format)]<- as.Date(dat_all$End[(other.format)], format = "%d.%m.%Y")
dat_all$End<-z

rm(z,x,y,other.format)

# there are some spaces in the location name of one landscape (HAR) - we have to remove them
dat_all$Loc[which(dat_all$Loc=='HAR ')]<-'HAR'

# create species list
spec.list<-unique(dat_all$GenSpec); spec.list<-spec.list[order(spec.list)]

# trait data
traits <- read_excel("analysis_bees_diversity/data/data_raw/all_traits.xlsx")
#rename species name (replace '.' with ' '):
traits$species <- gsub(".", " ", traits$species, fixed = TRUE)

# traits_raw <- read_excel("analysis_bees_diversity/data/data_raw/Traits_TerenoBees_20210913_updated.xlsx", skip=1)
# 
# # rename colnames in traits_raw data
# colnames(traits_raw)[c(1,5,6,9,10)]<- c("species", "ITD_mean_f_[mm]", "foraging range_ff_[km]", "habitat_specialisation", "sociality")

# A2) Deal with the quality scores
# quality scores of 3 and 4 are quite bad and maybe should be removed
#dat_all<- dat_all[-which(dat_all$QualityCheck==3 | dat_all$QualityCheck==4),]

# quality score of 5 indicates that the trap was slightly moved - we are for now not worried about it
# and set it to 0, as it is not affect trap quality per se...
dat_all$QualityCheck[which(dat_all$QualityCheck==5)]<-0



# A3) corrections of data entry mistakes
# there is one sample that is associated to the wrong year, we need to correct that// Mark said the correction is wrong, so let's remove it
dat_all$year<-year(dat_all$Start)
dat_all$uniqueID<-paste0(dat_all$TrapLoc, dat_all$year)

dat_all<-dat_all[-which(dat_all$uniqueID=='HAR152020' & dat_all$Start==as.Date("2020-08-18")),]

colnames(dat_all)[c(8, 10:11, 13)]<-c('LocName', 'StartDate', 'EndDate', 'LocTrap')

# further there is a wrong sampling starting date for one spring sampling in spring 2012 - correct that:
# location info has been added as security measure
dat_all$StartDate[which(dat_all$year=='2012' & dat_all$StartDate == as.Date('2012-05-14', '%Y-%m-%d') & 
                          (dat_all$LocName=='FBG'|dat_all$LocName=='SST'))]<- as.Date('2012-05-15', '%Y-%m-%d')
dat_all$StartDate[which(dat_all$year=='2012' & dat_all$StartDate == as.Date('2012-05-15', '%Y-%m-%d') & 
                          (dat_all$LocName=='WAN'|dat_all$LocName=='HAR'))]<- as.Date('2012-05-16', '%Y-%m-%d')
dat_all$StartDate[which(dat_all$year=='2012' & dat_all$StartDate == as.Date('2012-05-16', '%Y-%m-%d') & 
                          (dat_all$LocName=='GFH'|dat_all$LocName=='SIP'))]<- as.Date('2012-05-17', '%Y-%m-%d')

### A3) take all social bees - I would exclude the male social bees as those are probably not contributing to pollination. #####
communal<-c(traits$species[which(traits$sociality=='communal')],'Apis mellifera')

# merging parameter showing whether males should be used or not
dat_all$consider_males<-1
dat_all$consider_males[which(is.na(match(dat_all$GenSpec,communal))==F)]<-0

rm(communal)

### A4) create meta-data containing all location-year combinations ####
# create and trim the meta-data
meta<- aggregate(list(dat_all$Male), by=list(dat_all$LocName, dat_all$LocTrap, dat_all$year), 
                 function(x){mean(x, na.rm=T)})
meta<-meta[,1:3]; colnames(meta)<-c("LocName", "LocTrap","year")

# create a unique ID that can be used for the species data
meta$uniqueID<-paste0(meta$LocTrap,meta$year)

### A5) add sampling time and season to meta-data ####
# (i) first add season to dat_all
dat_all$start.day<-yday(dat_all$StartDate)
hist(dat_all$start.day) # day 180 is a good separation between seasons

dat_all$season<-'spring'
dat_all$season[dat_all$start.day>=180]<-'summer'

# (ii) now create a season meta data:
x<-which(dat_all$season=='spring')
meta.spring<- aggregate(list(dat_all$Male[x]), 
                        by=list(dat_all$LocName[x], dat_all$LocTrap[x], dat_all$year[x],
                                dat_all$StartDate[x], dat_all$EndDate[x], dat_all$QualityCheck[x]), 
                        function(x){mean(x, na.rm=T)})
colnames(meta.spring)<-c('LocName', 'LocTrap', 'year', 'StartDate', 'EndDate','QC')
meta.spring<-meta.spring[,-7]
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
meta.summer<- aggregate(list(dat_all$Male[x]), 
                        by=list(dat_all$LocName[x], dat_all$LocTrap[x], dat_all$year[x],
                                dat_all$StartDate[x], dat_all$EndDate[x], dat_all$QualityCheck[x]), 
                        function(x){mean(x, na.rm=T)})
colnames(meta.summer)<-c('LocName', 'LocTrap', 'year', 'StartDate', 'EndDate', 'QC')
meta.summer<-meta.summer[,-7]

# (vi) we can now add a starting date and an end date of the summer season to the meta data
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
# options for rarefaction thresholds if only days of exposure are considered (in my opinion)
length(which(meta$total.exposure<64))
length(which(meta$total.exposure<69))

rm(y, y.2, y.3, end.gap, ends, gaps, no.of.gaps, start.gap, starts)

# (xi) make some small changes in the variable names and create meta-data for each sample
# rename column names in the meta data
colnames(meta)[c(1,2)]<-c('landscape','site')
colnames(meta.spring)[c(1,2)]<-c('landscape','site'); colnames(meta.summer)[c(1,2)]<-c('landscape','site')

# combine the two season meta data
meta.spring$season<-'spring'; meta.summer$season<-'summer'
meta.sample<-rbind(meta.spring, meta.summer)

# create unique sample ID
meta.sample$sample_ID<-paste0(meta.sample$uniqueID,'_', meta.sample$StartDate)

# add honey bee abundance to sample meta-data
honey.bees<-dat_all[which(dat_all$GenSpec=='Apis mellifera'),]
honey.bees$sample_ID<-paste0(honey.bees$uniqueID,'_', honey.bees$StartDate)

meta.sample$honey.bee.abund<-0
meta.sample$honey.bee.abund[match(honey.bees$sample_ID, meta.sample$sample_ID)]<-
  honey.bees$Female+honey.bees$Male

# add honey bee abundance to sample site-year meta data
x<-aggregate(meta.sample$honey.bee.abund, by=list(meta.sample$uniqueID), function(x){sum(x)})
meta$honey.bee.abund[match(x$Group.1, meta$uniqueID)]<-x$x

# add info about mean sampling date
meta.sample$mean.sampling.date<- 
  yday(meta.sample$StartDate) + (yday(meta.sample$EndDate)-yday(meta.sample$StartDate))/2

rm(x, honey.bees, meta.spring, meta.summer)

###### B) create species matrices (abundance and biomass) ########
### B.1) General preparations ####
# (i) create new column on abundance of both females and males
# use: merging parameter showing whether males should be used or not: consider_males (1: males shall be considered; 0: males shall not be considered)
dat_all$Females_Males <- dat_all$Female + dat_all$consider_males*dat_all$Male

# (ii) remove honey bees from spec list
spec.list<- spec.list[which(spec.list!='Apis mellifera')]
dat_all<-dat_all[-which(dat_all$GenSpec=='Apis mellifera'),]

# (iii) add biomass to dat_all
#traits$`mean_body_length_f_[mm]`

# missing species in trait data
missing.species.traits <- unique(dat_all$GenSpec) [unique(dat_all$GenSpec) %in% unique(traits$species)==F]

# To do
# A) clarify missing species traits - are there any errors in species identity/ are there some other issues/
#    add missing trait data
# B) clarify how to handle missing size data for male bees. 
# C) create biomass data matrix

# (iv) add species richness to sample meta-data 
rich<-aggregate(dat_all$Females_Males,  by=list( dat_all$LocName, dat_all$LocTrap, dat_all$year, dat_all$StartDate), 
                            function(x){length(which(x>0))}) 
colnames(rich)<-c('landscape','site','year', 'StartDate', 'richness')
rich$sample_ID<-paste0(rich$site,rich$year,'_',rich$StartDate)

meta.sample$richness<-0
meta.sample$richness[match(rich$sample_ID,meta.sample$sample_ID)]<- rich$richness

# (iv) add total abundance to sample meta-data 
abund<-aggregate(dat_all$Females_Males,  by=list( dat_all$LocName, dat_all$LocTrap, dat_all$year, dat_all$StartDate), 
                function(x){sum(x)}) 
colnames(abund)<-c('landscape','site','year', 'StartDate', 'abund')
abund$sample_ID<-paste0(abund$site,abund$year,'_',abund$StartDate)

meta.sample$total.abundance<-0
meta.sample$total.abundance[match(abund$sample_ID,meta.sample$sample_ID)]<- abund$abund

rm(rich, abund)

# some diagnostics
hist(meta.sample$total.abundance[which(meta.sample$total.abundance<200)], breaks  = 35)
plot(meta.sample$total.abundance[which(meta.sample$total.abundance<500)], 
     meta.sample$richness[which(meta.sample$total.abundance<500)])

meta.sample$total.abundance[which(meta.sample$total.abundance>500)]

#hmm that is really quite high... is this realistic?

# (v) create meta data for site-year-season  
meta.season.site<-rbind(meta[ ,1:4], meta[ ,1:4])
meta.season.site$season<-rep(c('spring','summer'), each = nrow(meta))
meta.season.site$StartDate<-c(meta$spring.start, meta$summer.start)
meta.season.site$EndDate<-c(meta$spring.end, meta$summer.end)
meta.season.site$gap.start<-c(meta$spring.gap.start, meta$summer.gap.start)
meta.season.site$gap.end<-c(meta$spring.gap.end, meta$summer.gap.end)
meta.season.site$exposure<-c(meta$spring.exposure, meta$summer.exposure)

# remove rows where no sampling took place
meta.season.site<-meta.season.site[-which(is.na(meta.season.site$StartDate)),]

### B.2) create matrix for  total abundance (f and m (without m of communal species)) at sample level ####

# there are some species entered twice per sample, let's find out which those are
x<-as.data.frame(table(as.character(dat_all$StartDate), dat_all$uniqueID, dat_all$GenSpec ))
x<-x[which(x$Freq>1),]
x$year<-substr(as.character(x$Var2),6,9)
colnames(x)[1:3]<-c('StartDate','Trap_Year','Species')

write.csv(x,'analysis_bees_diversity/data/double_samples.csv')

# create a data frame where all samples are contained that have one double species
x<- dat_all[,c(20,10,4,24)]
cm.sample<-as.data.frame(pivot_wider(data = x, names_from = GenSpec, values_from = Females_Males, values_fn ={sum}, values_fill = 0))

# format columns
x<-cm.sample[,c(1,2)]; cm.sample<-cm.sample[,-c(1,2)]

# add rows where we have no bees captured - i.e. there were only honey bees and hence the
# community data frame is shorter than the meta-data frame
x$sample_ID<-paste0(x$uniqueID,'_',x$StartDate)
y<-matrix(0, nrow = length(which(is.element(meta.sample$sample_ID,x$sample_ID)==F)),ncol = ncol(cm.sample))
colnames(y)<-colnames(cm.sample)
cm.sample<-rbind(cm.sample, y)

x<-rbind(x, meta.sample[which(is.element(meta.sample$sample_ID,x$sample_ID)==F)  ,c(8,4,10)])

# now we have to order the columns in the CM so that they match with the meta-data
cm.sample<-cm.sample[match(meta.sample$sample_ID,x$sample_ID),]

### B.3) create matrix for  total abundance (f and m (without m of communal species)) at site-season level ####

# create a data frame where all samples are contained that have one double species
x<- dat_all[,c(20,23,4,24)]
cm.season.site<-as.data.frame(pivot_wider(data = x, names_from = GenSpec, values_from = Females_Males, values_fn ={sum}, values_fill = 0))

# format columns
x<-cm.season.site[,c(1,2)]; cm.season.site<-cm.season.site[,-c(1,2)]

# add rows where we have no bees captured - i.e. there were only honey bees and hence the
# community data frame is shorter than the meta-data frame
meta.season.site$season.site.ID<-paste0(meta.season.site$uniqueID,'_',meta.season.site$season)
x$season.site.ID<-paste0(x$uniqueID,'_',x$season)

y<-matrix(0, nrow = length(which(is.element(meta.season.site$season.site.ID,x$season.site.ID)==F)),ncol = ncol(cm.season.site))
colnames(y)<-colnames(cm.season.site)

cm.season.site<-rbind(cm.season.site, y)

x<-rbind(x, meta.season.site[which(is.element(meta.season.site$season.site.ID, x$season.site.ID)==F)  ,c(4,5,11)])

# now we have to order the columns in the cm so that they match with the meta-data
cm.season.site<-cm.season.site[match(meta.season.site$season.site.ID,x$season.site.ID),]

### B.4) create matrix for  total abundance (f and m (without m of communal species)) at site-year level ####
# select relevant columns
x<- dat_all[,c(20,19,4,24)]
cm.year.site<-as.data.frame(pivot_wider(data = x, names_from = GenSpec, values_from = Females_Males, values_fn ={sum}, values_fill = 0))

# format columns
x<-cm.year.site[,c(1,2)]; cm.year.site<-cm.year.site[,-c(1,2)]

# now we have to order the columns in the cm so that they match with the meta-data
cm.year.site<-cm.year.site[match(meta$uniqueID,x$uniqueID),]
x<-x[match(meta$uniqueID,x$uniqueID),] # works well

# order all CMs alphabetically 
colnames(cm.season.site)<-colnames(cm.season.site)[order(colnames(cm.season.site))]
colnames(cm.sample)<-colnames(cm.sample)[order(colnames(cm.sample))]
colnames(cm.year.site)<-colnames(cm.year.site)[order(colnames(cm.year.site))]

rm(x,y)

### B.5) add species richness and total abundance to the meta data at site and season-site level  ####
meta.season.site$richness<-specnumber(cm.season.site)
meta$richness<-specnumber(cm.year.site)

meta.season.site$total.abundance<-rowSums(cm.season.site)
meta$total.abundance<-rowSums(cm.year.site)

### B.6) save all the data  ####

# write meta data
write.csv(meta.sample,'analysis_bees_diversity/data/meta_sample.csv')
write.csv(meta,'analysis_bees_diversity/data/meta_site_year.csv')
write.csv(meta.season.site,'analysis_bees_diversity/data/meta_season_site.csv')

# write community data
write.csv(cm.sample,'analysis_bees_diversity/data/cm_sample.csv')
write.csv(cm.year.site,'analysis_bees_diversity/data/cm_year_site.csv')
write.csv(cm.season.site,'analysis_bees_diversity/data/cm_season_site.csv')




# To do:

# (v) create matrix total biomass combined (f and m (without communal species))
# create new column on abundance of both females and males

# add length data to abundance data on year-trap-season level and calculate biomass:
abundance_year_trap$mean_body_length_f_[mm] <- abundance_year_trap
traits$`mean_body_length_f_[mm]`

# species for which we already have trait data
x <- unique(traits$species)
# all species for which we have abundance data
y <- unique(abundance_year_trap$GenSpec)

# species for which we have to add trait data 
y[which(y%in%x == FALSE)] 

which(abundance_year_trap$GenSpec=="Sphaecodes miniatus" == TRUE)

abundance_year_trap$uniqueID[which(abundance_year_trap$GenSpec=="Sphaecodes miniatus")]
abundance_year_trap$uniqueID[which(abundance_year_trap$GenSpec=="Sphecodes miniatus")]




########## C) Create a list that contains for each year-season-site combination the exposure days #########
# This is needed for the environmental data (T, rainfall) extraction
sampling.days<-list(); meta.sampling.days<-c()

for (i in 1:nrow(meta)){
  #this is a wrapper in case the whole season is missing (no data)
  if(is.na(meta$spring.start[i])==F){
    
  # first get all day of the years in the spring
  if(is.na(meta$spring.gap.start[i])){ # here you get day of the year if you have no gap in sampling
    dyear.spring<-yday(meta$spring.start)[i]:yday(meta$spring.end)[i]
  }else{ # here you get day of the year if you have a gap in sampling
    dyear.spring<-c(yday(meta$spring.start)[i]:yday(meta$spring.gap.start)[i],
                    yday(meta$spring.gap.end)[i]:yday(meta$spring.end)[i])
  }
  # define which days are start or end days of the sampling period - start is 1, end is 2
  startend.spring<-rep(0,length(dyear.spring))
  # these are obvious start and end days
  startend.spring[1]<-1; startend.spring[length(startend.spring)]<-2
  # account for the start and end of gaps
  startend.spring[which(dyear.spring==yday(meta$spring.gap.start[i]))]<-2
  startend.spring[which(dyear.spring==yday(meta$spring.gap.end[i]))]<-1
  # now reconvert dyear.spring into date format
  dates.spring<-as.Date(dyear.spring-1, origin = paste0(year(meta$spring.start[i]),"-01-01"))
  # and get months and day of the month
  spring.month<-c(month(dates.spring)); spring.day<-c(mday(dates.spring))
  spring<-data.frame(dates.spring, spring.month, spring.day, startend.spring)
  
  # append the data
  sampling.days<-c(sampling.days, list(spring))
  meta.sampling.days<-rbind(meta.sampling.days, data.frame(
    LocTrap=rep(meta$LocTrap[i],1), year=rep(meta$year[i],1), season=c('spring')))
  }
  
  # now let's get the same for summer 
  
  #this is a wrapper in case the whole season is missing (no data)
  if(is.na(meta$summer.start[i])==F){
    # here starts the summer code
  if(is.na(meta$summer.gap.start[i])){ # here you get day of the year if you have no gap in sampling
    dyear.summer<-yday(meta$summer.start)[i]:yday(meta$summer.end)[i]
  }else{ # here you get day of the year if you have a gap in sampling
    dyear.summer<-c(yday(meta$summer.start)[i]:yday(meta$summer.gap.start)[i],
                    yday(meta$summer.gap.end)[i]:yday(meta$summer.end)[i])
  }
  # define which days are start or end days of the sampling period - start is 1, end is 2
  startend.summer<-rep(0,length(dyear.summer))
  # these are obvious start and end days
  startend.summer[1]<-1; startend.summer[length(startend.summer)]<-2
  # account for the start and end of gaps
  startend.summer[which(dyear.summer==yday(meta$summer.gap.start[i]))]<-2
  startend.summer[which(dyear.summer==yday(meta$summer.gap.end[i]))]<-1
  # now reconvert dyear.summer into date format
  dates.summer<-as.Date(dyear.summer-1, origin = paste0(year(meta$summer.start[i]),"-01-01"))
  # and get months and day of the month
  summer.month<-c(month(dates.summer)); summer.day<-c(mday(dates.summer))
  summer<-data.frame(dates.summer, summer.month, summer.day, startend.summer)
  
  sampling.days<-c(sampling.days, list(summer))
    meta.sampling.days<-rbind(meta.sampling.days, data.frame(
    LocTrap=rep(meta$LocTrap[i],1), year=rep(meta$year[i],1), season=c('summer')))
  }
  }

# comment: some sampling seasons are completely missing. Hence, the list is not exactly twice as long as the 
# meta data-frame. 

rm(spring.month, spring.day, dyear.spring, dates.spring, startend.spring,
   summer.month, summer.day, dyear.summer, dates.summer, startend.summer)

# save the list as file (open with readRDS)
saveRDS(sampling.days, file="analysis_bees_diversity/data/sampling_days_siteyseason.RData")
write.csv(meta.sampling.days, 'analysis_bees_diversity/data/meta_sampling_days_siteyseason.csv')










########## D) Comparison of spring and summer data #########

### 1) create a first overview of the total abundance ratios
# create a ratio between summer and spring abundance that takes the exposure days of the seasons into account
meta$season.ratios<- (rowSums(cm.ab.summer)/ meta$summer.exposure) / (rowSums(cm.ab.spring)/ meta$spring.exposure)

hist(meta$season.ratios[which(meta$season.ratios<5)])
summary(meta$season.ratios)

### 2) check species specific results
# define thresholds for abundance per year (e.g. 30) and then a threshold for number of year-trap combinations that full fill that requirement

# (i) create summer-spring ratios for all species that meet the requirement
abundance.thr<-25; n.thr<-30
sp.season.ratios<-list(); sp.season.ratios.plot<- data.frame(species =c(), summer_spring_ratios = c())
for(i in 1:ncol(cm.ab.total)){
  data.points<-which(cm.ab.total[,i]>=abundance.thr)
  if(length(data.points)>=n.thr){sp.season.ratios[[i]]<-(cm.ab.summer[data.points,i]/ meta$summer.exposure[data.points]) / 
      (cm.ab.spring[data.points,i]/ meta$spring.exposure[data.points])
      x<-data.frame(species =rep(spec.list[i]), 
                    summer_spring_ratios = c((cm.ab.summer[data.points,i]/ meta$summer.exposure[data.points]) / 
                                                               (cm.ab.spring[data.points,i]/ meta$spring.exposure[data.points])))
      sp.season.ratios.plot<-rbind(sp.season.ratios.plot,x)
  }
}

# (ii) plot the differences in species
sp.season.ratios.plot$summer_spring_ratios[which(is.infinite(sp.season.ratios.plot$summer_spring_ratios))]<-
  max(sp.season.ratios.plot$summer_spring_ratios)

ggplot(sp.season.ratios.plot, aes(x = log(summer_spring_ratios+1), y = species, fill = species)) + geom_boxplot() +theme_bw()

ggplot(sp.season.ratios.plot, aes(x=log(summer_spring_ratios+1) ,y=species,fill=species))+
  geom_density_ridges(alpha = 0.5,jittered_points = TRUE, point_alpha=1,point_shape=21) + 
  labs(x="Summer-spring ratio (logged)",y='')+ 
  guides(fill=FALSE,color=FALSE) + theme_bw()

rm(data.points, sp.season.ratios, sp.season.ratios.plot, abundance.thr, n.thr,x)


### 3) plot a PCA of the community matrix to check for the impact of season... 
library(factoextra) #http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

# (i) merge spring and summer data
cm.ab.two_season<-rbind(cm.ab.spring, cm.ab.summer)
meta.two_season<-rbind(meta[,1:4], meta[,1:4])
meta.two_season$season<-c(rep('spring', nrow(meta)), rep('summer', nrow(meta)))

cm.ab.two_season<-as.data.frame(cm.ab.two_season)
cm.ab.two_season.mod<-apply(cm.ab.two_season,1, function(x){x/sum(x)})
cm.ab.two_season.mod<-as.data.frame(t(cm.ab.two_season.mod))

x<-which(is.nan(rowSums(cm.ab.two_season.mod)))

cm.ab.two_season[672,] # there are some samples that contain no individuals (probably they only contained honey bees...)

cm.ab.two_season.mod[x,]<- rep(0, ncol(cm.ab.two_season.mod))

res.pca <- prcomp((cm.ab.two_season.mod), scale = F) # species need to be in columns
pca<-as.data.frame(res.pca$x)

# check explained variance
# summary(res.pca)

plot<-data.frame(meta.two_season, pca[,c(1:5)])
cbPalette7<-c(cbPalette6, "gray")

#define colour for helo
colour.points<- rep("#505050", nrow(plot))
colour.points[plot$season=="summer"]<- "#D8D8D8"

#create new shape column to combine species and location
plot$shape.var<-as.factor(plot$LocName)

# 500*340; LocName; season, year

ggplot(plot, aes(x=(PC1), y=PC2, group = season, color=as.factor(year))) + theme_bw() +
  stat_ellipse( aes(x=PC1, y=PC2, group=season), linetype = "longdash", size = 0.5, col = "lightgray") + 
  geom_hline(yintercept=0, color = "darkgrey", linetype = "dashed", size = 0.5)+
  geom_vline(xintercept=0, color = "darkgrey", linetype = "dashed", size = 0.5)+
  geom_point(size=4.5, colour = colour.points, fill = colour.points) + geom_point(size=2, alpha = 1, fill="white", colour = "white") + 
  geom_point(size=2.4, alpha = 0.2)
  

# # 
# ggplot(plot, aes(x=PC1, y=PC2, colour=as.factor(year), shape = shape.var)) + theme_bw() +
#   stat_ellipse( aes(x=PC1, y=PC2, group=season), linetype = "longdash", size = 0.5, col = "lightgray") + 
#   geom_hline(yintercept=0, color = "darkgrey", linetype = "dashed", size = 0.5)+
#   geom_vline(xintercept=0, color = "darkgrey", linetype = "dashed", size = 0.5)+
#   geom_point(size=4.5, colour = colour.points, fill = colour.points) + geom_point(size=2, alpha = 1, fill="white", colour = "white") + 
#   geom_point(size=2.4, colour = stage("black", after_scale = alpha(color, .01)), alpha = 1) + 
#   #geom_text(hjust=0, vjust=0) + shows that CS26 and CS27 have been mixed up during the metabolomic processing!
#   #stat_ellipse(linetype = "solid", size = 0.5) + 
#   scale_colour_manual(values=c(rainbow10[c(2,7)] ))+ scale_shape_manual(values=c(24,22,21,25,23)) + 
#   scale_fill_manual(values=c(rainbow10[c(2,7)] ))














# next step:
# control creation of species matrix
# create presence-absence matrix total 
# create biomass matrix (get male size data)
# transmit biomass matrix into metabolic rates
# add honey bee abundance (females) to meta-data
# think about removing trap-year combinations that contain 0 individuals (or which are below a certain threshold)









## Script Lili:
m <- read.csv2("analysis_bees_diversity/data/community_matrix_female.csv")

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

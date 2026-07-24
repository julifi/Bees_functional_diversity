
## Load Libraries ############################################################# 
library(readr); library(plyr); library(dplyr); library(stringr); library(tidyr); library(data.table); library(purrr)
library(countrycode); library(readxl); library(terra); library(sf); library(exactextractr); library(raster)
library(rlang); library(openxlsx); library(ggplot2); library(ggridges); library(vegan)

# remove.packages("Matrix")
# remove.packages("lme4")
install.packages("lme4", type = "source")
library(lme4)

# 1. load in data ----
meta.sample<- read.csv('analysis_bees_diversity/data/meta_sample.csv',sep = ',', dec = '.')
meta.season.site <- read.csv('analysis_bees_diversity/data/meta_season_site.csv',sep = ',', dec = '.')
#meta <- read.csv('analysis_bees_diversity/data/meta_site_year.csv',sep = ',', dec = '.')

weather.data<- read.csv('analysis_bees_diversity/data/data_weather/all_dat_temp_prec.csv',sep = ',', dec = '.')
# convert the dates into a date format
weather.data$hour.x<- as.POSIXct(weather.data$hour.x, format = "%Y-%m-%d %H:%M:%S")
# add day of the year to calculate exposure days
weather.data$yday <- yday(weather.data$hour.x)



# do the same for the meta data dates
meta.sample$StartDate<- as.POSIXct( paste0(meta.sample$StartDate, ' 12:00:00'), format = "%Y-%m-%d %H:%M:%S")
meta.sample$EndDate<- as.POSIXct( paste0(meta.sample$EndDate, ' 12:00:00'), format = "%Y-%m-%d %H:%M:%S")

meta.season.site$StartDate<- as.POSIXct( paste0(meta.season.site$StartDate, ' 12:00:00'), format = "%Y-%m-%d %H:%M:%S")
meta.season.site$EndDate<- as.POSIXct( paste0(meta.season.site$EndDate, ' 12:00:00'), format = "%Y-%m-%d %H:%M:%S")

# ## standardize predictors: 
# meta.sample$exposure_z <- scale(meta.sample$exposure)
# meta.sample$starting.day_z <- scale(meta.sample$starting.day)

# 1.1 data preparation ----
library(lme4); library(lmerTest);

# we work with log abundance & log biomass to make outliers less influential
meta.sample$log.ab<-log(meta.sample$total.abundance+1)
#meta.sample$log.ab.fem<-log(meta.sample$female.abundance+1)

meta.sample$log.bio.ITD<-log(meta.sample$total.biomass_ITD+1)
meta.sample$log.bio.BL<-log(meta.sample$total.biomass_BL+1)

# exclude extreme abundance values > 1000: as
# meta.sample$total.abundance.no.extr <- meta.sample$total.abundance
# meta.sample$total.abundance.no.extr[which(meta.sample$total.abundance.no.extr>1000)] <- NA
# meta.sample$log.ab.no.extr<-log(meta.sample$total.abundance.no.extr+1)

# we work with log abundance to make outliers less influential
meta.season.site$log.ab<-log(meta.season.site$total.abundance+1)
#meta.season.site$log.ab.fem<-log(meta.season.site$female.abundance+1)
meta.season.site$log.bio.ITD<-log(meta.season.site$total.biomass.ITD+1)
meta.season.site$log.bio.BL<-log(meta.season.site$total.biomass.BL+1)

# # exclude extreme abundance values > 1000: as
# meta.season.site$total.abundance.no.extr <- meta.season.site$total.abundance
# meta.season.site$total.abundance.no.extr[which(meta.season.site$total.abundance.no.extr>1000)] <- NA
# meta.season.site$log.ab.no.extr<-log(meta.season.site$total.abundance.no.extr+1)

## add day of the year of the mean day of the respective sampling interval:
# add starting day and end day of each sampling interval into the meta.sample data
library(lubridate)
meta.sample$starting.day <- yday(meta.sample$StartDate)
meta.sample$end.day <- yday(meta.sample$EndDate)
meta.sample$mean.day <- ceiling((meta.sample$starting.day + meta.sample$end.day)/2)

meta.season.site$starting.day <- yday(meta.season.site$StartDate)
meta.season.site$end.day <- yday(meta.season.site$EndDate)
meta.season.site$mean.day <- ceiling((meta.season.site$starting.day + meta.season.site$end.day)/2)

# add astronomical spring and summer start (day of the year respectively) to caluclate starting day within season
start_spring_summer <- read_excel("analysis_bees_diversity/data/data_raw/astronomical_start_spring_summer.xlsx")
start_spring_summer$yday_start_spring <- yday(start_spring_summer$start_spring)
start_spring_summer$yday_start_summer <- yday(start_spring_summer$start_summer)
start_spring_summer$yday_start_autumn <- yday(start_spring_summer$start_autumn)

start_spring_summer$length_spring <- (start_spring_summer$yday_start_summer-1) - start_spring_summer$yday_start_spring
start_spring_summer$intervall_length_spring <- start_spring_summer$length_spring/3
start_spring_summer$length_summer <- (start_spring_summer$yday_start_autumn-1)  - start_spring_summer$yday_start_summer
start_spring_summer$intervall_length_summer <- start_spring_summer$length_summer/3

start_spring_summer$start_spring_1 <- start_spring_summer$yday_start_spring
start_spring_summer$start_spring_2 <- start_spring_summer$start_spring_1 + ceiling(start_spring_summer$intervall_length_spring)
start_spring_summer$start_spring_3 <- start_spring_summer$start_spring_2 + ceiling(start_spring_summer$intervall_length_spring)

start_spring_summer$start_summer_1 <- start_spring_summer$yday_start_summer
start_spring_summer$start_summer_2 <- start_spring_summer$start_summer_1 + ceiling(start_spring_summer$intervall_length_summer)
start_spring_summer$start_summer_3 <- start_spring_summer$start_summer_2 + ceiling(start_spring_summer$intervall_length_summer)

meta.sample <- meta.sample %>%
  left_join(start_spring_summer, by = "year") %>%
  mutate(start.season = if_else(
    season == "spring",
    start_spring,
    start_summer
  )) %>%
  dplyr::select(-start_spring, -start_summer, -source_start_spring_summer, -start_autumn, 
                -length_spring, -length_summer, -intervall_length_spring, -intervall_length_summer,
                -yday_start_spring, -yday_start_summer)

# transform spring and summer start dates into day of the year values:
meta.sample$start.season <- as.POSIXct(meta.sample$start.season , format = "%Y-%m-%d")
meta.sample$start.season_yday <- yday(meta.sample$start.season) 
# calcuate year of the day within the spring or summer season:  
meta.sample$mean_day_within_season <- meta.sample$mean.day - meta.sample$start.season_yday
meta.sample$mean_day_within_season.qd <- (meta.sample$mean_day_within_season)^2


meta.season.site <- meta.season.site %>%
  left_join(start_spring_summer, by = "year") %>%
  mutate(start.season = if_else(
    season == "spring",
    start_spring,
    start_summer
  )) %>%
  dplyr::select(-start_spring, -start_summer, -source_start_spring_summer, -start_autumn)

# transform spring and summer start dates into day of the year values:
meta.season.site$start.season <- as.POSIXct(meta.season.site$start.season , format = "%Y-%m-%d")
meta.season.site$start.season_yday <- yday(meta.season.site$start.season) 
# calcuate year of the day within the spring or summer season:  
meta.season.site$mean_day_within_season <- meta.season.site$mean.day - meta.season.site$start.season_yday
meta.season.site$mean_day_within_season.qd <- (meta.season.site$mean_day_within_season)^2


## 1.2 create input.data list with data-frame including all daylight sampling hours for each sample ----
### 1.2.1 both seasons ----
# create a list that contains for each sample (row in meta.sample) a data-frame with all daylight sampling hours 
#### 1.2.1.1 2 week sampling ----
input.data<-list()
for(i in 1:nrow(meta.sample)){
  input.data[[i]]<-weather.data[which(weather.data$hour.x>=meta.sample$StartDate[i] & weather.data$hour.x<meta.sample$EndDate[i] &
                                        weather.data$trap==meta.sample$site[i]) , ]
}

#### 1.2.1.2 6 week sampling -----
input.data.6<-list()
for(i in 1:nrow(meta.season.site)){
  input.data.6[[i]]<-weather.data[which(weather.data$hour.x>=meta.season.site$StartDate[i] & weather.data$hour.x<meta.season.site$EndDate[i] &
                                        weather.data$trap==meta.season.site$site[i]) , ]
}

### 1.2.2 spring season ----
## create list that contains for each sample (row in meta.sample) a data-frame with all daylight sampling hours for SPRING
meta.sample.spring <- dplyr::filter(meta.sample, 
                                   `season` == "spring")

input.data.spring<-list()
for(i in 1:nrow(meta.sample.spring)){
  input.data.spring[[i]]<-weather.data[which(weather.data$hour.x>=meta.sample.spring$StartDate[i] & weather.data$hour.x<meta.sample.spring$EndDate[i] &
                                        weather.data$trap==meta.sample.spring$site[i]) , ]
}

### 1.2.3 summer season ----
## create list that contains for each sample (row in meta.sample) a data-frame with all daylight sampling hours for SUMMER
meta.sample.summer <- dplyr::filter(meta.sample, 
                                    `season` == "summer")
input.data.summer<-list()
for(i in 1:nrow(meta.sample.summer)){
  input.data.summer[[i]]<-weather.data[which(weather.data$hour.x>=meta.sample.summer$StartDate[i] & weather.data$hour.x<meta.sample.summer$EndDate[i] &
                                               weather.data$trap==meta.sample.summer$site[i]) , ]
}

rm(weather.data)



# 1.3 visualisation of relationships ----
library(ggplot2)
ggplot(data = meta.sample, aes(y = log(total.abundance), x = exposure, fill = as.factor(year), shape = landscape))+ geom_jitter(size = 2.5, width = 0.2) + theme_bw() +
  scale_shape_manual(values= c(21:25,21))

ggplot(data = meta.sample[which(meta.sample$season=='spring'),], aes(y = log(total.abundance), x = exposure, fill = as.factor(year), shape = landscape))+ 
  geom_jitter(size = 2.5, width = 0.2) + theme_bw() +
  scale_shape_manual(values= c(21:25,21))

ggplot(data = meta.sample[which(meta.sample$season=='summer'),], aes(y = log(total.abundance), x = exposure, fill = as.factor(year), shape = landscape))+ 
  geom_jitter(size = 2.5, width = 0.2) + theme_bw() +
  scale_shape_manual(values= c(21:25,21))

ggplot(data = meta.sample[which(meta.sample$season=='summer'),], aes(y = log(total.abundance), x = exposure, fill = as.factor(year), shape = landscape))+ 
  geom_jitter(size = 2.5, width = 0.2) + theme_bw() +
  scale_shape_manual(values= c(21:25,21)) + facet_wrap(~ landscape)

ggplot(data = meta.sample[which(meta.sample$season=='spring'),], aes(y = log(total.abundance), x = exposure, fill = as.factor(year), shape = landscape))+ 
  geom_jitter(size = 2.5, width = 0.2) + theme_bw() +
  scale_shape_manual(values= c(21:25,21)) + facet_wrap(~ landscape + as.factor(year))

# conclusion: at aggregated level, there is a lot of noise and no patterns can be seen
# the higher the differentation across year, season and landscape, the more clearly an impact of exposure days is visible. 
# Hence, it would probably be good to also account for exposure days in our models (and not only for season)




# 1.4 determine testing and training data ----

## 1.4.1 2 week sampling ----
set.seed(42)

testing.ID<-sample(1:nrow(meta.sample), round(nrow(meta.sample)*0.2))
training.data<- meta.sample[-testing.ID, ]
testing.data<- meta.sample[testing.ID, ]


## 1.4.2 6 week sampling ----
set.seed(42)

testing.ID.6<-sample(1:nrow(meta.season.site), round(nrow(meta.season.site)*0.2))
training.data.6<- meta.season.site[-testing.ID.6, ]
testing.data.6<- meta.season.site[testing.ID.6, ]


# 1.4.2 SPRING & SUMMER ----
set.seed(42)
# SPRING: 
testing.ID.spring<-sample(1:nrow(meta.sample.spring), round(nrow(meta.sample.spring)*0.2))
training.data.spring<- meta.sample.spring[-testing.ID.spring, ]
testing.data.spring<- meta.sample.spring[testing.ID.spring, ]

# SUMMER: 
set.seed(42)

testing.ID.summer <- sample(1:nrow(meta.sample.summer), round(nrow(meta.sample.summer)*0.2))
training.data.summer<- meta.sample.summer[-testing.ID.summer, ]
testing.data.summer<- meta.sample.summer[testing.ID.summer, ]

# 2. compute suitability scores for wild-bee pollination for each sampling interval  -------
## 2.1 create constant.grids
### 2.1.1 without plateau ----
# there are three constants in the formula that defines the suitability of temp for pollination
# here, we define their range
t.opt <- seq(5,27, length=15) # optimal temperature - highest activity
t.max <- seq(25,45, length=15) # maximal temperature - defines when activity becomes 0 
sigma <- seq(0.5,5, length=12) # defines the shape of the sigmoid shape of bee activity below t.opt
constants.grid<- expand.grid(t.opt, t.max, sigma)
names(constants.grid) <- c("t.opt", "t.max", "sigma")

# account for the fact that max temp needs to be at least 1 degree above opt. temperature
constants.grid<- constants.grid[which(constants.grid$t.opt<=constants.grid$t.max-1),]
rm(t.opt, t.max, sigma)

### 2.1.2 with plateau -----
# there are three constants in the formula that defines the suitability of temp for pollination
# here, we define their range
t.opt.min <- seq(5,25, length=15) # minimum plateau temperature - minimum temperature for highest activity
t.opt.max <- seq(6,35, length=15) #  maximum plateau temperature - maximum temperature for highest activity
t.max <- seq(35,50, length=10)  # maximal temperature - defines when activity becomes 0 
sigma <- seq(0.5,5, length=8) # defines the shape of the sigmoid shape of bee activity below t.opt
constants.grid2<- expand.grid(t.opt.min, t.opt.max, t.max, sigma)
names(constants.grid2) <- c("t.opt.min", "t.opt.max", "t.max", "sigma")

# account for the fact that max pleateau temp needs to be at least 1 degree above min plateau temperature temperature
constants.grid2<- constants.grid2[which(constants.grid2$t.opt.min<=constants.grid2$t.opt.max-1),]
# account for the fact that max temp needs to be at least 1 degree above max plateau (opt) temperature
constants.grid2<- constants.grid2[which(constants.grid2$t.opt.max<=constants.grid2$t.max-1),]
# define a max length of the plateau
constants.grid2<- constants.grid2[which(constants.grid2$t.opt.max-constants.grid2$t.opt.min<=12),]

rm(t.opt.min, t.opt.max, t.max, sigma)




## 2.2 create suitability matrix -----
### 2.2.1 2-week sampling ------
## 2.2.1.1 Normalised suitability score WITHOUT plateau --> output: suitability.matrix.norm ----
# create output data frame that will contain for each sample (column) all different suitability scores
output<- c()

# we create a procedure that will be implemented for each sampling period in a loop
for (i in 1:length(input.data)){
  
  # we extract the climate data of a given sampling period
  placeholder<-  input.data[[i]]
  
  # this was for testing the code and see whether the relationships are correctly coded 
  #placeholder<- data.frame(temp=seq(0,35, length=1300), prec = sample(c(0,0,0,0,0),100, replace = T))
  
  # prepare output data-frame for a given sampling period
  output.period<-c()
  
  # we compute for each hour the suitability score for each combinations of constants in the grid
  for(j in 1:nrow(constants.grid)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$prec==0 & placeholder$temp <= constants.grid$t.opt[j])
    above.opt<-which(placeholder$prec==0 & placeholder$temp > constants.grid$t.opt[j])
    
    # compute the suitability score for temp above and below the temp optimum separately 
    suitability.estimate[below.opt]<- exp(-((placeholder$temp[below.opt]-constants.grid$t.opt[j])/
                                              (2*constants.grid$sigma[j]))^2)
    suitability.estimate[above.opt]<- 1-((placeholder$temp[above.opt]-constants.grid$t.opt[j])/
                                           (constants.grid$t.opt[j]- constants.grid$t.max[j]))^2
    
    # negative suitability values need to be set to 0
    suitability.estimate[which(suitability.estimate<0)]<-0
    
    # diagnostics - works well
    #plot(suitability.estimate~placeholder$temp)
    
    # output for each sampling period needs to be prepared and saved
    suitability.score<-sum(suitability.estimate)
    # normalise suitability score by exposure days
    exposure.days <- length(unique(input.data[[i]]$yday))
    suitability.score<-suitability.score/exposure.days
    
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix.norm<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)

## 2.2.1.2 Normalised suitability score WITH plateau --> output: suitability.matrix2.norm ----
# create output data frame that will contain for each sample (column) all different suitability scores
output<- c()
# we create a procedure that will be implemented for each sampling period in a loop
for (i in 1:length(input.data)){
  
  # we extract the climate data of a given sampling period
  placeholder<-  input.data[[i]]
  
  # this was for testing the code and see whether the relationships are correctly coded 
  # placeholder<- data.frame(temp=seq(0,35, length=1300), prec = sample(c(0,0,0,0,0),100, replace = T))
  
  # prepare output data-frame for a given sampling period
  output.period<-c()
  
  # we compute for each hour the suitability score for each combinations of constants in the grid
  for(j in 1:nrow(constants.grid2)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$prec==0 & placeholder$temp <= constants.grid2$t.opt.min[j]) # below plateau
    opt<- which(placeholder$prec==0 & placeholder$temp > constants.grid2$t.opt.min [j] & placeholder$temp <= constants.grid2$t.opt.max[j])# plateau
    above.opt<-which(placeholder$prec==0 & placeholder$temp > constants.grid2$t.opt.max[j]) # above plateau
    
    # compute the suitability score for temp above, below and at the temp optimum plateau separately 
    suitability.estimate[below.opt]<- exp(-((placeholder$temp[below.opt]-constants.grid2$t.opt.min[j])/
                                              (2*constants.grid2$sigma[j]))^2)
    suitability.estimate[opt]<- 1
    suitability.estimate[above.opt]<- 1-((placeholder$temp[above.opt]-constants.grid2$t.opt.max[j])/
                                           (constants.grid2$t.opt.max[j]- constants.grid2$t.max[j]))^2
    
    # negative suitability values need to be set to 0
    suitability.estimate[which(suitability.estimate<0)]<-0
    
    # diagnostics - works well
    #plot(suitability.estimate~placeholder$temp)
    
    # output for each sampling period needs to be prepared and saved
    suitability.score<-sum(suitability.estimate)
    # normalise suitability score by exposure days
    exposure.days <- length(unique(input.data[[i]]$yday))
    suitability.score<-suitability.score/exposure.days
    
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix2.norm<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)

## 2.2.1.3 SPRING Normalised suitability score WITHOUT plateau --> output: suitability.matrix.norm.spring ----
# create output data frame that will contain for each sample (column) all different suitability score
output<- c()

# we create a procedure that will be implemented for each sampling period in a loop
for (i in 1:length(input.data.spring)){
  
  # we extract the climate data of a given sampling period
  placeholder<-  input.data.spring[[i]]
  
  # this was for testing the code and see whether the relationships are correctly coded 
  #placeholder<- data.frame(temp=seq(0,35, length=1300), prec = sample(c(0,0,0,0,0),100, replace = T))
  
  # prepare output data-frame for a given sampling period
  output.period<-c()
  
  # we compute for each hour the suitability score for each combinations of constants in the grid
  for(j in 1:nrow(constants.grid)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$prec==0 & placeholder$temp <= constants.grid$t.opt[j])
    above.opt<-which(placeholder$prec==0 & placeholder$temp > constants.grid$t.opt[j])
    
    # compute the suitability score for temp above and below the temp optimum separately 
    suitability.estimate[below.opt]<- exp(-((placeholder$temp[below.opt]-constants.grid$t.opt[j])/
                                              (2*constants.grid$sigma[j]))^2)
    suitability.estimate[above.opt]<- 1-((placeholder$temp[above.opt]-constants.grid$t.opt[j])/
                                           (constants.grid$t.opt[j]- constants.grid$t.max[j]))^2
    
    # negative suitability values need to be set to 0
    suitability.estimate[which(suitability.estimate<0)]<-0
    
    # diagnostics - works well
    #plot(suitability.estimate~placeholder$temp)
    
    # output for each sampling period needs to be prepared and saved
    suitability.score<-sum(suitability.estimate)
    # normalise suitability score by exposure days
    exposure.days <- length(unique(input.data[[i]]$yday))
    suitability.score<-suitability.score/exposure.days
    
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix.norm.spring<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)

## 2.2.1.4 SPRING Normalised suitability score WITH plateau --> output: suitability.matrix2.norm.spring ----
# create output data frame that will contain for each sample (column) all different suitability scores
output<- c()

# we create a procedure that will be implemented for each sampling period in a loop
for (i in 1:length(input.data.spring)){
  
  # we extract the climate data of a given sampling period
  placeholder<-  input.data.spring[[i]]
  
  # this was for testing the code and see whether the relationships are correctly coded 
  # placeholder<- data.frame(temp=seq(0,35, length=1300), prec = sample(c(0,0,0,0,0),100, replace = T))
  
  # prepare output data-frame for a given sampling period
  output.period<-c()
  
  # we compute for each hour the suitability score for each combinations of constants in the grid
  for(j in 1:nrow(constants.grid2)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$prec==0 & placeholder$temp <= constants.grid2$t.opt.min[j]) # below plateau
    opt<- which(placeholder$prec==0 & placeholder$temp > constants.grid2$t.opt.min [j] & placeholder$temp <= constants.grid2$t.opt.max[j])# plateau
    above.opt<-which(placeholder$prec==0 & placeholder$temp > constants.grid2$t.opt.max[j]) # above plateau
    
    # compute the suitability score for temp above, below and at the temp optimum plateau separately 
    suitability.estimate[below.opt]<- exp(-((placeholder$temp[below.opt]-constants.grid2$t.opt.min[j])/
                                              (2*constants.grid2$sigma[j]))^2)
    suitability.estimate[opt]<- 1
    suitability.estimate[above.opt]<- 1-((placeholder$temp[above.opt]-constants.grid2$t.opt.max[j])/
                                           (constants.grid2$t.opt.max[j]- constants.grid2$t.max[j]))^2
    
    # negative suitability values need to be set to 0
    suitability.estimate[which(suitability.estimate<0)]<-0
    
    # diagnostics - works well
    #plot(suitability.estimate~placeholder$temp)
    
    # output for each sampling period needs to be prepared and saved
    suitability.score<-sum(suitability.estimate)
    # normalise suitability score by exposure days
    exposure.days <- length(unique(input.data[[i]]$yday))
    suitability.score<-suitability.score/exposure.days
    
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix2.norm.spring<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)

## 2.2.1.5 SUMMER Normalised suitability score WITHOUT plateau --> output: suitability.matrix.norm.summer ----
# create output data frame that will contain for each sample (column) all different suitability scores
output<- c()

# we create a procedure that will be implemented for each sampling period in a loop
for (i in 1:length(input.data.summer)){
  
  # we extract the climate data of a given sampling period
  placeholder<-  input.data.summer[[i]]
  
  # this was for testing the code and see whether the relationships are correctly coded 
  #placeholder<- data.frame(temp=seq(0,35, length=1300), prec = sample(c(0,0,0,0,0),100, replace = T))
  
  # prepare output data-frame for a given sampling period
  output.period<-c()
  
  # we compute for each hour the suitability score for each combinations of constants in the grid
  for(j in 1:nrow(constants.grid)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$prec==0 & placeholder$temp <= constants.grid$t.opt[j])
    above.opt<-which(placeholder$prec==0 & placeholder$temp > constants.grid$t.opt[j])
    
    # compute the suitability score for temp above and below the temp optimum separately 
    suitability.estimate[below.opt]<- exp(-((placeholder$temp[below.opt]-constants.grid$t.opt[j])/
                                              (2*constants.grid$sigma[j]))^2)
    suitability.estimate[above.opt]<- 1-((placeholder$temp[above.opt]-constants.grid$t.opt[j])/
                                           (constants.grid$t.opt[j]- constants.grid$t.max[j]))^2
    
    # negative suitability values need to be set to 0
    suitability.estimate[which(suitability.estimate<0)]<-0
    
    # diagnostics - works well
    #plot(suitability.estimate~placeholder$temp)
    
    # output for each sampling period needs to be prepared and saved
    suitability.score<-sum(suitability.estimate)
    # normalise suitability score by exposure days
    exposure.days <- length(unique(input.data[[i]]$yday))
    suitability.score<-suitability.score/exposure.days
    
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix.norm.summer<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)

## 2.2.1.6 SUMMER Normalised suitability score WITH plateau --> output: suitability.matrix2.norm.summer ----
# create output data frame that will contain for each sample (column) all different suitability scores
output<- c()

# we create a procedure that will be implemented for each sampling period in a loop
for (i in 1:length(input.data.summer)){
  
  # we extract the climate data of a given sampling period
  placeholder<-  input.data.summer[[i]]
  
  # this was for testing the code and see whether the relationships are correctly coded 
  # placeholder<- data.frame(temp=seq(0,35, length=1300), prec = sample(c(0,0,0,0,0),100, replace = T))
  
  # prepare output data-frame for a given sampling period
  output.period<-c()
  
  # we compute for each hour the suitability score for each combinations of constants in the grid
  for(j in 1:nrow(constants.grid2)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$prec==0 & placeholder$temp <= constants.grid2$t.opt.min[j]) # below plateau
    opt<- which(placeholder$prec==0 & placeholder$temp > constants.grid2$t.opt.min [j] & placeholder$temp <= constants.grid2$t.opt.max[j])# plateau
    above.opt<-which(placeholder$prec==0 & placeholder$temp > constants.grid2$t.opt.max[j]) # above plateau
    
    # compute the suitability score for temp above, below and at the temp optimum plateau separately 
    suitability.estimate[below.opt]<- exp(-((placeholder$temp[below.opt]-constants.grid2$t.opt.min[j])/
                                              (2*constants.grid2$sigma[j]))^2)
    suitability.estimate[opt]<- 1
    suitability.estimate[above.opt]<- 1-((placeholder$temp[above.opt]-constants.grid2$t.opt.max[j])/
                                           (constants.grid2$t.opt.max[j]- constants.grid2$t.max[j]))^2
    
    # negative suitability values need to be set to 0
    suitability.estimate[which(suitability.estimate<0)]<-0
    
    # diagnostics - works well
    #plot(suitability.estimate~placeholder$temp)
    
    # output for each sampling period needs to be prepared and saved
    suitability.score<-sum(suitability.estimate)
    # normalise suitability score by exposure days
    exposure.days <- length(unique(input.data[[i]]$yday))
    suitability.score<-suitability.score/exposure.days
    
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix2.norm.summer<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)




### 2.2.2 6-week sampling ------
## 2.2.2.1 Normalised suitability score WITHOUT plateau --> output: suitability.matrix.6.norm ----
# create output data frame that will contain for each sample (column) all different suitability scores
output<- c()

# we create a procedure that will be implemented for each sampling period in a loop
for (i in 1:length(input.data.6)){
  
  # we extract the climate data of a given sampling period
  placeholder<-  input.data.6[[i]]
  
  # this was for testing the code and see whether the relationships are correctly coded 
  #placeholder<- data.frame(temp=seq(0,35, length=1300), prec = sample(c(0,0,0,0,0),100, replace = T))
  
  # prepare output data-frame for a given sampling period
  output.period<-c()
  
  # we compute for each hour the suitability score for each combinations of constants in the grid
  for(j in 1:nrow(constants.grid)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$prec==0 & placeholder$temp <= constants.grid$t.opt[j])
    above.opt<-which(placeholder$prec==0 & placeholder$temp > constants.grid$t.opt[j])
    
    # compute the suitability score for temp above and below the temp optimum separately 
    suitability.estimate[below.opt]<- exp(-((placeholder$temp[below.opt]-constants.grid$t.opt[j])/
                                              (2*constants.grid$sigma[j]))^2)
    suitability.estimate[above.opt]<- 1-((placeholder$temp[above.opt]-constants.grid$t.opt[j])/
                                           (constants.grid$t.opt[j]- constants.grid$t.max[j]))^2
    
    # negative suitability values need to be set to 0
    suitability.estimate[which(suitability.estimate<0)]<-0
    
    # diagnostics - works well
    #plot(suitability.estimate~placeholder$temp)
    
    # output for each sampling period needs to be prepared and saved
    suitability.score<-sum(suitability.estimate)
    # normalise suitability score by exposure days
    exposure.days <- length(unique(input.data.6[[i]]$yday))
    suitability.score<-suitability.score/exposure.days
    
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix.6.norm<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)


## 2.2.2.2 Normalised suitability score WITH plateau --> output: suitability.matrix2.6.norm ----
# create output data frame that will contain for each sample (column) all different suitability scores
output<- c()

# we create a procedure that will be implemented for each sampling period in a loop
for (i in 1:length(input.data.6)){
  
  # we extract the climate data of a given sampling period
  placeholder<-  input.data.6[[i]]
  
  # this was for testing the code and see whether the relationships are correctly coded 
  # placeholder<- data.frame(temp=seq(0,35, length=1300), prec = sample(c(0,0,0,0,0),100, replace = T))
  
  # prepare output data-frame for a given sampling period
  output.period<-c()
  
  # we compute for each hour the suitability score for each combinations of constants in the grid
  for(j in 1:nrow(constants.grid2)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$prec==0 & placeholder$temp <= constants.grid2$t.opt.min[j]) # below plateau
    opt<- which(placeholder$prec==0 & placeholder$temp > constants.grid2$t.opt.min [j] & placeholder$temp <= constants.grid2$t.opt.max[j])# plateau
    above.opt<-which(placeholder$prec==0 & placeholder$temp > constants.grid2$t.opt.max[j]) # above plateau
    
    # compute the suitability score for temp above, below and at the temp optimum plateau separately 
    suitability.estimate[below.opt]<- exp(-((placeholder$temp[below.opt]-constants.grid2$t.opt.min[j])/
                                              (2*constants.grid2$sigma[j]))^2)
    suitability.estimate[opt]<- 1
    suitability.estimate[above.opt]<- 1-((placeholder$temp[above.opt]-constants.grid2$t.opt.max[j])/
                                           (constants.grid2$t.opt.max[j]- constants.grid2$t.max[j]))^2
    
    # negative suitability values need to be set to 0
    suitability.estimate[which(suitability.estimate<0)]<-0
    
    # diagnostics - works well
    #plot(suitability.estimate~placeholder$temp)
    
    # output for each sampling period needs to be prepared and saved
    suitability.score<-sum(suitability.estimate)
    # normalise suitability score by exposure days
    exposure.days <- length(unique(input.data.6[[i]]$yday))
    suitability.score<-suitability.score/exposure.days
    
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix2.6.norm<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)




# 3. Determine the optimal constant combination (best.constants) for computing the suitability score  -------
# choose one model which serves as base to determine best constants (suitability score): use most complex model 
# --> including exposure, season:exposure, log(starting day), season:log(starting day)
## --> use these best constants for ALL models during model selection (exception: separate spring/summer models)

### 3.1 2-week sampling ------
## 3.1.1 using Normalised suitability score WITHOUT plateau --> best.constants ----
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix.norm)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix.norm[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season + 
                      exposure + season:exposure + 
                      log(mean_day_within_season) + season:log(mean_day_within_season) +
                      (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season +  
                      exposure + season:exposure + 
                      log(mean_day_within_season) + season:log(mean_day_within_season) +
                      (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output$min<-apply(mod.output,1,function(x){min(x)})

best.constants<- which(mod.output$min==min(mod.output$min))
constants.grid[best.constants,]

interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

## 3.1.2 using Normalised suitability score WITH plateau --> best.constants2 ----
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix2.norm)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix2.norm[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat,
              log.ab~ suitability.score + season + suitability.score:season + 
                      exposure + season:exposure + 
                      log(mean_day_within_season) + season:log(mean_day_within_season) +
                      (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat,
              log.ab~ suitability.score + season + 
                      exposure + season:exposure + 
                      log(mean_day_within_season) + season:log(mean_day_within_season) +
                      (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output2<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output2$min<-apply(mod.output2,1,function(x){min(x)})

best.constants2<-which(mod.output2$min==min(mod.output2$min))

constants.grid2[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]


## 3.1.3 using SPRING Normalised suitability score WITHOUT plateau --> best.constants.spring ----
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix.norm.spring)){
  dat<-data.frame(training.data.spring, suitability.score=suitability.matrix.norm.spring[-testing.ID.spring,i])
  # without interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + 
                exposure + 
                log(mean_day_within_season) + 
                (1|landscape/site)+(1|year), REML = T)
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.1))
  print(i)
}
mod.output.spring<-data.frame(AIC.without.interaction)
mod.output.spring$min<-apply(mod.output.spring,1,function(x){min(x)})

best.constants.spring<- which(mod.output.spring$min==min(mod.output.spring$min))

constants.grid[best.constants.spring,]

## 3.1.4 using SPRING Normalised suitability score WITH plateau --> best.constants2.spring ----
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix2.norm.spring)){
  dat<-data.frame(training.data.spring, suitability.score=suitability.matrix2.norm.spring[-testing.ID.spring,i])
  # without interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + 
                exposure + 
                log(mean_day_within_season) + 
                (1|landscape/site)+(1|year), REML = T)
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.1))
  print(i)
}
mod.output2.spring<-data.frame(AIC.without.interaction)
mod.output2.spring$min<-apply(mod.output2.spring,1,function(x){min(x)})

best.constants2.spring<- which(mod.output2.spring$min==min(mod.output2.spring$min))

constants.grid2[best.constants2.spring,]


## 3.1.5 using SUMMER Normalised suitability score WITHOUT plateau --> best.constants.summer ----
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix.norm.summer)){
  dat<-data.frame(training.data.summer, suitability.score=suitability.matrix.norm.summer[-testing.ID.summer,i])
  # without interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + 
                exposure + 
                log(mean_day_within_season) + 
                (1|landscape/site)+(1|year), REML = T)
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.1))
  print(i)
}
mod.output.summer<-data.frame(AIC.without.interaction)
mod.output.summer$min<-apply(mod.output.summer,1,function(x){min(x)})

best.constants.summer<- which(mod.output.summer$min==min(mod.output.summer$min))

constants.grid[best.constants.summer,]

## 3.1.6 using SUMMER Normalised suitability score WITH plateau --> best.constants2.summer ----
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix2.norm.summer)){
  dat<-data.frame(training.data.summer, suitability.score=suitability.matrix2.norm.summer[-testing.ID.summer,i])
  # without interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + 
                exposure +
                log(mean_day_within_season) + 
                (1|landscape/site)+(1|year), REML = T)
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.1))
  print(i)
}
mod.output2.summer<-data.frame(AIC.without.interaction)
mod.output2.summer$min<-apply(mod.output2.summer,1,function(x){min(x)})

best.constants2.summer<- which(mod.output2.summer$min==min(mod.output2.summer$min))

constants.grid2[best.constants2.summer,]




### 3.2 6-week sampling ------
## 3.2.1 using Normalised suitability score WITHOUT plateau --> best.constants.6 ----
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix.6.norm)){
  dat<-data.frame(training.data.6, suitability.score=suitability.matrix.6.norm[-testing.ID.6,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season + 
                exposure + season:exposure + 
                log(mean_day_within_season) + season:log(mean_day_within_season) +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season +  
                exposure + season:exposure + 
                log(mean_day_within_season) + season:log(mean_day_within_season) +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output.6<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output.6$min<-apply(mod.output.6,1,function(x){min(x)})

best.constants.6<- which(mod.output.6$min==min(mod.output.6$min))

constants.grid[best.constants.6,]
interaction.6<-which(mod.output.6[best.constants.6,]==min(mod.output.6[best.constants.6,]) )[1]

## 3.2.2 using Normalised suitability score WITH plateau --> best.constants2.6 ----
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix2.6.norm)){
  dat<-data.frame(training.data.6, suitability.score=suitability.matrix2.6.norm[-testing.ID.6,i])
  # with interaction effect
  mod.1<-lmer(data= dat,
              log.ab~ suitability.score + season + suitability.score:season + 
                exposure + season:exposure + 
                log(mean_day_within_season) + season:log(mean_day_within_season) +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat,
              log.ab~ suitability.score + season + 
                exposure + season:exposure + 
                log(mean_day_within_season) + season:log(mean_day_within_season) +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output2.6<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output2.6$min<-apply(mod.output2.6,1,function(x){min(x)})

best.constants2.6<-which(mod.output2.6$min==min(mod.output2.6$min))

constants.grid2[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]




# 4. Models: abundance ----- 
## base model with fixed effects not subject to selection: season, suitability score (weather) (normalized by exposure days), exposure days
## part of model selection: 
### season:weather --> already done with interaction-function during selection of best.constants
### season:exposuredays
### mean_day_within_season
### season:mean_day_within_season 
### log(mean_day_within_season)
### season:log(mean_day_within_season)
### (mean_day_within_season)²
### season:(mean_day_within_season)²


### 4.1 2-week sampling ------
## 4.1.1 normalised suitability score WITHOUT plateau ----
### 4.1.1.1 without mean_day_within_season: best.suit.mod1 ------
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect (suitability.score:season)
  best.suit.mod1<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + 
                                exposure +
                                (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect (suitability.score:season)
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod1<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                               exposure +
                              (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod1)
plot(best.suit.mod1)

### 4.1.1.2 without mean_day_within_season & with season:exposure: best.suit.mod2 ------
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect (suitability.score:season)
  best.suit.mod2<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + 
                         exposure + season:exposure +
                         (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect (suitability.score:season)
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod2<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                         exposure + season:exposure +
                         (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod2)
plot(best.suit.mod2)

### 4.1.1.3 including mean_day_within_season: best.suit.mod3 ------
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod3<-lmer(data= dat, 
                      log.ab~ suitability.score + season + suitability.score:season +
                        exposure + mean_day_within_season +
                        (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod3<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                         exposure + mean_day_within_season +
                         (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod3)
plot(best.suit.mod3)



### 4.1.1.4 including mean_day_within_season & season:exposure: best.suit.mod4 ------
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod4<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season +
                         exposure + season:exposure + mean_day_within_season +
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
                           best.suit.mod4<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + 
                                                  exposure + season:exposure + mean_day_within_season +
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod4)
plot(best.suit.mod4)



### 4.1.1.5 including mean_day_within_season & season:mean_day_within_season: best.suit.mod5 ------
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod5<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season +
                         exposure + mean_day_within_season + season:mean_day_within_season +
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
                           best.suit.mod5<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + 
                                                  exposure + mean_day_within_season + season:mean_day_within_season +
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod5)
plot(best.suit.mod5)



### 4.1.1.6 including mean_day_within_season & season:mean_day_within_season & season:exposure: best.suit.mod6 ------
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod6<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season +
                         exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                         (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod6<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                         exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                         (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod6)
plot(best.suit.mod6)









### 4.1.1.7 including log(mean_day_within_season): best.suit.mod7  -----
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod7<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                         exposure + log(mean_day_within_season) + 
                          (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod7<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                         exposure + log(mean_day_within_season) + 
                         (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod7)
plot(best.suit.mod7)

### 4.1.1.8 including log(mean_day_within_season) & season:exposure: best.suit.mod8  -----
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod8<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season +
                       exposure + season:exposure + log(mean_day_within_season) + 
                         (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod8<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                        exposure + season:exposure + log(mean_day_within_season) + 
                         (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod8)
plot(best.suit.mod8)


### 4.1.1.9 including log(mean_day_within_season) & season:log(mean_day_within_season): best.suit.mod9 -----
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod9<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                         exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                          (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod9<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                         exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                         (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod9)
plot(best.suit.mod9)


### 4.1.1.10 including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure: best.suit.mod10 -----
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod10<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + 
                         exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                         (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod10<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                         exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                         (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod10)
plot(best.suit.mod10)


### 4.1.1.11 including (mean_day_within_season)²: best.suit.mod11 -----
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod11<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                         exposure + mean_day_within_season.qd +
                          (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod11<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                         exposure + mean_day_within_season.qd +
                         (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod11)
plot(best.suit.mod11)


### 4.1.1.12 including (mean_day_within_season)² & season:exposure: best.suit.mod12 -----
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod12<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + 
                         exposure + season:exposure + mean_day_within_season.qd +
                         (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod12<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                         exposure + season:exposure + mean_day_within_season.qd + 
                         (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod12)
plot(best.suit.mod12)


### 4.1.1.13 including (mean_day_within_season)² & season:(mean_day_within_season)²: best.suit.mod13 -----
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod13<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                         exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                          (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod13<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                         exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                         (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod13)
plot(best.suit.mod13)




### 4.1.1.14 including (mean_day_within_season)² & season:(mean_day_within_season)² & season:exposure: best.suit.mod14 -----
constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod14<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + 
                         exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                         (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
  best.suit.mod14<-lmer(data= dat, 
                       log.ab~ suitability.score + season + 
                         exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                         (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod14)
plot(best.suit.mod14)











## 4.1.2 normalised suitability score WITH plateau ----
### 4.1.2.1 without mean_day_within_season: best.suit.mod1b ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect (suitability.score:season)
  best.suit.mod1b<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + 
                         exposure +
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect (suitability.score:season)
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                           best.suit.mod1b<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + 
                                                  exposure +
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod1b)
plot(best.suit.mod1b)

### 4.1.2.2 without mean_day_within_season & with season:exposure: best.suit.mod2b ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect (suitability.score:season)
  best.suit.mod2b<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + 
                         exposure + season:exposure +
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect (suitability.score:season)
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                           best.suit.mod2b<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + 
                                                  exposure + season:exposure +
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod2b)
plot(best.suit.mod2b)

### 4.1.2.3 including mean_day_within_season: best.suit.mod3b ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod3b<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season +
                         exposure + mean_day_within_season +
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                           best.suit.mod3b<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + 
                                                  exposure + mean_day_within_season +
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod3b)
plot(best.suit.mod3b)



### 4.1.2.4 including mean_day_within_season & season:exposure: best.suit.mod4b ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod4b<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season +
                         exposure + season:exposure + mean_day_within_season +
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                           best.suit.mod4b<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + 
                                                  exposure + season:exposure + mean_day_within_season +
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod4b)
plot(best.suit.mod4b)


### 4.1.2.5 including mean_day_within_season & season:mean_day_within_season: best.suit.mod5b ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod5b<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season +
                         exposure + mean_day_within_season + season:mean_day_within_season +
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                           best.suit.mod5b<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + 
                                                  exposure + mean_day_within_season + season:mean_day_within_season +
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod5b)
plot(best.suit.mod5b)



### 4.1.2.6 including mean_day_within_season & season:mean_day_within_season & season:exposure: best.suit.mod6b ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod6b<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season +
                         exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                           best.suit.mod6b<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + 
                                                  exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod6b)
plot(best.suit.mod6b)



### 4.1.2.7 including log(mean_day_within_season): best.suit.mod7b  -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod7b<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + 
                         exposure + log(mean_day_within_season) + 
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                           best.suit.mod7b<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + 
                                                  exposure + log(mean_day_within_season) + 
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod7b)
plot(best.suit.mod7b)

### 4.1.2.8 including log(mean_day_within_season) & season:exposure: best.suit.mod8b  -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod8b<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season +
                       exposure + season:exposure + log(mean_day_within_season) + 
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                           best.suit.mod8b<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + 
                                                  exposure + season:exposure + log(mean_day_within_season) + 
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod8b)
plot(best.suit.mod8b)


### 4.1.2.9 including log(mean_day_within_season) & season:log(mean_day_within_season): best.suit.mod9 -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod9b<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + 
                         exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                           best.suit.mod9b<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + 
                                                  exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod9b)
plot(best.suit.mod9b)


### 4.1.2.10 including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure: best.suit.mod10b -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod10b<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                          exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                            best.suit.mod10b<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod10b)
plot(best.suit.mod10b)









### 4.1.2.11 including (mean_day_within_season)²: best.suit.mod11b -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod11b<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                          exposure + mean_day_within_season.qd +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                            best.suit.mod11b<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + mean_day_within_season.qd +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod11b)
plot(best.suit.mod11b)


### 4.1.2.12 including (mean_day_within_season)² & season:exposure: best.suit.mod12b -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod12b<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                          exposure + season:exposure + mean_day_within_season.qd +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                            best.suit.mod12b<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + season:exposure + mean_day_within_season.qd + 
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod12b)
plot(best.suit.mod12b)


### 4.1.2.13 including (mean_day_within_season)² & season:(mean_day_within_season)²: best.suit.mod13b -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod13b<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                          exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                            best.suit.mod13b<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod13b)
plot(best.suit.mod13b)


### 4.1.2.14 including (mean_day_within_season)² & season:(mean_day_within_season)² & season:exposure: best.suit.mod14b -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod14b<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                          exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                            best.suit.mod14b<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod14b)
plot(best.suit.mod14b)



### 4.2 6-week sampling ------
## 4.2.2 normalised suitability score WITH plateau ----
### 4.2.2.1 without mean_day_within_season: best.suit.mod1b.6 ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect (suitability.score:season)
  best.suit.mod1b.6<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                          exposure +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect (suitability.score:season)
                            dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                            best.suit.mod1b.6<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod1b.6)
plot(best.suit.mod1b.6)

### 4.2.2.2 without mean_day_within_season & with season:exposure: best.suit.mod2b.6 ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]


if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect (suitability.score:season)
  best.suit.mod2b.6<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                          exposure + season:exposure +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect (suitability.score:season)
                            dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                            best.suit.mod2b.6<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + season:exposure +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod2b.6)
plot(best.suit.mod2b.6)

### 4.2.2.3 including mean_day_within_season: best.suit.mod3b.6 ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod3b.6<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season +
                          exposure + mean_day_within_season +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                            best.suit.mod3b.6<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + mean_day_within_season +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod3b.6)
plot(best.suit.mod3b.6)



### 4.2.2.4 including mean_day_within_season & season:exposure: best.suit.mod4b.6 ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]


if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod4b.6<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season +
                          exposure + season:exposure + mean_day_within_season +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                            best.suit.mod4b.6<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + season:exposure + mean_day_within_season +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod4b.6)
plot(best.suit.mod4b.6)


### 4.2.2.5 including mean_day_within_season & season:mean_day_within_season: best.suit.mod5b.6 ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod5b.6<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season +
                          exposure + mean_day_within_season + season:mean_day_within_season +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                            best.suit.mod5b.6<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + mean_day_within_season + season:mean_day_within_season +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod5b.6)
plot(best.suit.mod5b.6)



### 4.2.2.6 including mean_day_within_season & season:mean_day_within_season & season:exposure: best.suit.mod6b.6 ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]


if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod6b.6<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season +
                          exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                            best.suit.mod6b.6<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod6b.6)
plot(best.suit.mod6b.6)



### 4.2.2.7 including log(mean_day_within_season): best.suit.mod7b.6  -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

# if(interaction2.6==1){
#   dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
#   # with interaction effect
#   best.suit.mod7b.6<-lmer(data= dat, 
#                         log.ab~ suitability.score + season + suitability.score:season + 
#                           exposure + log(mean_day_within_season) + 
#                           (1|landscape/site)+(1|year), REML = T)}else{
#                             # without interaction effect
                            dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                            best.suit.mod7b.6<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + log(mean_day_within_season) + 
                                                    (1|landscape/site)+(1|year), REML = T)
                            #}

summary(best.suit.mod7b.6)
plot(best.suit.mod7b.6)

### 4.2.2.8 including log(mean_day_within_season) & season:exposure: best.suit.mod8b.6  -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod8b.6<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season +
                          exposure + season:exposure + log(mean_day_within_season) + 
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                            best.suit.mod8b.6<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + season:exposure + log(mean_day_within_season) + 
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod8b.6)
plot(best.suit.mod8b.6)


### 4.2.2.9 including log(mean_day_within_season) & season:log(mean_day_within_season): best.suit.mod9.6 -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod9b.6<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + 
                          exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                            best.suit.mod9b.6<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + 
                                                    exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod9b.6)
plot(best.suit.mod9b.6)


### 4.2.2.10 including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure: best.suit.mod10b.6 -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod10b.6<-lmer(data= dat, 
                         log.ab~ suitability.score + season + suitability.score:season + 
                           exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                           (1|landscape/site)+(1|year), REML = T)}else{
                             # without interaction effect
                             dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                             best.suit.mod10b.6<-lmer(data= dat, 
                                                    log.ab~ suitability.score + season + 
                                                      exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                      (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod10b.6)
plot(best.suit.mod10b.6)









### 4.2.2.11 including (mean_day_within_season)²: best.suit.mod11b.6 -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod11b.6<-lmer(data= dat, 
                         log.ab~ suitability.score + season + suitability.score:season + 
                           exposure + mean_day_within_season.qd +
                           (1|landscape/site)+(1|year), REML = T)}else{
                             # without interaction effect
                             dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                             best.suit.mod11b.6<-lmer(data= dat, 
                                                    log.ab~ suitability.score + season + 
                                                      exposure + mean_day_within_season.qd +
                                                      (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod11b.6)
plot(best.suit.mod11b.6)


### 4.2.2.12 including (mean_day_within_season)² & season:exposure: best.suit.mod12b.6 -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod12b.6<-lmer(data= dat, 
                         log.ab~ suitability.score + season + suitability.score:season + 
                           exposure + season:exposure + mean_day_within_season.qd +
                           (1|landscape/site)+(1|year), REML = T)}else{
                             # without interaction effect
                             dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                             best.suit.mod12b.6<-lmer(data= dat, 
                                                    log.ab~ suitability.score + season + 
                                                      exposure + season:exposure + mean_day_within_season.qd + 
                                                      (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod12b.6)
plot(best.suit.mod12b.6)


### 4.2.2.13 including (mean_day_within_season)² & season:(mean_day_within_season)²: best.suit.mod13b.6 -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod13b.6<-lmer(data= dat, 
                         log.ab~ suitability.score + season + suitability.score:season + 
                           exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                           (1|landscape/site)+(1|year), REML = T)}else{
                             # without interaction effect
                             dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                             best.suit.mod13b.6<-lmer(data= dat, 
                                                    log.ab~ suitability.score + season + 
                                                      exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                      (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod13b.6)
plot(best.suit.mod13b.6)

### 4.2.2.14 including (mean_day_within_season)² & season:(mean_day_within_season)² & season:exposure: best.suit.mod14b.6 -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod14b.6<-lmer(data= dat, 
                         log.ab~ suitability.score + season + suitability.score:season + 
                           exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                           (1|landscape/site)+(1|year), REML = T)}else{
                             # without interaction effect
                             dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                             best.suit.mod14b.6<-lmer(data= dat, 
                                                    log.ab~ suitability.score + season + 
                                                      exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                      (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod14b.6)
plot(best.suit.mod14b.6)


## 4.3 separate models for SPRING and SUMMER ----
# reason: very strong effect of season; check if running two models improves the models
# use model which behaved best in full model comparison
### 4.3.1 SPRING: without plateau; including log(mean_day_within_season): best.suit.mod7.spring---- 
# similar to best.suit.mod.7 but without season
dat<-data.frame(testing.data.spring,suitability.score=suitability.matrix.norm.spring[testing.ID.spring,best.constants.spring])
best.suit.mod7.spring<-lmer(data= dat, 
                     log.ab~ suitability.score +
                       exposure + log(mean_day_within_season) + 
                       (1|landscape/site)+(1|year), REML = T)
summary(best.suit.mod7.spring)
plot(best.suit.mod7.spring)

### 4.3.2 SUMMER: without plateau; including log(mean_day_within_season): best.suit.mod7.summer ----
# similar to best.suit.mod.7 but without season
dat<-data.frame(testing.data.summer,suitability.score=suitability.matrix.norm.spring[testing.ID.summer,best.constants.summer])
best.suit.mod7.summer<-lmer(data= dat, 
                     log.ab~ suitability.score +
                       exposure + log(mean_day_within_season) + 
                       (1|landscape/site)+(1|year), REML = T)

summary(best.suit.mod7.summer)
plot(best.suit.mod7.summer)





# 5. Models: biomass ----
## 5.2 normalised suitability score WITH plateau ----

## 5.2.1 based on ITD -----
### 5.2.1.1 2-week sampling ------
### 5.2.1.1.1 without mean_day_within_season: best.suit.mod1b.bio.ITD ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect (suitability.score:season)
  best.suit.mod1b.bio.ITD<-lmer(data= dat, 
                               log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                 exposure +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect (suitability.score:season)
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod1b.bio.ITD<-lmer(data= dat, 
                                                                log.bio.ITD~ suitability.score + season + 
                                                                  exposure +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod1b.bio.ITD)
plot(best.suit.mod1b.bio.ITD)

### 5.2.1.1.2 without mean_day_within_season & with season:exposure: best.suit.mod2b.bio.ITD ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect (suitability.score:season)
  best.suit.mod2b.bio.ITD<-lmer(data= dat, 
                               log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                 exposure + season:exposure +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect (suitability.score:season)
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod2b.bio.ITD<-lmer(data= dat, 
                                                                log.bio.ITD~ suitability.score + season + 
                                                                  exposure + season:exposure +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod2b.bio.ITD)
plot(best.suit.mod2b.bio.ITD)

### 5.2.1.1.3 including mean_day_within_season: best.suit.mod3b.bio.ITD ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod3b.bio.ITD<-lmer(data= dat, 
                        log.bio.ITD~ suitability.score + season + suitability.score:season +
                          exposure + mean_day_within_season +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                            best.suit.mod3b.bio.ITD<-lmer(data= dat, 
                                                  log.bio.ITD~ suitability.score + season + 
                                                    exposure + mean_day_within_season +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod3b.bio.ITD)
plot(best.suit.mod3b.bio.ITD)



### 5.2.1.1.4 including mean_day_within_season & season:exposure: best.suit.mod4b.bio.ITD ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod4b.bio.ITD<-lmer(data= dat, 
                               log.bio.ITD~ suitability.score + season + suitability.score:season +
                                 exposure + season:exposure + mean_day_within_season +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod4b.bio.ITD<-lmer(data= dat, 
                                                                log.bio.ITD~ suitability.score + season + 
                                                                  exposure + season:exposure + mean_day_within_season +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod4b.bio.ITD)
plot(best.suit.mod4b.bio.ITD)


### 5.2.1.1.5 including mean_day_within_season & season:mean_day_within_season: best.suit.mod5b.bio.ITD ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod5b.bio.ITD<-lmer(data= dat, 
                               log.bio.ITD~ suitability.score + season + suitability.score:season +
                                 exposure + mean_day_within_season + season:mean_day_within_season +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod5b.bio.ITD<-lmer(data= dat, 
                                                                log.bio.ITD~ suitability.score + season + 
                                                                  exposure + mean_day_within_season + season:mean_day_within_season +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod5b.bio.ITD)
plot(best.suit.mod5b.bio.ITD)



### 5.2.1.1.6 including mean_day_within_season & season:mean_day_within_season & season:exposure: best.suit.mod6b.bio.ITD ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod6b.bio.ITD<-lmer(data= dat, 
                               log.bio.ITD~ suitability.score + season + suitability.score:season +
                                 exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod6b.bio.ITD<-lmer(data= dat, 
                                                                log.bio.ITD~ suitability.score + season + 
                                                                  exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod6b.bio.ITD)
plot(best.suit.mod6b.bio.ITD)



### 5.2.1.1.7 including log(mean_day_within_season): best.suit.mod7b.bio.ITD  -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod7b.bio.ITD<-lmer(data= dat, 
                               log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                 exposure + log(mean_day_within_season) + 
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod7b.bio.ITD<-lmer(data= dat, 
                                                                log.bio.ITD~ suitability.score + season + 
                                                                  exposure + log(mean_day_within_season) + 
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod7b.bio.ITD)
plot(best.suit.mod7b.bio.ITD)

### 5.2.1.1.8 including log(mean_day_within_season) & season:exposure: best.suit.mod8b.bio.ITD  -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod8b.bio.ITD<-lmer(data= dat, 
                               log.bio.ITD~ suitability.score + season + suitability.score:season +
                                 exposure + season:exposure + log(mean_day_within_season) + 
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod8b.bio.ITD<-lmer(data= dat, 
                                                                log.bio.ITD~ suitability.score + season + 
                                                                  exposure + season:exposure + log(mean_day_within_season) + 
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod8b.bio.ITD)
plot(best.suit.mod8b.bio.ITD)


### 5.2.1.1.9 including log(mean_day_within_season) & season:log(mean_day_within_season): best.suit.mod9.bio.ITD -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod9b.bio.ITD<-lmer(data= dat, 
                               log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                 exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod9b.bio.ITD<-lmer(data= dat, 
                                                                log.bio.ITD~ suitability.score + season + 
                                                                  exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod9b.bio.ITD)
plot(best.suit.mod9b.bio.ITD)


### 5.2.1.1.10 including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure: best.suit.mod10b.bio.ITD -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod10b.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                  exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                    best.suit.mod10b.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod10b.bio.ITD)
plot(best.suit.mod10b.bio.ITD)


### 5.2.1.1.11 including (mean_day_within_season)²: best.suit.mod11b.bio.ITD -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod11b.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                  exposure + mean_day_within_season.qd +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                    best.suit.mod11b.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + mean_day_within_season.qd +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod11b.bio.ITD)
plot(best.suit.mod11b.bio.ITD)


### 5.2.1.1.12 including (mean_day_within_season)² & season:exposure: best.suit.mod12b.bio.ITD -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod12b.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                  exposure + season:exposure + mean_day_within_season.qd +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                    best.suit.mod12b.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + season:exposure + mean_day_within_season.qd + 
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod12b.bio.ITD)
plot(best.suit.mod12b.bio.ITD)


### 5.2.1.1.13 including (mean_day_within_season)² & season:(mean_day_within_season)²: best.suit.mod13b.bio.ITD -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod13b.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                  exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                    best.suit.mod13b.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod13b.bio.ITD)
plot(best.suit.mod13b.bio.ITD)

### 5.2.1.1.14 including (mean_day_within_season)² & season:(mean_day_within_season)² & season:exposure: best.suit.mod14b.bio.ITD -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod14b.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                  exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                    best.suit.mod14b.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod14b.bio.ITD)
plot(best.suit.mod14b.bio.ITD)









5

### 5.2.1.2 6-week sampling ------
### 5.2.1.2.1 without mean_day_within_season: best.suit.mod1b.6.bio.ITD ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect (suitability.score:season)
  best.suit.mod1b.6.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                  exposure +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect (suitability.score:season)
                                    dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                    best.suit.mod1b.6.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod1b.6.bio.ITD)
plot(best.suit.mod1b.6.bio.ITD)

### 5.2.1.2.2 without mean_day_within_season & with season:exposure: best.suit.mod2b.6.bio.ITD ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect (suitability.score:season)
  best.suit.mod2b.6.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                  exposure + season:exposure +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect (suitability.score:season)
                                    dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                    best.suit.mod2b.6.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + season:exposure +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod2b.6.bio.ITD)
plot(best.suit.mod2b.6.bio.ITD)

### 5.2.1.2.3 including mean_day_within_season: best.suit.mod3b.6.bio.ITD ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod3b.6.bio.ITD<-lmer(data= dat, 
                        log.bio.ITD~ suitability.score + season + suitability.score:season +
                          exposure + mean_day_within_season +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                            best.suit.mod3b.6.bio.ITD<-lmer(data= dat, 
                                                  log.bio.ITD~ suitability.score + season + 
                                                    exposure + mean_day_within_season +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod3b.6.bio.ITD)
plot(best.suit.mod3b.6.bio.ITD)



### 5.2.1.2.4 including mean_day_within_season & season:exposure: best.suit.mod4b.6.bio.ITD ------
constants.grid[best.constants2,]
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod4b.6.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season +
                                  exposure + season:exposure + mean_day_within_season +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                    best.suit.mod4b.6.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + season:exposure + mean_day_within_season +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod4b.6.bio.ITD)
plot(best.suit.mod4b.6.bio.ITD)


### 5.2.1.2.5 including mean_day_within_season & season:mean_day_within_season: best.suit.mod5b.6.bio.ITD ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod5b.6.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season +
                                  exposure + mean_day_within_season + season:mean_day_within_season +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                    best.suit.mod5b.6.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + mean_day_within_season + season:mean_day_within_season +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod5b.6.bio.ITD)
plot(best.suit.mod5b.6.bio.ITD)



### 5.2.1.2.6 including mean_day_within_season & season:mean_day_within_season & season:exposure: best.suit.mod6b.6.bio.ITD ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod6b.6.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season +
                                  exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                    best.suit.mod6b.6.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod6b.6.bio.ITD)
plot(best.suit.mod6b.6.bio.ITD)



### 5.2.1.2.7 including log(mean_day_within_season): best.suit.mod7b.6.bio.ITD  -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod7b.6.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                  exposure + log(mean_day_within_season) + 
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                    best.suit.mod7b.6.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + log(mean_day_within_season) + 
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod7b.6.bio.ITD)
plot(best.suit.mod7b.6.bio.ITD)

### 5.2.1.2.8 including log(mean_day_within_season) & season:exposure: best.suit.mod8b.6.bio.ITD  -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod8b.6.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season +
                                  exposure + season:exposure + log(mean_day_within_season) + 
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                    best.suit.mod8b.6.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + season:exposure + log(mean_day_within_season) + 
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod8b.6.bio.ITD)
plot(best.suit.mod8b.6.bio.ITD)


### 5.2.1.2.9 including log(mean_day_within_season) & season:log(mean_day_within_season): best.suit.mod9.6.bio.ITD -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod9b.6.bio.ITD<-lmer(data= dat, 
                                log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                  exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                    best.suit.mod9b.6.bio.ITD<-lmer(data= dat, 
                                                                  log.bio.ITD~ suitability.score + season + 
                                                                    exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod9b.6.bio.ITD)
plot(best.suit.mod9b.6.bio.ITD)


### 5.2.1.2.10 including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure: best.suit.mod10b.6.bio.ITD -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod10b.6.bio.ITD<-lmer(data= dat, 
                                 log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                   exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                   (1|landscape/site)+(1|year), REML = T)}else{
                                     # without interaction effect
                                     dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                     best.suit.mod10b.6.bio.ITD<-lmer(data= dat, 
                                                                    log.bio.ITD~ suitability.score + season + 
                                                                      exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                                      (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod10b.6.bio.ITD)
plot(best.suit.mod10b.6.bio.ITD)


### 5.2.1.2.11 including (mean_day_within_season)²: best.suit.mod11b.6.bio.ITD -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod11b.6.bio.ITD<-lmer(data= dat, 
                                 log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                   exposure + mean_day_within_season.qd +
                                   (1|landscape/site)+(1|year), REML = T)}else{
                                     # without interaction effect
                                     dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                     best.suit.mod11b.6.bio.ITD<-lmer(data= dat, 
                                                                    log.bio.ITD~ suitability.score + season + 
                                                                      exposure + mean_day_within_season.qd +
                                                                      (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod11b.6.bio.ITD)
plot(best.suit.mod11b.6.bio.ITD)


### 5.2.1.2.12 including (mean_day_within_season)² & season:exposure: best.suit.mod12b.6.bio.ITD -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod12b.6.bio.ITD<-lmer(data= dat, 
                                 log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                   exposure + season:exposure + mean_day_within_season.qd +
                                   (1|landscape/site)+(1|year), REML = T)}else{
                                     # without interaction effect
                                     dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                     best.suit.mod12b.6.bio.ITD<-lmer(data= dat, 
                                                                    log.bio.ITD~ suitability.score + season + 
                                                                      exposure + season:exposure + mean_day_within_season.qd + 
                                                                      (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod12b.6.bio.ITD)
plot(best.suit.mod12b.6.bio.ITD)


### 5.2.1.2.13 including (mean_day_within_season)² & season:(mean_day_within_season)²: best.suit.mod13b.6.bio.ITD -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod13b.6.bio.ITD<-lmer(data= dat, 
                                 log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                   exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                   (1|landscape/site)+(1|year), REML = T)}else{
                                     # without interaction effect
                                     dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                     best.suit.mod13b.6.bio.ITD<-lmer(data= dat, 
                                                                    log.bio.ITD~ suitability.score + season + 
                                                                      exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                                      (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod13b.6.bio.ITD)
plot(best.suit.mod13b.6.bio.ITD)

### 5.2.1.2.14 including (mean_day_within_season)² & season:(mean_day_within_season)² & season:exposure: best.suit.mod14b.6.bio.ITD -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod14b.6.bio.ITD<-lmer(data= dat, 
                                 log.bio.ITD~ suitability.score + season + suitability.score:season + 
                                   exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                   (1|landscape/site)+(1|year), REML = T)}else{
                                     # without interaction effect
                                     dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                     best.suit.mod14b.6.bio.ITD<-lmer(data= dat, 
                                                                    log.bio.ITD~ suitability.score + season + 
                                                                      exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                                      (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod14b.6.bio.ITD)
plot(best.suit.mod14b.6.bio.ITD)





## 5.2.2 based on BL -----
### 5.2.2.1 2-week sampling ------
### 5.2.2.1.1 without mean_day_within_season: best.suit.mod1b.bio.BL ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect (suitability.score:season)
  best.suit.mod1b.bio.BL<-lmer(data= dat, 
                               log.bio.BL~ suitability.score + season + suitability.score:season + 
                                 exposure +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect (suitability.score:season)
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod1b.bio.BL<-lmer(data= dat, 
                                                                log.bio.BL~ suitability.score + season + 
                                                                  exposure +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod1b.bio.BL)
plot(best.suit.mod1b.bio.BL)

### 5.2.2.1.2 without mean_day_within_season & with season:exposure: best.suit.mod2b.bio.BL ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect (suitability.score:season)
  best.suit.mod2b.bio.BL<-lmer(data= dat, 
                               log.bio.BL~ suitability.score + season + suitability.score:season + 
                                 exposure + season:exposure +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect (suitability.score:season)
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod2b.bio.BL<-lmer(data= dat, 
                                                                log.bio.BL~ suitability.score + season + 
                                                                  exposure + season:exposure +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod2b.bio.BL)
plot(best.suit.mod2b.bio.BL)

### 5.2.2.1.3 including mean_day_within_season: best.suit.mod3b.bio.BL ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod3b.bio.BL<-lmer(data= dat, 
                        log.bio.BL~ suitability.score + season + suitability.score:season +
                          exposure + mean_day_within_season +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                            best.suit.mod3b.bio.BL<-lmer(data= dat, 
                                                  log.bio.BL~ suitability.score + season + 
                                                    exposure + mean_day_within_season +
                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod3b.bio.BL)
plot(best.suit.mod3b.bio.BL)

### 5.2.2.1.4 including mean_day_within_season & season:exposure: best.suit.mod4b.bio.BL ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod4b.bio.BL<-lmer(data= dat, 
                               log.bio.BL~ suitability.score + season + suitability.score:season +
                                 exposure + season:exposure + mean_day_within_season +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod4b.bio.BL<-lmer(data= dat, 
                                                                log.bio.BL~ suitability.score + season + 
                                                                  exposure + season:exposure + mean_day_within_season +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod4b.bio.BL)
plot(best.suit.mod4b.bio.BL)


### 5.2.2.1.5 including mean_day_within_season & season:mean_day_within_season: best.suit.mod5b.bio.BL ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod5b.bio.BL<-lmer(data= dat, 
                               log.bio.BL~ suitability.score + season + suitability.score:season +
                                 exposure + mean_day_within_season + season:mean_day_within_season +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod5b.bio.BL<-lmer(data= dat, 
                                                                log.bio.BL~ suitability.score + season + 
                                                                  exposure + mean_day_within_season + season:mean_day_within_season +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod5b.bio.BL)
plot(best.suit.mod5b.bio.BL)



### 5.2.2.1.6 including mean_day_within_season & season:mean_day_within_season & season:exposure: best.suit.mod6b.bio.BL ------
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod6b.bio.BL<-lmer(data= dat, 
                               log.bio.BL~ suitability.score + season + suitability.score:season +
                                 exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod6b.bio.BL<-lmer(data= dat, 
                                                                log.bio.BL~ suitability.score + season + 
                                                                  exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod6b.bio.BL)
plot(best.suit.mod6b.bio.BL)



### 5.2.2.1.7 including log(mean_day_within_season): best.suit.mod7b.bio.BL  -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod7b.bio.BL<-lmer(data= dat, 
                               log.bio.BL~ suitability.score + season + suitability.score:season + 
                                 exposure + log(mean_day_within_season) + 
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod7b.bio.BL<-lmer(data= dat, 
                                                                log.bio.BL~ suitability.score + season + 
                                                                  exposure + log(mean_day_within_season) + 
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod7b.bio.BL)
plot(best.suit.mod7b.bio.BL)

### 5.2.2.1.8 including log(mean_day_within_season) & season:exposure: best.suit.mod8b.bio.BL  -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod8b.bio.BL<-lmer(data= dat, 
                               log.bio.BL~ suitability.score + season + suitability.score:season +
                                 exposure + season:exposure + log(mean_day_within_season) + 
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod8b.bio.BL<-lmer(data= dat, 
                                                                log.bio.BL~ suitability.score + season + 
                                                                  exposure + season:exposure + log(mean_day_within_season) + 
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod8b.bio.BL)
plot(best.suit.mod8b.bio.BL)


### 5.2.2.1.9 including log(mean_day_within_season) & season:log(mean_day_within_season): best.suit.mod9.bio.BL -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod9b.bio.BL<-lmer(data= dat, 
                               log.bio.BL~ suitability.score + season + suitability.score:season + 
                                 exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                 (1|landscape/site)+(1|year), REML = T)}else{
                                   # without interaction effect
                                   dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                   best.suit.mod9b.bio.BL<-lmer(data= dat, 
                                                                log.bio.BL~ suitability.score + season + 
                                                                  exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod9b.bio.BL)
plot(best.suit.mod9b.bio.BL)


### 5.2.2.1.10 including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure: best.suit.mod10b.bio.BL -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod10b.bio.BL<-lmer(data= dat, 
                                log.bio.BL~ suitability.score + season + suitability.score:season + 
                                  exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                    best.suit.mod10b.bio.BL<-lmer(data= dat, 
                                                                  log.bio.BL~ suitability.score + season + 
                                                                    exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod10b.bio.BL)
plot(best.suit.mod10b.bio.BL)


### 5.2.2.1.11 including (mean_day_within_season)²: best.suit.mod11b.bio.BL -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod11b.bio.BL<-lmer(data= dat, 
                                log.bio.BL~ suitability.score + season + suitability.score:season + 
                                  exposure + mean_day_within_season.qd +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                    best.suit.mod11b.bio.BL<-lmer(data= dat, 
                                                                  log.bio.BL~ suitability.score + season + 
                                                                    exposure + mean_day_within_season.qd +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod11b.bio.BL)
plot(best.suit.mod11b.bio.BL)


### 5.2.2.1.12 including (mean_day_within_season)² & season:exposure: best.suit.mod12b.bio.BL -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod12b.bio.BL<-lmer(data= dat, 
                                log.bio.BL~ suitability.score + season + suitability.score:season + 
                                  exposure + season:exposure + mean_day_within_season.qd +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                    best.suit.mod12b.bio.BL<-lmer(data= dat, 
                                                                  log.bio.BL~ suitability.score + season + 
                                                                    exposure + season:exposure + mean_day_within_season.qd + 
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod12b.bio.BL)
plot(best.suit.mod12b.bio.BL)


### 5.2.2.1.13 including (mean_day_within_season)² & season:(mean_day_within_season)²: best.suit.mod13b.bio.BL -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod13b.bio.BL<-lmer(data= dat, 
                                log.bio.BL~ suitability.score + season + suitability.score:season + 
                                  exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                    best.suit.mod13b.bio.BL<-lmer(data= dat, 
                                                                  log.bio.BL~ suitability.score + season + 
                                                                    exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod13b.bio.BL)
plot(best.suit.mod13b.bio.BL)

### 5.2.2.1.14 including (mean_day_within_season)² & season:(mean_day_within_season)² & season:exposure: best.suit.mod14b.bio.BL -----
constants.grid[best.constants2,]
interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction2==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod14b.bio.BL<-lmer(data= dat, 
                                log.bio.BL~ suitability.score + season + suitability.score:season + 
                                  exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                  (1|landscape/site)+(1|year), REML = T)}else{
                                    # without interaction effect
                                    dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
                                    best.suit.mod14b.bio.BL<-lmer(data= dat, 
                                                                  log.bio.BL~ suitability.score + season + 
                                                                    exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                                    (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod14b.bio.BL)
plot(best.suit.mod14b.bio.BL)











### 5.2.2.2 6-week sampling ------
### 5.2.2.2.1 without mean_day_within_season: best.suit.mod1b.6.bio.BL ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect (suitability.score:season)
  best.suit.mod1b.6.bio.BL<-lmer(data= dat, 
                                  log.bio.BL~ suitability.score + season + suitability.score:season + 
                                    exposure +
                                    (1|landscape/site)+(1|year), REML = T)}else{
                                      # without interaction effect (suitability.score:season)
                                      dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                      best.suit.mod1b.6.bio.BL<-lmer(data= dat, 
                                                                      log.bio.BL~ suitability.score + season + 
                                                                        exposure +
                                                                        (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod1b.6.bio.BL)
plot(best.suit.mod1b.6.bio.BL)

### 5.2.2.2.2 without mean_day_within_season & with season:exposure: best.suit.mod2b.6.bio.BL ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect (suitability.score:season)
  best.suit.mod2b.6.bio.BL<-lmer(data= dat, 
                                  log.bio.BL~ suitability.score + season + suitability.score:season + 
                                    exposure + season:exposure +
                                    (1|landscape/site)+(1|year), REML = T)}else{
                                      # without interaction effect (suitability.score:season)
                                      dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                      best.suit.mod2b.6.bio.BL<-lmer(data= dat, 
                                                                      log.bio.BL~ suitability.score + season + 
                                                                        exposure + season:exposure +
                                                                        (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod2b.6.bio.BL)
plot(best.suit.mod2b.6.bio.BL)

### 5.2.2.2.3 including mean_day_within_season: best.suit.mod3b.6.bio.BL ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod3b.6.bio.BL<-lmer(data= dat, 
                                  log.bio.BL~ suitability.score + season + suitability.score:season +
                                    exposure + mean_day_within_season +
                                    (1|landscape/site)+(1|year), REML = T)}else{
                                      # without interaction effect
                                      dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                      best.suit.mod3b.6.bio.BL<-lmer(data= dat, 
                                                                      log.bio.BL~ suitability.score + season + 
                                                                        exposure + mean_day_within_season +
                                                                        (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod3b.6.bio.BL)
plot(best.suit.mod3b.6.bio.BL)



### 5.2.2.2.4 including mean_day_within_season & season:exposure: best.suit.mod4b.6.bio.BL ------
constants.grid[best.constants2,]
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod4b.6.bio.BL<-lmer(data= dat, 
                                  log.bio.BL~ suitability.score + season + suitability.score:season +
                                    exposure + season:exposure + mean_day_within_season +
                                    (1|landscape/site)+(1|year), REML = T)}else{
                                      # without interaction effect
                                      dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                      best.suit.mod4b.6.bio.BL<-lmer(data= dat, 
                                                                      log.bio.BL~ suitability.score + season + 
                                                                        exposure + season:exposure + mean_day_within_season +
                                                                        (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod4b.6.bio.BL)
plot(best.suit.mod4b.6.bio.BL)


### 5.2.2.2.5 including mean_day_within_season & season:mean_day_within_season: best.suit.mod5b.6.bio.BL ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod5b.6.bio.BL<-lmer(data= dat, 
                                  log.bio.BL~ suitability.score + season + suitability.score:season +
                                    exposure + mean_day_within_season + season:mean_day_within_season +
                                    (1|landscape/site)+(1|year), REML = T)}else{
                                      # without interaction effect
                                      dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                      best.suit.mod5b.6.bio.BL<-lmer(data= dat, 
                                                                      log.bio.BL~ suitability.score + season + 
                                                                        exposure + mean_day_within_season + season:mean_day_within_season +
                                                                        (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod5b.6.bio.BL)
plot(best.suit.mod5b.6.bio.BL)



### 5.2.2.2.6 including mean_day_within_season & season:mean_day_within_season & season:exposure: best.suit.mod6b.6.bio.BL ------
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod6b.6.bio.BL<-lmer(data= dat, 
                                  log.bio.BL~ suitability.score + season + suitability.score:season +
                                    exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                                    (1|landscape/site)+(1|year), REML = T)}else{
                                      # without interaction effect
                                      dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                      best.suit.mod6b.6.bio.BL<-lmer(data= dat, 
                                                                      log.bio.BL~ suitability.score + season + 
                                                                        exposure + season:exposure + mean_day_within_season + season:mean_day_within_season +
                                                                        (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod6b.6.bio.BL)
plot(best.suit.mod6b.6.bio.BL)



### 5.2.2.2.7 including log(mean_day_within_season): best.suit.mod7b.6.bio.BL  -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod7b.6.bio.BL<-lmer(data= dat, 
                                  log.bio.BL~ suitability.score + season + suitability.score:season + 
                                    exposure + log(mean_day_within_season) + 
                                    (1|landscape/site)+(1|year), REML = T)}else{
                                      # without interaction effect
                                      dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                      best.suit.mod7b.6.bio.BL<-lmer(data= dat, 
                                                                      log.bio.BL~ suitability.score + season + 
                                                                        exposure + log(mean_day_within_season) + 
                                                                        (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod7b.6.bio.BL)
plot(best.suit.mod7b.6.bio.BL)


dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
best.suit.mod7b.6.bio.BL_no_int <-lmer(data= dat, 
                               log.bio.BL~ suitability.score + season + 
                                 exposure + log(mean_day_within_season) + 
                                 (1|landscape/site)+(1|year), REML = T)

summary(best.suit.mod7b.6.bio.BL_no_int)
plot(best.suit.mod7b.6.bio.BL_no_int)


### 5.2.2.2.8 including log(mean_day_within_season) & season:exposure: best.suit.mod8b.6.bio.BL  -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod8b.6.bio.BL<-lmer(data= dat, 
                                  log.bio.BL~ suitability.score + season + suitability.score:season +
                                    exposure + season:exposure + log(mean_day_within_season) + 
                                    (1|landscape/site)+(1|year), REML = T)}else{
                                      # without interaction effect
                                      dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                      best.suit.mod8b.6.bio.BL<-lmer(data= dat, 
                                                                      log.bio.BL~ suitability.score + season + 
                                                                        exposure + season:exposure + log(mean_day_within_season) + 
                                                                        (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod8b.6.bio.BL)
plot(best.suit.mod8b.6.bio.BL)


### 5.2.2.2.9 including log(mean_day_within_season) & season:log(mean_day_within_season): best.suit.mod9.6.bio.BL -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod9b.6.bio.BL<-lmer(data= dat, 
                                  log.bio.BL~ suitability.score + season + suitability.score:season + 
                                    exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                    (1|landscape/site)+(1|year), REML = T)}else{
                                      # without interaction effect
                                      dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                      best.suit.mod9b.6.bio.BL<-lmer(data= dat, 
                                                                      log.bio.BL~ suitability.score + season + 
                                                                        exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                                        (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod9b.6.bio.BL)
plot(best.suit.mod9b.6.bio.BL)


### 5.2.2.2.10 including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure: best.suit.mod10b.6.bio.BL -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod10b.6.bio.BL<-lmer(data= dat, 
                                   log.bio.BL~ suitability.score + season + suitability.score:season + 
                                     exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                     (1|landscape/site)+(1|year), REML = T)}else{
                                       # without interaction effect
                                       dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                       best.suit.mod10b.6.bio.BL<-lmer(data= dat, 
                                                                        log.bio.BL~ suitability.score + season + 
                                                                          exposure + season:exposure + log(mean_day_within_season) + season:log(mean_day_within_season) +
                                                                          (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod10b.6.bio.BL)
plot(best.suit.mod10b.6.bio.BL)


### 5.2.2.2.11 including (mean_day_within_season)²: best.suit.mod11b.6.bio.BL -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod11b.6.bio.BL<-lmer(data= dat, 
                                   log.bio.BL~ suitability.score + season + suitability.score:season + 
                                     exposure + mean_day_within_season.qd +
                                     (1|landscape/site)+(1|year), REML = T)}else{
                                       # without interaction effect
                                       dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                       best.suit.mod11b.6.bio.BL<-lmer(data= dat, 
                                                                        log.bio.BL~ suitability.score + season + 
                                                                          exposure + mean_day_within_season.qd +
                                                                          (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod11b.6.bio.BL)
plot(best.suit.mod11b.6.bio.BL)


### 5.2.2.2.12 including (mean_day_within_season)² & season:exposure: best.suit.mod12b.6.bio.BL -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod12b.6.bio.BL<-lmer(data= dat, 
                                   log.bio.BL~ suitability.score + season + suitability.score:season + 
                                     exposure + season:exposure + mean_day_within_season.qd +
                                     (1|landscape/site)+(1|year), REML = T)}else{
                                       # without interaction effect
                                       dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                       best.suit.mod12b.6.bio.BL<-lmer(data= dat, 
                                                                        log.bio.BL~ suitability.score + season + 
                                                                          exposure + season:exposure + mean_day_within_season.qd + 
                                                                          (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod12b.6.bio.BL)
plot(best.suit.mod12b.6.bio.BL)


### 5.2.2.2.13 including (mean_day_within_season)² & season:(mean_day_within_season)²: best.suit.mod13b.6.bio.BL -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod13b.6.bio.BL<-lmer(data= dat, 
                                   log.bio.BL~ suitability.score + season + suitability.score:season + 
                                     exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                     (1|landscape/site)+(1|year), REML = T)}else{
                                       # without interaction effect
                                       dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                       best.suit.mod13b.6.bio.BL<-lmer(data= dat, 
                                                                        log.bio.BL~ suitability.score + season + 
                                                                          exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                                          (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod13b.6.bio.BL)
plot(best.suit.mod13b.6.bio.BL)

### 5.2.2.2.14 including (mean_day_within_season)² & season:(mean_day_within_season)² & season:exposure: best.suit.mod14b.6.bio.BL -----
constants.grid[best.constants2.6,]
interaction2.6<-which(mod.output2.6[best.constants2.6,]==min(mod.output2.6[best.constants2.6,]) )[1]

if(interaction2.6==1){
  dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
  # with interaction effect
  best.suit.mod14b.6.bio.BL<-lmer(data= dat, 
                                   log.bio.BL~ suitability.score + season + suitability.score:season + 
                                     exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                     (1|landscape/site)+(1|year), REML = T)}else{
                                       # without interaction effect
                                       dat<-data.frame(testing.data.6,suitability.score=suitability.matrix2.6.norm[testing.ID.6,best.constants2.6])
                                       best.suit.mod14b.6.bio.BL<-lmer(data= dat, 
                                                                        log.bio.BL~ suitability.score + season + 
                                                                          exposure + season:exposure + mean_day_within_season.qd + season:mean_day_within_season.qd +
                                                                          (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod14b.6.bio.BL)
plot(best.suit.mod14b.6.bio.BL)









# 6. Model comparison: abundance (manually) ----

install.packages("sjPlot")
library(sjPlot)
library(performance)

## create dataframe with summary statistics on model quality

extract_model_stats <- function(model, model_name) {
  r2_vals <- r2(model)
  
  data.frame(
    model = model_name,
    AIC = AIC(model),
    BIC = BIC(model),
    R2_marginal = r2_vals$R2_marginal,
    R2_conditional = r2_vals$R2_conditional
  )
}

models_2weeks_ab <- list(
  m1 =  best.suit.mod1,  # without plateau; without mean_day_within_season
  m2 = best.suit.mod2,  # without plateau; without mean_day_within_season & with season:exposure
  m3 = best.suit.mod3,  # without plateau; including mean_day_within_season
  m4 = best.suit.mod4,  # without plateau; including mean_day_within_season & season:exposure
  m5 = best.suit.mod5,  # without plateau; including mean_day_within_season & season:mean_day_within_season
  m6 = best.suit.mod6,  # without plateau; including mean_day_within_season & season:mean_day_within_season & season:exposure
  m7 = best.suit.mod7,  # without plateau; including log(mean_day_within_season)
  m8 = best.suit.mod8,  # without plateau; including log(mean_day_within_season) & season:exposure
  m9 = best.suit.mod9,  # without plateau; including log(mean_day_within_season) & season:log(mean_day_within_season)
  m10 = best.suit.mod10, # without plateau; including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure
  m11 = best.suit.mod11, # without plateau; including (mean_day_within_season)²
  m12 = best.suit.mod12, # without plateau; including (mean_day_within_season)² & season:exposure
  m13 = best.suit.mod13, # without plateau; including (mean_day_within_season)² & season:(mean_day_within_season)²
  m14 = best.suit.mod14, # without plateau; including (mean_day_within_season)² & season:(mean_day_within_season)² & season:exposure
  m1b = best.suit.mod1b,    # with plateau; without mean_day_within_season
  m2b = best.suit.mod2b,    # with plateau; without mean_day_within_season & with season:exposure
  m3b = best.suit.mod3b,    # with plateau; including mean_day_within_season
  m4b = best.suit.mod4b,    # with plateau; including mean_day_within_season & season:exposure
  m5b = best.suit.mod5b,    # with plateau; including mean_day_within_season & season:mean_day_within_season
  m6b = best.suit.mod6b,    # with plateau; including mean_day_within_season & season:mean_day_within_season & season:exposure
  m7b = best.suit.mod7b,    # with plateau; including log(mean_day_within_season)
  m8b = best.suit.mod8b,    # with plateau; including log(mean_day_within_season) & season:exposure
  m9b = best.suit.mod9b,    # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season)
  m10b= best.suit.mod10b,   # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure
  m11b = best.suit.mod11b,   # with plateau; including (mean_day_within_season)²
  m12b = best.suit.mod12b,   # with plateau; including (mean_day_within_season)² & season:exposure
  m13b = best.suit.mod13b,   # with plateau; including (mean_day_within_season)² & season:(mean_day_within_season)²
  m14b = best.suit.mod14b)   # with plateau; including (mean_day_within_season)² & season:(mean_day_within_season)² & season:exposure


  ## 6 weeks
models_6weeks_ab <- list(
  m1b.6 = best.suit.mod1b.6,    # with plateau; without mean_day_within_season
  m2b.6 = best.suit.mod2b.6,    # with plateau; without mean_day_within_season & with season:exposure
  m3b.6 = best.suit.mod3b.6,    # with plateau; including mean_day_within_season
  m4b.6 = best.suit.mod4b.6,    # with plateau; including mean_day_within_season & season:exposure
  m5b.6 = best.suit.mod5b.6,    # with plateau; including mean_day_within_season & season:mean_day_within_season
  m6b.6 = best.suit.mod6b.6,    # with plateau; including mean_day_within_season & season:mean_day_within_season & season:exposure
  m7b.6 = best.suit.mod7b.6,    # with plateau; including log(mean_day_within_season)
  m8b.6 = best.suit.mod8b.6,    # with plateau; including log(mean_day_within_season) & season:exposure
  m9b.6 = best.suit.mod9b.6,    # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season)
  m10b.6 = best.suit.mod10b.6,   # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure
  m11b.6 = best.suit.mod11b.6,   # with plateau; including (mean_day_within_season)²
  m12b.6 = best.suit.mod12b.6,   # with plateau; including (mean_day_within_season)² & season:exposure
  m13b.6 = best.suit.mod13b.6,   # with plateau; including (mean_day_within_season)² & season:(mean_day_within_season)²
  m14b.6 = best.suit.mod14b.6)

models_2weeks_bio <- list(
  m1b.bio.ITD = best.suit.mod1b.bio.ITD,    # with plateau; without mean_day_within_season
  m2b.bio.ITD = best.suit.mod2b.bio.ITD,    # with plateau; without mean_day_within_season & with season:exposure
  m3b.bio.ITD = best.suit.mod3b.bio.ITD,    # with plateau; including mean_day_within_season
  m4b.bio.ITD = best.suit.mod4b.bio.ITD,    # with plateau; including mean_day_within_season & season:exposure
  m5b.bio.ITD = best.suit.mod5b.bio.ITD,    # with plateau; including mean_day_within_season & season:mean_day_within_season
  m6b.bio.ITD = best.suit.mod6b.bio.ITD,    # with plateau; including mean_day_within_season & season:mean_day_within_season & season:exposure
  m7b.bio.ITD = best.suit.mod7b.bio.ITD,    # with plateau; including log(mean_day_within_season)
  m8b.bio.ITD = best.suit.mod8b.bio.ITD,    # with plateau; including log(mean_day_within_season) & season:exposure
  m9b.bio.ITD = best.suit.mod9b.bio.ITD,    # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season)
  m10b.bio.ITD = best.suit.mod10b.bio.ITD,   # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure
  m11b.bio.ITD = best.suit.mod11b.bio.ITD,   # with plateau; including (mean_day_within_season)²
  m12b.bio.ITD = best.suit.mod12b.bio.ITD,   # with plateau; including (mean_day_within_season)² & season:exposure
  m13b.bio.ITD = best.suit.mod13b.bio.ITD,   # with plateau; including (mean_day_within_season)² & season:(mean_day_within_season)²
  m14b.bio.ITD = best.suit.mod14b.bio.ITD,
  m1b.bio.BL = best.suit.mod1b.bio.BL,    # with plateau; without mean_day_within_season
  m2b.bio.BL = best.suit.mod2b.bio.BL,    # with plateau; without mean_day_within_season & with season:exposure
  m3b.bio.BL = best.suit.mod3b.bio.BL,    # with plateau; including mean_day_within_season
  m4b.bio.BL = best.suit.mod4b.bio.BL,    # with plateau; including mean_day_within_season & season:exposure
  m5b.bio.BL = best.suit.mod5b.bio.BL,    # with plateau; including mean_day_within_season & season:mean_day_within_season
  m6b.bio.BL = best.suit.mod6b.bio.BL,    # with plateau; including mean_day_within_season & season:mean_day_within_season & season:exposure
  m7b.bio.BL = best.suit.mod7b.bio.BL,    # with plateau; including log(mean_day_within_season)
  m8b.bio.BL = best.suit.mod8b.bio.BL,    # with plateau; including log(mean_day_within_season) & season:exposure
  m9b.bio.BL = best.suit.mod9b.bio.BL,    # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season)
  m10b.bio.BL = best.suit.mod10b.bio.BL,   # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure
  m11b.bio.BL = best.suit.mod11b.bio.BL,   # with plateau; including (mean_day_within_season)²
  m12b.bio.BL = best.suit.mod12b.bio.BL,   # with plateau; including (mean_day_within_season)² & season:exposure
  m13b.bio.BL = best.suit.mod13b.bio.BL,   # with plateau; including (mean_day_within_season)² & season:(mean_day_within_season)²
  m14b.bio.BL = best.suit.mod14b.bio.BL
)

models_6weeks_bio <- list(
  m1b.6.bio.ITD = best.suit.mod1b.6.bio.ITD,    # with plateau; without mean_day_within_season
  m2b.6.bio.ITD = best.suit.mod2b.6.bio.ITD,    # with plateau; without mean_day_within_season & with season:exposure
  m3b.6.bio.ITD = best.suit.mod3b.6.bio.ITD,    # with plateau; including mean_day_within_season
  m4b.6.bio.ITD = best.suit.mod4b.6.bio.ITD,    # with plateau; including mean_day_within_season & season:exposure
  m5b.6.bio.ITD = best.suit.mod5b.6.bio.ITD,    # with plateau; including mean_day_within_season & season:mean_day_within_season
  m6b.6.bio.ITD = best.suit.mod6b.6.bio.ITD,    # with plateau; including mean_day_within_season & season:mean_day_within_season & season:exposure
  m7b.6.bio.ITD = best.suit.mod7b.6.bio.ITD,    # with plateau; including log(mean_day_within_season)
  m8b.6.bio.ITD = best.suit.mod8b.6.bio.ITD,    # with plateau; including log(mean_day_within_season) & season:exposure
  m9b.6.bio.ITD = best.suit.mod9b.6.bio.ITD,    # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season)
  m10b.6.bio.ITD = best.suit.mod10b.6.bio.ITD,   # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure
  m11b.6.bio.ITD = best.suit.mod11b.6.bio.ITD,   # with plateau; including (mean_day_within_season)²
  m12b.6.bio.ITD = best.suit.mod12b.6.bio.ITD,   # with plateau; including (mean_day_within_season)² & season:exposure
  m13b.6.bio.ITD = best.suit.mod13b.6.bio.ITD,   # with plateau; including (mean_day_within_season)² & season:(mean_day_within_season)²
  m14b.6.bio.ITD = best.suit.mod14b.6.bio.ITD,
  m1b.6.bio.BL = best.suit.mod1b.6.bio.BL,    # with plateau; without mean_day_within_season
  m2b.6.bio.BL = best.suit.mod2b.6.bio.BL,    # with plateau; without mean_day_within_season & with season:exposure
  m3b.6.bio.BL = best.suit.mod3b.6.bio.BL,    # with plateau; including mean_day_within_season
  m4b.6.bio.BL = best.suit.mod4b.6.bio.BL,    # with plateau; including mean_day_within_season & season:exposure
  m5b.6.bio.BL = best.suit.mod5b.6.bio.BL,    # with plateau; including mean_day_within_season & season:mean_day_within_season
  m6b.6.bio.BL = best.suit.mod6b.6.bio.BL,    # with plateau; including mean_day_within_season & season:mean_day_within_season & season:exposure
  m7b.6.bio.BL = best.suit.mod7b.6.bio.BL,    # with plateau; including log(mean_day_within_season)
  m8b.6.bio.BL = best.suit.mod8b.6.bio.BL,    # with plateau; including log(mean_day_within_season) & season:exposure
  m9b.6.bio.BL = best.suit.mod9b.6.bio.BL,    # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season)
  m10b.6.bio.BL = best.suit.mod10b.6.bio.BL,   # with plateau; including log(mean_day_within_season) & season:log(mean_day_within_season) & season:exposure
  m11b.6.bio.BL = best.suit.mod11b.6.bio.BL,   # with plateau; including (mean_day_within_season)²
  m12b.6.bio.BL = best.suit.mod12b.6.bio.BL,   # with plateau; including (mean_day_within_season)² & season:exposure
  m13b.6.bio.BL = best.suit.mod13b.6.bio.BL,   # with plateau; including (mean_day_within_season)² & season:(mean_day_within_season)²
  m14b.6.bio.BL = best.suit.mod14b.6.bio.BL
)

stats_models_2weeks_ab <- do.call(
  rbind,
  Map(extract_model_stats, models_2weeks_ab, names(models_2weeks_ab))
)

stats_models_6weeks_ab <- do.call(
  rbind,
  Map(extract_model_stats, models_6weeks_ab, names(models_6weeks_ab))
)

stats_models_2weeks_bio <- do.call(
  rbind,
  Map(extract_model_stats, models_2weeks_bio, names(models_2weeks_bio))
)

stats_models_6weeks_bio <- do.call(
  rbind,
  Map(extract_model_stats, models_6weeks_bio, names(models_6weeks_bio))
)

write.csv2(stats_models_2weeks_ab,'analysis_bees_diversity/data/stats_models_2weeks_ab.csv')
write.csv2(stats_models_6weeks_ab,'analysis_bees_diversity/data/stats_models_6weeks_ab.csv')
write.csv2(stats_models_2weeks_bio,'analysis_bees_diversity/data/stats_models_2weeks_bio.csv')
write.csv2(stats_models_6weeks_bio,'analysis_bees_diversity/data/stats_models_6weeks_bio.csv')

summary(best.suit.mod1b.6.bio.BL)



summary(best.suit.mod7b)
summary(best.suit.mod9b)
summary(best.suit.mod7b.bio.BL)
summary(best.suit.mod9b.bio.BL)
summary(best.suit.mod7b.6)
summary(best.suit.mod9b.6)
summary(best.suit.mod7b.6.bio.BL)
summary(best.suit.mod9b.6.bio.BL)

plot(best.suit.mod7b.6)
plot(best.suit.mod9b.6)
plot(best.suit.mod7b.6.bio.BL)
plot(best.suit.mod9b.6.bio.BL)

summary(best.suit.mod7b.6)
summary(best.suit.mod7b.6)


sjPlot::tab_model(best.suit.mod7b.6)
sjPlot::tab_model(best.suit.mod7b.6.bio.BL_no_int)

performance::check_model(best.suit.mod7b)
performance::check_model(best.suit.mod7b.6)

hist(dat$log.ab)
plot(meta.sample$exposure, dat$suitability.score)

# install.packages("MuMIn")
# library(MuMIn)
# r.squaredGLMM(best.suit.mod9)
# r.squaredGLMM(best.suit.mod9b)
# 
# install.packages("DHARMa")
# library(DHARMa)
# simulateResiduals(r.squaredGLMM(best.suit.mod1), plot = T) 

simulateResiduals(best.suit.mod1, plot = T)
simulateResiduals(best.suit.mod1b, plot = T)


# # 7. Model comparison (with buildmer) ----
# # This package does some automatic full-model comparison... all possible combinations... 
# install.packages("Matrix")
# library(buildmer); library(glmmTMB)
# 
# # teststs<-glmmTMB(total.abundance ~ exposure + season +
# #                         (1|landscape)+(1|year)+(1|site), 
# #                       family = poisson, data=meta.sample)
# # summary(teststs)
# # simulateResiduals(teststs, plot = T) # good model diagnostics...
# 
# ## 7.1 Models WITHOUT plateau ----
# constants.grid[best.constants,]
# interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]
# 
# dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants])
# 
# if(interaction==1){
#   test<-glmmTMB(log.ab ~ suitability.score + season + suitability.score:season + 
#                      exposure + season:exposure + 
#                      log(starting.day) + season:log(starting.day) +
#                     # starting.day + season:starting.day +
#                     # starting.day.qd + season:starting.day.qd +
#                      (1|landscape/site)+(1|year),
#                    family = poisson, data=dat)}else{
#                      test<-glmmTMB(log.ab ~ suitability.score + season +
#                                         exposure + season:exposure + 
#                                         log(starting.day) + season:log(starting.day) + 
#                                       # starting.day + season:starting.day +
#                                       # starting.day.qd + season:starting.day.qd +
#                                         (1|landscape/site)+(1|year),
#                                       family = poisson, data=dat)
#                    }
# 
# summary(test)
# simulateResiduals(test, plot = T) 
# 
# 
# if(interaction==1){
#   test_build<-buildmer(log.ab ~ suitability.score + season + suitability.score:season + 
#                   exposure + season:exposure + 
#                   log(starting.day) + season:log(starting.day) +
#                   # starting.day + season:starting.day +
#                   # starting.day.qd + season:starting.day.qd +
#                   (1|landscape/site)+(1|year),
#                   data=dat,
#                   family = poisson, )}else{
#                     test_build<-buildmer(log.ab ~ suitability.score + season +
#                                   exposure + season:exposure + 
#                                   log(starting.day) + season:log(starting.day) + 
#                                   # starting.day + season:starting.day +
#                                   # starting.day.qd + season:starting.day.qd +
#                                   (1|landscape/site)+(1|year),
#                                   data=dat,
#                                   family = poisson)
#                   }
# 
# 
# test_build<-buildmer(log.ab ~ suitability.score + season +
#                        exposure + season:exposure + 
#                        log(starting.day) + season:log(starting.day) + 
#                        (1|landscape/site)+(1|year),
#                      data=dat,
#                      buildmerControl = buildmerControl(include = ~ suitability.score, calc.anova = TRUE, ddf = "Satterthwaite"))
# 
# summary(test_build)
# summary(best.suit.mod7)
# 
# test_build 
# library(buildmer)
# max_mod <- buildmer(other_like ~ other_attr_c*other_intel_c + 
#                       (1 + other_attr_c*other_intel_c|iid) + 
#                       (1 + other_attr_c*other_intel_c|pid), 
#                     data=dat, 
#                     buildmerControl = buildmerControl(include = ~ other_attr_c*other_intel_c, calc.anova = TRUE, ddf = "Satterthwaite"))
# 
# ## 7.2 Models WITH plateau ----
# constants.grid[best.constants2,]
# interaction2<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]
# 
# dat<-data.frame(testing.data,suitability.score=suitability.matrix2.norm[testing.ID,best.constants2])
# 
# if(interaction2==1){
#   testb<-glmmTMB(log.ab ~ suitability.score + season + suitability.score:season + 
#                      exposure + season:exposure + 
#                      starting.day + season:starting.day +
#                      log(starting.day) + season:log(starting.day) +
#                      starting.day.qd + season:starting.day.qd +
#                      (1|landscape/site)+(1|year),
#                    family = poisson, data=dat)}else{
#                      testb<-glmmTMB(log.ab ~ suitability.score + season +
#                                         exposure + season:exposure + 
#                                         starting.day + season:starting.day +
#                                         log(starting.day) + season:log(starting.day) +
#                                         starting.day.qd + season:starting.day.qd +
#                                         (1|landscape/site)+(1|year),
#                                       family = poisson, data=dat)
#                    }
#   
# summary(testb)
# simulateResiduals(testb, plot = T)
# 
# 
# 
# 
# 
# 
# 
# 
# 




# 8. Visualization ------
## 8.1 extract effect sizes & confidence intervals of significant predictor variables -----
# install.packages("broom.mixed")
# library(broom.mixed)
# 
# m_ab_2 <- tidy(
#   best.suit.mod7b,
#   effects = "fixed",
#   conf.int = TRUE
# )
# m_ab_2$model <- "ab_2_weeks"
# 
# m_bio_BL_2 <- tidy(
#   best.suit.mod7b.bio.BL,
#   effects = "fixed",
#   conf.int = TRUE
# )
# m_bio_BL_2$model <- "bio_BL_2_weeks"
# 
# 
# m_ab_6 <- tidy(
#   best.suit.mod7b.6,
#   effects = "fixed",
#   conf.int = TRUE
# )
# m_ab_6$model <- "ab_6_weeks"
# 
# m_bio_BL_6 <- tidy(
#   best.suit.mod7b.6.bio.BL,
#   effects = "fixed",
#   conf.int = TRUE
# )
# m_bio_BL_6$model <- "bio_BL_6_weeks"

## standardize model parameters: 
# install.packages("parameters")
# library(parameters)

ab_2_scaled <- standardize_parameters(
  best.suit.mod7b,
  method = "refit",
  ci = 0.95,
  robust = FALSE,
  two_sd = FALSE,
  include_response = TRUE,
  verbose = TRUE
)
as.data.frame(ab_2_scaled)
ab_2_scaled$model <- "Abundance, 2 weeks"

bio_2_scaled <- standardize_parameters(
  best.suit.mod7b.bio.BL,
  method = "refit",
  ci = 0.95,
  robust = FALSE,
  two_sd = FALSE,
  include_response = TRUE,
  verbose = TRUE
)
as.data.frame(bio_2_scaled)
bio_2_scaled$model <- "Biomass (BL), 2 weeks"

ab_6_scaled <- standardize_parameters(
  best.suit.mod7b.6,
  method = "refit",
  ci = 0.95,
  robust = FALSE,
  two_sd = FALSE,
  include_response = TRUE,
  verbose = TRUE
)
as.data.frame(ab_6_scaled)
ab_6_scaled$model <- "Abundance, 6 weeks"

bio_6_scaled <- standardize_parameters(
  best.suit.mod7b.6.bio.BL_no_int,
  method = "refit",
  ci = 0.95,
  robust = FALSE,
  two_sd = FALSE,
  include_response = TRUE,
  verbose = TRUE
)
as.data.frame(bio_6_scaled)
bio_6_scaled$model <- "Biomass (BL), 6 weeks"

# combine into two data frames: one for abundance models, one for biomass models
df_ab_scaled <- rbind(
  ab_2_scaled, 
  ab_6_scaled)
df_ab_scaled <- subset(df_ab_scaled, Parameter != "(Intercept)")
df_ab_scaled$Parameter <- factor(df_ab_scaled$Parameter, 
                                 levels = c("suitability.score:seasonsummer",
                                            "log(mean_day_within_season)",
                                            "seasonsummer",
                                            "suitability.score",
                                            "exposure"))

# combine into two data frames: one for abundance models, one for biomass models
df_bio_scaled <- rbind(
  bio_2_scaled, 
  bio_6_scaled)
df_bio_scaled <- subset(df_bio_scaled, Parameter != "(Intercept)")
df_bio_scaled$Parameter <- factor(df_bio_scaled$Parameter, 
                                   levels = c("suitability.score:seasonsummer",
                                              "log(mean_day_within_season)",
                                              "seasonsummer",
                                              "suitability.score",
                                              "exposure"))
# combine into one data frame
df_scaled <- rbind(
  ab_2_scaled, 
  ab_6_scaled,
  bio_2_scaled, 
  bio_6_scaled)
df_scaled <- subset(df_scaled, Parameter != "(Intercept)")
df_scaled$Parameter <- factor(df_scaled$Parameter, 
                                  levels = c("suitability.score:seasonsummer",
                                             "log(mean_day_within_season)",
                                             "seasonsummer",
                                             "suitability.score",
                                             "exposure"))




## 8.2 create forest plots ------
library(ggplot2)

### 8.2.1 all predictors ------
pd <- position_dodge(width = 0.3)

forest_plot_ab_scaled  <- ggplot(df_ab_scaled,
                                  aes(x = Std_Coefficient,
                                      y = Parameter,
                                      xmin = CI_low,
                                      xmax = CI_high,
                                      color = model,
                                      group = model)) +
  geom_point(size = 3, position = pd) +
  geom_errorbarh(height = 0.2, position = pd) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Standardized Effect Size",
    y = "Predictor",
    color = "Model"
  ) +
  theme_minimal()

forest_plot_bio_scaled  <- ggplot(df_bio_scaled,
         aes(x = Std_Coefficient,
             y = Parameter,
             xmin = CI_low,
             xmax = CI_high,
             color = model,
             group = model)) +
  geom_point(size = 3, position = pd) +
  geom_errorbarh(height = 0.2, position = pd) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Standardized Effect Size",
    y = "Predictor",
    color = "Model"
  ) +
  theme_minimal()

forest_plot_scaled  <- ggplot(df_scaled,
                                  aes(x = Std_Coefficient,m m Abend mit deriner
                                      y = Parameter,
                                      xmin = CI_low,
                                      xmax = CI_high,
                                      color = model,
                                      group = model)) +
  geom_point(size = 3, position = pd) +
  geom_errorbarh(height = 0.2, position = pd) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Standardized Effect Size",
    y = "Predictor",
    color = "Model"
  ) +
  theme_minimal()





# 9. Key figures ----
## 9.1  (3b)	Histogram of average suitability score (eventuell 2 hist – nach den seasons getrennt) ---

# both seasons:
hist(suitability.matrix2.norm)

# SPRING/SUMMER: 
# # without plateau:
# hist(suitability.matrix.norm.spring)
# hist(suitability.matrix.norm.summer)

# with plateau:
hist(suitability.matrix2.norm.spring)
hist(suitability.matrix2.norm.summer)

## 9.2 (3d) Bi-plot: average suitability score versus abundance per day for each season 


# Next steps -------
# (0) Look through data from Mark to identify true negatives and add them to the data - then rerun ALL analysis 
# --> !!! still to DO !!!

# (1) change the code in the model comparison so that the testing is used to compare models
# ---> DONE:
  # - used training.data to (l.132-149) run mod.1 (with interaction effect) and mod.2 (without interaction effect) over ALL suitability score constant combinations and to identify best.constants
  # - then used testing.data to run best.suit.mod (l. 164-177)
  # - used testing.data to compare models with either suitability score or exposure days as predictor

## (2) Change the function for the suitability score in a way that it has a plateau - but keep the old version of the code 
# ---> DONE: model comparison shows that model without plateau performs slightly better

## (3) get starting day into the meta.sample data 
# --> DONE: l. 24

## (4) include starting day as additional fixed effect predictor in the models
# --> DONE: --> improves the model

## (5) check again: effect of not using training and testing data with best.suit.mod1
# --> DONE: but direct AIC comparison not possible as models are fitted to different number of observations

## (6) given that the effect of season is very strong, it might better to run two models - one for each season (to determine if this is really better)
#     --> suitability score optimisation needs to run twice as well (?)
# --> DONE: 
  # - Suitability score NOT significant in summer model 
  # - Slightly larger effect of starting day in spring model (0.026 vs 0.017 (summer))
  # EXPLANATION??

# (7) run a model with suitability score that has been normalized by exposure days
# --> DONE 
# to DO: check & interpret results 

# (8) try to play around with extreme data points 
# --> DONE but results still needs to be checked thoroughly
## alternatively: exclude males? 
# --> DONE but results still needs to be checked thoroughly



# (9) try to log-transform the numeric predictor (suitability score/ exposure days): this assume that trap efficiency decreases when traps get fuller
### ??? wouldn't it be the other way round???? --> ask ALFRED
#     this really only a side point
## additional question: why did we log-transform the response variable (abundance)?

# (10) rerun the model comparison and see what happens; check residuals

# (11) try to get the biomass data ready, so we can also run a biomass-optimsised model
# (12) biomass model - what happens here in the model comparison?



# thoughts from 03.12.2025:
# rerun model with exposure days but use 6-week sampling period as sample unit (instead of 2-week exposure); 
# idea behind: maybe 2 week-samplings don't show enough variability with respect to exposure days 
# --> thus: no effect in the model

# 
# for later:
# # (14) Beim letzten Analyseschritt waers auch moeglich die Daten innerhalb eines Jahres zu rarefizieren. 
# Dann koennte man sich auch z.b. das Verhaeltnis zwischen richness und abundance anschaun, und schaun, ob das durch das Rarefizieren klarer wird...


# discussed on 06.01.2026
## open to DOs

# 1. Redo comparison of model(s) WITH and WITHOUT plateau (for all model variants)
## therefore: recalculate suitability matrix for spring/sumemr/norm --> DONE
## --> DONE

# 2. choose ONE model which serves as base to determine best constants (suitability score) (use most complex model) 
## --> use these best constants for ALL models during model selection (exception: separate spring/summer models)
## --> DONE

# 3. Model comparison: 
## base model with fixed effects not subject to selection: season, suitability score (weather) (normalized by exposure days), exposure days
## part of model selection: 
### season:weather --> already done with interaction-function during selection of best.constants
### season:exposuredays
### starting.day
### season:starting.day 
### log(starting.day)
### season:log(starting.day)
### (starting-day)²
### season:(starting.day)²
## --> DONE

## (if including season:starting.day turns out to be the best model --> test if taking out collinearity between season and starting day by using starting day within season (subtrahiere 1. SamplingTag pro Season))
## YES --> OPEN

# 4. Redo Model comparison for separate SPRING and SUMMER models
## open

# 5. Redo steps above for sampling time of 6 weeks (now 2 weeks)

# 7. Redo all steps above for biomass as response variable (use best constant as determined for abundance as response variable)



## exclude for now, check if still needed: model where females or extreme values are excluded --------
### models without plateau:
# ## 3.1.1 Model including log(starting.day) and season:log(starting.day) just FEMALE abundance -----
# if(interaction==1){
#   dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.fem])
#   # with interaction effect
#   best.suit.mod1b.fem<-lmer(data= dat, 
#                             log.ab.fem~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
#                               (1|landscape/site)+(1|year), REML = T)}else{
#                                 # without interaction effect
#                                 dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.fem])
#                                 best.suit.mod1b.fem<-lmer(data= dat, 
#                                                           log.ab.fem~ suitability.score + season + log(starting.day) + season:log(starting.day) +
#                                                             (1|landscape/site)+(1|year), REML = T)}

# ## 3.1.2b Model INCLUDING log(starting.day) and season:log(starting.day) without extreme abundance values > 1000 -----
# 
# AIC.with.interaction<-c()
# AIC.without.interaction<-c()
# 
# for(i in 1:ncol(suitability.matrix)){
#   dat<-data.frame(training.data, suitability.score=suitability.matrix[-testing.ID,i])
#   # with interaction effect
#   mod.1<-lmer(data= dat, 
#               log.ab.no.extr~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
#                 (1|landscape/site)+(1|year), REML = T)
#   # without interaction effect
#   mod.2<-lmer(data= dat, 
#               log.ab.no.extr~ suitability.score + season + log(starting.day) + season:log(starting.day) +
#                 (1|landscape/site)+(1|year), REML = T)
#   AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
#   AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
#   print(i)
# }
# mod.output1b.no.extr<-data.frame(AIC.with.interaction, AIC.without.interaction)
# mod.output1b.no.extr$min<-apply(mod.output1b.no.extr,1,function(x){min(x)})
# 
# best.constants1b.no.extr<- which(mod.output1b.no.extr$min==min(mod.output1b.no.extr$min))
# 
# constants.grid[best.constants1b.no.extr,]
# interaction<-which(mod.output1b.no.extr[best.constants1b.no.extr,]==min(mod.output1b.no.extr[best.constants1b.no.extr,]) )[1]
# 
# if(interaction==1){
#   dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.no.extr])
#   # with interaction effect
#   best.suit.mod1b.no.extr<-lmer(data= dat, 
#                                 log.ab.no.extr~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
#                                   (1|landscape/site)+(1|year), REML = T)}else{
#                                     # without interaction effect
#                                     dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.no.extr])
#                                     best.suit.mod1b.no.extr<-lmer(data= dat, 
#                                                                   log.ab.no.extr~ suitability.score + season + log(starting.day) + season:log(starting.day) +
#                                                                     (1|landscape/site)+(1|year), REML = T)}

### models with plateau:
# ## 3.1.2b Model INCLUDING log(starting.day) and season:log(starting.day) just FEMALE abundance -----
# 
# AIC.with.interaction<-c()
# AIC.without.interaction<-c()
# 
# for(i in 1:ncol(suitability.matrix)){
#   dat<-data.frame(training.data, suitability.score=suitability.matrix[-testing.ID,i])
#   # with interaction effect
#   mod.1<-lmer(data= dat, 
#               log.ab.fem~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
#                 (1|landscape/site)+(1|year), REML = T)
#   # without interaction effect
#   mod.2<-lmer(data= dat, 
#               log.ab.fem~ suitability.score + season + log(starting.day) + season:log(starting.day) +
#                 (1|landscape/site)+(1|year), REML = T)
#   AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
#   AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
#   print(i)
# }
# mod.output1b.fem<-data.frame(AIC.with.interaction, AIC.without.interaction)
# mod.output1b.fem$min<-apply(mod.output1b.fem,1,function(x){min(x)})
# 
# best.constants1b.fem<- which(mod.output1b.fem$min==min(mod.output1b.fem$min))
# 
# constants.grid[best.constants1b.fem,]
# interaction<-which(mod.output1b.fem[best.constants1b.fem,]==min(mod.output1b.fem[best.constants1b.fem,]) )[1]
# 
# if(interaction==1){
#   dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.fem])
#   # with interaction effect
#   best.suit.mod1b.fem<-lmer(data= dat, 
#                             log.ab.fem~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
#                           (1|landscape/site)+(1|year), REML = T)}else{
#                             # without interaction effect
#                             dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.fem])
#                             best.suit.mod1b.fem<-lmer(data= dat, 
#                                                       log.ab.fem~ suitability.score + season + log(starting.day) + season:log(starting.day) +
#                                                     (1|landscape/site)+(1|year), REML = T)}
# 
# 
# ## 3.1.2b Model INCLUDING log(starting.day) and season:log(starting.day) without extreme abundance values > 1000 -----
# 
# AIC.with.interaction<-c()
# AIC.without.interaction<-c()
# 
# for(i in 1:ncol(suitability.matrix)){
#   dat<-data.frame(training.data, suitability.score=suitability.matrix[-testing.ID,i])
#   # with interaction effect
#   mod.1<-lmer(data= dat, 
#               log.ab.no.extr~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
#                 (1|landscape/site)+(1|year), REML = T)
#   # without interaction effect
#   mod.2<-lmer(data= dat, 
#               log.ab.no.extr~ suitability.score + season + log(starting.day) + season:log(starting.day) +
#                 (1|landscape/site)+(1|year), REML = T)
#   AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
#   AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
#   print(i)
# }
# mod.output1b.no.extr<-data.frame(AIC.with.interaction, AIC.without.interaction)
# mod.output1b.no.extr$min<-apply(mod.output1b.no.extr,1,function(x){min(x)})
# 
# best.constants1b.no.extr<- which(mod.output1b.no.extr$min==min(mod.output1b.no.extr$min))
# 
# constants.grid[best.constants1b.no.extr,]
# interaction<-which(mod.output1b.no.extr[best.constants1b.no.extr,]==min(mod.output1b.no.extr[best.constants1b.no.extr,]) )[1]
# 
# if(interaction==1){
#   dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.no.extr])
#   # with interaction effect
#   best.suit.mod1b.no.extr<-lmer(data= dat, 
#                             log.ab.no.extr~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
#                               (1|landscape/site)+(1|year), REML = T)}else{
#                                 # without interaction effect
#                                 dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.no.extr])
#                                 best.suit.mod1b.no.extr<-lmer(data= dat, 
#                                                           log.ab.no.extr~ suitability.score + season + log(starting.day) + season:log(starting.day) +
#                                                             (1|landscape/site)+(1|year), REML = T)}
# 




hist(meta.sample$log.ab, breaks= 500)
hist(weather.data$temp, breaks= 500)
hist(log(weather.data$prec+1), breaks= 500)



abund.1<-lmer(data= meta.trapyearseason, 
              abundance~ exposure_days + season + exposure_days:season + 
                elevation_mean_400m + elevation_range_400m + hab.even + hab.proportion + hab.div+ year +
                (1|location)+(1|year)+(1|site), REML = T)
abund.1<-lmer(data= meta.trapyearseason, 
              abundance~ exposure_days + season + exposure_days:season + 
                elevation_mean_400m + elevation_range_400m + year +
                (1|location)+(1|year)+(1|site), REML = T)
summary(abund.1)


rich.1<-lmer(data= meta.trapyearseason, 
             richness~ exposure_days + season + exposure_days:season + 
               elevation_mean_400m + elevation_range_400m + hab.even + hab.proportion + hab.div+ year +
               (1|location)+(1|year)+(1|site), REML = T)
summary(rich.1)

library(performance) 
r2(rich.1)
r2(abund.1)


library(ggplot2)


# look at abundance trend over time... 
ggplot(data= meta.trapyearseason, aes(x=year,))

colnames(meta.trapyearseason)
str(meta.trapyearseason)
# IDEA: water (rivers) might be a good additional predictor - we could explore this at a later point in time


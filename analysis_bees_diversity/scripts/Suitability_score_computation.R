# 1. load in data
meta.sample<- read.csv('analysis_bees_diversity/data/meta_sample.csv',sep = ',', dec = '.')

weather.data<- read.csv('analysis_bees_diversity/data/data_weather/all_dat_temp_prec.csv',sep = ',', dec = '.')

# add starting day into the meta.sample data
library(lubridate)
meta.sample$starting.day <- yday(meta.sample$StartDate)
meta.sample$starting.day.qd <- (meta.sample$starting.day)^2

# convert the dates into a date format
weather.data$hour.x<- as.POSIXct(weather.data$hour.x, format = "%Y-%m-%d %H:%M:%S")
# add day of the year to calculate exposure days
weather.data$yday <- yday(weather.data$hour.x)

# do the same for the meta data dates
meta.sample$StartDate<- as.POSIXct( paste0(meta.sample$StartDate, ' 12:00:00'), format = "%Y-%m-%d %H:%M:%S")
meta.sample$EndDate<- as.POSIXct( paste0(meta.sample$EndDate, ' 12:00:00'), format = "%Y-%m-%d %H:%M:%S")

# create a list that contains for each sample (row in meta.sample) a data-frame with all daylight sampling hours 
input.data<-list()
for(i in 1:nrow(meta.sample)){
  input.data[[i]]<-weather.data[which(weather.data$hour.x>=meta.sample$StartDate[i] & weather.data$hour.x<meta.sample$EndDate[i] &
                                        weather.data$trap==meta.sample$site[i]) , ]
}

## create list that contains for each sample (row in meta.sample) a data-frame with all daylight sampling hours for SPRING
meta.sample.spring <- dplyr::filter(meta.sample, 
                                   `season` == "spring")
input.data.spring<-list()
for(i in 1:nrow(meta.sample.spring)){
  input.data.spring[[i]]<-weather.data[which(weather.data$hour.x>=meta.sample.spring$StartDate[i] & weather.data$hour.x<meta.sample.spring$EndDate[i] &
                                        weather.data$trap==meta.sample.spring$site[i]) , ]
}

## create list that contains for each sample (row in meta.sample) a data-frame with all daylight sampling hours for SUMMER
meta.sample.summer <- dplyr::filter(meta.sample, 
                                    `season` == "summer")
input.data.summer<-list()
for(i in 1:nrow(meta.sample.summer)){
  input.data.summer[[i]]<-weather.data[which(weather.data$hour.x>=meta.sample.summer$StartDate[i] & weather.data$hour.x<meta.sample.summer$EndDate[i] &
                                               weather.data$trap==meta.sample.summer$site[i]) , ]
}

rm(weather.data)


# 1.2 visualisation of relationships
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


# 2. compute suitability scores for wild-bee pollination for each sampling interval  -------

## 2.1 Suitability score WITHOUT plateau --> output: suitability.matrix ----
# there are three constants in the formula that defines the suitability of temp for pollination
# here, we define their range
t.opt <- seq(15,27, length=15) # optimal temperature - highest activity
t.max <- seq(25,45, length=15) # maximal temperature - defines when activity becomes 0 
sigma <- seq(0.5,5, length=12) # defines the shape of the sigmodid shape of bee activity below t.opt
constants.grid<- expand.grid(t.opt, t.max, sigma)
names(constants.grid) <- c("t.opt", "t.max", "sigma")

# account for the fact that max temp needs to be at least 1 degree above opt. temperature
constants.grid<- constants.grid[which(constants.grid$t.opt<=constants.grid$t.max-1),]
rm(t.opt, t.max, sigma)

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
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)

## 2.2 Suitability score WITH plateau --> output: suitability.matrix2 ----
# there are three constants in the formula that defines the suitability of temp for pollination
# here, we define their range
t.opt.min <- seq(15,25, length=10) # minimum plateau temperature - minimum temperature for highest activity
t.opt.max <- seq(25,35, length=10) #  maximum plateau temperature - maxmimum temperature for highest activity
t.max <- seq(35,50, length=10)  # maximal temperature - defines when activity becomes 0 
sigma <- seq(0.5,5, length=8) # defines the shape of the sigmodid shape of bee activity below t.opt
constants.grid2<- expand.grid(t.opt.min, t.opt.max, t.max, sigma)
names(constants.grid2) <- c("t.opt.min", "t.opt.max", "t.max", "sigma")

# account for the fact that max pleateau temp needs to be at least 1 degree above min plateau temperature temperature
constants.grid2<- constants.grid2[which(constants.grid2$t.opt.min<=constants.grid2$t.opt.max-1),]
# account for the fact that max temp needs to be at least 1 degree above max plateau (opt) temperature
constants.grid2<- constants.grid2[which(constants.grid2$t.opt.max<=constants.grid2$t.max-1),]

rm(t.opt.min, t.opt.max, t.max, sigma)

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
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix2<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)



### 3. determine the optimal constant combination for computing the suitability score  -------
## 3.1 using Suitability score WITHOUT plateau ----
library(lme4); library(lmerTest);

# we work with log abundance to make outliers less influential
meta.sample$log.ab<-log(meta.sample$total.abundance+1)
# we work with log abundance to make outliers less influential
meta.sample$log.ab.fem<-log(meta.sample$female.abundance+1)

# exclude extreme abundance values > 1000: 
meta.sample$total.abundance.no.extr <- meta.sample$total.abundance
meta.sample$total.abundance.no.extr[which(meta.sample$total.abundance.no.extr>1000)] <- NA
meta.sample$log.ab.no.extr<-log(meta.sample$total.abundance.no.extr+1)

#determine testing and training data:
set.seed(42)

testing.ID<-sample(1:nrow(meta.sample), round(nrow(meta.sample)*0.2))
training.data<- meta.sample[-testing.ID, ]
testing.data<- meta.sample[testing.ID, ]

nrow(suitability.matrix)

## 3.1.1 Model without starting.day ------
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season + starting.day +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season + starting.day +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output0<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output0$min<-apply(mod.output0,1,function(x){min(x)})

best.constants0<-which(mod.output0$min==min(mod.output0$min))

## OLD Version (no differentiation in training and test data), with: 
#                     t.opt <- seq(15,27, length=10) # optimal temperature - highest activity
#                     t.max <- seq(25,45, length=10) # maximal temperature - defines when activity becomes 0 
#                     sigma <- seq(0.5,5, length=10) 
# --> best constants: 951 -->    t.opt t.max sigma#
#                       991    15    45     5

constants.grid[best.constants0,]
interaction<-which(mod.output0[best.constants0,]==min(mod.output0[best.constants0,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants0])
  # with interaction effect
  best.suit.mod0<-lmer(data= dat, 
                      log.ab~ suitability.score + season + suitability.score:season + 
                        (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
                          dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants0])
                          best.suit.mod0<-lmer(data= dat, 
                                              log.ab~ suitability.score + season + 
                                                (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod0)
plot(best.suit.mod0)

## 3.1.2 Model INCLUDING starting.day ------
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season + starting.day +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season + starting.day +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output$min<-apply(mod.output,1,function(x){min(x)})

best.constants<-which(mod.output$min==min(mod.output$min))

constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants])
  # with interaction effect
  best.suit.mod<-lmer(data= dat, 
                      log.ab~ suitability.score + season + suitability.score:season + starting.day +
                        (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
   dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants])
  best.suit.mod<-lmer(data= dat, 
                      log.ab~ suitability.score + season + starting.day +
                        (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod)
plot(best.suit.mod)


# # OLD Version (no differentiation in training and test data)
# constants.grid[best.constants,]
# interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]
# 
# if(interaction==1){
#   dat<-data.frame(meta.sample,suitability.score=suitability.matrix[,best.constants])
#   # without interaction effect
#   best.suit.mod<-lmer(data= dat, 
#               log.ab~ suitability.score + season +  
#                 (1|landscape/site)+(1|year), REML = T)}else{
#   dat<-data.frame(meta.sample,suitability.score=suitability.matrix[,best.constants])
#   best.suit.mod<-lmer(data= dat, 
#               log.ab~ suitability.score + season + suitability.score:season +
#                 (1|landscape/site)+(1|year), REML = T)}

## 3.1.2a Model INCLUDING log(starting.day)  -----

AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season + log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season + log(starting.day) + 
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output1a<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output1a$min<-apply(mod.output1a,1,function(x){min(x)})

best.constants1a<- which(mod.output1a$min==min(mod.output1a$min))

constants.grid[best.constants1a,]
interaction<-which(mod.output1a[best.constants1a,]==min(mod.output1a[best.constants1a,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1a])
  # with interaction effect
  best.suit.mod1a<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + 
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1a])
                            best.suit.mod1a<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + log(starting.day) + 
                                                    (1|landscape/site)+(1|year), REML = T)}


## 3.1.2b Model INCLUDING log(starting.day) and season:log(starting.day) -----

AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output1b<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output1b$min<-apply(mod.output1b,1,function(x){min(x)})

best.constants1b<- which(mod.output1b$min==min(mod.output1b$min))

constants.grid[best.constants1b,]
interaction<-which(mod.output1b[best.constants1b,]==min(mod.output1b[best.constants1b,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b])
  # with interaction effect
  best.suit.mod1b<-lmer(data= dat, 
                      log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
                        (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
   dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b])
  best.suit.mod1b<-lmer(data= dat, 
                      log.ab~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                        (1|landscape/site)+(1|year), REML = T)}


## 3.1.2b Model INCLUDING log(starting.day) and season:log(starting.day) just FEMALE abundance -----

AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab.fem~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab.fem~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output1b.fem<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output1b.fem$min<-apply(mod.output1b.fem,1,function(x){min(x)})

best.constants1b.fem<- which(mod.output1b.fem$min==min(mod.output1b.fem$min))

constants.grid[best.constants1b.fem,]
interaction<-which(mod.output1b.fem[best.constants1b.fem,]==min(mod.output1b.fem[best.constants1b.fem,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.fem])
  # with interaction effect
  best.suit.mod1b.fem<-lmer(data= dat, 
                            log.ab.fem~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.fem])
                            best.suit.mod1b.fem<-lmer(data= dat, 
                                                      log.ab.fem~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                                                    (1|landscape/site)+(1|year), REML = T)}


## 3.1.2b Model INCLUDING log(starting.day) and season:log(starting.day) without extreme abundance values > 1000 -----

AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab.no.extr~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab.no.extr~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output1b.no.extr<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output1b.no.extr$min<-apply(mod.output1b.no.extr,1,function(x){min(x)})

best.constants1b.no.extr<- which(mod.output1b.no.extr$min==min(mod.output1b.no.extr$min))

constants.grid[best.constants1b.no.extr,]
interaction<-which(mod.output1b.no.extr[best.constants1b.no.extr,]==min(mod.output1b.no.extr[best.constants1b.no.extr,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.no.extr])
  # with interaction effect
  best.suit.mod1b.no.extr<-lmer(data= dat, 
                            log.ab.no.extr~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
                              (1|landscape/site)+(1|year), REML = T)}else{
                                # without interaction effect
                                dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1b.no.extr])
                                best.suit.mod1b.no.extr<-lmer(data= dat, 
                                                          log.ab.no.extr~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                                                            (1|landscape/site)+(1|year), REML = T)}


## 3.1.2c Model INCLUDING (starting.day)² -----
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season + starting.day.qd + 
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season + starting.day.qd +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output1c<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output1c$min<-apply(mod.output1c,1,function(x){min(x)})

best.constants1c<- which(mod.output1c$min==min(mod.output1c$min))

constants.grid[best.constants1c,]
interaction<-which(mod.output1c[best.constants1c,]==min(mod.output1c[best.constants1c,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1c])
  # with interaction effect
  best.suit.mod1c<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + starting.day.qd +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1c])
                            best.suit.mod1c<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + starting.day.qd +
                                                    (1|landscape/site)+(1|year), REML = T)}



## 3.1.2d Model INCLUDING (starting.day)² and season:(starting.day)²-----
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season + starting.day.qd + season:starting.day.qd +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season + starting.day.qd + season:starting.day.qd +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output1d<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output1d$min<-apply(mod.output1d,1,function(x){min(x)})

best.constants1d<- which(mod.output1d$min==min(mod.output1d$min))

constants.grid[best.constants1d,]
interaction<-which(mod.output1d[best.constants1d,]==min(mod.output1d[best.constants1d,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1d])
  # with interaction effect
  best.suit.mod1d<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + starting.day.qd + season:starting.day.qd +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix[testing.ID,best.constants1d])
                            best.suit.mod1d<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + starting.day.qd + season:starting.day.qd +
                                                    (1|landscape/site)+(1|year), REML = T)}

## 3.2 using Suitability score WITH plateau ----
library(lme4); library(lmerTest);

# we work with log abundance to make outliers less influential
meta.sample$log.ab<-log(meta.sample$total.abundance+1)

#determine testing and training data:
set.seed(42)

testing.ID<-sample(1:nrow(meta.sample), round(nrow(meta.sample)*0.2))
training.data<- meta.sample[-testing.ID, ]
testing.data<- meta.sample[testing.ID, ]

AIC.with.interaction<-c()
AIC.without.interaction<-c()

nrow(suitability.matrix2)

## 3.2.1 Model without starting.day ------

for(i in 1:ncol(suitability.matrix2)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix2[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output2<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output2$min<-apply(mod.output2,1,function(x){min(x)})

best.constants2<-which(mod.output2$min==min(mod.output2$min))

constants.grid2[best.constants2,]
interaction<-which(mod.output2[best.constants2,]==min(mod.output2[best.constants2,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants2])
  # with interaction effect
  best.suit.mod2<-lmer(data= dat, 
                      log.ab~ suitability.score + season + suitability.score:season +
                        (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
                          dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants2])
                          best.suit.mod2<-lmer(data= dat, 
                                              log.ab~ suitability.score + season + 
                                                (1|landscape/site)+(1|year), REML = T)}


# model without starting.day
summary(best.suit.mod2)
plot(best.suit.mod2)


## 3.2.2 Model INCLUDING starting.day ------
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix2)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix2[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season + starting.day +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season + starting.day +
                (1|landscape/site)+(1|year), REML = T)

  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output3<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output3$min<-apply(mod.output3,1,function(x){min(x)})

best.constants3<-which(mod.output3$min==min(mod.output3$min))

constants.grid2[best.constants3,]
interaction<-which(mod.output3[best.constants3,]==min(mod.output3[best.constants3,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants3])
  # with interaction effect
  best.suit.mod3<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + starting.day + 
                         (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants3])
                           best.suit.mod3<-lmer(data= dat, 
                                                log.ab~ suitability.score + season +  starting.day + 
                                                  (1|landscape/site)+(1|year), REML = T)}


# model WITH starting day
summary(best.suit.mod3)
plot(best.suit.mod3)


## 3.2.3 Model INCLUDING starting.day & season:starting.day ------

AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix2)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix2[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat,
              log.ab~ suitability.score + season + suitability.score:season + starting.day + season:starting.day +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat,
              log.ab~ suitability.score + season + starting.day + season:starting.day +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output4<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output4$min<-apply(mod.output4,1,function(x){min(x)})

best.constants4<-which(mod.output4$min==min(mod.output4$min))

constants.grid2[best.constants4,]
interaction<-which(mod.output4[best.constants4,]==min(mod.output4[best.constants4,]) )[1]



if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants4])
  # with interaction effect
  best.suit.mod4<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + starting.day + season:starting.day +
                         (1|landscape/site)+(1|year), REML = T)}else{ 
  # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants4])
                           best.suit.mod4<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + starting.day + season:starting.day +
                                                  (1|landscape/site)+(1|year), REML = T)}

# model WITH starting day
summary(best.suit.mod4)
plot(best.suit.mod4)



## 3.2.4 Model with log(starting.day) ------

AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix2)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix2[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat,
              log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + 
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat,
              log.ab~ suitability.score + season + log(starting.day) + 
                (1|landscape/site)+(1|year), REML = T)

  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output5<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output5$min<-apply(mod.output5,1,function(x){min(x)})

best.constants5<-which(mod.output5$min==min(mod.output5$min))

constants.grid2[best.constants5,]
interaction<-which(mod.output5[best.constants5,]==min(mod.output5[best.constants5,]) )[1]



if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants5])
  # with interaction effect
  best.suit.mod5<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + log(starting.day) +
                         (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants5])
                           best.suit.mod5<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + log(starting.day) +
                                                  (1|landscape/site)+(1|year), REML = T)}

# model WITH starting day
summary(best.suit.mod5)
plot(best.suit.mod5)


## 3.2.5 Model with log(starting.day) and season:log(starting.day) ------

AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix2)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix2[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat,
              log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat,
              log.ab~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output6<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output6$min<-apply(mod.output6,1,function(x){min(x)})

best.constants6<-which(mod.output6$min==min(mod.output6$min))

constants.grid2[best.constants6,]
interaction<-which(mod.output5[best.constants6,]==min(mod.output5[best.constants6,]) )[1]


if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants6])
  # with interaction effect
  best.suit.mod6<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) + 
                         (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants6])
                           best.suit.mod6<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                                                  (1|landscape/site)+(1|year), REML = T)}

# model WITH starting day
summary(best.suit.mod6)
plot(best.suit.mod6)


## 3.2.6 Model with (starting.day)^2 ------
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix2)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix2[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat,
              log.ab~  suitability.score + season + suitability.score:season + starting.day.qd +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat,
              log.ab~ suitability.score + season + starting.day.qd +
                (1|landscape/site)+(1|year), REML = T)

  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output7<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output7$min<-apply(mod.output7,1,function(x){min(x)})

best.constants7<-which(mod.output7$min==min(mod.output7$min))

constants.grid2[best.constants7,]
interaction<-which(mod.output7[best.constants7,]==min(mod.output7[best.constants7,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants7])
  # with interaction effect
  best.suit.mod7<-lmer(data= dat, 
                       log.ab~ suitability.score + season + suitability.score:season + starting.day.qd +
                         (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
                           dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants7])
                           best.suit.mod7<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + starting.day.qd +
                                                  (1|landscape/site)+(1|year), REML = T)}


# model WITH starting day
summary(best.suit.mod7)
plot(best.suit.mod7)


## 3.2.7 Model with (starting.day)^2 and season:(starting.day)^2 ----
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix2)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix2[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat,
              log.ab~  suitability.score + season + suitability.score:season + starting.day.qd + season:starting.day.qd +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat,
              log.ab~ suitability.score + season + starting.day.qd + season:starting.day.qd +
                (1|landscape/site)+(1|year), REML = T)

  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output8<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output8$min<-apply(mod.output8,1,function(x){min(x)})

best.constants7<-which(mod.output7$min==min(mod.output7$min))

constants.grid2[best.constants8,]
interaction<-which(mod.output8[best.constants8,]==min(mod.output8[best.constants8,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants8])
  # with interaction effect
  best.suit.mod8<-lmer(data= dat, 
                      log.ab~ suitability.score + season + starting.day.qd + season:starting.day.qd +
                        (1|landscape/site)+(1|year), REML = T)}else{
  # without interaction effect
                          dat<-data.frame(testing.data,suitability.score=suitability.matrix2[testing.ID,best.constants8])
                          best.suit.mod8<-lmer(data= dat, 
                                              log.ab~ suitability.score + season + suitability.score:season + starting.day.qd + season:starting.day.qd +
                                                (1|landscape/site)+(1|year), REML = T)}

# model WITH starting day
summary(best.suit.mod8)
plot(best.suit.mod8)
        
        

### 4. check whether suitability score is a better predictor of abundance than exposure days  -------

# determine what exposure days model is better
mod.days<-lmer(data= testing.data,   
               log.ab~ exposure + season + 
                 (1|landscape/site)+(1|year), REML = T)
mod.days2<-lmer(data= testing.data, 
                log.ab~ exposure + season + season:exposure + 
                  (1|landscape/site)+(1|year), REML = T)

### incluing starting.day
mod.days3<-lmer(data= testing.data,    # equivalent to best.suit.mod without interaction 
                log.ab~ exposure + season + starting.day + 
                  (1|landscape/site)+(1|year), REML = T)
mod.days4<-lmer(data= testing.data,    # equivalent to best.suit.mod with interaction 
                 log.ab~ exposure + season + exposure:season + starting.day +
                   (1|landscape/site)+(1|year), REML = T)
mod.days5<-lmer(data= testing.data,    
                log.ab~ exposure + season + starting.day + season:starting.day +
                  (1|landscape/site)+(1|year), REML = T)
mod.days6<-lmer(data= testing.data,    
                log.ab~ exposure + season + log(starting.day) + 
                  (1|landscape/site)+(1|year), REML = T)

AIC(mod.days, mod.days2,  # models without starting day
    mod.days3, mod.days4, mod.days5, mod.days6, # models including starting day
    best.suit.mod0, # using suitability.score without plateau & model without starting day
    best.suit.mod, # using suitability.score without plateau & model including starting day
    best.suit.mod1a, # using suitability.score without plateau & model including log(starting day) 
    best.suit.mod1b, # using suitability.score without plateau & model including log(starting day) and season:log(starting day)
    best.suit.mod1c, # using suitability.score without plateau & model including (starting day)^2
    best.suit.mod1d, # using suitability.score without plateau & model including (starting day)^2 and season:(starting day)^2
    #best.suit.mod1_all, # like best.suit.mod1 but NO differentiation in training and testing data
    #best.suit.mod2, # using suitability.score WITH plateau & model without starting day
    best.suit.mod3, # using suitability.score WITH plateau & model including starting day
    #best.suit.mod4, # using suitability.score WITH plateau & model including starting day including season:starting.day
    best.suit.mod5, # using suitability.score WITH plateau & model including log(starting day)
    best.suit.mod6, # using suitability.score WITH plateau & model including log(starting day) and season:log(starting day)
    #best.suit.mod7, # using suitability.score WITH plateau & model including (starting.day)^2
    #best.suit.mod8 # using suitability.score WITH plateau & model including (starting.day)^2 and season:(starting.day)^2
    best.suit.mod1b.norm, # best.suit.mod1b but suitability score normalized by exposure days
    best.suit.mod1b.fem, # only female abundance 
    best.suit.mod1b.no.extr # excluding extreme abundance values > 1000
)


summary(mod.days3)
summary(best.suit.mod1b)
summary(best.suit.mod1b.fem)
summary(best.suit.mod1b.no.extr)
summary(best.suit.mod1b.norm)
summary(best.suit.mod6)

plot(mod.days3)
plot(best.suit.mod1a)
plot(best.suit.mod1b)
plot(best.suit.mod1b.norm)
plot(best.suit.mod1b.fem)
plot(best.suit.mod1c)
plot(best.suit.mod1d)
plot(best.suit.mod6)
# --> model without plateau performs slightly better than model with plateau --> continue with model without plateau


# CHECK: performance package for plots

library(performance) 
r2(mod.days3)
# Conditional R2: 0.548
# Marginal R2: 0.339

r2(best.suit.mod0)
# Conditional R2: 0.496
# Marginal R2: 0.266

r2(best.suit.mod)
# Conditional R2: 0.555
# Marginal R2: 0.342

r2(best.suit.mod1a)
# Conditional R2: 0.551
# Marginal R2: 0.344

r2(best.suit.mod1b)
# Conditional R2: 0.555
# Marginal R2: 0.345

r2(best.suit.mod1b.norm)
# Conditional R2: 0.554
# Marginal R2: 0.346

r2(best.suit.mod1b.fem)
# Conditional R2: 0.540
# Marginal R2: 0.322

r2(best.suit.mod1b.no.extr)
# Conditional R2: 0.557
# Marginal R2: 0.346

r2(best.suit.mod1c)
# Conditional R2: 0.554
# Marginal R2: 0.332

r2(best.suit.mod1d)
# Conditional R2: 0.553
# Marginal R2: 0.342

r2(best.suit.mod3)
# Conditional R2: 0.554
# Marginal R2: 0.343

r2(best.suit.mod5)
# Conditional R2: 0.550
# Marginal R2: 0.346

r2(best.suit.mod6)
# Conditional R2: 0.554
# Marginal R2: 0.346




performance::check_model(best.suit.mod)
performance::check_model(best.suit.mod0)
performance::check_model(best.suit.mod1a)
performance::check_model(best.suit.mod1b)
performance::check_model(best.suit.mod1c)
performance::check_model(best.suit.mod1d)
performance::check_model(best.suit.mod1.spring)
performance::check_model(best.suit.mod1.summer) 
performance::check_model(best.suit.mod3)
performance::check_model(best.suit.mod5)
performance::check_model(best.suit.mod6)

install.packages("sjPlot")
library(sjPlot)

sjPlot::tab_model(best.suit.mod1b)
sjPlot::tab_model(best.suit.mod1b.fem)
sjPlot::tab_model(best.suit.mod1b.no.extr)
sjPlot::tab_model(best.suit.mod1b.norm)
sjPlot::tab_model(best.suit.mod1.spring)
sjPlot::tab_model(best.suit.mod1.summer)

plot(meta.sample$exposure , dat$suitability.score)

library(MuMIn)
r.squaredGLMM(mod.days)

install.packages("DHARMa")
library(DHARMa)
simulateResiduals(mod.days3, plot = T) # good model diagnostics...

simulateResiduals(best.suit.mod1a, plot = T)
simulateResiduals(best.suit.mod1b, plot = T)
simulateResiduals(best.suit.mod1b.norm, plot = T)
simulateResiduals(best.suit.mod1b.fem, plot = T)

# This package does some automatic full-model comparison... all possible combinations... 
library(buildmer); library(glmmTMB)
teststs<-glmmTMB(total.abundance ~ exposure + season +
                        (1|landscape)+(1|year)+(1|site), 
                      family = poisson, data=meta.sample)
summary(teststs)
simulateResiduals(teststs, plot = T) # good model diagnostics...



### 5. Effect of using training and testing data or not (with best.suit.mod1) -----

AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(meta.sample, suitability.score=suitability.matrix[,i])
  # without interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  # with interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output1_all<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output1_all$min<-apply(mod.output1_all,1,function(x){min(x)})

best.constants1_all<-which(mod.output1_all$min==min(mod.output1_all$min))

constants.grid[best.constants1_all,]
interaction<-which(mod.output1_all[best.constants1_all,]==min(mod.output1_all[best.constants1_all,]) )[1]

if(interaction==1){
  dat<-data.frame(meta.sample,suitability.score=suitability.matrix[,best.constants1_all])
  # with interaction effect
  best.suit.mod1_all<-lmer(data= dat, 
                       log.ab~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                         (1|landscape/site)+(1|year), REML = T)}else{
                           # without interaction effect
                           dat<-data.frame(meta.sample,suitability.score=suitability.matrix[,best.constants1_all])
                           best.suit.mod1_all<-lmer(data= dat, 
                                                log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
                                                  (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod1_all)
plot(best.suit.mod1_all)


# compare best.suit.mod1 when using ALL data and when differentiating in training and testing data: 
AIC(
  best.suit.mod1, # using suitability.score without plateau & model including log(starting day) and season:log(starting day)
  best.suit.mod1_all)
# Warning message:
# In AIC.default(best.suit.mod1, best.suit.mod1_all) :
#  models are not all fitted to the same number of observations
## --> direct model comparison not possible...?

library(performance) 
r2(best.suit.mod1_all)
# Conditional R2: 0.550
# Marginal R2: 0.340

r2(best.suit.mod1)


# 6. Run two models: one for each season ------
# reason: very strong effect of season; check if running two models improves the models
#  --> this means then also that the suitability score optimisation probably needs to run twice (?)
# use model with lowest AIC --> best.suit.mod1

## 6.1 Suitability score WITHOUT plateau --> output: suitability.matrix ----
# there are three constants in the formula that defines the suitability of temp for pollination
# here, we define their range
t.opt <- seq(15,27, length=15) # optimal temperature - highest activity
t.max <- seq(25,45, length=15) # maximal temperature - defines when activity becomes 0 
sigma <- seq(0.5,5, length=12) # defines the shape of the sigmodid shape of bee activity below t.opt
constants.grid<- expand.grid(t.opt, t.max, sigma)
names(constants.grid) <- c("t.opt", "t.max", "sigma")

# account for the fact that max temp needs to be at least 1 degree above opt. temperature
constants.grid<- constants.grid[which(constants.grid$t.opt<=constants.grid$t.max-1),]
rm(t.opt, t.max, sigma)

## 6.1.1 Suitability score for spring ------

# create output data frame that will contain for each sample (column) all different suitability scores
constants.grid.spring <- constants.grid
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
  for(j in 1:nrow(constants.grid.spring)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$prec==0 & placeholder$temp <= constants.grid.spring$t.opt[j])
    above.opt<-which(placeholder$prec==0 & placeholder$temp > constants.grid.spring$t.opt[j])
    
    # compute the suitability score for temp above and below the temp optimum separately 
    suitability.estimate[below.opt]<- exp(-((placeholder$temp[below.opt]-constants.grid.spring$t.opt[j])/
                                              (2*constants.grid.spring$sigma[j]))^2)
    suitability.estimate[above.opt]<- 1-((placeholder$temp[above.opt]-constants.grid.spring$t.opt[j])/
                                           (constants.grid.spring$t.opt[j]- constants.grid.spring$t.max[j]))^2
    
    # negative suitability values need to be set to 0
    suitability.estimate[which(suitability.estimate<0)]<-0
    
    # diagnostics - works well
    #plot(suitability.estimate~placeholder$temp)
    
    # output for each sampling period needs to be prepared and saved
    suitability.score<-sum(suitability.estimate)
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix.spring<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)

## 6.1.1 Suitability score for summer ------

# create output data frame that will contain for each sample (column) all different suitability scores
constants.grid.summer <- constants.grid
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
  for(j in 1:nrow(constants.grid.summer)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$prec==0 & placeholder$temp <= constants.grid.summer$t.opt[j])
    above.opt<-which(placeholder$prec==0 & placeholder$temp > constants.grid.summer$t.opt[j])
    
    # compute the suitability score for temp above and below the temp optimum separately 
    suitability.estimate[below.opt]<- exp(-((placeholder$temp[below.opt]-constants.grid.summer$t.opt[j])/
                                              (2*constants.grid.summer$sigma[j]))^2)
    suitability.estimate[above.opt]<- 1-((placeholder$temp[above.opt]-constants.grid.summer$t.opt[j])/
                                           (constants.grid.summer$t.opt[j]- constants.grid.summer$t.max[j]))^2
    
    # negative suitability values need to be set to 0
    suitability.estimate[which(suitability.estimate<0)]<-0
    
    # diagnostics - works well
    #plot(suitability.estimate~placeholder$temp)
    
    # output for each sampling period needs to be prepared and saved
    suitability.score<-sum(suitability.estimate)
    output.period<-c(output.period, suitability.score)
  }
  output<-cbind(output, output.period)
  # columns are samples, rows are combinations of constants
  print(i)
}
suitability.matrix.summer<-t(output) # columns are combinations of constants, rows are samples
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder, output)


## 6.2 Models for two seasons
## 6.2.1 Model for spring -----
# following best.suit.mod1: log.ab ~ suitability.score + season + log(starting.day) + season:log(starting.day) +
#                                   (1 | landscape/site) + (1 | year)

#determine testing and training data:
set.seed(42)

testing.ID.spring<-sample(1:nrow(meta.sample.spring), round(nrow(meta.sample.spring)*0.2))
training.data.spring<- meta.sample.spring[-testing.ID.spring, ]
testing.data.spring<- meta.sample.spring[testing.ID.spring, ]


AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(training.data.spring, suitability.score=suitability.matrix.spring[-testing.ID.spring,i])
  # without interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.1))
  print(i)
}
mod.output1.spring<-data.frame(AIC.without.interaction)
mod.output1.spring$min<-apply(mod.output1.spring,1,function(x){min(x)})

best.constants1.spring<-which(mod.output1.spring$min==min(mod.output1.spring$min))

constants.grid[best.constants1.spring,]
#interaction<-which(mod.output1.spring[best.constants1.spring,]==min(mod.output1.spring[best.constants1.spring,]) )[1]

#if(interaction==1){
  dat<-data.frame(testing.data.spring,suitability.score=suitability.matrix.spring[testing.ID.spring,best.constants1.spring])
  # with interaction effect
  best.suit.mod1.spring<-lmer(data= dat, 
                       log.ab~ suitability.score +  log(starting.day) + 
                         (1|landscape/site)+(1|year), REML = T)
  
  # }else{
  #                          # without interaction effect
  #                          dat<-data.frame(testing.data.spring,suitability.score=suitability.matrix[testing.ID,best.constants1.spring])
  #                          best.suit.mod1.spring<-lmer(data= dat, 
  #                                               log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
  #                                                 (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod1.spring)
plot(best.suit.mod1.spring)
r2(best.suit.mod1.spring)
#   Conditional R2: 0.522
#   Marginal R2: 0.101


## 6.2.2 Model for summer -----
# following best.suit.mod1: log.ab ~ suitability.score + season + log(starting.day) + season:log(starting.day) +
#                                   (1 | landscape/site) + (1 | year)
#determine testing and training data:
set.seed(42)

testing.ID.summer <- sample(1:nrow(meta.sample.summer), round(nrow(meta.sample.summer)*0.2))
training.data.summer<- meta.sample.summer[-testing.ID.summer, ]
testing.data.summer<- meta.sample.summer[testing.ID.summer, ]

  
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(training.data.summer, suitability.score=suitability.matrix.summer[-testing.ID.summer,i])
  # without interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + log(starting.day) + 
                (1|landscape/site)+(1|year), REML = T)
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.1))
  print(i)
}
mod.output1.summer<-data.frame(AIC.without.interaction)
mod.output1.summer$min<-apply(mod.output1.summer,1,function(x){min(x)})

best.constants1.summer<-which(mod.output1.summer$min==min(mod.output1.summer$min))

constants.grid[best.constants1.summer,]
#interaction<-which(mod.output1.summer[best.constants1.summer,]==min(mod.output1.summer[best.constants1.summer,]) )[1]

#if(interaction==1){
  dat<-data.frame(testing.data.summer,suitability.score=suitability.matrix[testing.ID.summer,best.constants1.summer])
  # with interaction effect
  best.suit.mod1.summer<-lmer(data= dat, 
                              log.ab~ suitability.score + log(starting.day) + 
                                (1|landscape/site)+(1|year), REML = T)
  # 
  # }else{
  #                                 # without interaction effect
  #                                 dat<-data.frame(testing.data.summer,suitability.score=suitability.matrix[testing.ID,best.constants1.summer])
  #                                 best.suit.mod1.summer<-lmer(data= dat, 
  #                                                             log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
  #                                                               (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod1.summer)
plot(best.suit.mod1.summer)
r2(best.suit.mod1.summer)
#   Conditional R2: 0.403
#   Marginal R2: 0.059


# 7.1 normalise suitability score by exposure days --> rerun model

## suitability score WITHOUT plateau --> output: suitability.matrix ----
# using constants.grid calculated above

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
rm(output.period, suitability.score, suitability.estimate, placeholder, output)



## 7.2 Model INCLUDING log(starting.day) and season:log(starting.day) -----
AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(training.data, suitability.score=suitability.matrix.norm[-testing.ID,i])
  # with interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  # without interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output1b.norm<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output1b.norm$min<-apply(mod.output1b.norm,1,function(x){min(x)})

best.constants1b.norm<- which(mod.output1b.norm$min==min(mod.output1b.norm$min))

constants.grid[best.constants1b.norm,]
interaction<-which(mod.output1b.norm[best.constants1b.norm,]==min(mod.output1b.norm[best.constants1b.norm,]) )[1]

if(interaction==1){
  dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants1b.norm])
  # with interaction effect
  best.suit.mod1b.norm<-lmer(data= dat, 
                        log.ab~ suitability.score + season + suitability.score:season + log(starting.day) + season:log(starting.day) +
                          (1|landscape/site)+(1|year), REML = T)}else{
                            # without interaction effect
                            dat<-data.frame(testing.data,suitability.score=suitability.matrix.norm[testing.ID,best.constants1b.norm])
                            best.suit.mod1b.norm<-lmer(data= dat, 
                                                  log.ab~ suitability.score + season + log(starting.day) + season:log(starting.day) +
                                                    (1|landscape/site)+(1|year), REML = T)}


plot(best.suit.mod1b.norm)
summary(best.suit.mod1b.norm)


### Next steps -------
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

### 8. start modelling the grid for the suitability score and identify the best solution  -------

# run the regression analysis for all constant combinations...
for(i in 1:ncol(output)){print(i)
  meta.trapyearseason$suitability<-output[,i]
}
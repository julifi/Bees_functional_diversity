# 1. load in data
meta.sample<- read.csv('analysis_bees_diversity/data/meta_sample.csv',sep = ',', dec = '.')

weather.data<- read.csv('analysis_bees_diversity/data/data_weather/all_dat_temp_prec.csv',sep = ',', dec = '.')

# convert the dates into a date format
weather.data$hour.x<- as.POSIXct(weather.data$hour.x, format = "%Y-%m-%d %H:%M:%S")

# do the same for the meta data dates
meta.sample$StartDate<- as.POSIXct( paste0(meta.sample$StartDate, ' 12:00:00'), format = "%Y-%m-%d %H:%M:%S")
meta.sample$EndDate<- as.POSIXct( paste0(meta.sample$EndDate, ' 12:00:00'), format = "%Y-%m-%d %H:%M:%S")

# create a list that contains for each sample (row in meta.sample) a data-frame with all daylight sampling hours 
input.data<-list()
for(i in 1:nrow(meta.sample)){
  input.data[[i]]<-weather.data[which(weather.data$hour.x>=meta.sample$StartDate[i] & weather.data$hour.x<meta.sample$EndDate[i] &
                                        weather.data$trap==meta.sample$site[i]) , ]
  }

rm(weather.data)

# 2. compute suitability scores for wild-bee pollination for each sampling interval  -------

# there are three constants in the formula that defines the suitability of temp for pollination
# here, we define their range
t.opt <- seq(10,27, length=15) # optimal temperature - highest activity
t.max <- seq(25,50, length=15) # maximal temperature - defines when activity becomes 0 
sigma <- seq(0.5,8, length=12) # defines the shape of the sigmodid shape of bee activity below t.opt
constants.grid<- expand.grid(t.opt, t.max, sigma)
names(constants.grid) <- c("t.opt", "t.max", "sigma")

# account for the fact that max temp needs to be at least 1 degree above opt. temperature
constants.grid<- constants.grid[which(constants.grid$t.opt<=constants.grid$t.max-1),]
rm(t.opt, t.max, sigma)


#i = 50
#j = 1339

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

### 3. determine the optimal constant combination for computing the suitability score  -------

library(lme4); library(lmerTest)

# we work with log abundance to make outliers less influential
meta.sample$log.ab<-log(meta.sample$total.abundance+1)

AIC.with.interaction<-c()
AIC.without.interaction<-c()

for(i in 1:ncol(suitability.matrix)){
  dat<-data.frame(meta.sample,suitability.score=suitability.matrix[,i])
  # without interaction effect
  mod.1<-lmer(data= dat, 
              log.ab~ suitability.score + season +  
                (1|landscape/site)+(1|year), REML = T)
  # with interaction effect
  mod.2<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season +
                (1|landscape/site)+(1|year), REML = T)
  AIC.with.interaction<-c(AIC.with.interaction, AIC(mod.1))
  AIC.without.interaction<-c(AIC.without.interaction, AIC(mod.2))
  print(i)
}
mod.output<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output$min<-apply(mod.output,1,function(x){min(x)})

best.constants<-which(mod.output$min==min(mod.output$min))
#best.constants<-which(mod.output$AIC.without.interaction==min(mod.output$AIC.without.interaction))


# --> currently (with t.opt <- seq(15,27, length=10) # optimal temperature - highest activity
#                     t.max <- seq(25,45, length=10) # maximal temperature - defines when activity becomes 0 
#                     sigma <- seq(0.5,5, length=10) 
# : best constants: 951 -->    t.opt t.max sigma#
#                       991    15    45     5

constants.grid[best.constants,]
interaction<-which(mod.output[best.constants,]==min(mod.output[best.constants,]) )[1]

if(interaction==1){
  dat<-data.frame(meta.sample,suitability.score=suitability.matrix[,best.constants])
  # without interaction effect
  best.suit.mod<-lmer(data= dat, 
              log.ab~ suitability.score + season +  
                (1|landscape/site)+(1|year), REML = T)}else{
  dat<-data.frame(meta.sample,suitability.score=suitability.matrix[,best.constants])
  best.suit.mod<-lmer(data= dat, 
              log.ab~ suitability.score + season + suitability.score:season +
                (1|landscape/site)+(1|year), REML = T)}

### START TEST 
# version 2: change t.opt, t.max & sigma to: 
#                     t.opt <- seq(10,27, length=15) # optimal temperature - highest activity
#                     t.max <- seq(25,50, length=15) # maximal temperature - defines when activity becomes 0 
#                     sigma <- seq(0.5,8, length=12) 

mod.output_2<-data.frame(AIC.with.interaction, AIC.without.interaction)
mod.output_2$min<-apply(mod.output_2,1,function(x){min(x)})

best.constants_2<-which(mod.output_2$min==min(mod.output_2$min))
#best.constants<-which(mod.output$AIC.without.interaction==min(mod.output$AIC.without.interaction))

# : best constants_2: 1315 -->    t.opt      t.max   sigma#
#                       1339   13.64286    50      3.909091

constants.grid[best.constants_2,]
interaction<-which(mod.output_2[best.constants_2,]==min(mod.output_2[best.constants_2,]) )[1]

if(interaction==1){
  dat<-data.frame(meta.sample,suitability.score=suitability.matrix[,best.constants_2])
  # without interaction effect
  best.suit.mod_2<-lmer(data= dat, 
                      log.ab~ suitability.score + season +  
                        (1|landscape/site)+(1|year), REML = T)}else{
                          dat<-data.frame(meta.sample,suitability.score=suitability.matrix[,best.constants_2])
                          best.suit.mod_2<-lmer(data= dat, 
                                              log.ab~ suitability.score + season + suitability.score:season +
                                                (1|landscape/site)+(1|year), REML = T)}

summary(best.suit.mod_2)
plot(best.suit.mod_2)

### END TEST 


### 4. check whether suitability score is a better predictor of abundance than exposure days  -------

# determine what exposure days model is better
mod.days<-lmer(data=meta.sample, 
               log.ab ~ exposure + season +
              (1|landscape)+(1|year)+(1|site) , REML = T)
mod.days2<-lmer(data= meta.sample, 
                log.ab~ exposure + season + season:exposure + 
                 (1|landscape)+(1|year)+(1|site) , REML = T)
mod.days3<-lmer(data= meta.sample, 
                log.ab~ exposure + season:exposure + 
                  (1|landscape)+(1|year)+(1|site) , REML = T)
AIC(mod.days, mod.days2, mod.days3, best.suit.mod)

summary(best.suit.mod)
plot(best.suit.mod)

plot(meta.sample$exposure , dat$suitability.score)

library(MuMIn)
r.squaredGLMM(mod.days)

library(DHARMa)
simulateResiduals(mod.days, plot = T) # good model diagnostics...

# This package does some automatic full-model comparison... all possible combinations... 
library(buildmer); library(glmmTMB)
teststs<-glmmTMB(total.abundance ~ exposure + season +
                        (1|landscape)+(1|year)+(1|site), 
                      family = poisson, data=meta.sample)
summary(teststs)
simulateResiduals(teststs, plot = T) # good model diagnostics...

# To do: 
# reflect on the results
# try to run a model with suitability score that has been normalised by exposure days
# try to extend the constant matrix and see what happens






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
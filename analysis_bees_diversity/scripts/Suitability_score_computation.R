# 1. compute suitability scores for wild-bee pollination for each sampling interval  -------

# there are three constants in the formula that defines the suitability of temp for pollination
# here, we define their range
t.opt <- seq(15,27, length=10) # optimal temperature - highest activity
t.max <- seq(25,45, length=10) # maximal temperature - defines when activity becomes 0 
sigma <- seq(0.5,5, length=10) # defines the shape of the sigmodid shape of bee activity below t.opt
constants.grid<- expand.grid(t.opt, t.max, sigma)
names(constants.grid) <- c("t.opt", "t.max", "sigma")

# account for the fact that max temp needs to be at least 1 degree above opt. temperature
constants.grid<- constants.grid[which(constants.grid$t.opt<=constants.grid$t.max+1),]
rm(t.opt, t.max, sigma)

# create output data frame that will contain for each sample (column) all different suitability scores
output <- c()

# we create a procedure that will be implemented for each sampling period in a loop
for (i in 1:length(input.data)){
  
  # we extract the climate data of a given sampling period
  placeholder <-  input.data[[i]]
  
  # for now, we assume made-up data, we can delete this later ... 
  placeholder <- data.frame(temp=seq(10,35, length=100), rainfall = sample(c(0,0,0,0,10),100, replace = T))
  
  # prepare output data-frame for a given sampling period
  output.period <-c()

  # we compute for each hour the suitability score for each combinations of constants in the grid
  for(j in 1:nrow(constants.grid)){
    suitability.estimate<-rep(0, nrow(placeholder)) #we create a vector for the suitability scores for each hr
    
    #define which hrs had a rainfall of 0 and temp below or above the optimum
    below.opt<-which(placeholder$rainfall==0 & placeholder$temp <= constants.grid$t.opt[j])
    above.opt<-which(placeholder$rainfall==0 & placeholder$temp > constants.grid$t.opt[j])
    
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
}
rm(output.period, suitability.score, suitability.estimate, above.opt, below.opt, placeholder)



### 2. load in predictors and response variables and prepare them  -------

# load in data
meta.trapyearseason<- read.csv('analysis_bees_diversity/data/meta.trapyearseason.csv')
site.env.data<-read.csv('analysis_bees_diversity/data/env_data_ecosystematlas_elevation.csv', 
                        dec = '.', sep = ',')

# match site.data with meta-data
matching<-match(meta.trapyearseason$site, site.env.data$TRAP)
site.env.data<-site.env.data[matching,]
site.env.data<-site.env.data[,-which(colnames(site.env.data)=='YEAR')]

# compute additional predictors 
# habitat richness: should probably only be calculated from semi-natural habitats
library(vegan)
seminat<- colnames(site.env.data)[c(5,8:12,16)]
meta.trapyearseason$hab.div<-apply(site.env.data[ ,c(5,8:12,16)],1, function(x){length(which(x>0))})
meta.trapyearseason$hab.even<-apply(site.env.data[ ,c(5,8:12,16)],1, function(x){
  diversity(x, index = 'shannon')/log(specnumber(x))})
meta.trapyearseason$hab.proportion<-apply(site.env.data[ ,c(5,8:12,16)],1, function(x){sum(x)})

rm(seminat)

# add the elevation data to the meta data
meta.trapyearseason$elevation_mean_400m<-site.env.data$elevation_mean_400m
meta.trapyearseason$elevation_range_400m<-site.env.data$elevation_range_400m

### 3. start modelling the impact of exposure time on abundance and richness  -------
library(lme4); library(lmerTest)
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

### 4. start modelling the grid for the suitability score and identify the best solution  -------

# run the regression analysis for all constant combinations...
for(i in 1:ncol(output)){print(i)
  meta.trapyearseason$suitability<-output[,i]
}
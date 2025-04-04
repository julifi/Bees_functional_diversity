########### Regression Tree ########### 

## Global Setting ####################################################

# BEFORE: run setting.R
setwd(wrkpath)
getwd()
# Data Path with all Nexus_Indicators
datpath <- wrkpath %+% "data/"


## Load Libraries #############################################################  
library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging
library(ggplot2)
library(MASS)        # checks for different transformations

# based on: https://uc-r.github.io/regression_trees

tree_1 <- rpart(dryweight ~ sunshine + mean_air_temp + max_air_temp + min_air_temp +
                  humidity + precipitation + SMI_top_soil + SMI_total_soil + 
                  drought_intensity_top_soil + drought_intensity_total_soil +
                  elevation_mean + elevation_range + seminatural_habitats + 
                  proximity_index + habitat_richness, 
                data = d1,
                method = "anova", 
                control = list(minsplit = 20, maxdepth = 10))
# The fitting process and the visual output of regression trees and classification trees 
# are very similar. Both use the formula method for expressing the model (similar to lm). 
# However, when fitting a regression tree, we need to set method = "anova". By default, 
# rpart will make an intelligent guess as to what the method value should be based on the 
# data type of your response column, but it's recommened that you explictly set the method 
# for reproducibility reasons (since the auto-guesser may change in the future).

# rpart is automatically applying a range of cost complexity (?? values to prune the tree). 
# To compare the error for each ?? value, rpart performs a 10-fold cross validation so that 
# the error associated with a given ?? value is computed on the hold-out validation data. But
# there are additional tuning parameters to avoud overfitting, most importantly: minsplit (minimum 
# number of observations that must exist in a node in order for a split to be attempted, often set
# at 20) and maxdepth (maximum depth of any node of the final tree, with the root node counted as 
# depth 0 -> default is 30, which is quite liberal)

print(tree_1) 
# -> elevation_range and elevatoin_mean the most important variables

# tree plot
rpart.plot(tree_1)

plotcp(tree_1) # cp = cost complexity parameter
printcp(tree_1)
# y-axis is cross validation error, lower x-axis is cost complexity (??) value, upper x-axis 
# is the number of terminal nodes
# The dashed line is based on Breiman et al. (1984) -> it suggests that in actual practice, 
# its common to use the smallest tree within 1 standard deviation of the minimum cross 
# validation error (aka the 1-SE rule). -> does not look to promising here, bascially suggests
# only 1 nod
# here we have 7 terminal nods so our xerror is 0.82555
# -> goal is to select a value of the complexity parameter that minimizes this cross-validated 
# error while avoiding overfitting


### Grid search ###

# -> automatically searches across a range of differently tuned models to identify the optimal 
# hyerparameter setting

# create the hyperparameter grid
hyper_grid <- expand.grid(
  minsplit = seq(10, 30, 1),
  maxdepth = seq(3, 15, 1)
)

# total number of combinations
nrow(hyper_grid) # -> 273

# loop to go through each hyperparameter combination 
models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(dryweight ~ sunshine + mean_air_temp + max_air_temp + min_air_temp +
                         humidity + precipitation + SMI_top_soil + SMI_total_soil + 
                         drought_intensity_top_soil + drought_intensity_total_soil +
                         elevation_mean + elevation_range + seminatural_habitats + 
                         proximity_index + habitat_richness, 
                       data = d1,
                       method = "anova",
                       control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

# filtering for the top 5 minimal error values -> shows optimal models
hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

# -> best model with minsplit = 13 and maxdepth = 14 

best_tree <- rpart(dryweight ~ sunshine + mean_air_temp + max_air_temp + min_air_temp +
                     humidity + precipitation + SMI_top_soil + SMI_total_soil + 
                     drought_intensity_top_soil + drought_intensity_total_soil +
                     elevation_mean + elevation_range + seminatural_habitats + 
                     proximity_index + habitat_richness, 
                   data = d1,
                   method = "anova", 
                   control = list(minsplit = 13, maxdepth = 14))

best_tree
rpart.plot(best_tree)
plotcp(best_tree)
printcp(best_tree)

# now you would apply the final model aka best_tree to predict the dryweight with 
# test_data (which we do not have) like in the following code:
# pred <- predict(best_tree, newdata = test_data)
# RMSE(pred = pred, obs = test_data$dryweight)



### Bagging with ipred ###

# bagging = bootstrap aggregating (based on Breiman, 1996)

# -> single tree models suffer from high variance (pruning helps reducing the variance,
# but bagging actually exploite the variability of single trees in a way that can 
# significantly improve performance over and above that of single trees)

# bagging follows 3 steps:
# 1. Create m bootstrap samples from the training data.
# 2. For each bootstrap sample train a single, unpruned regression tree.
# 3. Average individual predictions from each tree to create an overall average 
# predicted value.

# One benefit of bagging is that, on average, a bootstrap sample will contain 63% of 
# the training data. This leaves about 33% of the data out of the bootstrapped sample. 
# We call this the out-of-bag (OOB) sample. We can use the OOB observations to estimate 
# the model's accuracy, creating a natural cross-validation process.

set.seed(123)

# train bagged model
bagged_tree_1 <- bagging(dryweight ~ sunshine + mean_air_temp + max_air_temp + min_air_temp +
                           humidity + precipitation + SMI_top_soil + SMI_total_soil + 
                           drought_intensity_top_soil + drought_intensity_total_soil +
                           elevation_mean + elevation_range + seminatural_habitats + 
                           proximity_index + habitat_richness,
                         data = d1,
                         method = "anova",
                         control = list(minsplit = 13, maxdepth = 14),
                         coob = TRUE) # means we use the OOB sample to estimate the test error
bagged_tree_1

# the default is 25 bootstrap rounds -> eventually the reduction in error will flatline 
# signaling an appropriate number of trees to create a stable model. Rarely will you need 
# more than 50 trees to stabilize the error

# checking how many bootstrap rounds aka trees we need: 

# assess 10-60 bagged trees
ntree <- 10:60

# create empty vector to store OOB RMSE values
rmse <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
  # reproducibility
  set.seed(123)
  
  # perform bagged model
  model <- bagging(dryweight ~ sunshine + mean_air_temp + max_air_temp + min_air_temp +
                     humidity + precipitation + SMI_top_soil + SMI_total_soil + 
                     drought_intensity_top_soil + drought_intensity_total_soil +
                     elevation_mean + elevation_range + seminatural_habitats + 
                     proximity_index + habitat_richness,
                   data = d1,
                   control = list(minsplit = 13, maxdepth = 14),
                   method = "anova",
                   coob = TRUE,
                   nbagg = ntree[i]
  )
  # get OOB error
  rmse[i] <- model$err # rmse = root mean square error
}

plot(ntree, rmse, type = 'l', lwd = 2) # x-axis = nr of trees; y-axis = rmse
abline(v = 14, col = "red", lty = "dashed") # -> rmse lowest at 14 (why does it go up again?)
abline(v = 39, col = "red", lty = "dashed") # -> actual lowest value at 39?

# -> best bagging model with nbagg = 14 (in theory, but when I do it e.g. with nbagg = 39, 
# it is still better -> why?)

best_bagged_tree <- bagging(dryweight ~ sunshine + mean_air_temp + max_air_temp + min_air_temp +
                              humidity + precipitation + SMI_top_soil + SMI_total_soil + 
                              drought_intensity_top_soil + drought_intensity_total_soil +
                              elevation_mean + elevation_range + seminatural_habitats + 
                              proximity_index + habitat_richness,
                            data = d1,
                            control = list(minsplit = 13, maxdepth = 14),
                            method = "anova",
                            coob = TRUE, # means we use the OOB sample to estimate the test error
                            nbagg = 39 # rmse way lower with 39 then 14!
)
best_bagged_tree



### Bagging with caret ###

# -> alternative to ipred -> uses cross validation instead of OOB error -> more 
# robust understanding of the true expected test error

# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10) 

# CV bagged model
bagged_cv <- train(dryweight ~ sunshine + mean_air_temp + max_air_temp + min_air_temp +
                     humidity + precipitation + SMI_top_soil + SMI_total_soil + 
                     drought_intensity_top_soil + drought_intensity_total_soil +
                     elevation_mean + elevation_range + seminatural_habitats + 
                     proximity_index + habitat_richness,
                   data = d1,
                   control = list(minsplit = 13, maxdepth = 14),
                   method = "treebag",
                   trControl = ctrl,
                   importance = TRUE)

bagged_cv

# plot the 10 most important variables
plot(varImp(bagged_cv), 10) 
# Variable importance for regression trees is measured by assessing the total amount SSE 
# is decreased by splits over a given predictor, averaged over all m trees. The predictors 
# with the largest average impact to SSE (= sum of sqared errors) are considered most 
# important. The importance value is simply the relative mean decrease in SSE compared 
# to the most important variable (provides a 0-100 scale).

### -> caret SEEMS LIKE THE BETTER APPROACH FOR THE BAGGING THEN ipred TO ME !
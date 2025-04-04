## Global Setting ####################################################

# BEFORE: run setting.R
setwd(wrkpath)
getwd()
# Data Path with all Nexus_Indicators
datpath <- wrkpath %+% "data/"


############################################################# 

# lowest AIC
min_AIC <- min(data$AICs)

# select models max. 2 AIC units above the minimum AIC
selected_models <- subset(data, AICs <= min_AIC + 2)

# weights based on AIC for each model
weights <- exp(-(selected_models$AICs - min_AIC) / 2)
# "-" ensures that models with lower AIC values (better models) have larger weights
# "/ 2" penalizes larger differences less than smaller ones
# "exp" exponential function further amplifies the differences, resulting in larger weights for models with smaller AIC values
# similar done for example here: 
# 1) https://www.stat.umn.edu/geyer/f20/5421/slides/glmbb.html 
# 2) https://stats.stackexchange.com/questions/494133/aic-model-averaging-when-models-are-correlated#:~:text=AIC%20model%2Daveraging%3A%20In%20%22,in%20terms%20of%20AIC)%20model.
weights <- weights / sum(weights) # normalizing (sum up to 1)

min(weights)/max(weights)
weights.trial <- exp(-(c(230,232,233) - 230) / 2)
weights.trial <- weights.trial / sum(weights.trial) # normalizing (sum up to 1)


# extract predictors from selected models -> converting the text string to a formula object 
# (necassary to extract the predictors)
predictors <- lapply(selected_models$grid.formula, function(formula_str) {
  # convert formula string to formula object
  formula <- as.formula(formula_str)
  # extract predictors from formula
  attr(terms(formula), "term.labels")
})

# get that data together with weights in a data frame
predictor.weights<-data.frame()
for(i in 1:length(predictors)){
  preds<-predictors[[i]]; weight<-rep(weights[i], length(preds))
  predictor.weights<-rbind(predictor.weights, data.frame(preds, weight))}

# getting the mean weight for each predictor
mean_weight <- aggregate(weight ~ preds, predictor.weights, sum)
mean_weight <- mean_weight[-c(1, 2), ] # excluding the random effects (year and site) from weighting

# subset mean_weight to keep rows with weights >= 0.5
subset_mean_weight <- mean_weight[mean_weight$weight >= 0.5, ]

# creating a vector combining predictor and weight
predictor_weight_vector <- paste0(subset_mean_weight$preds, "*", round(subset_mean_weight$weight, digits = 3))

# final formula with weighted predictors and random effects
final_formula <- paste("(1 | site) + (1 | year) +", paste(predictor_weight_vector, collapse = " + "))
print(final_formula)
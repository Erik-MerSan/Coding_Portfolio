# Erik Mercado Sanchez
# emm4376

# load libraries
library(tidyverse)
library(dplyr)
library(glmnet)
library(randomForest)
library(caret)
library(ggplot2)
library(binsreg)

# read in data
prediction_data <- read.csv("/Users/erik/Downloads/prediction_data.csv")

# prepping the data 
prediction_data$rearrest[prediction_data$rearrest == "NULL"] <- NA
prediction_data$rearrested <- ifelse(is.na(prediction_data$rearrest) | prediction_data$rearrest == "No Arrest", 0, 1)
prediction_data$black_male <- prediction_data$male*prediction_data$black

# split data into training and testing sets
set.seed(42)
train_index <- sample(nrow(prediction_data), 0.8 * nrow(prediction_data))
train_data <- prediction_data[train_index, ]
test_data <- prediction_data[-train_index, ]

#OLS method
ols_rearrested <- lm(train_data$rearrested ~ train_data$male + train_data$black + 
                       train_data$black_male + train_data$assault + train_data$burglary +
                       train_data$conspiracy + train_data$crim_contempt + train_data$weapon_possesion +
                       train_data$dwi + train_data$drug + train_data$endanger + 
                       train_data$homicide_related + train_data$larceny + train_data$obstruction +
                       train_data$other + train_data$other_vtl + train_data$property +
                       train_data$robbery + train_data$unlicensed_operation)
ols_ror <- lm(train_data$rearrested ~ train_data$male + train_data$black + 
                train_data$black_male + train_data$assault + train_data$burglary +
                train_data$conspiracy + train_data$crim_contempt + train_data$weapon_possesion +
                train_data$dwi + train_data$drug + train_data$endanger + 
                train_data$homicide_related + train_data$larceny + train_data$obstruction +
                train_data$other + train_data$other_vtl + train_data$property +
                train_data$robbery + train_data$unlicensed_operation) 
# make predictions on testing data
pred_ols_rearrested <- predict(ols_rearrested, data = test_data)
pred_ols_ror <- predict(ols_ror, data = test_data)
# calculate OOSMSE
oosmse_ols_rearrested <- mean((test_data$rearrested - pred_ols_rearrested)^2)
oosmse_ols_ror <- mean((test_data$ror - pred_ols_ror)^2)
# find best features
summary(ols_rearrested)
summary(ols_ror)
# binsreg of ols rearrested and ror
binsreg(test_data$rearrested, pred_ols_rearrested)
binsreg(test_data$ror, pred_ols_ror)

# Lasso method
# fit Lasso model using cross-validation
lasso_cv_rearrested <- cv.glmnet(as.matrix(train_data[,c("male", "black", "black_male",
                                                         "assault", "burglary", "conspiracy",
                                                         "crim_contempt", "weapon_possesion", 
                                                         "dwi", "drug", "endanger",
                                                         "homicide_related", "larceny", 
                                                         "obstruction", "other", "other_vtl", 
                                                         "property", "robbery", "unlicensed_operation")]),
                                 train_data$rearrested, alpha = 1)
lasso_cv_ror <- cv.glmnet(as.matrix(train_data[, c("male", "black", "black_male",
                                                   "assault", "burglary", "conspiracy",
                                                   "crim_contempt", "weapon_possesion", 
                                                   "dwi", "drug", "endanger", "homicide_related",
                                                   "larceny", "obstruction", "other",
                                                   "other_vtl", "property", "robbery", 
                                                   "unlicensed_operation")]),
                          train_data$ror, alpha = 1, nfolds = 5)
# make predictions on testing data
pred_lasso_cv_rearrested <- predict(lasso_cv_rearrested, newx = as.matrix(test_data[, c("male", "black", "black_male",
                                                                                        "assault", "burglary", "conspiracy",
                                                                                        "crim_contempt", "weapon_possesion", 
                                                                                        "dwi", "drug", "endanger", "homicide_related",
                                                                                        "larceny", "obstruction", "other",
                                                                                        "other_vtl", "property", "robbery", 
                                                                                        "unlicensed_operation")]))
pred_lasso_cv_ror <- predict(lasso_cv_ror, newx = as.matrix(test_data[, c("male", "black", "black_male",
                                                                          "assault", "burglary", "conspiracy",
                                                                          "crim_contempt", "weapon_possesion", 
                                                                          "dwi", "drug", "endanger", "homicide_related",
                                                                          "larceny", "obstruction", "other",
                                                                          "other_vtl", "property", "robbery", 
                                                                          "unlicensed_operation")]))
# calculate OOSMSE
oosmse_lasso_cv_rearrested <- mean((test_data$rearrested - pred_lasso_cv_rearrested)^2)
oosmse_lasso_cv_ror <- mean((test_data$ror - pred_lasso_cv_ror)^2)
# selecting best lambda 
best_lambda_lasso_rearrested <- lasso_cv_rearrested$lambda.min
best_lambda_lasso_ror <- lasso_cv_ror$lambda.min
# Lasso fit with best lambda
lasso_fitted_rearrested <- glmnet(as.matrix(train_data[,c("male", "black", "black_male",
                                                          "assault", "burglary", "conspiracy",
                                                          "crim_contempt", "weapon_possesion", 
                                                          "dwi", "drug", "endanger", "homicide_related",
                                                          "larceny", "obstruction", "other",
                                                          "other_vtl", "property", "robbery", 
                                                          "unlicensed_operation")]),
                                  train_data$rearrested, alpha = 1, lambda = best_lambda_lasso_rearrested)
lasso_fitted_ror <- glmnet(as.matrix(train_data[,c("male", "black", "black_male",
                                                   "assault", "burglary", "conspiracy",
                                                   "crim_contempt", "weapon_possesion", 
                                                   "dwi", "drug", "endanger", "homicide_related",
                                                   "larceny", "obstruction", "other",
                                                   "other_vtl", "property", "robbery", 
                                                   "unlicensed_operation")]),
                           train_data$ror, alpha = 1, lambda = best_lambda_lasso_ror)
# get coefficients from lasso
coef(lasso_fitted_rearrested)
coef(lasso_fitted_ror)
# binsreg of lasso rearrested and ror
binsreg(test_data$rearrested, pred_lasso_cv_rearrested)
binsreg(test_data$ror, pred_lasso_cv_ror)

# Ridge Method
# fit Ridge model using cross-validation
ridge_cv_rearrested <- cv.glmnet(as.matrix(train_data[, c("male", "black", "black_male",
                                                          "assault", "burglary", "conspiracy",
                                                          "crim_contempt", "weapon_possesion", 
                                                          "dwi", "drug", "endanger", "homicide_related",
                                                          "larceny", "obstruction", "other",
                                                          "other_vtl", "property", "robbery", 
                                                          "unlicensed_operation")]),
                                 train_data$rearrested, alpha = 0, nfolds = 5)
ridge_cv_ror <- cv.glmnet(as.matrix(train_data[, c("male", "black", "black_male",
                                                   "assault", "burglary", "conspiracy",
                                                   "crim_contempt", "weapon_possesion", 
                                                   "dwi", "drug", "endanger", "homicide_related",
                                                   "larceny", "obstruction", "other",
                                                   "other_vtl", "property", "robbery", 
                                                   "unlicensed_operation")]),
                          train_data$ror, alpha = 0, nfolds = 5)
# make predictions on testing data
pred_ridge_cv_rearrested <- predict(ridge_cv_rearrested, newx = as.matrix(test_data[, c("male", "black", "black_male",
                                                                                        "assault", "burglary", "conspiracy",
                                                                                        "crim_contempt", "weapon_possesion", 
                                                                                        "dwi", "drug", "endanger", "homicide_related",
                                                                                        "larceny", "obstruction", "other",
                                                                                        "other_vtl", "property", "robbery", 
                                                                                        "unlicensed_operation")]))
pred_ridge_cv_ror <- predict(ridge_cv_ror, newx = as.matrix(test_data[, c("male", "black", "black_male",
                                                                          "assault", "burglary", "conspiracy",
                                                                          "crim_contempt", "weapon_possesion", 
                                                                          "dwi", "drug", "endanger", "homicide_related",
                                                                          "larceny", "obstruction", "other",
                                                                          "other_vtl", "property", "robbery", 
                                                                          "unlicensed_operation")]))
# calculate OOSMSE
oosmse_ridge_rearrested <- mean((test_data$rearrested - pred_ridge_cv_rearrested)^2)
oosmse_ridge_ror <- mean((test_data$ror - pred_ridge_cv_ror)^2)
# selecting best lambda 
best_lambda_ridge_rearrested <- ridge_cv_rearrested$lambda.min
best_lambda_ridge_ror <- ridge_cv_ror$lambda.min
# Lasso fit with best lambda
ridge_fitted_rearrested <- glmnet(as.matrix(train_data[,c("male", "black", "black_male",
                                                          "assault", "burglary", "conspiracy",
                                                          "crim_contempt", "weapon_possesion", 
                                                          "dwi", "drug", "endanger", "homicide_related",
                                                          "larceny", "obstruction", "other",
                                                          "other_vtl", "property", "robbery", 
                                                          "unlicensed_operation")]),
                                  train_data$rearrested, alpha = 0, lambda = best_lambda_ridge_rearrested)
ridge_fitted_ror <- glmnet(as.matrix(train_data[,c("male", "black", "black_male",
                                                   "assault", "burglary", "conspiracy",
                                                   "crim_contempt", "weapon_possesion", 
                                                   "dwi", "drug", "endanger", "homicide_related",
                                                   "larceny", "obstruction", "other",
                                                   "other_vtl", "property", "robbery", 
                                                   "unlicensed_operation")]),
                           train_data$ror, alpha = 0, lambda = best_lambda_ridge_ror)
# get coefficients from lasso
coef(ridge_fitted_rearrested)
coef(ridge_fitted_ror)
# binsreg of lasso rearrested and ror
binsreg(test_data$rearrested, pred_ridge_cv_rearrested)
binsreg(test_data$ror, pred_ridge_cv_ror)

# Random Forest method
# fit Random Forest model using cross-validation
rf_cv_rearrested <- randomForest(train_data$rearrested ~ train_data$male + train_data$black + 
                                   train_data$black_male + train_data$assault + train_data$burglary +
                                   train_data$conspiracy + train_data$crim_contempt + train_data$weapon_possesion +
                                   train_data$dwi + train_data$drug + train_data$endanger + 
                                   train_data$homicide_related + train_data$larceny + train_data$obstruction +
                                   train_data$other + train_data$other_vtl + train_data$property +
                                   train_data$robbery + train_data$unlicensed_operation)
rf_cv_ror <- randomForest(train_data$ror ~ train_data$male + train_data$black + 
                            train_data$black_male + train_data$assault + train_data$burglary +
                            train_data$conspiracy + train_data$crim_contempt + train_data$weapon_possesion +
                            train_data$dwi + train_data$drug + train_data$endanger + 
                            train_data$homicide_related + train_data$larceny + train_data$obstruction +
                            train_data$other + train_data$other_vtl + train_data$property +
                            train_data$robbery + train_data$unlicensed_operation)
# make predictions on test data
pred_rf_rearrested <- predict(rf_cv_rearrested, data = test_data)
pred_rf_ror <- predict(rf_cv_ror, data = test_data)
# calculate OOSMSE
oosmse_rf_rearrested <- mean((test_data$rearrested - pred_rf_rearrested)^2)
oosmse_rf_ror <- mean((test_data$ror - pred_rf_ror)^2)
# get list to determine top 2 features
importance(rf_cv_rearrested)
importance(rf_cv_ror)
# binsreg of random forest rearrested and ror
binsreg(test_data$rearrested, pred_rf_rearrested)
binsreg(test_data$ror, pred_rf_ror)

# predicting rearested and ror across the sample 
pred_rearrested <- predict(ridge_fitted_rearrested, newx = as.matrix(prediction_data[, c("male", "black", "black_male",
                                                                                         "assault", "burglary", "conspiracy",
                                                                                         "crim_contempt", "weapon_possesion", 
                                                                                         "dwi", "drug", "endanger", "homicide_related",
                                                                                         "larceny", "obstruction", "other",
                                                                                         "other_vtl", "property", "robbery", 
                                                                                         "unlicensed_operation")]))
pred_ror <- predict(ridge_fitted_ror, newx = as.matrix(prediction_data[, c("male", "black", "black_male",
                                                                           "assault", "burglary", "conspiracy",
                                                                           "crim_contempt", "weapon_possesion", 
                                                                           "dwi", "drug", "endanger", "homicide_related",
                                                                           "larceny", "obstruction", "other",
                                                                           "other_vtl", "property", "robbery", 
                                                                           "unlicensed_operation")]))
# prepare data frame for plotting 
plot_df <- data.frame(race = prediction_data$race,
                      pred_rearrested = pred_rearrested,
                      pred_ror = pred_ror)
# density plots for ror and rearrested
ggplot(plot_df, aes(x = pred_rearrested, fill = race)) +
  geom_density(alpha = 0.5)
ggplot(plot_df, aes(x = pred_ror, fill = race)) +
  geom_density(alpha = 0.5)

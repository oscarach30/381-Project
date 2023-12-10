install.packages('readr')
install.packages("caret")
install.packages("Matrix")
install.packages('dplyr')
install.packages('tidyverse')
library(tidyverse)
library(dplyr)
library(caret)
library(readr)
library(Matrix)
library(readr)


# Read the CSV file
df <- data.for.dom

#Fixing column names
colnames(df)<- as.character(unlist(df[1,]))

#Removing duplicate column name in first row
df <- df[-c(1),]

#Transforming data frame to numeric
# Assuming your data frame is named df

# Transform all columns to numeric
df[] <- lapply(df, function(x) as.numeric(as.character(x)))

#Renamimg DEFAULT column
df <-df %>% rename(DEFAULT = 'default payment next month')

#Removing empty space before column names
names(df)<- gsub(" ", "", names(df))


#view cleaned data frame
View(df)

# View the cleaned dataframe
head(df)

# Set seed for reproducibility
set.seed(123)

#partitioning the data
library(rsample)
set.seed(123)
split<-initial_split(df, prop = .7, strata = 'DEFAULT')
train<-training(split)
holdout<-testing(split)
split_holdout <- initial_split(holdout, prop = .5) #splitting holdout into 50% each
test <- training(split_holdout) #extracting the testing st
validation <- testing(split_holdout) #extracting the validation set


# Check for conversion issues
summary(df)

# Assuming df is your data frame
corMatrix <- cor(df)

# View the correlation matrix
print(corMatrix)

#check for NA values 
na_check <- colSums(is.na(data.for.dom))
na_check

# Simple Linear Model
# CC Balance as a function of Age
model_linear <- lm(LIMIT_BAL ~ AGE, train)


summary(model_linear)

#Predicting training data to benchmark in-sample performance
M1_in <- predict(model_linear, train) 


#Predicting validation data to benchmark out-of-sample model performance
M1_out <- predict(model_linear, validation) 


#Computing and Reporting in-sample and out-of-sample RMSE
(RMSE_1_IN<-sqrt(sum((M1_in-train$LIMIT_BAL)^2)/length(M1_in))) #computing in-sample error
(RMSE_1_OUT<-sqrt(sum((M1_out-validation$LIMIT_BAL)^2)/length(M1_out))) #computing out-of-sample 

## Bivariate model with nonlinear transformation ##
M2 <- lm(LIMIT_BAL ~ AGE + I(AGE^2) + I(AGE^3), data = train)
summary(M2)


#Predicting training data to benchmark in-sample performance
LIMIT_BAL_in_2 <- predict(M2, train) 


#Predicting validation data to benchmark out-of-sample model performance
LIMIT_BAL_out_2 <- predict(M2, validation) 


#Computing and Reporting in-sample and out-of-sample RMSE
(RMSE_2_IN<-sqrt(sum((LIMIT_BAL_in_2-train$LIMIT_BAL)^2)/length(LIMIT_BAL_in_2))) #computing in-sample error
(RMSE_2_OUT<-sqrt(sum((LIMIT_BAL_out_2-validation$LIMIT_BAL)^2)/length(LIMIT_BAL_out_2))) #computing out-of-sample 


#installing packages for ridge regression 
library(lmridge)
library(broom)
library(MASS)

## building regularized model of above bivariate model ## 
M3<-lmridge(LIMIT_BAL ~ AGE + I(AGE^2) + I(AGE^3), data = train, K=seq(.05, .01))

summary(M3)

coef(M3)
pred<-predict(M3, df) #GENERATE PREDICTIONS FROM ALL MODELS FOR ALL LAMBDAS
View(pred)

#Predicting training data to benchmark in-sample performance
LIMIT_BAL_in_3 <- predict(M3, train) 


#Predicting validation data to benchmark out-of-sample model performance
LIMIT_BAL_out_3 <- predict(M3, validation) 


#Computing and Reporting in-sample and out-of-sample RMSE
(RMSE_3_IN<-sqrt(sum((LIMIT_BAL_in_3-train$LIMIT_BAL)^2)/length(LIMIT_BAL_in_3))) #computing in-sample error
(RMSE_3_OUT<-sqrt(sum((LIMIT_BAL_out_3-validation$LIMIT_BAL)^2)/length(LIMIT_BAL_out_3))) #computing out-of-sample


## Beginning implementation of generalized additive structure ##
library(mgcv)
M4 <- gam(LIMIT_BAL ~ AGE, data = train, family = 'quasipoisson')
summary(M4) #generates summary diagnostic output

#Predictions on the training data
LIMIT_BAL_IN_4 <- predict(M4, train, type = 'response') #generate predictions on the (in-sample) training data


#Predictions for the validation data
LIMIT_BAL_OUT_4 <- predict(M4, validation, type = 'response') #generate predictions on the (out-of-sample) testing data

# RMSE for in-sample and out-of-sample 
(RMSE_4_IN<-sqrt(sum((LIMIT_BAL_IN_4-train$LIMIT_BAL)^2)/length(LIMIT_BAL_IN_4)))  #computes in-sample error
(RMSE_4_OUT<-sqrt(sum((LIMIT_BAL_OUT_4-validation$LIMIT_BAL)^2)/length(LIMIT_BAL_OUT_4))) #computes out-of-sample 

# Creating a bivariate plot

x_grid <- seq(0,80,.1) #CREATES GRID OF X-AXIS VALUES
plot(train$LIMIT_BAL ~ train$AGE, col='blue')
predictions_1 <- predict(model_linear, list(AGE=x_grid))
predictions_2 <- predict(M2, list(AGE=x_grid, AGE2=x_grid^2))
predictions_3 <- predict(M3, list(AGE=x_grid, AGE2=x_grid^2, AGE3=x_grid^3))
predictions_4 <- predict(M4, list(AGE=x_grid, AGE2=x_grid^2, AGE3=x_grid^3, AGE4=x_grid^4))
lines(x_grid, predictions_1, col='blue', lwd=3) #PLOTS model_linear
lines(x_grid, predictions_2, col='lightgreen', lwd=3) #PLOTS M2
lines(x_grid, predictions_3, col='yellow', lwd=3) #PLOTS M3
lines(x_grid, predictions_4, col='darkgreen', lwd=3) #PLOTS M4
points(validation$LIMIT_BAL ~ validation$AGE, col='red', pch=3, cex=.5)

TABLE_VAL_1 <- as.table(matrix(c(RMSE_1_IN, RMSE_2_IN, RMSE_3_IN, RMSE_4_IN, RMSE_1_OUT, RMSE_2_OUT, RMSE_3_OUT, RMSE_4_OUT), ncol=4, byrow=TRUE))
colnames(TABLE_VAL_1) <- c('LINEAR', 'NONLINEAR', 'REGULARIZED', 'QUASIPOISSON')
rownames(TABLE_VAL_1) <- c('RMSE_IN', 'RMSE_OUT')
TABLE_VAL_1 #REPORT OUT-OF-SAMPLE ERRORS FOR BOTH HYPOTHESIS

#nonlinear model performed the best ##
LIMIT_BAL_test_2 <- predict(M2, test) 
(RMSE_2_TEST<-sqrt(sum((LIMIT_BAL_test_2-test$LIMIT_BAL)^2)/length(LIMIT_BAL_test_2))) #computing out-of-sample 

#117833.6 on the uncontaminated test partition 



## Multivariate Regression Modeling ##
#linear model without nonlinear transformations and the waterfront categorical variable
M5 <- lm(LIMIT_BAL ~ AGE + BILL_AMT1 + PAY_0 + PAY_AMT1, train)
summary(M5)


PRED_5_IN<-predict(M5, train) #Generating predictions from all models
PRED_5_OUT <- predict(M5, validation) #predictions for validation


(RMSE_5_IN <- sqrt(sum((PRED_5_IN-train$LIMIT_BAL)^2)/length(PRED_5_IN))) #in-sample error
(RMSE_5_OUT <- sqrt(sum((PRED_5_OUT-validation$LIMIT_BAL)^2)/length(PRED_5_OUT))) #out-of-sample error


#Regularized version of first multivariate model
M6 <- lmridge(LIMIT_BAL ~ AGE + BILL_AMT1 + PAY_0 + PAY_AMT1, train, K=seq(0, .05, .01))

summary(M6) #Running validation
coef(M6)
PRED_6_IN<-predict(M6, train) #Generating predictions from all models
PRED_6_OUT <- predict(M6, validation) #predictions for validation

(RMSE_6_IN <- sqrt(sum((PRED_6_IN-train$LIMIT_BAL)^2)/length(PRED_6_IN))) #in-sample error
(RMSE_6_OUT <- sqrt(sum((PRED_6_OUT-validation$LIMIT_BAL)^2)/length(PRED_6_OUT))) #out-of-sample error

#Nonlinear transformation of model
M7 <- lm(LIMIT_BAL ~ AGE + I(BILL_AMT1^2) + PAY_0 + PAY_AMT1, train)
summary(M7)

#Generating predictions on in-sample data
PRED_7_IN <- predict(M7, train)

#Generating predictions on validation data
PRED_7_OUT <- predict(M7, validation)

#In-sample and out-of-sample RMSE
(RMSE_7_IN<-sqrt(sum((PRED_7_IN-train$LIMIT_BAL)^2)/length(PRED_7_IN)))  #in-sample error
(RMSE_7_OUT<-sqrt(sum((PRED_7_OUT-validation$LIMIT_BAL)^2)/length(PRED_7_OUT))) #out-of-sample error

#Estimating a support vector machine
set.seed(123)
split<-initial_split(df, .7, strata=LIMIT_BAL) #CREATE THE SPLIT
train<-training(split) #TRAINING PARTITION
holdout<-testing(split) #test PARTITION
split_holdout<-initial_split(holdout, prop = .5)  # Splitting holdout into 50% each
test <- training(split_holdout)  # Extracting the testing set
validation <- testing(split_holdout)  # Extracting the validation set

kern_type<-"radial" #kernel type for SVM

# Selecting columns 7, 8, 6, and 12 for x
x_subset <- df[, c(7, 8, 11, 12)]
y <- df[, 9]


# Install the e1071 package if not already installed
if (!requireNamespace("e1071", quietly = TRUE)) 
  install.packages("e1071")


# Load the e1071 package
library(e1071)

#BUILD SVM CLASSIFIER
SVM_Model<- svm(LIMIT_BAL ~ AGE + PAY_0 + PAY_2 + PAY_5 + PAY_6, 
                data = train, 
                kernel = kern_type,
                type = "eps-regression",
                cost=1,                   
                gamma = 1/(ncol(training)-1), 
                coef0 = 0,                    
                degree=2,                     
                scale = FALSE)                

print(SVM_Model) #diagnostics


#Generating predictions pre-tune
pred_train_svm <- predict(SVM_Model, train)  # Predictions on the training set
pred_validation_svm <- predict(SVM_Model, validation)  # Predictions on the validation set


#RMSE calculation for SVM
RMSE_8_IN <- sqrt(mean((pred_train_svm - train$LIMIT_BAL)^2))  # For the training set
RMSE_8_OUT <- sqrt(mean((pred_validation_svm - validation$LIMIT_BAL)^2))  # For the validation set

RMSE_8_IN  # in-sample RMSE for SVM pre-tuned
RMSE_8_OUT  #out-of-sample RMSE for SVM pre-tuned

#Tuning SVM via cross-validation
tune_control<-tune.control(cross=10) #SET K-FOLD CV PARAMETERS
set.seed(123)
TUNE <- tune.svm(x = x_subset,
                 y = y,
                 type = "eps-regression",
                 kernel = kern_type,
                 tunecontrol=tune_control,
                 cost=c(.01, .1, 1, 10, 100, 1000), 
                 gamma = c(.1, .3, .4, .5, .6), 
                 coef0 = 0,           
                 degree = 2)        

print(TUNE) #OPTIMAL TUNING PARAMETERS FROM VALIDATION PROCEDURE

#RE-BUILD MODEL USING OPTIMAL TUNING PARAMETERS
SVM_Retune<- svm(LIMIT_BAL ~ PAY_0 + PAY_2 + PAY_5 + PAY_6, 
                 data = train, 
                 type = "eps-regression", 
                 kernel = kern_type,
                 degree = TUNE$best.parameters$degree,
                 gamma = TUNE$best.parameters$gamma,
                 coef0 = TUNE$best.parameters$coef0,
                 cost = TUNE$best.parameters$cost,
                 scale = FALSE)

print(SVM_Retune) #Diagnostics


#Generating predictions on retuned model
(pred_train_retune<-(predict(SVM_Retune, train)))
(pred_validation_retune<-(predict(SVM_Retune, validation)))

#In and out-of-sample errors on retuned model
RMSE_8_RETUNE_IN <- sqrt(mean((pred_train_retune - train$LIMIT)^2))  # For the training set
RMSE_8_RETUNE_OUT <- sqrt(mean((pred_validation_retune - validation$LIMIT_BAL)^2))  # For the validation set


#Results summarized in a table:
TUNE_TABLE <- matrix(c(RMSE_8_IN, 
                       RMSE_8_RETUNE_IN,
                       RMSE_8_OUT,
                       RMSE_8_RETUNE_OUT),
                     ncol=2, 
                     byrow=TRUE)

colnames(TUNE_TABLE) <- c('UNTUNED', 'TUNED')
rownames(TUNE_TABLE) <- c('E_IN', 'E_OUT')
TUNE_TABLE #Out-of-sample errors

## Estimating regression tree ##
library(tidymodels)
library(caret)
library(rpart.plot)
library(hardhat)
library(magrittr)


##PARTITIONING THE DATA##
set.seed(123)
split<-initial_split(df, .7, strata=LIMIT_BAL)
train<-training(split) #training partition
holdout<-testing(split) #holdout partition
split_holdout<-initial_split(holdout, prop = .5)  # Splitting holdout into 50% each
test <- training(split_holdout)  # Extracting the testing set
validation <- testing(split_holdout)  # Extracting the validation set

##Regression tree model
reg_spec <- decision_tree(min_n = 40 , #minimum number of observations for split
                          tree_depth = 30, #max tree depth
                          cost_complexity = 0.01)  %>% #regularization parameter
  set_engine("rpart") %>%
  set_mode("regression")
print(reg_spec)

#Estimating the model
reg_fmla <- LIMIT_BAL ~ AGE + SEX + PAY_2
reg_tree <- reg_spec %>%
  fit(formula = reg_fmla, data = train)
print(reg_tree)

#Visualizing regression tree
reg_tree$fit %>%
  rpart.plot(type = 4, roundint = FALSE)

#GENERATE IN-SAMPLE PREDICTIONS ON THE TRAIN SET AND COMBINE WITH TRAIN DATA
pred_reg_in <- predict(reg_tree, new_data = train) %>%
  bind_cols(train) #Adding predictions to training data


#Generating out-of-sample predictions
pred_reg_out <- predict(reg_tree, new_data = validation) %>%
  bind_cols(validation)

RMSE_REG_IN <- sqrt(mean((pred_reg_in$.pred - train$LIMIT_BAL)^2))#in-sample RMSE
RMSE_REG_OUT<-sqrt(mean((pred_reg_out$.pred - validation$LIMIT_BAL)^2)) #out-of-sample RMSE

RMSE_REG_IN
RMSE_REG_OUT

#Blank tree for tuning
reg_tune <- decision_tree(min_n = tune(),
                          tree_depth = tune(),
                          cost_complexity= tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

###
tree_parameters <- extract_parameter_set_dials(reg_tune)

# Generate a regular grid for tuning
tree_grid <- grid_regular(tree_parameters, levels = 3)

#Tuning model w/ cross-validation
set.seed(123)
tune_results <- tune_grid(reg_tune,
                          reg_fmla, 
                          resamples = vfold_cv(train, v=5),
                          grid = tree_grid, #grid
                          metrics = metric_set(rmse)) #benchmark metric

#Optimal parameters
best_params <- select_best(tune_results)

#Finalizing the model
final_spec <- finalize_model(reg_tune, best_params)

#Fitting the model with training set
final_model <- final_spec %>% fit(reg_fmla, train)

#In-sample predictions on training set and combining it with the data
pred_reg_in_2 <- predict(final_model, new_data = train, type="numeric") %>%
  bind_cols(train) #Adding the predictions

#Out-of-sample predictions on validation set and combining it with the data
pred_reg_out_2 <- predict(final_model, new_data = validation, type="numeric") %>%
  bind_cols(validation) #Adding predictions to validation data

#Tuned performance
RMSE_REG_IN_TUNE<-sqrt(mean((pred_reg_in_2$.pred - train$LIMIT_BAL)^2))#in-sample RMSE
RMSE_REG_OUT_TUNE<-sqrt(mean((pred_reg_out_2$.pred - validation$LIMIT_BAL)^2)) #out-of-sample RMSE

## Tuned regression tree has better predictive performance ##
TREE_TABLE <- matrix(c(RMSE_REG_IN, 
                       RMSE_REG_OUT,
                       RMSE_REG_IN_TUNE,
                       RMSE_REG_OUT_TUNE),
                     ncol=2, 
                     byrow=TRUE)

colnames(TREE_TABLE) <- c('UNTUNED', 'TUNED')
rownames(TREE_TABLE) <- c('E_IN', 'E_OUT')
TREE_TABLE #Out-of-sample errors

## Beginning tree-based ensemble model ##
library(tidymodels)
library(vip)

reg_rf <- rand_forest(min_n = 30 , #minimum number of observations for split
                      trees = 100, #of ensemble members
                      mtry = 2)  %>% #number of variables to consider at each split
  set_mode("regression") %>% 
  set_engine("ranger") 
reg_rf #Viewing diagnostics

install.packages('ranger')
library(ranger)
#Fitting the model
set.seed(123)
random_forest <- reg_rf %>%
  fit(formula = reg_fmla, data = train) #%>%
print(random_forest)

#Ranking variable importance
set.seed(123)
rand_forest(min_n = 30 , #minimum number of observations for split
            trees = 100, #of ensemble members
            mtry = 2)  %>% #number of variables to consider
  set_mode("regression") %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(reg_fmla, data = train) %>%
  vip()

#In-sample predictions on the training set and combining it with the data
pred_reg_rf_in <- predict(random_forest, new_data = train, type="numeric") %>%
  bind_cols(train) 

#Out-of-sample predictions on validation set and combining it with data
pred_reg_rf_out <- predict(random_forest, new_data = validation, type="numeric") %>%
  bind_cols(validation)

#Examining benchmark performance
RMSE_REG_RF_IN <- sqrt(mean((pred_reg_rf_in$.pred - train$LIMIT_BAL)^2)) #in-sample RMSE
RMSE_REG_RF_OUT <- sqrt(mean((pred_reg_rf_out$.pred - validation$LIMIT_BAL)^2)) #out-of-sample RMSE

RMSE_REG_RF_IN
RMSE_REG_RF_OUT

## Tuning the model ##
#Blank tree for tuning
forest_tune <- decision_tree(min_n = tune(),
                             tree_depth = tune(),
                             cost_complexity= tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

#Blank tree for tuning
reg_tune <- decision_tree(min_n = tune(),
                          tree_depth = tune(),
                          cost_complexity= tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

#Tuning parameter grid
tree_grid <- grid_regular(parameters(reg_tune), levels = 3) #For random grid

## INSTEAD DO BELOW CODE; ASK ABOUT THIS ###
tree_parameters <- extract_parameter_set_dials(reg_tune)

# Generate a regular grid for tuning
tree_grid <- grid_regular(tree_parameters, levels = 3)

#Tuning model w/ cross-validation
set.seed(123)
tune_results <- tune_grid(reg_tune,
                          reg_fmla, 
                          resamples = vfold_cv(train, v=5),
                          grid = tree_grid, #grid
                          metrics = metric_set(rmse)) #benchmark metric

#Optimal parameters
best_params <- select_best(tune_results)

#Finalizing the model
final_spec <- finalize_model(reg_tune, best_params)

#Fitting the model with training set
final_model <- final_spec %>% fit(reg_fmla, train)

#In-sample predictions on training set and combining it with the data
pred_reg_in_2 <- predict(final_model, new_data = train, type="numeric") %>%
  bind_cols(train) #Adding the predictions

#Out-of-sample predictions on validation set and combining it with the data
pred_reg_out_2 <- predict(final_model, new_data = validation, type="numeric") %>%
  bind_cols(validation) #Adding predictions to validation data

#Tuned performance
RMSE_REG_IN_TUNE<-sqrt(mean((pred_reg_in_2$.pred - train$LIMIT_BAL)^2))#in-sample RMSE

RMSE_REG_OUT_TUNE<-sqrt(mean((pred_reg_out_2$.pred - validation$LIMIT_BAL)^2)) #out-of-sample RMSE

## Tuned regression tree has better predictive performance ##
TREE_TABLE <- matrix(c(RMSE_REG_IN, 
                       RMSE_REG_IN_TUNE,
                       RMSE_REG_OUT,
                       RMSE_REG_OUT_TUNE),
                     ncol=2, 
                     byrow=TRUE)

colnames(TREE_TABLE) <- c('UNTUNED', 'TUNED')
rownames(TREE_TABLE) <- c('E_IN', 'E_OUT')
TREE_TABLE #Out-of-sample errors

## Beginning tree-based ensemble model ##
library(tidymodels)
library(vip)

reg_rf <- rand_forest(min_n = 30 , #minimum number of observations for split
                      trees = 100, #of ensemble members
                      mtry = 2)  %>% #number of variables to consider at each split
  set_mode("regression") %>% 
  set_engine("ranger") 
reg_rf #Viewing diagnostics

#Fitting the model
set.seed(123)
random_forest <- reg_rf %>%
  fit(formula = reg_fmla, data = train) #%>%
print(random_forest)

#Ranking variable importance
set.seed(123)
rand_forest(min_n = 30 , #minimum number of observations for split
            trees = 100, #of ensemble members
            mtry = 2)  %>% #number of variables to consider
  set_mode("regression") %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(reg_fmla, data = train) %>%
  vip()

#In-sample predictions on the training set and combining it with the data
pred_reg_rf_in <- predict(random_forest, new_data = train, type="numeric") %>%
  bind_cols(train)

#Out-of-sample predictions on validation set and combining it with data
pred_reg_rf_out <- predict(random_forest, new_data = validation, type="numeric") %>%
  bind_cols(validation)

#Examining benchmark performance
RMSE_REG_RF_IN <- sqrt(mean((pred_reg_rf_in$.pred - train$LIMIT_BAL)^2)) #in-sample RMSE
RMSE_REG_RF_OUT <- sqrt(mean((pred_reg_rf_out$.pred - validation$LIMIT_BAL)^2)) #out-of-sample RMSE

RMSE_REG_RF_IN
RMSE_REG_RF_OUT

## Tuning the model ##
#Blank tree for tuning
forest_tune <- decision_tree(min_n = tune(),
                             tree_depth = tune(),
                             cost_complexity= tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")


# Create the combined table
TABLE_VAL_7 <- as.table(matrix(c(
  RMSE_1_IN, RMSE_2_IN, RMSE_3_IN, RMSE_4_IN, RMSE_5_IN, RMSE_6_IN, RMSE_7_IN, 
   RMSE_1_OUT, RMSE_2_OUT, RMSE_3_OUT, RMSE_4_OUT, RMSE_5_OUT,
  RMSE_6_OUT, RMSE_7_OUT
), ncol = 14, byrow = TRUE))


#Combined table
TABLE_VAL_7 <- as.table(matrix(c(RMSE_1_IN, RMSE_2_IN, RMSE_3_IN, RMSE_4_IN, RMSE_5_IN, RMSE_6_IN, RMSE_7_IN, 
                                 RMSE_1_OUT, RMSE_2_OUT, RMSE_3_OUT, RMSE_4_OUT, RMSE_5_OUT,
                                 RMSE_6_OUT, RMSE_7_OUT), ncol = 14, byrow = TRUE))
colnames(TABLE_VAL_7) <- c('SIMPLE LINEAR', 'NONLINEAR', 'REGULARIZED', 'SPLINE', 'MULTI-LINEAR', 'MULTI-REGULARIZED', 'MULTI-NONLINEAR',
                           'SVM', 'Tuned SVM', 'RMSE_IN', 'RMSE_OUT', 'Ens Tree')

rownames(TABLE_VAL_7) <- c('RMSE_IN', 'RMSE_OUT')
TABLE_VAL_7

TABLE_VAL_7 #REPORT OUT-OF-SAMPLE ERRORS FOR BOTH HYPOTHESIS




##### Classification tasks 

names(df)

# Load necessary library
library(dplyr)

# current number of 1s and 0s in the 'DEFAULT' column
current_count_1 <- sum(df$'DEFAULT' == 1)
current_count_0 <- sum(df$'DEFAULT' == 0)

#counts of "true" and "false"
current_count_1
current_count_0

# Splitting the data into "True" and "False" subsets
current_count_1 <- df[df$'DEFAULT' == 1, ]
current_count_0 <- df[df$'DEFAULT' == 0, ]

# Counting the number of "true" and "false" values in the dataset
final_count_1 <- sum(df$'DEFAULT' == 1)
final_count_0 <- sum(df$'DEFAULT'== 0)

# Output the counts of "true" and "false" in the balanced dataset
final_count_1
final_count_0


# Fit the Logistic Regression Model
logit_model <- glm(`DEFAULT` ~ ., data = train, family = binomial("logit"))

# Model Summary
summary(logit_model)

# In-sample Performance
in_sample_preds <- predict(logit_model, train, type = "response")
in_sample_class <- ifelse(in_sample_preds > 0.5, 1, 0)
in_sample_accuracy <- mean(in_sample_class == train$`DEFAULT`)

# Out-of-sample Performance
out_sample_preds <- predict(logit_model, test, type = "response")
out_sample_class <- ifelse(out_sample_preds > 0.5, 1, 0)
out_sample_accuracy <- mean(out_sample_class == test$`DEFAULT`)



print(out_sample_accuracy)


# Fit the probit Regression Model
probit_model <- glm(`DEFAULT` ~ ., data = train, family = binomial("probit"))

summary(probit_model)

# In-sample Performance
in_sample_probit <- predict(probit_model, train, type = "response")
in_sample_class_2 <- ifelse(in_sample_preds > 0.5, 1, 0)
in_sample_accuracy_2 <- mean(in_sample_class == train$`DEFAULT`)

# Out-of-sample Performance
out_sample_probit <- predict(probit_model, test, type = "response")
out_sample_class_2 <- ifelse(out_sample_preds > 0.5, 1, 0)
out_sample_accuracy_2 <- mean(out_sample_class == test$`DEFAULT`)


out_sample_accuracy_2
in_sample_accuracy_2

#VERIFY STRATIFIED SAMPLING YIELDS EQUALLY SKEWED PARTITIONS
mean(train$'DEFAULT'==1)
mean(validation$'DEFAULT'==1)

kern_type<-"radial" #SPECIFY KERNEL TYPE

#BUILD SVM CLASSIFIER
SVM_Model<- svm(AGE ~ ., 
                data = train, 
                type = "C-classification", #set to "eps-regression" for numeric prediction
                kernel = kern_type,
                cost=1,                   #REGULARIZATION PARAMETER
                gamma = 1/(ncol(training)-1), #DEFAULT KERNEL PARAMETER
                coef0 = 0,                    #DEFAULT KERNEL PARAMETER
                degree=2,                     #POLYNOMIAL KERNEL PARAMETER
                scale = FALSE)                #RESCALE DATA? (SET TO TRUE TO NORMALIZE)

print(SVM_Model) #DIAGNOSTIC SUMMARY

#REPORT IN AND OUT-OF-SAMPLE ERRORS (1-ACCURACY)
(E_IN_PRETUNE<-1-mean(predict(SVM_Model, train)==train$`DEFAULT`))
(E_OUT_PRETUNE<-1-mean(predict(SVM_Model, test)==test$`DEFAULT`))

#SET 'DEFAULT' AS FACTOR
as.factor(df$DEFAULT)
y_factor <- as.factor(train[,25])

#NOW TUNING THE SVM BY CROSS-VALIDATION
tune_control<-tune.control(cross=10) #SET K-FOLD CV PARAMETERS
set.seed(123)
TUNE <- tune.svm(x = train[,2],
                 y = y_factor,
                 type = "C-classification",
                 kernel = kern_type,
                 tunecontrol=tune_control,
                 cost=c(.01, .1, 1, 10, 100, 1000), #REGULARIZATION PARAMETER
                 gamma = 1/(ncol(train)-1), #KERNEL PARAMETER
                 coef0 = 0,           #KERNEL PARAMETER
                 degree = 2)          #POLYNOMIAL KERNEL PARAMETER

print(TUNE) #OPTIMAL TUNING PARAMETERS FROM VALIDATION PROCEDURE

#RE-BUILD MODEL USING OPTIMAL TUNING PARAMETERS
SVM_Retune<- svm(`DEFAULT` ~ ., 
                 data = train, 
                 type = "C-classification", 
                 kernel = kern_type,
                 degree = TUNE$best.parameters$degree,
                 gamma = TUNE$best.parameters$gamma,
                 coef0 = TUNE$best.parameters$coef0,
                 cost = TUNE$best.parameters$cost,
                 scale = FALSE)

print(SVM_Retune) #DIAGNOSTIC SUMMARY

#REPORT IN AND OUT-OF-SAMPLE ERRORS (1-ACCURACY) ON RETUNED MODEL
(E_IN_RETUNE<-1-mean(predict(SVM_Retune, train)==train$`DEFAULT`))
(E_OUT_RETUNE<-1-mean(predict(SVM_Retune, test)==test$`DEFAULT`))

#SUMMARIZE RESULTS IN A TABLE:
TUNE_TABLE <- matrix(c(E_IN_PRETUNE, 
                       E_IN_RETUNE,
                       E_OUT_PRETUNE,
                       E_OUT_RETUNE),
                     ncol=2, 
                     byrow=TRUE)

colnames(TUNE_TABLE) <- c('UNTUNED', 'TUNED')
rownames(TUNE_TABLE) <- c('E_IN', 'E_OUT')
TUNE_TABLE #REPORT OUT-OF-SAMPLE ERRORS FOR BOTH HYPOTHESIS


# Loading up libraries
library(rpart)
library(rpart.plot)

# Fit Classification Tree
tree_model <- rpart(`DEFAULT` ~ ., data = train, method = "class")

# Visualize the Tree
rpart.plot(tree_model)

# In-sample Performance
in_sample_tree_preds <- predict(tree_model, train, type = "class")
in_sample_tree_accuracy <- mean(in_sample_tree_preds == train$`DEFAULT`)

# Out-of-sample Performance
out_sample_tree_preds <- predict(tree_model, test, type = "class")
out_sample_tree_accuracy <- mean(out_sample_tree_preds == test$`DEFAULT`)

# Print Accuracies
print(in_sample_tree_accuracy)
print(out_sample_tree_accuracy)

# Loading in the library
library(randomForest)

# Fit Random Forest Model
rf_model <- randomForest(`DEFAULT` ~ ., data = train)

# In-sample Performance
in_sample_rf_preds <- predict(rf_model, train)
in_sample_rf_accuracy <- mean(in_sample_rf_preds == train$`DEFAULT`)

# Out-of-sample Performance
out_sample_rf_preds <- predict(rf_model, test)
out_sample_rf_accuracy <- mean(out_sample_rf_preds == test$`DEFAULT`)

# Print Accuracies
print(in_sample_rf_accuracy)
print(out_sample_rf_accuracy)




####Multi-class Prediction###

#training train as the dataframe


class(train)

library(caret)
set.seed(123)  # for reproducibility
index <- createDataPartition(df$AGE, p = 0.7, list = FALSE)
train <- df[index, ]
test <- df[-index, ]

svm_model <- svm(AGE ~ ., data = train, type = 'C-classification', kernel = 'radial')

train <- as.data.frame(train)

library(e1071)

# Discretizing a numeric variable (e.g., AGE) into categories
df$AGE_cat <- cut(df$AGE, breaks=3, labels=c("Young", "Middle-aged", "Senior"))

# Split data into training and testing (Assuming train and test datasets are already created)

# Fit SVM Model
svm_model <- svm(df$AGE_cat ~ ., data = train, type = 'C-classification', kernel = 'radial')

# Predict and Evaluate Performance
train_pred <- predict(svm_model, train)
test_pred <- predict(svm_model, test)

train_accuracy <- mean(train_pred == train$AGE)
test_accuracy <- mean(test_pred == test$AGE)

# Output the accuracy
print(train_accuracy)
print(test_accuracy)


#tree creation

library(rpart)
library(rpart.plot)

# Ensure that the AGE_cat variable is created before splitting the data
df$AGE_cat <- cut(df$AGE, breaks=3, labels=c("Young", "Middle-aged", "Senior"))

# Split the data
set.seed(123)  # for reproducibility
index <- createDataPartition(df$AGE_cat, p = 0.7, list = FALSE)
train <- df[index, ]
test <- df[-index, ]

# converting to train the data
train <- as.data.frame(train)

# Fit Classification Tree
tree_model <- rpart(AGE_cat ~ ., data = train, method = "class")

# Visualizing the tree
rpart.plot(tree_model)

# Optionally, evaluating tree's performance
train_pred <- predict(tree_model, train, type = "class")
test_pred <- predict(tree_model, test, type = "class")

train_accuracy <- mean(train_pred == train$AGE_cat)
test_accuracy <- mean(test_pred == test$AGE_cat)

# Output the accuracy
print(paste("Training Accuracy:", train_accuracy))
print(paste("Testing Accuracy:", test_accuracy))


# Function to calculate accuracy
calculate_accuracy <- function(predictions, actual) {
  mean(predictions == actual)
}

# Calculate Accuracies for SVM Model
train_accuracy_svm <- calculate_accuracy(train_pred, train$AGE)
test_accuracy_svm <- calculate_accuracy(test_pred, test$AGE)

# Calculate Accuracies for Decision Tree Model
# Assuming you have predictions from decision tree as train_pred_tree and test_pred_tree
train_accuracy_tree <- calculate_accuracy(train_pred, train$AGE)
test_accuracy_tree <- calculate_accuracy(test_pred, test$AGE)

# Summarize accuracies in a data frame
accuracy_summary <- data.frame(
  Model = c("SVM", "Decision Tree"),
  In_sample_Accuracy = c(train_accuracy_svm, train_accuracy_tree),
  Out_of_sample_Accuracy = c(test_accuracy_svm, test_accuracy_tree)
)

# Print accuracy summary
print(accuracy_summary)

# Choose the best model based on out-of-sample accuracy
best_model <- ifelse(test_accuracy_svm > test_accuracy_tree, "SVM", "Decision Tree")

# Print best model
print(paste("Best Model based on Out-of-sample Accuracy:", best_model))



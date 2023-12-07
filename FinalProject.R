install.packages('readr')
install.packages("caret")
install.packages("Matrix")
install.packages('dplyr')
install.packages('tidyverse')
library(tidyverse)
library(dplyr)
library(caret)
library(readr)
library(Martix)
library(lmridge) #FOR lmridge()
library(broom) #FOR glance() AND tidy()
library(MASS) #FOR lm.ridge()
library(ggplot2) #FOR ggplot()

###CLEANING THE DATA###

# Read the CSV file
df<-`default.of.credit.card.clients.(1)`
View(df)

# Set the correct column names
colnames(df) <- as.character(unlist(df[2, ]))

# Remove the first two rows
df <- df[-c(1, 2), ]
rownames(df) <- NULL

# Remove spaces in column names
names(df) <- gsub(" ", "", names(df))

# renaming Default column
df <- df %>% rename(DEFAULT = `defaultpaymentnextmonth`)

# Function to remove '$' and convert to numeric
clean_column <- function(column) {
  column <- gsub("\\$", "", column) # Remove '$'
  column <- gsub(",", "", column)   # Remove commas
  as.numeric(column)               # Convert to numeric
}
#remove all '$' signs from the dataset
df <- df %>% 
  mutate(across(where(~any(str_detect(., fixed("$")))), clean_column))

#replace all NA values with 0s
df <- df %>%
  mutate(across(everything(), ~as.numeric(replace_na(., 0))))

#view cleaned data frame
View(df)
head(df)


######DATA PARTITIONING######

# Set seed for reproducibility
set.seed(123)

# Step 1: Create a random index for partitioning
index <- sample(1:nrow(df), nrow(df))

# Step 2: Specify the proportions for each partition
train_prop <- 0.7  # 70% for training
valid_prop <- 0.15  # 15% for validation
test_prop <- 0.15   # 15% for testing


# Step 3: Calculate the number of observations for each partition
train_size <- round(nrow(df) * train_prop)
valid_size <- round(nrow(df) * valid_prop)
test_size <- nrow(df) - train_size - valid_size

# Step 4: Create the partitions
train_set <- df[index[1:train_size], ]
valid_set <- df[index[(train_size + 1):(train_size + valid_size)], ]
test_set <- df[index[(train_size + valid_size + 1):nrow(df)], ]

# Step 5: Save the partitions to CSV files
write.csv(train_set, "train_set.csv", row.names = FALSE)
write.csv(valid_set, "valid_set.csv", row.names = FALSE)
write.csv(test_set, "test_set.csv", row.names = FALSE)

df <- data.frame(lapply(df, function(x) as.numeric(as.character(x))))
View(df)

#Adding new column to calculate total 6 month revolving balance
# Calculate ST_BAL
df$ST_BAL <- (df$BILL_AMT1 - df$PAY_AMT1) +
  (df$BILL_AMT2 - df$PAY_AMT2) +
  (df$BILL_AMT3 - df$PAY_AMT3) +
  (df$BILL_AMT4 - df$PAY_AMT4) +
  (df$BILL_AMT5 - df$PAY_AMT5) +
  (df$BILL_AMT6 - df$PAY_AMT6)

# If balance is >0, 1, else, 0
# Create PAY_FULL based on ST_BAL
df$PAY_FULL <- ifelse(df$ST_BAL > 0, 1, 0)

# Display the first few rows of the dataframe to check the new columns
head(df)

######CORRELATION MATRIX######

# Creating the corMatrix
corMatrix <- cor(df)

#Setting 1.0 values to NA in order to visualize actual correlations
diag(corMatrix) <- NA  # Set diagonal values to NA

# Function to find maximum correlation and the corresponding variable
find_max_corr <- function(column, matrix) {
  max_value <- max(column, na.rm = TRUE)  # Find maximum value
  max_var <- names(which(column == max_value))  # Find variable name with max correlation
  return(c(max_var, max_value))
}

# Apply this function to each column
max_correlations <- apply(corMatrix, MARGIN = 2, find_max_corr)

# Display the results
max_correlations

######SIMPLE LINEAR MODEL######

# Paying full statement balance as a function of their first payment
model_linear <- lm(df$PAY_FULL ~ df$PAY_2, data = train_set)

# Summary of the model
summary(model_linear)

######TESTING LM######

ML_Predict_IN <- predict(model_linear, train_set)
View(ML_Predict)
View(model_linear$fitted.values)
LM_Predict_OUT <- predict(model_linear, test_set)

#COMPUTING / REPORTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
(RMSE_1_IN<-sqrt(sum((ML_Predict_IN-train_set$PAY_FULL)^2)/length(ML_Predict_IN))) #computes in-sample error
(RMSE_1_OUT<-sqrt(sum((LM_Predict_OUT-train_set$PAY_FULL)^2)/length(LM_Predict_OUT))) #computes out-of-sample 

#PLOTTING THE MODEL IN 2D AGAINST BOTH DATA PARTITIONS
x_grid <- seq(0,70,.1) #CREATES GRID OF X-AXIS VALUES

predictions <- predict(model_linear, list(ad_time=x_grid))
plot(train_set$PAY_FULL ~ train_set$PAY_2, col='blue')
lines(x_grid, predictions, col='green', lwd=3)
points(train_set$PAY_FULL ~ train_set$PAY_2, col='red', pch=3, cex=.5)

#####BIVARIATE MODEL#######

# Bivariate model with a quadratic transformation
model_nonlinear_PAY_FULL <- lm(PAY_FULL ~ (PAY_2^2), data = train_set)

# Summary of the model
summary(model_nonlinear_PAY0)

# Install and load the mgcv package
install.packages("mgcv")
library(mgcv)

# Poisson Regression GAM
model_poisson_gam <- gam(DEFAULT ~ s(PAY_0^2), 
                         family = poisson, data = train_set)
# Summary of the model
summary(model_poisson_gam)

# Quasi-Poisson Regression GAM
model_quasi_poisson_gam <- gam(DEFAULT ~ s(PAY_0), 
                               family = quasipoisson, data = train_set)

# Summary of the model
summary(model_quasi_poisson_gam)

# GAM with Splines
model_spline_gam <- gam(DEFAULT ~ s(LIMIT_BAL) + s(AGE) + SEX + EDUCATION + MARRIAGE + PAY_AMT1, 
                        data = train_set)

# Summary of the model
summary(model_spline_gam)

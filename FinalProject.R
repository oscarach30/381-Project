install.packages("caret")
install.packages("Matrix")
library(caret)
library(Martix)
# Set seed for reproducibility
set.seed(123)
# Set the correct column names
colnames(df) <- as.character(unlist(df[2, ]))

# Remove the first two rows
df <- df[-c(1, 2), ]

#convert the data to correct types
df <- data.frame(lapply(df, function(x) as.numeric(as.character(x))))
View(df)

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

# Assuming df is your data frame
corMatrix <- cor(df, use = "complete.obs")

# View the correlation matrix
print(corMatrix)

# Simple Linear Model
# CC Balance as a function of income
model_linear <- lm(Balance ~ Income, data = train_set)


# Linear regression model predicting default.payment.next.month
model_linear <- lm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, data = train_set)

# Summary of the model
summary(model_linear)

# Bivariate model with a quadratic transformation on PAY_0
model_nonlinear_PAY0 <- lm(default.payment.next.month ~ PAY_0 + I(PAY_0^2) + BILL_AMT1, data = train_set)

# Summary of the model
summary(model_nonlinear_PAY0)

# Install and load the mgcv package
install.packages("mgcv")
library(mgcv)

# Poisson Regression GAM
model_poisson_gam <- gam(default.payment.next.month ~ s(LIMIT_BAL) + s(AGE) + SEX + EDUCATION + MARRIAGE, 
                         family = poisson, data = train_set)

# Summary of the model
summary(model_poisson_gam)

# Quasi-Poisson Regression GAM
model_quasi_poisson_gam <- gam(default.payment.next.month ~ s(LIMIT_BAL) + s(AGE) + SEX + EDUCATION + MARRIAGE, 
                               family = quasipoisson, data = train_set)

# Summary of the model
summary(model_quasi_poisson_gam)

# GAM with Splines
model_spline_gam <- gam(default.payment.next.month ~ s(LIMIT_BAL) + s(AGE) + SEX + EDUCATION + MARRIAGE, 
                        data = train_set)

# Summary of the model
summary(model_spline_gam)



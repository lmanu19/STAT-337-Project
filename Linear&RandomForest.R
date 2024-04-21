################################# READ IN DATA #################################

library(foreign)
data <- read.arff("~/Downloads/bone-marrow.arff")

################################ LINEAR MODEL ##################################

# check for missing values
sum(is.na(data))

data <- data[complete.cases(data), ]
sum(is.na(data))

# convert categorical variables to dummy variables
data <- model.matrix(~ . - 1, data = data)
data <- as.data.frame(data)

# set seed for reproducibility
set.seed(2022)

# split data into 80% training and 20% testing sets
index.train <- sample(1:dim(data)[1], 0.8 * dim(data)[1])
data.train <- data[index.train,]
data.test <- data[-index.train,]

lm.model <- lm(survival_time ~ ., data = data.train)

# predict on the test set
yhat.test <- predict(lm.model, data.test)

# calculate test MSE
y.test <- data.test$survival_time
MSE.test <- mean((y.test - yhat.test)^2)
MSE.test
# 501282.2

# root MSE
RMSE.test <- sqrt(MSE.test)
RMSE.test
# 708.0129

# normalized root MSE
NRMSE.test <- RMSE.test / mean(y.test)
NRMSE.test
# 0.5818514

################################ RANDOM FOREST ################################# 

# fit Random Forest model
library(randomForest)
rf.model <- randomForest(survival_time ~ ., data = data.train)

# feature importance
importance(rf.model)
varImpPlot(rf.model)

######################### LINEAR MODEL USING CD DATAS ##########################

#lm.model <- lm(survival_time ~ ., data = data.train)
CDlm.model <- lm(survival_time ~ CD3dCD34 + CD3dkgx10d8 + CD34kgx10d6, data = data.train)

# predict on the test set
yhat.test <- predict(CDlm.model, data.test)

# calculate test MSE
y.test <- data.test$survival_time
MSE.test <- mean((y.test - yhat.test)^2)
MSE.test
# 615125.1 

# root MSE
RMSE.test <- sqrt(MSE.test)
RMSE.test
# 784.2991

# normalized root MSE
NRMSE.test <- RMSE.test / mean(y.test)
NRMSE.test
# 0.6445442

################################################################################

# Selected important variables
important_vars <- c("CD34kgx10d6", "CD3dCD34", "CD3dkgx10d8", "Rbodymass", "ANCrecovery", "PLTrecovery", "time_to_aGvHD_III_IV", "Relapse1", "Recipientage", "Gendermatch1", "Donorage", "Recipientgender0", "Recipientgender1")

# Fit linear regression model with selected important variables
newlm.model <- lm(survival_time ~ ., data = data.train[, c("survival_time", important_vars)])

# Predict on the test set
yhat.test <- predict(newlm.model, newdata = data.test[, important_vars])

# Calculate test MSE
y.test <- data.test$survival_time
MSE.test <- mean((y.test - yhat.test)^2)
MSE.test
# 798821.7

# root MSE
RMSE.test <- sqrt(MSE.test)
RMSE.test
# 893.7682

# normalized root MSE
NRMSE.test <- RMSE.test / mean(y.test)
NRMSE.test
# 0.7345069





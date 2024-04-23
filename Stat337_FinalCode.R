################################# READ IN DATA #################################
library(boot)
library(foreign)
data <- read.arff("~/Downloads/bone-marrow.arff")

# check for missing values
sum(is.na(data))

data <- data[complete.cases(data), ]
sum(is.na(data))

# convert categorical variables to dummy variables
data <- model.matrix(~ . - 1, data = data)
data <- as.data.frame(data)
################################ LINEAR MODEL ##################################
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

############### BAGGING, RANDOM FOREST, BOOSTING - survival_time ###############
library(randomForest)
library(gbm)
# random split the data into 80% training and 20% test
set.seed(2024)
index.train <- sample(1:dim(data)[1], 0.8 * dim(data)[1])
data.train <- data[index.train,]
data.test <- data[-index.train,]

# bagging
bagging.data <- randomForest(survival_time~., mtry=17, ntree=500, importance = TRUE, data=data.train)

# random forest
rf.data <- randomForest(survival_time~., mtry=4, ntree=500, data=data.train)

# boosting
boost.data <- gbm(survival_time~., n.trees=500, distribution = "gaussian", data=data.train)

# predict on the test set
yhat.test.bag <- predict(bagging.data , newdata = data.test)
yhat.test.rf <- predict(rf.data , newdata = data.test)
yhat.test.boost <- predict(boost.data , newdata = data.test)

# calculate test MSE
y.test <- data.test$survival_time
MSE.test.bag <- mean((y.test - yhat.test.bag)^2)
MSE.test.rf <- mean((y.test - yhat.test.rf)^2)
MSE.test.boost <- mean((y.test - yhat.test.boost)^2)

# compare MSE
MSE.test.bag; MSE.test.rf; MSE.test.boost
# 345602.4
# 501202.6
# 377543.3

# root MSE
RMSE.test.bag <- sqrt(MSE.test.bag)
RMSE.test.rf <- sqrt(MSE.test.rf)
RMSE.test.boost <- sqrt(MSE.test.boost)

# normalized root MSE
RMSE.test.bag / mean(y.test); RMSE.test.rf / mean(y.test); RMSE.test.boost / mean(y.test)
# 0.5894243
# 0.7098169
# 0.6160601

# visualize feature importance (take bagging as one example)
imp <- bagging.data$importance[,2]
imp.norm <- imp/max(imp)
imp.norm <- sort(imp.norm, decreasing = T)
top10_vars <- names(imp.norm)[1:10]
barplot(imp.norm[1:10], names.arg = top10_vars, las = 2, main = "Bagging - Survival_time", ylab = "Normalized Importance",cex.names = 0.7)

# visualize feature importance (take random forest as one example)
imp <- rf.data$importance[,1]
imp.norm <- imp/max(imp)
imp.norm <- sort(imp.norm, decreasing = T)
top10_vars <- names(imp.norm)[1:10]
barplot(imp.norm[1:10], names.arg = top10_vars, las = 2, main = "Random Forest - Survival_time", ylab = "Normalized Importance",cex.names = 0.7)

##################### LINEAR MODEL WITH IMPORANT VARIABLES #####################
# Selected important variables
important_vars <- c("CD34kgx10d6", "CD3dCD34", "Donorage", "Rbodymass", "CD3dkgx10d8", "Recipientage", "ANCrecovery", "PLTrecovery", "Relapse1")

# Fit linear regression model with selected important variables
newlm.model <- lm(survival_time ~ ., data = data.train[, c("survival_time", important_vars)])

# Predict on the test set
yhat.test <- predict(newlm.model, newdata = data.test[, important_vars])

# Calculate test MSE
y.test <- data.test$survival_time
MSE.test <- mean((y.test - yhat.test)^2)
MSE.test
# 689855.2

# root MSE
RMSE.test <- sqrt(MSE.test)
RMSE.test
# 830.5752

# normalized root MSE
NRMSE.test <- RMSE.test / mean(y.test)
NRMSE.test
# 0.6825743

############## BAGGING, RANDOM FOREST, BOOSTING - survival_status ############## 

library(randomForest)
library(gbm)
# random split the data into 80% training and 20% test
set.seed(2024)
index.train <- sample(1:dim(data)[1], 0.8 * dim(data)[1])
data.train <- data[index.train,]
data.test <- data[-index.train,]

# bagging
bagging.data <- randomForest(survival_status~., mtry=17, ntree=500, importance = TRUE, data=data.train)

# random forest
rf.data <- randomForest(survival_status~., mtry=4, ntree=500, data=data.train)

# boosting
boost.data <- gbm(survival_status~., n.trees=500, distribution = "gaussian", data=data.train)

# predict on the test set
yhat.test.bag <- predict(bagging.data , newdata = data.test)
yhat.test.rf <- predict(rf.data , newdata = data.test)
yhat.test.boost <- predict(boost.data , newdata = data.test)

# calculate test MSE
y.test <- data.test$survival_status
MSE.test.bag <- mean((y.test - yhat.test.bag)^2)
MSE.test.rf <- mean((y.test - yhat.test.rf)^2)
MSE.test.boost <- mean((y.test - yhat.test.boost)^2)

# compare MSE
MSE.test.bag; MSE.test.rf; MSE.test.boost
# 0.04811237
# 0.1216365
# 0.05853375

# root MSE
RMSE.test.bag <- sqrt(MSE.test.bag)
RMSE.test.rf <- sqrt(MSE.test.rf)
RMSE.test.boost <- sqrt(MSE.test.boost)

# normalized root MSE
RMSE.test.bag / mean(y.test); RMSE.test.rf / mean(y.test); RMSE.test.boost / mean(y.test)
# 0.5300845
# 0.8428469
# 0.5846823

# visualize feature importance (take bagging as one example)
imp <- bagging.data$importance[,2]
imp.norm <- imp/max(imp)
imp.norm <- sort(imp.norm, decreasing = T)
top10_vars <- names(imp.norm)[1:10]
barplot(imp.norm[1:10], names.arg = top10_vars, las = 2, main = "Bagging - Survival_status", ylab = "Normalized Importance",cex.names = 0.7)

# visualize feature importance (take random forest as one example)
imp <- rf.data$importance[,1]
imp.norm <- imp/max(imp)
imp.norm <- sort(imp.norm, decreasing = T)
top10_vars <- names(imp.norm)[1:10]
barplot(imp.norm[1:10], names.arg = top10_vars, las = 2, main = "Random Forest - Survival_status", ylab = "Normalized Importance",cex.names = 0.7)

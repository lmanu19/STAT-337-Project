library(caret)
library(foreign)
library(RWeka)
library(class)
setwd("C:/Users/laure/OneDrive/Documents/GitHub/STAT-337-Project/")

# KNN
data <- read.arff("C:/Users/laure/OneDrive/Documents/GitHub/STAT-337-Project/bone-marrow.arff")
summary(data)

#finding and removing missing values
#data <- data[complete.cases(data), ]
#sum(is.na(data))
data <- na.omit(data)

#showing the new summary
summary(data)

#changing all data into numeric data
for (col in names(data)) {
  data[[col]] <- as.numeric(data[[col]])
}

# Check the result
str(data)


# random split the data into 80% training and 20% test
set.seed(2024)
dim(data)

index.train <- sample(1:dim(data)[1], 0.8 * dim(data)[1])
data.train <- data[index.train,]
data.test <- data[-index.train,]


# knn model
#knnmodel <- knnreg(data.train[,c('Recipientgender','Stemcellsource','Donorage','Donorage35', 'IIIV', 'Gendermatch', 'DonorABO', 'RecipientABO', 'RecipientABO', 'RecipientRH', 'ABOmatch', 'CMVstatus', 'DonorCMV', 'RecipientCMV', 'Disease' , 'Riskgroup', 'Txtpostrelapse')], data.train$CD3dCD34, k=5)
#knnmodel <- knnreg(data.train[,c('Donorage','Donorage35', 'DonorABO', 'RecipientABO')], data.train$CD3dCD34, k=5)

#model <- IBk(survival_status ~ ., data = trainData)  # Assuming "survival_status" is the target variable
#predictions <- predict(model, testData)


  # Load the 'class' package for knn function
predicted <- knn(train = data.train[, -ncol(data)], test = data.test[, -ncol(data)], cl = data.train$RecipientABO, k = 5)

accuracy <- mean(predicted == data.test$RecipientABO)
cat("Accuracy:", accuracy, "\n")


# predict on the test set
#yhat.test = predict(knnmodel, data.test[,c('Recipientgender','Stemcellsource','Donorage','Donorage35', 'IIIV', 'Gendermatch', 'DonorABO', 'RecipientABO', 'RecipientABO', 'RecipientRH', 'ABOmatch', 'CMVstatus', 'DonorCMV', 'RecipientCMV', 'Disease' , 'Riskgroup', 'Txtpostrelapse')])
yhat.test = predict(knnmodel, data.test[,c('Recipientgender','Stemcellsource','Donorage','Donorage35', 'IIIV', 'Gendermatch', 'DonorABO', 'RecipientABO', 'RecipientABO', 'RecipientRH', 'ABOmatch', 'CMVstatus', 'DonorCMV', 'RecipientCMV', 'Disease' , 'Riskgroup', 'Txtpostrelapse')])


# calculate test MSE
y.test <- data.test$CD3dCD34
MSE.test <- mean((y.test - yhat.test)^2)
MSE.test

# root MSE
RMSE.test <- sqrt(MSE.test)
RMSE.test

# normalized root MSE
NRMSE.test <- RMSE.test / mean(y.test)
NRMSE.test

############################################################################################
# Cross-validation for best k
############################################################################################
# read fat dataset
fat <- read.csv('fat.csv')
set.seed(2024)

# randomly shuffle the index
index.random <- sample(1:dim(fat)[1])

# split the data (index) into 5 folds 
groups <- cut(1:252, 5, labels = FALSE)
index.fold <- split(index.random, groups)

MSEs.k <- c()
for(k in 1:20){
  # an empty vector to save individual MSE
  MSEs <- c()
  
  # 5-fold cross-validation
  for(index.test in index.fold){
    
    # creat training and test set
    data.test <- fat[index.test,]
    data.train <- fat[-index.test,]
    
    # knn model
    knnmodel <- knnreg(data.train[,c('weight','abdom','forearm','wrist')], data.train$brozek, k = k)
    
    # predict on the test set
    yhat.test = predict(knnmodel, data.test[,c('weight','abdom','forearm','wrist')])
    
    
    # calculate test MSE
    y.test <- data.test$brozek
    MSE.test <- mean((y.test - yhat.test)^2)
    MSEs <- c(MSEs, MSE.test)
  }
  
  # Average 5 MSEs
  MSEs.k <- c(MSEs.k, mean(MSEs))
}

plot(MSEs.k, type='b', col='red', xlab='K', ylab='MSE',)
which.min(MSEs.k)





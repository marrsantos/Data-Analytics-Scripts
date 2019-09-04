# Titanic challenge
# 06/15/2018 - Author: Marcelo Santos
#
library(dplyr)
library(randomForest)
library(xgboost)
library(car)
library(ROSE)
library(e1071)
#
data_folder <- paste(getwd(), '/./', sep='')
#dftrainreal <- read.csv(paste(data_folder,"train.csv",sep=''), sep=',', header=T)
#dftestreal <- read.csv(paste(data_folder,"test.csv",sep=''), sep=',', header=T)
dftrainreal <- read.csv(file.choose(), sep=',', header=T)
dftestreal <- read.csv(file.choose(), sep=',', header=T)

#
# Data preparation (dftrain)
#
dftrainreal$Sex<-as.character(dftrainreal$Sex)
dftrainreal$Sex[dftrainreal$Sex == "male"] <- "1"
dftrainreal$Sex[dftrainreal$Sex == "female"] <- "0"
dftrainreal$Sex<-as.integer(dftrainreal$Sex)
dftrainreal$Age[dftrainreal$Sex == 0 & is.na(dftrainreal$Age)]<-mean(dftrainreal$Age[dftrainreal$Sex == 0],na.rm=TRUE)
dftrainreal$Age[dftrainreal$Sex == 1 & is.na(dftrainreal$Age)]<-mean(dftrainreal$Age[dftrainreal$Sex == 1],na.rm=TRUE)
dftrainreal$Embarked<-as.character(dftrainreal$Embarked)
dftrainreal$Embarked[dftrainreal$Embarked == "C"] <- "1"
dftrainreal$Embarked[dftrainreal$Embarked == "Q"] <- "2"
dftrainreal$Embarked[dftrainreal$Embarked == "S"] <- "3"
# PROBLEMA AQUI - É POSSÍVEL MELHORAR A ALOCAÇÃO DE 0 PARA OS REGISTROS QUE FALTAM INFORMAÇÃO?
dftrainreal$Embarked[dftrainreal$Embarked == ""] <- "0"
dftrainreal$Embarked<-as.integer(dftrainreal$Embarked)
dftrainreal$FamilySize <- dftrainreal$SibSp+dftrainreal$Parch+1
# grouping by Ticket
dfaux <- dftrainreal %>% count(Ticket)
# Combining datasets - inserting the number of people with same ticket (left join)
dftrainreal<-left_join(dfaux, dftrainreal, by = c("Ticket"="Ticket"))
names(dftrainreal)[2]<-"SameFamilyCount"
write.table(dftrainreal, file = "train++.csv", sep = ",", row.names=FALSE)
#
# Data preparation (dftestreal)
#
dftestreal$Sex<-as.character(dftestreal$Sex)
dftestreal$Sex[dftestreal$Sex == "male"] <- "1"
dftestreal$Sex[dftestreal$Sex == "female"] <- "0"
dftestreal$Sex<-as.integer(dftestreal$Sex)
dftestreal$Age[dftestreal$Sex == 0 & is.na(dftestreal$Age)]<-mean(dftestreal$Age[dftestreal$Sex == 0],na.rm=TRUE)
dftestreal$Age[dftestreal$Sex == 1 & is.na(dftestreal$Age)]<-mean(dftestreal$Age[dftestreal$Sex == 1],na.rm=TRUE)
dftestreal$Embarked<-as.character(dftestreal$Embarked)
dftestreal$Embarked[dftestreal$Embarked == "C"] <- "1"
dftestreal$Embarked[dftestreal$Embarked == "Q"] <- "2"
dftestreal$Embarked[dftestreal$Embarked == "S"] <- "3"
dftestreal$Embarked[dftestreal$Embarked == ""] <- "0"
dftestreal$Embarked<-as.integer(dftestreal$Embarked)
dftestreal$FamilySize <- dftestreal$SibSp+dftestreal$Parch
# grouping by Ticket
dfaux <- dftestreal %>% count(Ticket)
# Combining datasets - inserting the number of people with same ticket (left join)
dftestreal<-left_join(dfaux, dftestreal, by = c("Ticket"="Ticket"))
names(dftestreal)[2]<-"SameFamilyCount"
write.table(dftestreal, file = "test++.csv", sep = ",", row.names=FALSE)
dftestreal<-arrange(dftestreal, PassengerId)
#
# Creating train and test datasets
set.seed(1111)
Tdummy<-sample(2, nrow(dftrainreal), replace=T, prob=c(.7,0.3))
dftrain<-dftrainreal[Tdummy==1,]
dftest<-dftrainreal[Tdummy==2,]
#
# removing id, name, ticket and cabin
dfcor<-dftrain[-c(1,3,6,12)]
# Generating a correlation matrix
round(cor(dfcor),2)
#
dftrain$Survived <- as.factor(dftrain$Survived)
dftrain$FarePas <- dftrain$Fare/dftrain$SameFamilyCount
dftest$FarePas <- dftest$Fare/dftest$SameFamilyCount
#
# Analyzing dataset inspired by the gender 
#
summary(dftrain)
dftrain %>% count( Sex, Survived)
table(dftrain$Survived)
#
# oversampling
# there is much more not records to Survived==0 than 1. 426*2 = 852
#
dftrain <- ovun.sample(Survived~., data = dftrain, method = "over", N=852)$data
table(dftrain$Survived)
dftrain %>% count( Sex, Survived)
#
# Making dftest also data.frame
#
dftest<-as.data.frame(dftest)
#
# PROBLEMA - na divisão de treino e teste o treino não está
# carregando todos as Cabins existentes no teste.
#
# Generating Binomial logistic regression
#
set.seed(1234)
mymodel<-glm(Survived~Sex+Pclass+Embarked+Cabin, data=dftrain, family='binomial')
summary(mymodel)
t1<-predict(mymodel, dftest, type = 'response')
pred1<-ifelse(t1>0.5,1,0)
tab1<-table(Predicted=pred1, Actual=dftest$Survived)
tab1
(tab1[1,1]+tab1[2,2])/(tab1[1,1]+tab1[2,2]+tab1[1,2]+tab1[2,1])
#
# multicolinearity test
#
car::vif(mymodel)
#
# Generating a SVM model
#
set.seed(1234)
mymodel<-svm(Survived~Sex+Pclass+Embarked, data=dftrain)
summary(mymodel)
pred1<-predict(mymodel, dftest, type = 'response')
tab1<-table(Predicted=pred1, Actual=dftest$Survived)
tab1
(tab1[1,1]+tab1[2,2])/(tab1[1,1]+tab1[2,2]+tab1[1,2]+tab1[2,1])
#
# Random Forest
#
dftrain$out <- relevel(as.factor(dftrain$Survived), ref = "0")
dftest$out <- relevel(as.factor(dftest$Survived), ref = "0")
#tuneRF
#t <- tuneRF(dftrain[,c(7,5,13)], dftrain$out, plot = TRUE, ntreeTry = 300, trace = TRUE, improve = 0.05, doBest=TRUE)
#t$mtry
set.seed(1234)
mymodel1 <- randomForest(out~Sex+Pclass+Embarked, data = dftrain, mtry=1)
#mymodel <- randomForest(out~Sex+Pclass+Age+Embarked+n+Fare+Parch+SibSp, data = dftrain, mtry=4)
mymodel1
#Prediction (test data)
t1<-predict(mymodel1, dftest)
# confusion matrix
tab1<-table(t1,dftest$out)
tab1
(tab1[1,1]+tab1[2,2])/(tab1[1,1]+tab1[2,2]+tab1[1,2]+tab1[2,1])
#
# Gradient Boosting 
#
#dfaux <- data.matrix(dftrain[,c(2,5,7,8,9,10,11,13)])
dfaux <- data.matrix(dftrain[,c(7,5,13)])
dfauxL <- data.matrix(as.integer(as.character(dftrain$out)))
dfauxM <- xgb.DMatrix(data = dfaux, label = dfauxL) 

best_param = list()
best_seednumber = 1234
best_error = Inf
best_error_index = 0

for (iter in 1:100) {
  param <- list(objective = "binary:logistic",
                eval_metric = "error",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2), 
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround = 1000
  cv.nfold = 5
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=dfauxM, params = param, nthread=6, 
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early_stopping_rounds = 8, maximize=FALSE)
  
  min_error = min(mdcv$evaluation_log[,4])
  min_error_index = which.min(data.matrix(mdcv$evaluation_log[,4]))
  
  if (min_error < best_error) {
    best_error = min_error
    best_error_index = min_error_index
    best_seednumber = seed.number
    best_param = param
  }
}

nround = best_error_index
set.seed(best_seednumber)
mymodel2 <- xgb.train(data=dfauxM, params=best_param, nrounds=nround, nthread=6)
# Generating Confusion matrix
pred1 <- predict (mymodel2,dfauxM)
pred1 <- ifelse (pred1 > 0.5,1,0)
tab1<-table(Predicted=pred1, Actual=dftrain$out)
tab1
(tab1[1,1]+tab1[2,2])/(tab1[1,1]+tab1[2,2]+tab1[1,2]+tab1[2,1])
# calculating Error
err <- mean(as.numeric(pred1 > 0.5) != dftrain$out)
print(paste("test-error=", err))
#
# same test using test dataset
#
#dfauxMT <- data.matrix(dftest[,c(2,5,7,8,9,10,11,13)])
dfauxMT <- data.matrix(dftest[,c(7,5,13)])
dfauxLT <- data.matrix(dftest$out)
dfauxMT <- xgb.DMatrix(data = dfauxMT, label = dfauxLT) 

mymodel2 <- xgb.train(data=dfauxMT, params=best_param, nrounds=nround, nthread=6)
# Generating Confusion matrix
pred1 <- predict (mymodel2,dfauxMT)
pred1 <- ifelse (pred1 > 0.5,1,0)
tab1<-table(Predicted=pred1, Actual=dftest$out)
tab1
(tab1[1,1]+tab1[2,2])/(tab1[1,1]+tab1[2,2]+tab1[1,2]+tab1[2,1])
# calculating Error
err <- mean(as.numeric(pred1 > 0.5) != dftest$out)
print(paste("test-error=", err))

#
####### Generating prediction to real test file
#
#dfauxFT <- data.matrix(dftestreal[,c(2,4,6,7,8,9,10,12)])
dfauxFT <- data.matrix(dftestreal[,c(6,4,12)])
#model prediction
pred1 <- predict (mymodel2,dfauxFT)
#pred1 <- predict(mymodel, dfauxT)
pred1 <- ifelse (pred1 > 0.5,1,0)

Result <- data.frame(PassengerId=dftestreal$PassengerId,Survived=pred1)
write.table(Result, file = paste(data_folder, "Result Gb v5.csv", sep=''), sep = ",", row.names = FALSE)

Result %>% count(Survived)

############
# LIXO
## https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
#dfaux <- data.matrix(dftrain[,c(2,5,7,8,9,10,11,13)])
dfaux <- data.matrix(dftrain[,c(7,5,13)])
dfauxL <- data.matrix(as.integer(as.character(dftrain$out)))
dfauxM <- xgb.DMatrix(data = dfaux, label = dfauxL) 
#params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3)
set.seed(1234)
xgbcv <- xgb.cv(params = params, data = dfauxM, nrounds = 10000, nfold = 10, showsd = T, stratified = T, print_every_n = 5, early_stopping_rounds = 20, metrics="error")
xgbt <- xgb.train(params = params, data = dfauxM, nrounds = 1000, maximize = F , eval_metric = "error")
#model prediction
#dfauxMT <- data.matrix(dftest[,c(2,5,7,8,9,10,11,13)])
dfauxMT <- data.matrix(dftest[,c(6,4,12)])
dfauxLT <- data.matrix(dftest$out)
dfauxMT <- xgb.DMatrix(data = dfaux, label = dfauxL) 
pred1 <- predict (xgbt,dfauxMT)
pred1 <- ifelse (pred1 > 0.5,1,0)
tab1<-table(Predicted=pred1, Actual=dftrain$out)
tab1
(tab1[1,1]+tab1[2,2])/(tab1[1,1]+tab1[2,2]+tab1[1,2]+tab1[2,1])
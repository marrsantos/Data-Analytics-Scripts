#
# Template for some data science methods
# Author: Marcelo Santos
#
# Load datasets
#
data_folder <- paste(getwd(), '/./datasets/', sep='')
mydata <- read.csv(file.choose(), sep=',', header=T)
mydata <- read.csv(paste(data_folder,"Iris.csv",sep=''), sep=',', header=T)
#
#@@@@@@@@@@@@@@@@@@@@@@@
## 1 - CLUSTERING ANALYSIS
# @@@@@@@@@@@@@@@@@@@@@@
#
# Analyzing raw datset features
str(mydata)
# examples of instances
head(mydata)
# summary of instances
summary(mydata)
# plotting pairs of features
pairs(mydata)

# Scaling data
# excluding the categorial variable
mydatascaled <- scale(mydata[,-5])
mydatascaled

# K-means method
#
result <- kmeans(mydatascaled,3)
result
plot(mydata, col=result$cluster)

# Analyzing k ....
k<- list()
for(i in 1:10){
  k[[i]] <- kmeans(mydatascaled, i)
}
k 

btss<- list()
for(i in 1:10){
  btss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}
plot(1:10, btss, type="b", ylab ="Between SS/ Total SS", xlab ="Clusters (k)")

for (i in 1:4) {
  plot(mydata, col=k[[i]]$cluster)
}

# Compare btss breackdown and visual analysis to define k
# for this example k = 3 looks interesting

# Hierarchical clustering

mydatadist <- dist(mydatascaled)
resultH <- hclust(mydatadist, "ward.D2")
plot(resultH)
rect.hclust(resultH, k=3)
# creating a vector with the generated cluster classification
resultcl <- cutree(resultH,3)
resultcl
plot(mydata, col=clresult)

# model-based clustering
# does not provide way to change the number of k
library(mclust)
resultMb <- Mclust(mydatascaled)
resultMb
plot(resultMb)

# Density-based clustering
#
library(dbscan)
kNNdistplot(mydatascaled, k=3)
abline(h=0.7, col='red', lty=2)
resultDB <- dbscan(mydatascaled, eps = 0.7 , minPts = 5)
resultDB
plot(mydata, col=resultDB$cluster)

#Comparing results with original dataset
# Confusion Matrix
#
# benchmark number -> 
# (number of items of the class with larger number of itens)/total number of items.  
# the accuracy of the new model should be better than it.
#

tab1<-table(mydata[,5],result$cluster)
tab1
#accuracy
sum(diag(tab1))/sum(tab1)

tab2<-table(mydata[,5],clresult)
tab2
#accuracy
sum(diag(tab2))/sum(tab2)

aggregate(mydatascaled, list(result$cluster), mean)

#@@@@@@@@@@@@@@@@@@@@@@@
## 1 - CLASSIFICATION ANALYSIS
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @@@@@ Multinominal logistic regression
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(nnet)
# reading dataset
# Iris.csv
mydata <- read.csv(file.choose(), sep=',', header=T)
str(mydata)
#
# Training & test
#
set.seed(1234)
Tdummy<-sample(2, nrow(mydata), replace=T, prob=c(.8,0.2))
train<-mydata[Tdummy==1,]
train$out <- relevel(train$Species, ref="Iris-virginica")
test<-mydata[Tdummy==2,]
test$out <- relevel(test$Species, ref="Iris-virginica")
# creating model
mymodel <- multinom(out~.,data = train)
#Prediction (train data)
t1<-predict(mymodel, train)
head(t1)
head(train)
# confusion matrix
tab1<-table(t1,train$out)
tab1
# accuracy
sum(diag(tab1))/sum(tab1)
#Prediction (test data)
t2<-predict(mymodel, test)
head(t2)
head(test)
#confusion matrix
tab2<-table(t2,test$out)
tab2
# accuracy
sum(diag(tab2))/sum(tab2)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @ Binomial logistic regression
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# reading dataset
# binary.csv
mydata <- read.csv(file.choose(), sep=',', header=T)
#
str(mydata)
mydata$admit<-factor(mydata$admit)
mydata$rank<-factor(mydata$rank)
table(mydata$admit, mydata$rank)
#
# Training & test
#
set.seed(1234)
Tdummy<-sample(2, nrow(mydata), replace=T, prob=c(.8,0.2))
train<-mydata[Tdummy==1,]
test<-mydata[Tdummy==2,]
## Creating the model
# 
mymodel<-glm(admit~gre+gpa+rank, data=train, family='binomial')
summary(mymodel)
# excluding not significant variables
mymodel<-glm(admit~gpa+rank, data=train, family='binomial')
summary(mymodel)
#Prediction (train data)
t1<-predict(mymodel, train, type = 'response')
head(t1)
head(train)
pred1<-ifelse(t1>0.5,1,0)
tab1<-table(Predicted=pred1, Actual=train$admit)
# accuracy
sum(diag(tab1))/sum(tab1)
#Prediction (teste data)
t2<-predict(mymodel, test, type = 'response')
head(t2)
head(test)
pred2<-ifelse(t2>0.5,1,0)
tab2<-table(Predicted=pred2, Actual=test$admit)
# accuracy
sum(diag(tab2))/sum(tab2)
# goodness of fit test
with(mymodel, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail=F))
# 
# Implement ROC/AUC
# ROC / AUC - library(ROCR)
# https://www.youtube.com/watch?v=ypO1DPEKYFo&t=349s
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @ Support Vector Machine
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Library
library(e1071)
# reading dataset
# Iris.csv
mydata <- read.csv(file.choose(), sep=',', header=T)
#
str(mydata)
plot(mydata$Petal.length, mydata$Petal.width, col=mydata$Species)
plot(mydata$Sepal.length, mydata$Sepal.width, col=mydata$Species)
## Creating the model
# without kernel info algo will find mbest approach
mymodel<-svm(Species~., data=mydata, kernel='linear')
mymodel<-svm(Species~., data=mydata, kernel='polynomial')
mymodel<-svm(Species~., data=mydata, kernel='radial')
summary(mymodel)
# plotting support vectors
# x in the graphs represents support vectors
plot(mymodel, data=mydata, Petal.width~Petal.length, slice=list(Sepal.width=3, Sepal.length=4))
#
# prediction
pred<-predict(mymodel, mydata)
# accuracy
(tab1<-table(Predicted=pred, Actual=mydata$Species))
sum(diag(tab1))/sum(tab1)
# using tune to find best model
set.seed(123)
tmodel<-tune(svm, Species~., data=mydata, range = list(epsilon = seq(0,1,0.1), cost=2^(2:7)))
plot(tmodel)
summary(tmodel)
# best model
ttmodel<-tmodel$best.model
summary(ttmodel)
plot(ttmodel, data=mydata, Petal.width~Petal.length, slice=list(Sepal.width=3, Sepal.length=4))
# prediction based on best model
pred<-predict(ttmodel, mydata)
# accuracy for best model
(tab1<-table(Predicted=pred, Actual=mydata$Species))
sum(diag(tab1))/sum(tab1)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @multiple linear regression
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Very important look to a scatter plot to confirm linearity
# vehicle.csv
mydata <- read.csv(file.choose(), sep=',', header=T)
#
str(mydata)
pairs(mydata)
#
mymodel1<-lm(mpg~cylinders+displacement+weight+acceleration,data=mydata)
summary(mymodel1)
#multiple R-squared is 0.69 (good) and p-values are ok for two variables
mymodel2<-lm(mpg~weight+acceleration,data=mydata)
summary(mymodel2)
# anova (analysis of variance)
anova(mymodel2, mymodel1)

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @Polynomial regression
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# vehicle.csv
mydata <- read.csv(file.choose(), sep=',', header=T)
#
# Very important look to a scatter plot to confirm linearity
str(mydata)
plot(mydata$mpg, mydata$displacement)
lines(lowess(mydata$displacement ~ mydata$mpg), col='green')
# first approach
# fitting x
fit1<-lm(displacement~mpg, data=mydata)
abline(fit1, col='red')
anova(fit1)
# fitting x2
x2<-mydata$mpg^2
x<-mydata$mpg
fit2<-lm(displacement~x+x2, data=mydata)
anova(fit2)
xv<-seq(min(mydata$mpg), max(mydata$mpg), 0.01)
yv<-predict(fit2, list(x=xv, x2=xv^2))
lines(xv,yv, col='blue')
# fitting x3
x3<-mydata$mpg^3
x<-mydata$mpg
fit3<-lm(displacement~x+x2+x3, data=mydata)
anova(fit3)
xv<-seq(min(mydata$mpg), max(mydata$mpg), 0.01)
yv<-predict(fit3, list(x=xv, x2=xv^2, x3=xv^3))
lines(xv,yv, col='magenta')
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# implement random forest example
# https://www.youtube.com/watch?v=dJclNIN-TPo&t=12s
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(randomForest)
library(caret)
# reading dataset
# Iris.csv
mydata <- read.csv(file.choose(), sep=',', header=T)
str(mydata)
#
# Training & test
#
set.seed(1234)
Tdummy<-sample(2, nrow(mydata), replace=T, prob=c(.8,0.2))
train<-mydata[Tdummy==1,]
train$out <- relevel(train$Species, ref="Iris-virginica")
test<-mydata[Tdummy==2,]
test$out <- relevel(test$Species, ref="Iris-virginica")
# creating model (random forest)
mymodel <- randomForest(out~.,data = train)
mymodel
mymodel$confusion
#Prediction (train data)
t1<-predict(mymodel, train)
head(t1)
head(train)
# confusion matrix
tab1<-table(t1,train$out)
tab1
# other way to create a confusion matrix
confusionMatrix(t1, train$out)
# accuracy
sum(diag(tab1))/sum(tab1)
# 
plot(mymodel)
# automaticaly tunning RF
t <- tuneRF(train[,1:4], train$out, plot = TRUE, ntreeTry = 300, trace = TRUE, improve = 0.05)
#
hist(treesize(mymodel))
#
## implement example of optimization in R
# https://www.youtube.com/watch?v=4M5wHzCzUyc
#
library(lpSolve)
# The problem
# 
# A small business sells two products named Product 1 and Product 2.
# Each ton of product 1 consumes 30 working hours and each ton of product 2 consumes 20 WH.
# The business has a maximum of 2700 working hours for the period.
# As for machine hours, each ton of product 1 and 2 consumes 5 and 10 MH respectively.
# There are 850 machine hours avaiable.
# Each ton of product 1 yield 20$ of profit while product 2 yields 60$ for each ton sold.
# For technical reasons, the firm must produce a minimum of 95 tons in total between both products.
# We need to know how many tons of product 1 and product 2 must be produced to maximize total profit.
#
# Formalizing the problem
# 
# max Z=20x1+60x2
# st. 
#    WH 30x1+20x2 <= 2700
#    MH  5x1+10x2 <=  850
#    PM   x1+  x2 >=   95
#         x1, x2  >=    0
# creating the vectors
obj.fun <- c(20, 60) # as in Z
constr  <- matrix(c(30,20,5,10,1,1), ncol=2, byrow=TRUE) #as in WH, MH, PM
constr.dir <- c("<=", "<=", ">=") # signs given for each constraints
rhs <- c(2700, 850, 95) # as were in the right hand side
# solving
prod.sol<-lp("max", obj.fun, constr, constr.dir, rhs, compute.sens=TRUE)
# solution x1= 20 and x2=75
prod.sol$solution
#
#
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @ Oversampling -> leading with inbalanced classes
# https://www.youtube.com/watch?v=Ho2Klvzjegg
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#
library(ROSE)
# dataset - binary.csv
#
# Binary.csv
mydata <- read.csv(file.choose(), sep=',', header=T)
mydata$admit <- as.factor(mydata$admit)
str(mydata)
# oversampling
summary(mydata)
table(mydata$admit) 
# there is much more not admitted that admitted. 273*2 = 546
varover <- ovun.sample(admit~., data = mydata, method = "over", N=546)$data
# checking new sample
table(varover$admit)
#
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Gradient Boosting - Algorithm to find best params
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# 
library(xgboost)
# titanictrain.csv
# reading database
dftrainreal <- read.csv(file.choose(), sep=',', header=T)
# Creating train and test datasets
set.seed(1111)
Tdummy<-sample(2, nrow(dftrainreal), replace=T, prob=c(.8,0.2))
dftrain<-dftrainreal[Tdummy==1,]
dftest<-dftrainreal[Tdummy==2,]
# Configuring the xgb.DMatrix object
dfaux <- data.matrix(dftrain[,c(7,5,13)])
dfauxL <- data.matrix(as.integer(as.character(dftrain$out)))
dfauxM <- xgb.DMatrix(data = dfaux, label = dfauxL) 
#
# Algorithm to find best params
best_param = list()
best_seednumber = 1234
best_error = Inf
best_error_index = 0
#
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
# Generating model with best params
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


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ### neural networks
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

library(neuralnet)
library(MASS)

set.seed(123)

df<-Boston

# scaling variables
maxv <- apply(df, 2, max)
minv <- apply(df, 2, min)

dfx <- as.data.frame(scale(df, center=minv, scale=maxv-minv))

#create sample - train/test
set.seed(123)
ind<-sample(1:nrow(dfx),400)
dftrain<-dfx[ind,]
dftest<-dfx[-ind,]

n <- names(dftrain)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,hidden=c(4,3),linear.output=T,data=dftrain)

plot(nn)

dfpred<- compute(nn, dftest[,1:13])

str(dfpred)

dfpred1<-dfpred$net.result*(max(df$medv)-min(df$medv))+min(df$medv)
dfactual<-dftest$medv*(max(df$medv)-min(df$medv))+min(df$medv)

mse<-sum((dfpred1-dfactual)^2)/nrow(dftest)
mse

rmse<-mse^-2
rmse

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ### neural networks - Caret
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

library(caret)
library(MASS)

df<-Boston

#scaling variables
maxv <- apply(df, 2, max)
minv <- apply(df, 2, min)

dfx <- as.data.frame(scale(df, center=minv, scale=maxv-minv))

#create sample - train/test
set.seed(123)
ind<-sample(1:nrow(dfx),400)
dftrain<-dfx[ind,]
dftest<-dfx[-ind,]

n <- names(dftrain)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
# nn <- neuralnet(f,hidden=c(4,3),linear.output=T,data=dftrain)

my.grid <- expand.grid(.decay = c(0.5, 0.3, 0.1), .size = c(2, 3, 4, 5, 6, 7, 8))
set.seed(123)
nn <- train(f, data = dftrain,
      method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)    

# summary of processing
nn
# ANN
a<-summary(nn)
a$n

dfpred<- predict(nn, dftest[,1:13])

dfpred1<-dfpred*(max(df$medv)-min(df$medv))+min(df$medv)
dfactual<-dftest$medv*(max(df$medv)-min(df$medv))+min(df$medv)

mse<-sum((dfpred1-dfactual)^2)/nrow(dftest)
mse

rmse<-mse^-2
rmse

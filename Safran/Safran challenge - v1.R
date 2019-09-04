# Safran challenge
# 05/02/2018 - Author: Marcelo Santos
#
data_folder <- paste(getwd(), '/./Safran/', sep='')
dfflights <- read.csv(paste(data_folder,"flights.csv",sep=''), sep=',', header=T)
dfweather <- read.csv(paste(data_folder,"weather.csv",sep=''), sep=',', header=T)
#
# Item #1 and #2
#
library(dplyr)
library(DAAG)
#
# Preparing dfflights and dfweather
# Analysing dataset looking for NAs
(summary(dfflights))
# Analysing data types
(str(dfflights))
# preparing dates
dfflights$date<-as.POSIXct(dfflights$date, format="%Y-%m-%d")
# removing instances without date
dfflights<-subset(dfflights,!is.na(date))
# duration > 0 and < 6h
dfflights<-subset(dfflights, duration>0 & duration<=6)
# number of passengers <= 150 but keeping NAs
dfflights<-subset(dfflights, number_passengers<=150|is.na(number_passengers))
# preliminary checking for completeness of flights dataset
boxplot(dfflights$duration, xlab="Months", ylab="Duration")
#
# Analysing dataset looking for NAs
(summary(dfweather))
# Analysing data types
(str(dfweather))
# preparing dates
dfweather$date<-as.POSIXct(dfweather$date, format="%Y-%m-%d")
# removing instances without date
dfweather<-subset(dfweather,!is.na(date))
# temperature >= -30 and <= 42
dfweather<-subset(dfweather,temperature>=-30 & temperature<=42)
# Analysing temperature - certainly, some seasonality is happening here
boxplot(dfweather$temperature~format(dfweather$date,"%m"), xlab="Months", ylab="Temperature")
boxplot(dfweather$temperature~format(dfweather$date,"%d"), xlab="Days", ylab="Temperature")
# Analysing windSpeed
dfweather<-subset(dfweather, windSpeed<=100)
boxplot(dfweather$windSpeed~format(dfweather$date,"%m"), xlab="Months", ylab="WindSpeed")
# Analysing precipitation
boxplot(dfweather$precipitation~format(dfweather$date,"%m"), xlab="Months", ylab="Precipitation")

# Combining datasets
dfflights_new<-left_join(dfweather, dfflights, by = c("date"="date"))

# Checking completeness and coherence of data by visual inspection.
(summary(dfflights_new))
a<-group_by(dfflights_new, id)%>%
  summarize(count=n()) %>% filter(count>1)

# Item #3
# Assuming number_passengers is the dependent variable and other variables as independent variables.
# My idea is starting creating scatter plottings with different variable and see possible linearity.
# Run a preliminary linear model and verify coefficients and p-values to exclude variables that are not significant.
# Complement this analysis with a correlation matrix to understand how the different variables correlates to the dependent variable and analyse collinearity aspects.
# 
#
# creating Training & Test datasets
#
set.seed(1111)
dfflights_reduNA<-subset(dfflights_new, !is.na(number_passengers))
dfflights_onlyNA<-subset(dfflights_new, is.na(number_passengers))
Tdummy<-sample(2, nrow(dfflights_reduNA), replace=T, prob=c(.8,0.2))
train<-dfflights_reduNA[Tdummy==1,]
test<-dfflights_reduNA[Tdummy==2,]
# Analyzing the correlation matrix...
# duration and temperature have high correlation with number_passengers and low correlation among both.
cmaux<-matrix(c(train$duration, train$temperature, train$windSpeed, train$precipitation, train$number_passengers),nrow=(nrow(train)), ncol=5)
cor(cmaux)

#### FIRST APPROACH
# Creating a model with these two independent variables
modelAll <- lm(number_passengers~duration+temperature, data=train)
summary(modelAll)
rSquaredAll<-summary(modelAll)$r.squared
# According to R-squared these variables explain 95.12% of the number_passengers variance
# predict values for test
resultsAll<-predict(modelAll, newdata=test, type='response')
rmseAll<-mean((resultsAll-test$number_passengers)^2)^(1/2)
# Cross validation - 10 k-folds
kfoldVal<-suppressWarnings(CVlm(data=train, form.lm=number_passengers~duration+temperature, m=10, seed=1, legend.pos="topleft",  printit=FALSE, main="10 folds validation - number_passengers"))
msekfAll<-attr(kfoldVal, 'ms')
# mean squared error -> 22.159149
#####

#### SECOND APPROACH
plot(number_passengers~duration, data=train)
abline(v=1.5, col='red', lwd=3, lty=2)
plot(number_passengers~temperature, data=train)

# Scatter graphs are showing an interesting pattern for number_passengers related to duration and temperature
# Duration is splitting the dataset in two well defined classes: duration<1.5h and duration>=1.5
# Based on this observation, could be interesting creating models for these two different classes.

trainSmaller1.5<-subset(train,duration<(1.5))
testSmaller1.5<-subset(test,duration<(1.5))
plot(number_passengers~temperature, data=trainSmaller1.5)
lines(lowess(trainSmaller1.5$number_passengers~trainSmaller1.5$duration+trainSmaller1.5$temperature), col='green')
modelSmaller1.5 <- lm(number_passengers~duration+temperature, data=trainSmaller1.5)
summary(modelSmaller1.5)
# Predicting values for test dataset
resultsSmaller1.5<-predict(modelSmaller1.5, newdata=testSmaller1.5, type='response')
resultsSmaller1.5<-ifelse(resultsSmaller1.5>100, 100, resultsSmaller1.5) # limiting to 100 passengers
rmseSmaller1.5<-mean((resultsSmaller1.5-testSmaller1.5$number_passengers)^2)^(1/2)
# Cross validation - 10 k-folds
kfoldValSmaller<-suppressWarnings(CVlm(data=trainSmaller1.5, form.lm=number_passengers~duration+temperature, m=10, dots=FALSE, seed=1, legend.pos="topleft",  printit=FALSE, main="10 folds validation - number_passengers (class: duration<1.5h)"))
msekfSmaller1.5<-attr(kfoldValSmaller, 'ms')

trainLarger1.5<-subset(train,duration>=(1.5))
testLarger1.5<-subset(test,duration>=(1.5))
plot(number_passengers~temperature, data=trainLarger1.5)
lines(lowess(trainLarger1.5$number_passengers~trainLarger1.5$duration+trainLarger1.5$temperature), col='green')
modelLarger1.5 <- lm(number_passengers~temperature, data=trainLarger1.5)
abline(modelLarger1.5, col='red')
summary(modelLarger1.5)
# Predicting values for test dataset
resultsLarger1.5<-predict(modelLarger1.5, newdata=testLarger1.5, type='response')
resultsLarger1.5<-ifelse(resultsLarger1.5>50, 50, resultsLarger1.5) # limiting to 100 passengers
rmseLarger1.5<-mean((resultsLarger1.5-testLarger1.5$number_passengers)^2)^(1/2)
# Cross validation - 10 k-folds
kfoldValLarger<-suppressWarnings(CVlm(data=trainLarger1.5, form.lm=number_passengers~temperature, m=10, dots=FALSE, seed=1, legend.pos="topleft",  printit=FALSE, main="10 folds validation - number_passengers (class: duration>=1.5h)"))
msekfLarger1.5<-attr(kfoldValLarger, 'ms')

# Calculating total rmse based Smaller and Larger datasets
resultsT<-c(resultsSmaller1.5,resultsLarger1.5)
testSmallerT<-c(testSmaller1.5$number_passengers,testLarger1.5$number_passengers)
rmseT<-mean((resultsT-c(testSmaller1.5$number_passengers,testLarger1.5$number_passengers))^2)^(1/2)
rSquaredT<- sum((resultsT-mean(resultsT))^2)/sum((testSmallerT-mean(testSmallerT))^2)

# Comparing error measures for both approaches
rmseAll
rmseT

rmseSmaller1.5
rmseLarger1.5

msekfAll
msekfSmaller1.5
msekfLarger1.5

rSquaredAll
rSquaredT

# CONCLUSION: R-Squared was not conclusive for comparing the two approaches. 
# However, models created for flights with duration >= 1.5 performed much better than others in terms of rmse. 
# 
# Feeding dfflights dataset with predicted values based on the second approach.
# Generating results for two classes: Smaller1.5 and Larger1.5

dfflights_onlyNA_Smaller1.5<-subset(dfflights_onlyNA,duration< (1.5))
dfflights_onlyNA_Larger1.5 <-subset(dfflights_onlyNA,duration>=(1.5))
dfflights_onlyNA_Smaller1.5[,6]<-predict(modelSmaller1.5, newdata=dfflights_onlyNA_Smaller1.5, type='response')
dfflights_onlyNA_Smaller1.5[,6]<-ifelse(dfflights_onlyNA_Smaller1.5$number_passengers>100, 100, dfflights_onlyNA_Smaller1.5$number_passengers) # limiting to 100 passengers
dfflights_onlyNA_Larger1.5 [,6]<-predict(modelLarger1.5, newdata=dfflights_onlyNA_Larger1.5, type='response')
dfflights_onlyNA_Larger1.5[,6] <-ifelse(dfflights_onlyNA_Larger1.5$number_passengers>50, 50, dfflights_onlyNA_Larger1.5$number_passengers) # limiting to 50 passengers

dfflights_final<-rbind(dfflights_onlyNA_Smaller1.5, dfflights_onlyNA_Larger1.5, dfflights_reduNA)
dfflights_final[,6]<-round(dfflights_final$number_passengers)

# ITEM #4
# As mentioned, there are two well defined classes for this study:
#   - Class 1 - flights with duration<1.5h until 100 passengers
#   - Class 2 - flights with duration>=1.5  until 50 passengers
plot(number_passengers~duration, data=dfflights_final)
# For flights of Class 1, mean of duration is 1h, max 1.39h, and mean of passengers number around 81.
dfflights_final_Smaller1.5<-subset(dfflights_final, duration<1.5)
summary(dfflights_final_Smaller1.5)
# For flights of Class 2, mean of duration is 2h, max 2.45h, and mean of passengers number around 40.
dfflights_final_Larger1.5<-subset(dfflights_final, duration>=1.5)
summary(dfflights_final_Larger1.5)
# aggregating datasets by date counting number of records
dfflights_final_Smaller1.5_aggr<-group_by(dfflights_final_Smaller1.5, date)%>%
  summarize(count=n())
dfflights_final_Larger1.5_aggr<-group_by(dfflights_final_Larger1.5, date)%>%
  summarize(count=n())

# There is a linear increasing for the daily number of flights for both classes.
# The slopes show that the increasing rate is similiar for both.
#
# plotting flights with duration smaller than 1.5h
# 
plot(dfflights_final_Smaller1.5_aggr)
a<-lm(count~date,data=dfflights_final_Smaller1.5_aggr)
abline(a, col='red')
summary(a)
# until 2020
newdates<-data.frame(date=seq.POSIXt(from=ISOdate(2016,01,01), to=ISOdate(2020,12,31), by="day"))
aa<-predict(a, newdata=newdates)
tail(aa)

# plotting flights with duration bigger than 1.5h
plot(dfflights_final_Larger1.5_aggr)
b<-lm(count~.,data=dfflights_final_Larger1.5_aggr)
abline(b, col='red')
summary(b)
# until 2020
bb<-predict(b, newdata=newdates)
tail(bb)

# Histogram for 2015 number of flights by day
a<-subset(dfflights_final_Smaller1.5_aggr, date>'2015-01-01')
hist(a$count, main='Frequency of flights with duration <1.5h (2015)')

#12 trips per day >> 6 round trip by day >> 2 aircarfts 
#   >> ~3.5hours per round trip

a<-subset(dfflights_final_Larger1.5_aggr, date>'2015-01-01')
hist(a$count, main='Frequency of flights with duration >=1.5h (2015)')

# 11 trips per day >> 6 round trip by day >> 2 aircarfts >> 3 round trip per aircraft per day
#   >> ~3.5hours per round trip

# CONCLUSION: 
# Suggested fleet:
#    - 2 aircrafts type A (capacity: 100 passengers, autonomy:1.5h)
#           until 13 trips per day (until 2020) >> 7 round trip by day >> 2 aircarfts >> 3 round trip per aircraft per day
#           ~3.5hours per round trip
#           ~4000 flights per year
#    - 2 aircrafts type B (capacity 50 passengers, autonomy: 2.5h)
#           until 13 trips per day (until 2020) >> 7 round trip by day >> 2 aircarfts >> 3 round trip per aircraft per day
#           ~3.5hours per round trip
#           ~3700 flights per year
#
# Certainly, other data could be used for calculating this estimative.
#    >>  (e.g. scheduled maintenances, maximmum trips per day per aircraft,
#               costs per trip per aircraft, revenue per trip per aircraft,
#               destinations, demand per destinations, etc.)
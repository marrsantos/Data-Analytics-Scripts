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

# combining datasets
dfflights_new<-left_join(dfweather, dfflights, by = c("date"="date"))

# Checking completeness and coherence of data by visual inspection.
(summary(dfflights_new))
a<-group_by(dfflights_new, id)%>%
  summarize(count=n()) %>% filter(count>1)

# Item #3
# Assuming number_passengers is the dependent variable and other variables as independent variables.
# My ideas is starting creating scatter plottings with different variable and see possible linearity.
# Run a preliminary linear model and verify coefficients and p-values to exclude variables that are not significant.
# Complement this analysis with a correlation matrix to understand how the different variables correlates to the dependent variable and analyse collinearity aspects.
# Certainly, temperature would include some seasonality aspects that need to be considered for this model. Also, I would need to understand this effect also to other weather variables.
# Model creation: I would try an autoregressive model (ARMA) with dummies variables at least for temperature/month and other relevant variables as external variables (xreg).

#starting a preliminary linear model analysis...
mymodel <- lm(number_passengers~., data=dfflights_new)

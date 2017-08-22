# UNIVARIATE PREDICTIVE MODELING

# Data sourced from https://datamarket.com/data/set/22or/monthly-bituminous-coal-production-in-usa-1952-1959#!ds=22or&display=line

# install.packages('dplyr')
# install.packages('data.table')
# install.packages('tseries')
# install.packages('forecast')
library(stats)
library(ggplot2)
library(dplyr)
require(graphics)
require(tseries)
library(forecast)
library(tseries)
library(dplyr)

data <- read.csv(file="monthly-bituminous-coal-producti.csv", header=TRUE, sep=",")

# DATA CLEANING ##################################################################################

# remove NAs
data <- na.omit(data)

# rename column
names(data)[names(data) == "Monthly.bituminous.coal.production.in.U.S.A..1952.1959"] = "Coal"

# make column of months easy to convert to Date format
data$Month <- as.character(data$Month)
data$Month <- as.Date(paste(data$Month,"-01",sep=""))

# convert Coal column to numeric
data$Coal <- as.numeric(data$Coal)

# CREATE TRAINING/TEST SETS ############################################################################
train <- data[data$Month < "1959-02-01",]
test <- data[data$Month >= "1959-02-01",]

# GENERATE PREDICTIONS ############################################################################

Date=character()
Actual=numeric()
HW_Simple=numeric()
HW_Simple_Error=numeric()
ARIMA=numeric()
ARIMA_Error=numeric()
ETS_STL=numeric()
ETS_STL_Error=numeric()

count <- 1
months <- c("February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
#months <- c("February", "March", "April")

# loop to generate 1959 values
for (month in months){ 
  m <- months[count]
  print(m)
  # create train series
  train_series <- ts(data$Coal, start=c(1952, 1), end=c(1959, count), frequency=12)
  train_series
  
  # Holt-Winters simple exponential - models level
  fit1 <- HoltWinters(train_series, beta=FALSE, gamma=FALSE)
  forecast1 <- as.data.frame(forecast(fit1, 1))
  pred1 <- forecast1[1,1]
  
  # ARIMA
  fit2 <- auto.arima(train_series)
  forecast2 <- as.data.frame(forecast(fit2, 1))
  pred2 <- forecast2[1,1]
  
  # ETS/STL
  #fit3 <- HoltWinters(train_series)
  forecast3 <- as.data.frame(forecast(train_series, 1))
  pred3 <- forecast3[1,1]
  
  Date[count] <- paste(m, "1959", sep=" ")
  Actual[count] <- test[count, "Coal"]
  HW_Simple[count] <- pred1
  HW_Simple_Error[count] <- (pred1 - test[count, "Coal"])/pred1*100
  ARIMA[count] <- pred2
  ARIMA_Error[count] <- (pred2 - test[count, "Coal"])/pred2*100
  ETS_STL[count] <- pred3
  ETS_STL_Error[count] <- (pred3 - test[count, "Coal"])/pred3*100
 
  #date_val <- as.character(list(paste(month, "1959", sep=" ")))
  #df <- rbind(df, list(paste(m, "1959", sep=" "),test[count, "Coal"], pred1, pred2, pred3))
  count <- count + 1
}

df <- data.frame(Date, Actual, HW_Simple, HW_Simple_Error, ARIMA, ARIMA_Error, ETS_STL, ETS_STL_Error)

# PLOT PREDICTIONS ############################################################################

par(mfrow=c(2,2))
test_series <- ts(test$Coal, start=c(1959, 2), end=c(1959, 12), frequency=12)
hw_ts <- ts(df$HW_Simple, start=c(1959, 2), end=c(1959, 12), frequency=12)
arima_ts <- ts(df$ARIMA, start=c(1959, 2), end=c(1959, 12), frequency=12)
ets_ts <- ts(df$ETS_STL, start=c(1959, 2), end=c(1959, 12), frequency=12)

ts.plot(test_series, hw_ts, gpars = list(col = c("black", "red")))
title("Simple Holt-Winters Prediction")

ts.plot(test_series, arima_ts, gpars = list(col = c("black", "red")))
title("ARIMA Prediction")

ts.plot(test_series, ets_ts, gpars = list(col = c("black", "red")))
title("ETS Prediction")


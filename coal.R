# ARIMA MODELING

# Indicate which website data was sourced from 

# install.packages('dplyr')
# install.packages('data.table')
# install.packages('tseries')
# install.packages('forecast')
# install.packages('zoo')
library(stats)
library(ggplot2)
library(dplyr)
require(graphics)
require(tseries)
library(forecast)
library(zoo)
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
train <- data[data$Month < "1959-01-01",]
test <- data[data$Month >= "1959-01-01",]

# train_series <- ts(train$Coal, start=c(1952, 1), end=c(1958, 12), frequency=12)
# test_series <- ts(test$Coal, start=c(1959, 1), end=c(1959, 12), frequency=12)

# # Seasonal decomposition
# fit <- stl(train_series, s.window="period")
# plot(fit)

# # additional plots
# monthplot(train_series)
# seasonplot(train_series)

# CREATE FITS BASED ON:
# HOLTWINTERS
# ARIMA

# # simple exponential - models level
# fit <- HoltWinters(train_series, beta=FALSE, gamma=FALSE)
# # double exponential - models level and trend
# fit <- HoltWinters(train_series, gamma=FALSE)
# # triple exponential - models level, trend, and seasonal components
# fit <- HoltWinters(train_series)
# 
# # predictive accuracy
# library(forecast)
# accuracy(fit)
# 
# # predict next three future values
# library(forecast)
# forecast(fit, 3)
# plot(forecast(fit, 3))

# cols: date, actual 1959, prediction 1, pred 2, pred 3
#df <- data.frame(matrix(ncol = 5, nrow = 0))
#x <- c("Date", "Actual", "HW Simple", "HW Double", "HW Triple")


# df <- data.frame(Date=character(),
#                  Actual=numeric(), 
#                  HW_Simple=numeric(), 
#                  HW_Double=numeric(),
#                  HW_Triple=numeric(),
#                  stringsAsFactors = FALSE) 

Date=character(3)
Actual=numeric(3)
HW_Simple=numeric(3)
ARIMA=numeric(3)
ETS_STL=numeric(3)

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
  pred2 <- forecast1[1,1]
  
  # ETS/STL
  #fit3 <- HoltWinters(train_series)
  forecast3 <- as.data.frame(forecast(train_series, 1))
  pred3 <- forecast1[1,1]
  
  Date[count] <- paste(m, "1959", sep=" ")
  Actual[count] <- test[count+1, "Coal"]
  HW_Simple[count] <- pred1
  ARIMA[count] <- pred2
  ETS_STL[count] <- pred3
 
  #date_val <- as.character(list(paste(month, "1959", sep=" ")))
  #df <- rbind(df, list(paste(m, "1959", sep=" "),test[count, "Coal"], pred1, pred2, pred3))
  count <- count + 1
}

df <- data.frame(Date, Actual, HW_Simple, ARIMA, ETS_STL)

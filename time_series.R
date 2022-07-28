############################################
#This file loads order demand data per day
#We will use this file to forecast the next
#two years of sales per day
#
#Created by Sebastian
############################################


#Load all the libraries necessary for the analysis
library(ggplot2)
library(ggfortify)
library(fpp2)
require(tidyverse)
library(dplyr)
library(lubridate)

#Clear all variables in the workspace
rm(list = ls())

#Load the data
data <- read.csv("/Users/sebastianpenning/Public/SP/Codam/Piscine/Nike/project_2_r/product_demand.csv")

#Clean data and pick a certain category of products
df <- data %>% 
  select(Product_Category, Date, Order_Demand) %>%
  #filter(Product_Category == "Category_028") %>%
  filter(Order_Demand >= "1") %>%
  filter(!is.na(as.numeric(Order_Demand))) %>%
  select(Date, Order_Demand) %>%
  na.omit()

#Date column become date type
df$Date <- as.Date(df$Date, format = "%Y/%m/%d")

#dates are sorted on date
df <- df[order(df$Date), ]

#Order_demand becomes numeric
df$Order_Demand <- as.numeric(df$Order_Demand)

#Prepare data to be grouped by month and year
df_new <-df
df_new$year_month <- floor_date(df_new$Date, "month")

#Group and summarize data into monthly data
df_aggr <- df_new %>%
  group_by(year_month) %>%
  dplyr::summarize(Order_Demand = sum(Order_Demand)) %>%
  as.data.frame()

#Filter years with lower values (2011, 2017)
df_aggr <- subset(df_aggr, Order_Demand > 7762236)

#Time series object created
df_ts <- ts(df_aggr[,2], start=c(2012, 1), frequency = 12)

######################
# Preliminary analysis
######################

#Plot time series for first impression
autoplot(df_ts) + 
  ggtitle("time plot: order demand per month") +
  ylab("demand")

#Not sure if data has trend

#First diff
dy <- diff(df_ts)

#Plot of difference data
autoplot(dy) + 
  ggtitle("time plot: change in order demand per month") +
  ylab("demand")

#Series made stationary, used to investigate seasonality 
ggseasonplot(dy) + ggtitle ("Seasonal Plot: Change in demand") + 
  ylab("demand")

#Subseries plot
ggsubseriesplot(dy)

###################################################
# Series has small upward trend and downward trend
# Trend removed with first difference
# First difference series has some degree of seasonality
# Difference methods to forecast
##################################################

###########
#Benchmark
#Seasonsal naive
###########
fit <- snaive(dy) #Residual SD = 10396974 bad fit for regression line 
print(summary(fit))
checkresiduals(fit)

#########
# fit ETS
#########
fit_ets <- ets(df_ts) #Residual SD = 0.0932 Good fit for regression line
print(summary(fit_ets))
checkresiduals(fit_ets)

#########
#Fit Arima
#########
fit_arime <- auto.arima(df_ts, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arime)) #Residual SD = 7461233 bad fit for regression line
checkresiduals(fit_arime)

####################
# Forecast with ETS
#####################
fcst <- forecast(fit_arime, h = 24)
autoplot(fcst, include = 48)
print(summary(fcst))

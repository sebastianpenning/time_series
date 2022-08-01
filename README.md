# time_series
A project using R in order to create a forecast with data regarding sales 

## Getting started

The minimal setup you need in order to further develop this is to have R, and an IDE such as RStudio. 

## Project  

### Preparing data

First the neccessary libraries are loaded in for the project.

```r
library(ggplot2)
library(ggfortify)
library(fpp2)
require(tidyverse)
library(dplyr)
library(lubridate)
```

Together with the data that is supposed to be loaded in.

```r
data <- read.csv("Path/to/csv")
```

Afterwards, the data is cleaned.

```r
df <- data %>% 
  select(Product_Category, Date, Order_Demand) %>%
  filter(Order_Demand >= "1") %>%
  filter(!is.na(as.numeric(Order_Demand))) %>%
  select(Date, Order_Demand) %>%
  na.omit()
 ```
 
The date column is then transformed into a date data type, and is sorted by date.
 
```r
df <- df[order(df$Date), ]
df$Order_Demand <- as.numeric(df$Order_Demand)
```

Following this, the data is then grouped and summarized into monthly data.

```r
df_new <-df
df_new$year_month <- floor_date(df_new$Date, "month")

df_aggr <- df_new %>%
  group_by(year_month) %>%
  dplyr::summarize(Order_Demand = sum(Order_Demand)) %>%
  as.data.frame()
```

The years with very little data is the filtered out.

```r
df_aggr <- subset(df_aggr, Order_Demand > 7762236)
```

And a time series object is created.

```r
df_ts <- ts(df_aggr[,2], start=c(2012, 1), frequency = 12)
```

### Preliminary Analysis

A plot is made to get a first impression

```r
autoplot(df_ts) + 
  ggtitle("time plot: order demand per month") +
  ylab("demand")
```

![r_1](https://user-images.githubusercontent.com/88779306/182176574-ab8a6ff6-0a79-4a02-8049-733abf2f2ec8.jpeg)

It is not really sure if the data has a trend. Nonetheless, we still do a difference. 

```r
dy <- diff(df_ts)

autoplot(dy) + 
  ggtitle("time plot: change in order demand per month") +
  ylab("demand")
```

![r_2](https://user-images.githubusercontent.com/88779306/182181227-f9af1665-d4fc-4abe-8539-47040b6e3700.jpeg)

The seasonality is then checked

```r
ggseasonplot(dy) + ggtitle ("Seasonal Plot: Change in demand") + 
  ylab("demand")
```
![r_3](https://user-images.githubusercontent.com/88779306/182181585-d647a91c-dde2-489e-abcf-f4e79ea1b5d0.jpeg)

```r
ggsubseriesplot(dy)
```

![r_4](https://user-images.githubusercontent.com/88779306/182181747-1459268a-db7d-4e94-95ed-ed8a409593d4.jpeg)

What can be concluded is that the series has small upward trend and downward trend, we removed this with with the first difference. Furthemore, the first difference series has some degree of seasonality. Moving on, we are going to try different forecast models to see which one fits the best. 

First, the seasonal naive model

```r
fit <- snaive(dy)  
print(summary(fit))
checkresiduals(fit)
```
![r_5](https://user-images.githubusercontent.com/88779306/182182502-97332cfc-f2fe-4720-ad6a-ced07ac731fd.jpeg)
![r_6](https://user-images.githubusercontent.com/88779306/182182560-c91d3096-0dde-45ab-890c-f9104f5bf4c3.png)

Even though the ACF looks pretty good, we can see that in the second picture the Residual SD is very high, which is not a good sign. 

Next up is the ETS. 

```r
fit_ets <- ets(df_ts) 
print(summary(fit_ets))
checkresiduals(fit_ets)
```
![r_7](https://user-images.githubusercontent.com/88779306/182183127-893420b7-3aa3-4bb9-8003-6dea0bcddc0f.jpeg)
![r_8](https://user-images.githubusercontent.com/88779306/182183363-ec0ca99e-2202-4696-929c-0525e76c54d7.png)

Again, the ACF is looking good, and this time the Residual SD (signified by the sigma) is also pretty good. 

Finally, we test the Arima model. 

```r
fit_arime <- auto.arima(df_ts, d=1, D=1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arime)) 
checkresiduals(fit_arime)
```
![r_10](https://user-images.githubusercontent.com/88779306/182189119-d56a66a3-638a-4e34-ad91-16187c81faa5.jpeg)
![r_11](https://user-images.githubusercontent.com/88779306/182189140-85e4f419-8bac-462e-9809-c22d29b66a87.png)

We can see that here the ACF looks very good. Nonetheless, the Residual SD is very high with the Arima model. However, the results of the Ljung-Box test show a p value over 0.05. Therefore, we go ahead with this model 

```r
fcst <- forecast(fit_arime, h = 24)
autoplot(fcst, include = 48)
print(summary(fcst))
```
![r_12](https://user-images.githubusercontent.com/88779306/182189656-8586d6e8-ec49-4904-96a0-bc645bb9b2d4.jpeg)

This is the final result of the forecast.









 


## Data

The data for this project came from a kaggle dataset which can be found through this link: https://www.kaggle.com/datasets/felixzhao/productdemandforecasting


## Licensing

The license used for this project is the MIT license which can be found in the files of this repository

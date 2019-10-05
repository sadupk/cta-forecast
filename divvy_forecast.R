library(data.table)
library(dplyr)
library(forecast)
library(lubridate)

rm(list = ls())
setwd("Documents/IE594 TS/Final Project/")
data_list = list.files("Data", full.names = TRUE)
trip_list = data_list[grepl("Trips", data_list)]

q1_2017 = fread(trip_list[1])
q1_2017$start_day = sub(" .+", "", q1_2017$start_time)
q1_2017$start_day = as.Date(q1_2017$start_day, format = '%m/%d/%Y')

daily_rides = count(x = q1_2017, start_day)

msts = msts(daily_rides$n, seasonal.periods = c(7, 30.4), start=decimal_date(daily_rides$start_day[1]))
plot(msts)

tbats_training = tbats(msts)
plot(tbats_training, main='Multiple Season Decomposition')

tbats_training %>% 
  forecast(h=10) %>%
  autoplot()

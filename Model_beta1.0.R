library(forecast)
library(lubridate)
library(data.table)

dt = fread("Data/CTA_data.csv")
dt_daily = dt[,c("service_date", "total_rides")]
dt_daily = unique(dt_daily)
dt_daily$service_date = as.Date(dt_daily$service_date, format = "%m/%d/%Y")
dt_daily$month = strftime(dt_daily$service_date, "%m")
dt_daily$year = strftime(dt_daily$service_date, "%Y")
dt_monthly = aggregate(total_rides ~ month + year, dt_daily, sum)

# Scale data to 100,000
dt_monthly$total_rides = dt_monthly$total_rides/100000

plot(dt_monthly$total_rides)

aggregate(total_rides ~ service_date, dt_daily, length)

msts = msts(dt_monthly$total_rides, seasonal.periods = c(6,12), start=decimal_date(dt_daily$service_date[1]))
plot(msts)

# Split into train and test datasets
training = subset(msts, end = length(msts)-12)
test = subset(msts, start = length(msts)-11)

x = seq(1, length(dt_monthly$total_rides))
fit_poly = lm(dt_monthly$total_rides ~ poly(x, 1))
A1 = sum(fit_poly$residuals^2)
print(paste0("RSS: ", A1))
for (i in 2:5){
  fit_poly = lm(dt_monthly$total_rides ~ poly(x, i))
  A0 = sum(fit_poly$residuals^2)
  n_r = length(x) - length(fit_poly$coefficients)
  F0 = (A1 - A0)/A0/n_r
  F_dist = 1 - pf(F0, 1, n_r)
  print(paste0("Comparing polynomials with degrees ", i-1, " and ", i))
  print(paste0("RSS: " , A0))
  print(paste0("F0: ", F0))
  print(paste0("Significance: ", F_dist))
  A1=A0
}
fit_poly = lm(dt_monthly$total_rides ~ poly(x, 2))
rss = sum(fit_poly$residuals^2)
plot(x, dt_monthly$total_rides,'b-')
lines(x, fitted(fit_poly))


# Method 1: TBATS
# Run TBATS model
tbats_training = tbats(training)
plot(tbats_training, main='Multiple Season Decomposition')

# Forecast test model
tbats_training %>% 
  forecast(h=12, level=50) %>%
  autoplot() + autolayer(test)

fc_tbats <- forecast(tbats_training,h=5)
plot(fc_tbats, main = "TBATS Forecast")

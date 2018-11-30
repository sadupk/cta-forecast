library(forecast)
library(lubridate)
library(data.table)
library(tseries)

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

x = seq(1, length(training))
fit_poly = lm(training ~ poly(x, 1))
A1 = sum(fit_poly$residuals^2)
print(paste0("RSS: ", A1))
for (i in 2:5){
  fit_poly = lm(training ~ poly(x, i))
  A0 = sum(fit_poly$residuals^2)
  n_r = length(x) - length(fit_poly$coefficients)
  F0 = (A1 - A0)/(A0/n_r)
  F_dist = 1- pf(F0, 1, n_r)
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

# Choose polynomial with degree 3 as best fit
fit_poly = lm(training ~ poly(x, 3))

dt_notrend = fit_poly$residuals
dt_ts = ts(dt_notrend, frequency = 12)

  
f12 = fourier(ts(dt_notrend, frequency = 12), K=1)
f6 = fourier(ts(dt_notrend, frequency = 6), K=3)
dt_notrend_noseason = lm(dt_notrend ~ f12 + f6)
adf.test(dt_notrend_noseason$residuals, alternative = "stationary")
Box.test(dt_notrend_noseason$residuals, lag = 20, type = 'Ljung-Box')


f12 = fourier(ts(dt_notrend, frequency = 12), K=1)
fit_poly = lm(dt_notrend ~ 0+f12)
A1 = sum(fit_poly$residuals^2)
print(paste0("RSS: ", A1))

for (j in 2:6){
  f12 = fourier(ts(dt_notrend, frequency = 12), K=j)
  fit_poly = lm(dt_notrend ~ 0 + f12)  
  A0 = sum(fit_poly$residuals^2)
  n_r = length(x) - length(fit_poly$coefficients)
  F0 = ((A1 - A0)/2)/(A0/n_r)
  F_dist = 1- pf(F0, 2, n_r)
  print(paste0("Comparing polynomials with degrees ", 2*(j-1), " and ", 2*j))
  print(paste0("F12 degree: ", j))
  print(paste0("RSS: " , A0))
  print(paste0("Significance: ", F_dist))
  A1=A0
}

sin_cos = cbind(sin(2*pi*x*7/12),cos(2*pi*x*7/12))
fit_poly = lm(dt_notrend~f12+sin_cos)
A0 = sum(fit_poly$residuals^2)
n_r = length(x) - length(fit_poly$coefficients)
F0 = ((A1 - A0)/2)/(A0/n_r)
F_dist = 1- pf(F0, 2, n_r)
print(paste0("RSS: " , A0))
print(paste0("Significance: ", F_dist))


f_test = function(x){
  print('hello')
}

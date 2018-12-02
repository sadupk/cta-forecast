library(forecast)
library(lubridate)
library(data.table)
library(readxl)
library(tseries)
library(stats)
library(nlme)
fp =("C:/Time Series/Project/CTA_data.csv")
#x1=read.csv(fp)
dt = fread(fp)
dt_daily = dt[,c("service_date", "total_rides")]
dt_daily = unique(dt_daily)
dt_daily$service_date = as.Date(dt_daily$service_date, format = "%m/%d/%Y")

#Pedro tell me more about this aggregation - AB
dt_daily$month = strftime(dt_daily$service_date, "%m")
dt_daily$year = strftime(dt_daily$service_date, "%Y")
dt_monthly = aggregate(total_rides ~ month + year, dt_daily, sum)

# Scale data to 100,000
dt_monthly$total_rides = dt_monthly$total_rides/100000

lines(dt_monthly$total_rides)

aggregate(total_rides ~ service_date, dt_daily, length)

#http://www.statosphere.com.au/check-time-series-stationary-r/
#for kpss test pvalue <0.05 means data is not stationary
#for adf test pvalue <0.05 means data is statioary
kpss.test(dt_monthly$total_rides)
adf.test(dt_monthly$total_rides)

#kpss test shows data is not stationary and adf has p value close to 0.05 thus it confirms our data is not stationary

#performing acf and pacf to check if data is correlated
acf(dt_monthly$total_rides,lag.max = 48)
pacf(dt_monthly$total_rides)

msts = msts(dt_monthly$total_rides, seasonal.periods = c(6,12), start=decimal_date(dt_daily$service_date[1]))
plot(msts)

# Split into train and test datasets
training = subset(msts, end = length(msts)-12)
test = subset(msts, start = length(msts)-11)

#don't you think there should be two subsets of length of training and testing set
x = seq(1, length(training))
fit_poly = lm(training ~ poly(x, 1))
A1 = sum(fit_poly$residuals^2)
print(paste0("RSS: ", A1))
for (i in 2:8){
  fit_poly = lm(training ~ poly(x, i))
  A0 = sum(fit_poly$residuals^2)
  n_r = length(x) - length(fit_poly$coefficients)
  F0 = (A1 - A0)/(A0/n_r)
  F_dist =1-pf(F0, 1, n_r)
  print(paste0("Comparing polynomials with degrees ", i-1, " and ", i))
  print(paste0("RSS: " , A0))
  print(paste0("F0: ", F0))
  print(paste0("Significance: ", F_dist))
  A1=A0
}

#optimal order is 3 for the polynomial deterministic trend
fit_poly = lm(training ~ poly(x, 3))
rss = sum(fit_poly$residuals^2)
print(rss)
plot(x, training,'b-')
lines(x, fitted(fit_poly),col='red')

#detrended  series 
#training_dtrend = fit_poly$residuals
#plot(x,training_dtrend,'b-')
#lines(x, training_dtrend)
#performing stationarity tests
#kpss.test(training_dtrend)

#fitting sine cosine terms
#sin_cos1 = cbind(sin(2*pi*x/12), cos(2*pi*x/12))
#sin_cos2 = cbind(sin(2*pi*x/12), cos(2*pi*x/12), sin(2*pi*x/6), cos(2*pi*x/6))
#sin_cos3 =  cbind(sin(2*pi*x/12), cos(2*pi*x/12), sin(2*pi*x/6), cos(2*pi*x/6),sin(2*pi*x/4), cos(2*pi*x/4))
#sin_cos4 =  cbind(sin(2*pi*x/12), cos(2*pi*x/12), sin(2*pi*x/6), cos(2*pi*x/6),sin(2*pi*x/4), cos(2*pi*x/4),sin(2*pi*x/3), cos(2*pi*x/3))
#fit_poly_dtrend = gls(training_dtrend ~ sin_cos1)
#summary(fit_poly_dtrend)
#A0sin_cos1 = 
#plot(x,fit_poly_dtrend$residuals,'b-')
#plot(x,training_dtrend,'b-')

dt_notrend = fit_poly$residuals
dt_ts = ts(dt_notrend, frequency = 12)

tbats_notrend = tbats(dt_notrend)
plot(tbats_notrend, "Multiple Season Decomposition")

f12 = fourier(ts(dt_notrend, frequency = 12), K=1)
#f6 = fourier(ts(dt_notrend, frequency = 6), K=3)
#dt_notrend_noseason = lm(dt_notrend ~ f12 + f6)
adf.test(dt_notrend, alternative = "stationary")
kpss.test(dt_notrend,"Trend")

f12 = fourier(ts(dt_notrend, frequency = 12), K=1)
fit_poly = lm(dt_notrend ~ 0+f12)
A1 = sum(fit_poly$residuals^2)
summary(fit_poly)
print(paste0("RSS: ", A1))

for (j in 2:6){
  f12 = fourier(ts(dt_notrend, frequency = 12), K=j)
  fit_poly = lm(dt_notrend ~ 0 + f12)  
  print(summary(fit_poly))
  A0 = sum(fit_poly$residuals^2)
  n_r = length(x) - length(fit_poly$coefficients)
  F0 = ((A1 - A0)/2)/(A0/n_r)
  F_dist = 1- pf(F0, 2, n_r)
  #print(paste0("Comparing polynomials with degrees ", 2*(j-1), " and ", 2*j))
  print(paste0("F12 degree: ", j))
  print(paste0("RSS: " , A0))
  print(paste0("Significance: ", F_dist))
  A1=A0
}

sin_cos = cbind(sin(2*pi*x*7/12),cos(2*pi*x*7/12))
fit_poly = lm(dt_notrend~0+f12+sin_cos)
summary(fit_poly)
A0 = sum(fit_poly$residuals^2)
n_r = length(x) - length(fit_poly$coefficients)
F0 = ((A1 - A0)/2)/(A0/n_r)
F_dist = 1- pf(F0, 2, n_r)
print(paste0("RSS: " , A0))
print(paste0("Significance: ", F_dist))

f12 = fourier(ts(dt_notrend, frequency = 12),K=6)
fit_poly = lm(dt_notrend ~ 0+f12)
summary(fit_poly)
n2_r = length(fit_poly$coefficients)
n_r=length(dt_notrend)
A0=sum(fit_poly$residuals^2)
f12_sig = f12[,cbind('S1-12','C1-12','S2-12','C2-12','C3-12','S4-12','S5-12','C6-12')]
fit_poly = lm(dt_notrend ~ 0+f12_sig)
summary(fit_poly)
A1 = sum(fit_poly$residuals^2)
n1_r = length(fit_poly$coefficients)
F0 = ((A1-A0)/(n2_r-n1_r))/(A0/(n_r))
F_dist = 1- pf(F0, n2_r-n1_r, n_r)
print(paste0("Significance: ", F_dist))

#thus pvalue is > 0,05 hence the reduction in RSS due to higher terms are not significant hence use f12_sig


dt_notrend_noseason = fit_poly$residuals
tbats_noseason = tbats(dt_notrend_noseason)
plot(tbats_noseason)
#the tbats model doesn't choose any seasonality hence the data has no seasonality left
#it can be used to supplement no seasonality in data

dt_ts = ts(data = dt_notrend_noseason,frequency = 12)
plot(dt_ts)
plot(x,dt_notrend_noseason,'l-')

acf(dt_notrend_noseason,lag=100)

ar = Arima(dt_notrend_noseason,order = c(2,0,1),include.mean = FALSE,method = 'CSS')
summary(ar)

plot(ar,type='both')
A1 = sum(ar$residuals^2)
print(A1)
for (n in 1:4){
  #f12 = fourier(ts(dt_notrend, frequency = 12), K=j)
  #fit_poly = lm(dt_notrend ~ 0 + f12)  
  ar = Arima(dt_notrend_noseason,order = c(2*n+2,0,2*n+1),include.mean = FALSE,method='CSS')
  #print(summary(fit_poly))
  print(summary(ar))
  A0 = sum(ar$residuals^2)
  n_r = length(x) - length(ar$coefficients)
  F0 = ((A1 - A0)/4)/(A0/n_r)
  F_dist = 1- pf(F0, 2, n_r)
  print(paste0("Comparing ARMA model with ", 2*(n)+2, " autoregressive order and ", 2*n+1,' ma order with ',2*n," ",2*n-1))
  #print(paste0( ", j))
  print(paste0("RSS: " , A0))
  print(paste0("Significance: ", F_dist))
  A1=A0
}


#optimal order is (4,1)
ar = Arima(dt_notrend_noseason,order=c(4,0,1),include.mean = FALSE,method = 'CSS')
at=ar$residuals
plot(ar)
sum(at^2)
plot(x,at,type = 'l-')
#test of stationarity
kpss.test(at)
adf.test(at)
acf(at)

#acf shows there is some correlation in higher lags seems to have correlation but we will neglect those 
#as we saw higer order models were not stable/invertible
#arma(4,1) is assymptotically stable with 

### Integrated Model
trend_regressor = cbind(poly(x,3),f12_sig)
test_range = seq.int(length(training)+1,length(msts))
f12_test = fourier(ts(msts,frequency = 12),K=6)[test_range,cbind('S1-12','C1-12','S2-12','C2-12','C3-12','S4-12','S5-12','C6-12')]
test_reg = cbind(poly(test_range,3),f12_test)
##fit integrated model
ari_high = Arima(training,order = c(4,0,1),xreg = trend_regressor,method = 'CSS',include.mean = TRUE)
summary(ari_high)
plot(ari_high)
A0=sum(ari_high$residuals^2)
n2_r = length(ari_high$coef)
ari_low = Arima(training,order = c(4,0,0),xreg = trend_regressor,method = 'CSS',include.mean = TRUE)
summary(ari_low)
plot(ari_low)
n1_r=length(ari_low$coef)
A1=sum(ari_low$residuals^2)
F0=((A1-A0)/(n2_r-n1_r))/(A0/(length(x)-n2_r))
F_dist =1-pf(F0,n2_r-n1_r,length(x)-n2_r)

#optimal integrated model is stable and invertible
#check acf
acf(ari_high$residuals)


ari_opt = Arima(training,order = c(4,0,0),xreg = trend_regressor,method = 'CSS',include.mean = TRUE)
plot(ari_opt)
summary(ari_opt)

prediction = predict(ari_opt,n.ahead = 12,newxreg = test_reg)

naive_pred=msts[test_range-1]
naive_error = sum((msts[test_range]-naive_pred)^2)
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

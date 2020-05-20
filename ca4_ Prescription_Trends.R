# ca4_ni
# Prescription trends


datapath <- "data/years"
#datapath <- "data/test"

file_in <- paste(datapath, "NI_prescribed_items_ts.csv", sep = "/")

data_in <- read.csv(file_in)


# Monthly items prescribed from April 2013 until latest figures available, Feb 2020
# frequnecy = no of obvs per unit in time
# in this example theres 2 x 12 months of data

# yearly - frequency = 1
# monthly - frequency = 12
# daily = freq depends on data content, typically either 7 or 1

ts_monthly = ts(data_in$Total_Items,
                      start = c(2013, 4),
                      frequency = 12 # monthly data
)
ts_monthly

cycle(ts_monthly)

plot(ts_monthly)

start(ts_monthly)
end(ts_monthly)
frequency(ts_monthly)

# install.packages("forecast")
library(forecast)

opar <- par(no.readonly = TRUE)

par( mfrow = c(2,2))

ylimit <- c(min(ts_monthly), max(ts_monthly))

# ma() allows smoothing of the data (moving average)

plot(Nile, 
     main = "ts_monthly raw data",
)

plot(ma(ts_monthly, 3),      main = "ts_monthly MA, 3",     ylim = ylimit )
plot(ma(ts_monthly, 7),      main = "ts_monthly MA, 7",     ylim = ylimit )
plot(ma(ts_monthly, 15),     main = "ts_monthly MA, 15",     ylim = ylimit )

par(opar)

# stationarity check
# augmented dick fuller test - ADF test

library(tseries)

adf.test(ts_monthly)

# is the data multiplicative or additive ?
# additive, when the data is teh trend plus the seasonality 
# multiplicative, trend still there and seasonailty, but the seasonality range (peak to trough) is changing
plot(ts_monthly, xlab = " Date",
     ylab = "PAX (000's)",
     main = "PAX numbers, 1949 - 1960"
)

# add abline to get straight regression line
abline( reg = lm(ts_monthly ~ time(ts_monthly) ) )

# Get the trend
# whats adding to the data year on year
plot(aggregate(ts_monthly, FUN = mean))

# seasonality - how things change in a the periods
# via boxplot
boxplot(ts_monthly ~ cycle(ts_monthly),
        xlab = "Date",
        ylab = "PAX",
        main = "Monthly PAX")

seasonal_decomp <- stl(ts_monthly, 
                       s.window = "period")

plot(seasonal_decomp)

# Test the stationarity of the object
# H0 - the time series is non-stationary
# H1 - the time series is stationary

adf.test(ts_monthly)

#lags - how correlated a time series is with its previous values
acf(ts_monthly)
# Acf(my_pax) --- don't use the upper case A model

Pacf(ts_monthly)

# get rid of seasonality
ts_monthly_log <- log(ts_monthly)
plot(ts_monthly_log)

par(mfrow = c(1,2))
plot(ts_monthly)
plot(ts_monthly_log)

par(opar)


adf.test( ts_monthly_log, alternative = "stationary")

acf(ts_monthly_log)

# peaks and troughs in Acf means there are seasonality, want it to be more random

ndiffs(ts_monthly_log)
# needs to moved one to teh left

ts_monthly_diff <- diff(ts_monthly_log, lag = 1)

# check if data needs further differencing
ndiffs(ts_monthly_diff)

# now is zero, so now more needed

acf(ts_monthly_diff)

# plot(diff_pax)

# summary
# log_pax - unequal variances in the pax data
# diff_pax - lagged air passengers


# ARIMA models
# Use the acf plot to figure which to use 
# +ve in acf in lag 1 then use AR model
# -ve in acf in lag 1 then use MA model

Pacf(ts_monthly_diff)

# Use the Pacf plot to figure which to use 
# +ve in acf in lag 1 then use AR model
# -ve in acf in lag 1 then use MA model

# SARIMA(p, d, q)
# p - no if autoregressive
# d = degree of differencing (d = 1)
# q = the no of moving average terms

# SARIMA(p, d, q)(P, D, Q)m
# p - no if autoregressive
# d = degree of differencing (d = 1)
# q = the no of moving average terms
# m = no of periods in the season


# degree of differencing, d, is 1, the no of lags

# SARIMA(0, 1, 1)(0, 1, 1)12

# using log in this case because of smoothing effect
fit <-arima(ts_monthly_log, 
            c(0, 1, 1),
            seasonal = list(order = c(0, 1, 1),
                            period = 12)
)
fit

# generate some forecasts
# 2 ways
# predict
prediction <- predict(fit, n.ahead = 3 * 12 ) # 3 years at 12 months
prediction

forecast_model <- forecast(fit, 
                           level = c(95), # 36 momnths (though could be 3 * 16), 
                           h = 36) # h is confidence interval
forecast_model


plot(forecast(forecast_model, 3))

auto_arima_model <- auto.arima(ts_monthly)
auto_arima_model

fit





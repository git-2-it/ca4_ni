# ca4_ni
# Prescription trends

library(tidyverse)
library(tseries)

datapath <- "data/years"
#datapath <- "data/test"

file_in <- paste(datapath, "NI_prescribed_items_ts.csv", sep = "/")

data_in <- read.csv(file_in)

str(data_in)

# Check for NA values
colSums(is.na(data_in))

# make sure to check ordering
data_in <- data_in %>% arrange(Year, Month)

# Monthly items prescribed from April 2013 until latest figures available, Feb 2020
# frequnecy = no of obvs per unit in time


# yearly - frequency = 1
# monthly - frequency = 12
# daily = freq depends on data content, typically either 7 or 1

ts_monthly = ts(data_in$item_patient_ratio,
                      start = c(2013, 4),
                      frequency = 12 # monthly data
)
ts_monthly

# Check start and end, frequency
start(ts_monthly)
end(ts_monthly)
frequency(ts_monthly)

# check and view the cycle and series
cycle(ts_monthly)

plot(ts_monthly, 
     main = "Raw time series plot",
     ylab = "Avg item patient ratio")

# grab defaults
opar <- par(no.readonly = FALSE)

# install.packages("forecast")
library(forecast)

# # run the auto armina model as a reference point
# auto_arima_model <- auto.arima(ts_monthly_ratio)
# auto_arima_model
# 
# forecast_model <- forecast(auto_arima_model, 
#                            level = c(95), # 36 momnths (though could be 3 * 16), 
#                            h = 8) # h is confidence interval
# forecast_model
# plot(forecast(forecast_model, 4))
# 
# 


opar <- par(no.readonly = TRUE)

par( mfrow = c(2,2))

ylimit <- c(min(ts_monthly), max(ts_monthly))

# ma() allows smoothing of the data (moving average)

plot(ma(ts_monthly, 3),      main = "ts_monthly MA, 3",     ylim = ylimit )
plot(ma(ts_monthly, 7),      main = "ts_monthly MA, 7",     ylim = ylimit )
plot(ma(ts_monthly, 15),     main = "ts_monthly MA, 15",    ylim = ylimit )

par(opar)

# All moving average plots show upwards trend

# stationarity check
# Test the stationarity of the object
# augmented Dicky-Fuller test - ADF test
# H0 - the time series is non-stationary
# H1 - the time series is stationary
adf.test(ts_monthly)
# adf.test( ts_monthly, alternative = "stationary", k= 12)
kpss.test(ts_monthly)
# Also indicates stationary
# Trend stationary?


nsdiffs(ts_monthly)
# No diffs necessary

pp.test(ts_monthly)


# low p-value of less than 0.01 indicates the data is a stationary dataset
# that is, mean, variance etc don't change over time.

# is the data multiplicative or additive ?
# additive, when the data is trend plus the seasonality 
# multiplicative, trend still there and seasonailty, but the seasonality range (peak to trough) is changing
plot(ts_monthly, 
     xlab = "Date",
     ylab = "Item Ratio",
     main = "NI Prescribed Item/Patient ratios\nApr 2013 - Feb 2020",
     sub = "Average Number of items perscribed per patient"
)

# add abline to get straight regression line
abline( reg = lm(ts_monthly ~ time(ts_monthly) ) )

# Obvious upward trend as the years progress
# Indicates an increasing number of items are prescribed each year even if 
# patient/popoulation remained stable

# Get the YoY trend
# whats adding to the data year on year
plot(aggregate(ts_monthly, FUN = mean))


# seasonality - do things change in the monthly periods
# via boxplot
boxplot(ts_monthly ~ cycle(ts_monthly),
        xlab = "Month",
        ylab = "Item Ratio",
        main = "Monthly Item/Patient Ratio")

boxplot(clean_month ~ cycle(clean_month),
        xlab = "Month",
        ylab = "Item Ratio",
        main = "Monthly Item/Patient Ratio")

clean_month

# Note the outliers - what are they?

chk <- subset(data_in, data_in$Month == 12)
chk

# December 2013 and 2019
# Note the outliers, leave in place and continue.

# Looks like there is a seasonal component, ie the data changes from month to month

seasonal_decomp <- stl(ts_monthly, 
                       s.window = "period")

plot(seasonal_decomp)

# Can see the seasonal nature of the data
# A series of peaks at certain times of the year, notablly around 
# year end, followed by a sharp trough, except around teh middle of
# the year, when figures almost (ut not quite) plateau, or at least
# decline more slowly. Digging into the natures of teh items might be
# illustrative to show why there are peaks, eg could peaks be cold/flu 
# related in winter, with sharp drop off if practices are closed over the
# festive season, while in summer could there a more sustained demand for 
# hayfever treatmetns

# Multiplicative or additive?
# Looks additive, that is there is a year trend, and a season trend, but the
# season trend is not changing over time

# compare plot and log log plot?

log_ts <- log(ts_monthly)
plot(log_ts)

# par(mfrow = c(1,2))
plot(ts_monthly)
par(new = TRUE)
plot(log_ts, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "red")

par(opar)

# Still looks additive - the base plot and the log plot look the same.


clean_month <- tsclean(ts_monthly) 

#lags - how correlated a time series is with its previous values
acf(ts_monthly,
    main = "Item/Patient ratio\nAuto-corellation")
acf(clean_month,
    main = "Item/Patient ratio\nAuto-corellation")

# Acf(my_pax) --- don't use the upper case A model
# Indicates non-stationary? Hhmmmm
# Multiple lag points?

# Partial auto-cor
Pacf(ts_monthly,
     main = "Item/Patient ratio\nPartial auto-corellation"
)

# Each cross over the blue dotted line indicates a potential al at that point


plot(ts_monthly)
trained_model<-lm(ts_monthly~ c(1:length(ts_monthly)))
# de-trended series.
plot(resid(trained_model), type = "l")


ts_decompose<-stl(ts_monthly, "periodic")
# de-seasonalisethe time series
ts_seasonal_adjust<-seasadj(ts_decompose)
# original series
plot(ts_monthly, type = "l")
# seasonal adjusted
plot(ts_seasonal_adjust, type = "l")

# Plot showing monthly ratio, by year
seasonplot(ts_seasonal_adjust, 
           12, 
           col = rainbow(12), 
           year.labels= TRUE, 
           main = "Seasonally adjusted: Item/Patient ratio")
# Shows the YoY upwards trend


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

forecast_model <- forecast(auto_arima_model, 
                           level = c(95), # 36 momnths (though could be 3 * 16), 
                           h = 12) # h is confidence interval
forecast_model
plot(forecast(forecast_model, 3),
     ylim=c(0,200000))



ts_monthly_items = ts(data_in$Total_Items,
                start = c(2013, 4),
                frequency = 12 # monthly data
)
ts_monthly_items

ts_monthly_patients = ts(data_in$Total_Patients,
                start = c(2013, 4),
                frequency = 12 # monthly data
)
ts_monthly_patients


ts_monthly_ratio = ts(data_in$item_patient_ratio , 
                start = c(2013, 4),
                frequency = 12 # monthly data
)
ts_monthly_ratio



# -----------------------------------------------------

auto_arima_model <- auto.arima(ts_monthly_ratio)
auto_arima_model

forecast_model <- forecast(auto_arima_model, 
                           level = c(95), # 36 momnths (though could be 3 * 16), 
                           h = 8) # h is confidence interval
forecast_model
plot(forecast(forecast_model, 4))


auto_arima_model <- auto.arima(ts_monthly_items)
auto_arima_model

forecast_model <- forecast(auto_arima_model, 
                           level = c(95), # 36 momnths (though could be 3 * 16), 
                           h = 8) # h is confidence interval
forecast_model
plot(forecast(forecast_model, 4))

# auto_arima_model <- auto.arima(ts_monthly_patients)
# auto_arima_model
# 
# forecast_model <- forecast(auto_arima_model, 
#                            level = c(95), # 36 momnths (though could be 3 * 16), 
#                            h = 8) # h is confidence interval
# forecast_model
# plot(forecast(forecast_model, 4))


# plot(forecast(forecast_model, 3),
#      ylim=c(0,2) )



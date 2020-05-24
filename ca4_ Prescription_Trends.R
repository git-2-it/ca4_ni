<<<<<<< HEAD
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

head(data_in)

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
     ylab = "Avg item patient ratio",
     xlab = "Years"
     )

# grab defaults
opar <- par(no.readonly = FALSE)

# install.packages("forecast")
library(forecast)

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
# This would indicates an increasing number of items are prescribed each year
# even if patient numbers/popoulation remained stable

par( mfrow = c(2,2))

ylimit <- c(min(ts_monthly), max(ts_monthly))

# ma() allows smoothing of the data (moving average)

plot(ma(ts_monthly, 3),      main = "ts_monthly MA, 3",     ylim = ylimit )
plot(ma(ts_monthly, 7),      main = "ts_monthly MA, 7",     ylim = ylimit )
plot(ma(ts_monthly, 15),     main = "ts_monthly MA, 15",    ylim = ylimit )
plot(aggregate(ts_monthly, FUN = mean))

par(opar)

# The various MA plots confirm the trend

# Multiplicative or additive?
# Looks additive, that is there is a year trend, and a season trend, 
# and the season trend is not changing over time

# Check, by comparing compare plot and log plot

log_ts <- log(ts_monthly)

plot(ts_monthly,
     xlab = "Years",
     ylab = "Item ratio")
par(new = TRUE)
plot(log_ts, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "red",
     main = "Data plot with log of data overlay",
)

par(opar)

# nsdiffs(ts_monthly) # Value of zero matches the observation
# Data still looks additive - the base plot and the log plot look the same.


# seasonality - do things change in the monthly periods
# via boxplot
boxplot(ts_monthly ~ cycle(ts_monthly),
        xlab = "Month",
        ylab = "Item Ratio",
        main = "Monthly Item/Patient Ratio")

# Note the outliers - what are they?

chk <- subset(data_in, data_in$Month == 12)
chk

# December 2013 and 2019
# Note the outliers, leave in place and continue.

# Looks like there is a seasonal component, ie the data changes from month to month
# boxplot doesn't really help show it clearly


######################
# Decompose the data
######################

ts_decompose <- stl(ts_monthly, s.window="periodic")

deseason_ct <- seasadj(ts_decompose)

plot (ts_decompose,
      main = "Item ratio decomposition over time",
      )


# stationarity check
# Test the stationarity of the object
# augmented Dicky-Fuller test - ADF test
# H0 - the time series is non-stationary
# H1 - the time series is stationary
adf.test( ts_monthly, alternative = "stationary")

# suggested_k <- trunc((length(ts_monthly)-1)^(1/3))
# suggested_k

# Indicates its stationary, can see from trend its not
# Reset the test to be monthly, ie lag order of 12 not the calculated 4
adf.test( ts_monthly, alternative = "stationary", k= 12)

# Not stationary at this lag order

# Check the auto-corellation

#lags - how correlated a time series is with its previous values
acf(ts_monthly,
    main = "Item/Patient ratio\nAuto-corellation")

# Acf(my_pax) --- don't use the upper case A model
# Indicates non-stationary, no immediate tail off to zero
# Also 

# Partial auto-cor
Pacf(ts_monthly,
     main = "Item/Patient ratio\nPartial auto-corellation"
)


# Need to de-trend and de-seasonalise the data

# De-trend the data
# Its additive already, no need for log diffs

nsdiffs(ts_monthly)
# 0

ndiffs(ts_monthly)
# 1

diff_ts_monthly <- diff(ts_monthly, lag = 12 , differences = 1)
plot(diff_ts_monthly)

diff_ts_monthly <- diff(ts_monthly, lag = 12 , differences = 1)

adf.test(diff_ts_monthly, alternative = "stationary", k = 12)
# low p-value  - Stationary

# Which ARIMA model to use?
# Collect the decision points
# Corellelation at lag one for dataset = negative - indicates the AR model
# ACF at lag n - drops of at 0 (ie drops below threshold after 0) - p
# PACF at lag n - drops of at 2 (ie drops below threshold after 2) - q
# d = number of diffs, 1
# (0, 1, 2)
par(mfrow = c(1,2))

acf(ts_monthly) 
# significant at 0 - P
Pacf(ts_monthly)
# significant at 1 - Q
ndiffs(ts_monthly)
# 1 - D

# Seasonal component - (P, D, Q)[frequency]
# frequency = 12
acf(diff_ts_monthly) 
# significant at 0 - P
Pacf(diff_ts_monthly)
# significant at 1
ndiffs(diff_ts_monthly)
# 0 - D
# (0, 0, 1)

# total model = (0, 1, 2)(0, 0, 1)[12]

arima_items <-arima(ts_monthly, 
                    c(0, 1, 2),
                    seasonal = list(order = c(1, 0, 0),
                                    period = 12)
                    )
arima_items

# generate some forecasts
# 2 ways
# predict
prediction <- predict(arima_items, n.ahead = 3 * 12 ) # 3 years at 12 months
prediction

forecast_model <- forecast(arima_items, 
                           level = c(95), # confidence level
                           h = 18) # h is frequency
forecast_model

plot(forecast_model,
     ylab = "Item ratio",
     xlab = "Years")


#------------------------------------------
# Model Evaluation
#------------------------------------------

# Use the auto generate function
auto_model <- auto.arima(ts_monthly)
auto_model
# AIC -269.48
arima_items
# AIC -273.55

# Different model proposed - ARIMA(5,1,0)(1,0,0)[12]


# Which is better?

qqnorm(arima_items$residuals)
qqline(arima_items$residuals)

qqnorm(auto_model$residuals)
qqline(auto_model$residuals)


# Look similar ...

accuracy(arima_items)
accuracy(auto_model)

# Box test

Box.test(arima_items$residuals, type = "Ljung-Box")
Box.test(auto_model$residuals, type = "Ljung-Box")


# Manually selected model has a better p-value
# manual        - 0.6194
# auto model    - 0.4049
# yee-haa



#------------------------------------------
# Model testing
#------------------------------------------

# split the data in train and test
# 83 obvs
# Set last 18 months as the test dataset, not quite 80/20 but a decent period

items_train <- stats::window(x = ts_monthly, end = c(2018, 8) )
items_train
str(items_train)

items_test <- stats::window(x = ts_monthly, start = c(2018, 9) )
items_test
str(items_test)

# Items based on manual model
man_trained_arima_items <-arima(items_train, 
                    c(0, 1, 2),
                    seasonal = list(order = c(1, 0, 0),
                                    period = 12)
)
man_trained_arima_items

man_predict <- forecast(man_trained_arima_items, 18)
man_predict



actuals_man_predictions <- data.frame(cbind(actuals = items_test, predicted = man_predict))
head(actuals_man_predictions)

actuals_man_predictions

correlation_accuracy_man <- cor(actuals_man_predictions)
correlation_accuracy_man

library(psych)

par(opar)

pairs.panels(actuals_man_predictions, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             , main = "Correlation check of actual and predicted item ratios"
)

accuracy(man_predict, items_test)

man_predict %>%
    autoplot() +
    geom_line(
        aes(
            x = as.numeric(time(items_test)),
            y = as.numeric(items_test)
        ),
        col = "red"
    )



# more training data, smaller prediction?
items_train12 <- stats::window(x = ts_monthly, end = c(2019, 2) )
items_train12
str(items_train12)

items_test12 <- stats::window(x = ts_monthly, start = c(2019, 3) )
items_test12
str(items_test12)

# Items based on manual model
man_trained_arima_items12 <-arima(items_train12, 
                                c(0, 1, 2),
                                seasonal = list(order = c(1, 0, 0),
                                                period = 12)
)
man_trained_arima_items12

man_predict12 <- forecast(man_trained_arima_items12, 12)
man_predict12



actuals_man_predictions12 <- data.frame(cbind(actuals = items_test12, predicted = man_predict12))
head(actuals_man_predictions12)
str(actuals_man_predictions12)

actuals_man_predictions12

correlation_accuracy_man12 <- cor(actuals_man_predictions12)
correlation_accuracy_man12

accuracy(man_predict, items_test)

pairs.panels(actuals_man_predictions12, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             , main = "Correlation check of actual and predicted item ratios"
)

accuracy(man_predict12, items_test12)

man_predict %>%
    autoplot() +
    geom_line(
        aes(
            x = as.numeric(time(items_test12)),
            y = as.numeric(items_test12)
        ),
        col = "red"
    )

# No Benefit

#------------------------------------------
# EOF
#------------------------------------------

=======
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

head(data_in)

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
     ylab = "Avg item patient ratio",
     xlab = "Years"
     )

# grab defaults
opar <- par(no.readonly = FALSE)

# install.packages("forecast")
library(forecast)

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
# This would indicates an increasing number of items are prescribed each year
# even if patient numbers/popoulation remained stable

par( mfrow = c(2,2))

ylimit <- c(min(ts_monthly), max(ts_monthly))

# ma() allows smoothing of the data (moving average)

plot(ma(ts_monthly, 3),      main = "ts_monthly MA, 3",     ylim = ylimit )
plot(ma(ts_monthly, 7),      main = "ts_monthly MA, 7",     ylim = ylimit )
plot(ma(ts_monthly, 15),     main = "ts_monthly MA, 15",    ylim = ylimit )
plot(aggregate(ts_monthly, FUN = mean))

par(opar)

# The various MA plots confirm the trend

# Multiplicative or additive?
# Looks additive, that is there is a year trend, and a season trend, 
# and the season trend is not changing over time

# Check, by comparing compare plot and log plot

log_ts <- log(ts_monthly)

plot(ts_monthly,
     xlab = "Years",
     ylab = "Item ratio")
par(new = TRUE)
plot(log_ts, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col = "red",
     main = "Data plot with log of data overlay",
)

par(opar)

# nsdiffs(ts_monthly) # Value of zero matches the observation
# Data still looks additive - the base plot and the log plot look the same.


# seasonality - do things change in the monthly periods
# via boxplot
boxplot(ts_monthly ~ cycle(ts_monthly),
        xlab = "Month",
        ylab = "Item Ratio",
        main = "Monthly Item/Patient Ratio")

# Note the outliers - what are they?

chk <- subset(data_in, data_in$Month == 12)
chk

# December 2013 and 2019
# Note the outliers, leave in place and continue.

# Looks like there is a seasonal component, ie the data changes from month to month
# boxplot doesn't really help show it clearly


######################
# Decompose the data
######################

ts_decompose <- stl(ts_monthly, s.window="periodic")

deseason_ct <- seasadj(ts_decompose)

plot (ts_decompose,
      main = "Item ratio decomposition over time",
      )


# stationarity check
# Test the stationarity of the object
# augmented Dicky-Fuller test - ADF test
# H0 - the time series is non-stationary
# H1 - the time series is stationary
adf.test( ts_monthly, alternative = "stationary")

# suggested_k <- trunc((length(ts_monthly)-1)^(1/3))
# suggested_k

# Indicates its stationary, can see from trend its not
# Reset the test to be monthly, ie lag order of 12 not the calculated 4
adf.test( ts_monthly, alternative = "stationary", k= 12)

# Not stationary at this lag order

# Check the auto-corellation

#lags - how correlated a time series is with its previous values
acf(ts_monthly,
    main = "Item/Patient ratio\nAuto-corellation")

# Acf(my_pax) --- don't use the upper case A model
# Indicates non-stationary, no immediate tail off to zero
# Also 

# Partial auto-cor
Pacf(ts_monthly,
     main = "Item/Patient ratio\nPartial auto-corellation"
)


# Need to de-trend and de-seasonalise the data

# De-trend the data
# Its additive already, no need for log diffs

nsdiffs(ts_monthly)
# 0

ndiffs(ts_monthly)
# 1

diff_ts_monthly <- diff(ts_monthly, lag = 12 , differences = 1)
plot(diff_ts_monthly)

diff_ts_monthly <- diff(ts_monthly, lag = 12 , differences = 1)

adf.test(diff_ts_monthly, alternative = "stationary", k = 12)
# low p-value  - Stationary

# Which ARIMA model to use?
# Collect the decision points
# Corellelation at lag one for dataset = negative - indicates the AR model
# ACF at lag n - drops of at 0 (ie drops below threshold after 0) - p
# PACF at lag n - drops of at 2 (ie drops below threshold after 2) - q
# d = number of diffs, 1
# (0, 1, 2)
par(mfrow = c(1,2))

acf(ts_monthly) 
# significant at 0 - P
Pacf(ts_monthly)
# significant at 1 - Q
ndiffs(ts_monthly)
# 1 - D

# Seasonal component - (P, D, Q)[frequency]
# frequency = 12
acf(diff_ts_monthly) 
# significant at 0 - P
Pacf(diff_ts_monthly)
# significant at 1
ndiffs(diff_ts_monthly)
# 0 - D
# (0, 0, 1)

# total model = (0, 1, 2)(0, 0, 1)[12]

arima_items <-arima(ts_monthly, 
                    c(0, 1, 2),
                    seasonal = list(order = c(1, 0, 0),
                                    period = 12)
                    )
arima_items

# generate some forecasts
# 2 ways
# predict
prediction <- predict(arima_items, n.ahead = 3 * 12 ) # 3 years at 12 months
prediction

forecast_model <- forecast(arima_items, 
                           level = c(95), # confidence level
                           h = 18) # h is frequency
forecast_model

plot(forecast_model,
     ylab = "Item ratio",
     xlab = "Years")


#------------------------------------------
# Model Evaluation
#------------------------------------------

# Use the auto generate function
auto_model <- auto.arima(ts_monthly)
auto_model

# Different model proposed - ARIMA(5,1,0)(1,0,0)[12]


# Which is better?

qqnorm(arima_items$residuals)
qqline(arima_items$residuals)

qqnorm(auto_model$residuals)
qqline(auto_model$residuals)


# Look similar ...

accuracy(arima_items)
accuracy(auto_model)

# Box test

Box.test(arima_items$residuals, type = "Ljung-Box")
Box.test(auto_model$residuals, type = "Ljung-Box")


# Manually selected model has a better p-value
# manual        - 0.6194
# auto model    - 0.4049
# yee-haa



#------------------------------------------
# Model testing
#------------------------------------------

# split the data in train and test
# 83 obvs
# Set last 18 months as the test dataset, not quite 80/20 but a decent period

items_train <- stats::window(x = ts_monthly, end = c(2018, 8) )
items_train
str(items_train)

items_test <- stats::window(x = ts_monthly, start = c(2018, 9) )
items_test
str(items_test)

# Items based on manual model
man_trained_arima_items <-arima(items_train, 
                    c(0, 1, 2),
                    seasonal = list(order = c(1, 0, 0),
                                    period = 12)
)
man_trained_arima_items

man_predict <- forecast(man_trained_arima_items, 18)
man_predict



actuals_man_predictions <- data.frame(cbind(actuals = items_test, predicted = man_predict))
head(actuals_man_predictions)

actuals_man_predictions

correlation_accuracy_man <- cor(actuals_man_predictions)
correlation_accuracy_man

library(psych)

par(opar)

pairs.panels(actuals_man_predictions, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             , main = "Correlation check of actual and predicted item ratios"
)

accuracy(man_predict, items_test)

man_predict %>%
    autoplot() +
    geom_line(
        aes(
            x = as.numeric(time(items_test)),
            y = as.numeric(items_test)
        ),
        col = "red"
    )



# more training data, smaller prediction?
items_train12 <- stats::window(x = ts_monthly, end = c(2019, 2) )
items_train12
str(items_train12)

items_test12 <- stats::window(x = ts_monthly, start = c(2019, 3) )
items_test12
str(items_test12)

# Items based on manual model
man_trained_arima_items12 <-arima(items_train12, 
                                c(0, 1, 2),
                                seasonal = list(order = c(1, 0, 0),
                                                period = 12)
)
man_trained_arima_items12

man_predict12 <- forecast(man_trained_arima_items12, 12)
man_predict12



actuals_man_predictions12 <- data.frame(cbind(actuals = items_test12, predicted = man_predict12))
head(actuals_man_predictions12)
str(actuals_man_predictions12)

actuals_man_predictions12

correlation_accuracy_man12 <- cor(actuals_man_predictions12)
correlation_accuracy_man12

accuracy(man_predict, items_test)

pairs.panels(actuals_man_predictions12, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             , main = "Correlation check of actual and predicted item ratios"
)

accuracy(man_predict12, items_test12)

man_predict %>%
    autoplot() +
    geom_line(
        aes(
            x = as.numeric(time(items_test12)),
            y = as.numeric(items_test12)
        ),
        col = "red"
    )

# No Benefit

#------------------------------------------
# EO First model, check the auto again
#------------------------------------------

auto_model <- auto.arima(ts_monthly)
auto_model

auto_arima_items <-arima(items_train, 
                                c(5, 1, 0),
                                seasonal = list(order = c(1, 0, 0),
                                                period = 12)
)
auto_arima_items
arima_items

auto_predict <- forecast(auto_arima_items, 18)
auto_predict



actuals_auto_predictions <- data.frame(cbind(actuals = items_test, predicted = auto_predict))
head(actuals_auto_predictions)

actuals_auto_predictions

correlation_accuracy_auto <- cor(actuals_auto_predictions)
correlation_accuracy_auto

pairs.panels(actuals_auto_predictions, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
             , main = "Correlation check of actual and predicted item ratios (AUTO)"
)

accuracy(man_predict, items_test)
accuracy(auto_predict)

man_predict %>%
    autoplot() +
    geom_line(
        aes(
            x = as.numeric(time(items_test)),
            y = as.numeric(items_test)
        ),
        col = "red"
    )


#------------------------------------------
# EOF
#------------------------------------------


library(TSA)
library(ggplot2)
library(stats)
library(dplyr)
library(lubridate)
library(tseries)
library(forecast)
library(rugarch)
library(xts)

Data <- read.csv("C:/Users/misra/Documents/Master's Assignments/Semester_2_Spring24/MA641 - Time Series Analysis/US_Gasoline_Prices.csv")
head(Data)
summary(Data)
Data$Date <- as.POSIXct(Data$Date, format = "%m/%d/%Y")

Data$Average <- rowMeans(Data[, -1])

# ------------------------------Applying ARIMA model-------------------------- #

ts_data_Average <- ts(Data$Average)

plot(ts_data_Average, type = "l", main="Time Series plot of Average Gasoline prices")

par(mfrow = c(1, 1))
acf(ts_data_Average, lag.max = 100, main="ACF plot of gasoline prices")
pacf(ts_data_Average, lag.max = 100, main="PACF plot of gasoline prices")

eacf(ts_data_Average)

# Determining if the data is stationary

adf.test(ts_data_Average)

# Data is not Stationary.

ndiffs(ts_data_Average)

# 1 differencing is required

ts_data_Average_diff <- diff(ts_data_Average)

# Checking the differenced time series

par(mfrow = c(1, 1))
plot(ts_data_Average_diff, type = "l", main = "Time Series plot of differenced average Gasoline prices")
adf.test(ts_data_Average_diff)

par(mfrow = c(2, 1))
acf(ts_data_Average_diff, lag.max = 100, main="ACF plot of differenced average Gasoline prices")
pacf(ts_data_Average_diff, lag.max = 100, main="PACF plot of differenced average Gasoline prices")

eacf(ts_data_Average_diff)

# Determining the orders of the ARIMA model

aic_results <- data.frame(
  p = numeric(), q = numeric(), AIC = numeric()
)
for (p in 4:5) {
  for (q in 0:2) {
    model = arima(ts_data_Average, order = c(p, 1, q))
    
    # Calculate the AIC score
    aic_score <- model$aic
    
    # Store the results
    aic_results <- rbind(aic_results, data.frame(
      p = p, q = q, AIC = aic_score
    ))
  }
}

print(aic_results)


# ARIMA(5, 1, 2) has the lowest aic score.

model512 <- arima(ts_data_Average, order = c(5, 1, 2))

print(model512)

# Analysis of residulas of the ARIMA(4,1,1) model

residual512 <- model512$residuals

par(mfrow = c(1, 1))
plot(residual512)

par(mfrow = c(2, 1))
acf(residual512, lag.max = 50, main="ACF plot of the residual")
pacf(residual512, lag.max = 50, main="PACF plot of the residual")

Box.test(residual512, lag = 12, type = "Ljung-Box")

tsdiag(model512)

par(mfrow = c(1,1))
qqnorm(residual512)
qqline(residual512)

hist(residual512)
shapiro.test(residual512)

forecast_512 <- predict(model512, n.ahead = 100, type = "both")

print(forecast_512)

par(mfrow = c(2,1))
plot(forecast_512$pred)
plot(forecast_512$se)






#---------------------- Trying to Apply sGARCH model -------------------------#

ts_data_Average_log <- log(ts_data_Average)

par(mfrow = c(1, 1))
plot(ts_data_Average_log, type = "l")

par(mfrow = c(2, 1))
acf(ts_data_Average_log, lag.max = 100)
pacf(ts_data_Average_log, lag.max = 100)

eacf(ts_data_Average_log)

# Testing stationary or not

adf.test(ts_data_Average_log)

# log data not stationary

ndiffs(ts_data_Average_log)

# 1 difference is required

ts_data_Average_log_diff <- diff(ts_data_Average_log)

par(mfrow = c(1, 1))
plot(ts_data_Average_log_diff, type = "l")

par(mfrow = c(2, 1))
acf(ts_data_Average_log_diff, lag.max = 100)
pacf(ts_data_Average_log_diff, lag.max = 100)

eacf(ts_data_Average_log_diff)


# Determining the orders of the ARIMA model

aic_results_log <- data.frame(
  p = numeric(), q = numeric(), AIC = numeric()
)
for (p in 0:5) {
  for (q in 0:5) {
    model = arima(ts_data_Average_log, order = c(p, 1, q))
    
    # Calculate the AIC score
    aic_score <- model$aic
    
    # Store the results
    aic_results_log <- rbind(aic_results_log, data.frame(
      p = p, q = q, AIC = aic_score
    ))
  }
}

print(aic_results_log)

# ARIMA(4,1,1) has the lowest AIC score

model_log_411 <- arima(ts_data_Average, order = c(4, 1, 1))

residual_log_411 <- model_log_411$residuals

par(mfrow = c(1, 1))
plot(residual_log_411, type = "l")

par(mfrow = c(2, 1))
acf(residual_log_411, lag.max = 100)
pacf(residual_log_411, lag.max = 100)

# Taking square of the residual

sq_residual_log_411 <- residual_log_411^2

par(mfrow = c(1, 1))
plot(sq_residual_log_411, type = "l")

par(mfrow = c(2, 1))
acf(sq_residual_log_411, lag.max = 100)
pacf(sq_residual_log_411, lag.max = 100)

eacf(sq_residual_log_411)

# determining the p and q of ARMA of squared residual series

aic_results_sq_residual <- data.frame(
  p = numeric(), q = numeric(), AIC = numeric()
)
for (p in 0:7) {
  for (q in 0:5) {
    model = arima(sq_residual_log_411, order = c(p, 0, q))
    
    # Calculate the AIC score
    aic_score <- model$aic
    
    # Store the results
    aic_results_sq_residual <- rbind(aic_results_sq_residual, data.frame(
      p = p, q = q, AIC = aic_score
    ))
  }
}

print(aic_results_sq_residual)

# ARMA(3,0) has the lowest AIC value

# Fitting ARIMA-GARCH model with ARIMA(4,1,1) and GARCH(3,0)

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 0)),
                   mean.model = list(armaOrder = c(4, 1, 1), include.mean = TRUE, 
                                     archm = FALSE, archpow = 1))

fit <- ugarchfit(spec, ts_data_Average_log)

print(fit)

# Trying to fit sGARCH model with garch(3,0) with The Skewed Generalized Error Distribution

spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 0)),
                    mean.model = list(armaOrder = c(4, 1, 1), include.mean = TRUE, 
                                      archm = FALSE, archpow = 1),
                    distribution.model = "sged")
fit2 <- ugarchfit(data = ts_data_Average_log, spec = spec2, solver ='hybrid')

print(fit2)

# The Ljung-Box test yields p value more than 0.05 and the Adjusted Pearson 
# Goodness-of-Fit Test yeilds p value greater than 0.05

par(mfrow = c(2, 1))
acf(residuals(fit2), main = "ACF of Residuals of ARIMA-GARCH model")
pacf(residuals(fit2), main = "PACF of Residuals of ARIMA-GARCH model")

par(mfrow = c(1, 1))
plot(fit2, which = "all")

forecast_fit2 <- ugarchforecast(fit2, n.ahead = 10)
fitted(forecast_fit2)

print(forecast_fit2)

plot(forecast_fit2, type = "multiple", ncol = 2, main = "ARIMA-GARCH Forecasts and Previous Values")



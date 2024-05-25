library(ggplot2)
library(forecast)
library(TSA)
library(stats)
library(fpp2)
library(urca)
library(dplyr)
library(tseries)
library(forecast)
library(zoo)
library(tidyverse)
library(lubridate)
library(rugarch)
df <- read.csv("C:/Users/misra/Documents/Master's Assignments/Semester_2_Spring24/MA641 - Time Series Analysis/Traffic_TS.csv")

head(df)

summary(df)

head(df$Total)
# Assuming your data frame is named "df"
# Convert index to datetime
# df$DateTime <- as.POSIXct(row.names(df), format="%Y-%m-%d %H:%M:%S")
# rownames(df) <- NULL

plot(df$Total, type="l")

# Plot ACF and PACF
# Assuming Total column is to be analyzed
# ACF Plot
acf(df$Total, main = "ACF Plot", lag.max = 5000)

# PACF Plot
pacf(df$Total, main = "PACF Plot", lag.max = 5000)

eacf(df$Total)

# Create a new dataset with hourly data
df_hourly <- df %>%
  mutate(hour = format(as.POSIXlt(Datetime), "%Y-%m-%d %H:00:00")) %>%
  group_by(hour) %>%
  summarise(
    CarCount_hourly = sum(CarCount),
    BikeCount_hourly = sum(BikeCount),
    BusCount_hourly = sum(BusCount),
    TruckCount_hourly = sum(TruckCount),
    Total_hourly = sum(Total)
  )

head(df_hourly)
summary(df_hourly)

plot(df_hourly$Total_hourly, type="l", main="Time series plot of hourly traffic data")

acf(df_hourly$Total_hourly, lag = 100, main="ACF plot of Hourly traffic data")
pacf(df_hourly$Total_hourly, lag = 100, main="PACF plot of Hourly traffic data")

adf.test(df_hourly$Total_hourly)

df_hourly$hour <- as.POSIXct(df_hourly$hour, format = "%Y-%m-%d %H:%M:%S")
ts_hourlyTotal <- ts(df_hourly$Total_hourly, start = c(year(df_hourly$hour[1]), month(df_hourly$hour[1]), day(df_hourly$hour[1]), hour(df_hourly$hour[1])), frequency = 24)

par(mfrow = c(3, 1))
plot(ts_hourlyTotal, type="l")
acf(ts_hourlyTotal, lag.max = 100)
pacf(ts_hourlyTotal, lag.max = 100)

ndiffs(ts_hourlyTotal)
nsdiffs(ts_hourlyTotal)

# here we can see the no of difference requited for non seasonal differencing is 0, but for seasonal difference we require 1 difference

#ts_hourlyTotal %>%
#  ggtsdisplay()

#ts_hourlyTotal_diff <- ts_hourlyTotal %>% 
#  diff() %>%
#  diff(lag=24) %>%
#  ggtsdisplay()

#acf(df_hourly$Total_hourly, lag.max = 100)
#pacf(df_hourly$Total_hourly, lag.max = 100)
#eacf(df_hourly$Total_hourly)

ndiffs(diff(ts_hourlyTotal, lag=24, difference=1))
nsdiffs(diff(ts_hourlyTotal, lag=24, difference=1))


adf.test(diff(ts_hourlyTotal, lag=24, difference=1))

ts_hourlyTotal_sdiff <- diff(ts_hourlyTotal, lag=24, difference=1)

par(mfrow = c(1, 1))
plot(ts_hourlyTotal_sdiff, type="l", main="Time series of seasonally differenced hourly traffic data")
adf.test(ts_hourlyTotal_sdiff)
par(mfrow=c(2,1))
acf(ts_hourlyTotal_sdiff, lag.max = 100, main="ACF of the seasonally differenced time series")
pacf(ts_hourlyTotal_sdiff, lag.max = 100, main="PACF of the seasonally differenced time series")

eacf(ts_hourlyTotal_sdiff)

# SARIMA(0,0,2)
# SARIMA(2,0,1)
# SARIMA(2,0,2)
# SARIMA(2,0,3)
# SARIMA(2,0,4)
# SARIMA(3,0,2)
# SARIMA(3,0,3)
# SARIMA(3,0,5)
# SARIMA(4,0,3)
# SARIMA(4,0,4)
# SARIMA(4,0,5)
# SARIMA(5,0,5)
# SARIMA(6,0,0)
# SARIMA(6,0,5)
# SARIMA(7,0,1)
# SARIMA(7,0,3)
# SARIMA(7,0,5)

model_new_1 = arima(ts_hourlyTotal, order = c(0,0,2), seasonal = c(0, 1, 1, 24))
model_new_2 = arima(ts_hourlyTotal, order = c(2,0,1), seasonal = c(0, 1, 1, 24))
model_new_3 = arima(ts_hourlyTotal, order = c(2,0,2), seasonal = c(0, 1, 1, 24))
model_new_4 = arima(ts_hourlyTotal, order = c(2,0,3), seasonal = c(0, 1, 1, 24))
model_new_5 = arima(ts_hourlyTotal, order = c(2,0,4), seasonal = c(0, 1, 1, 24))
model_new_6 = arima(ts_hourlyTotal, order = c(3,0,2), seasonal = c(0, 1, 1, 24))
model_new_7 = arima(ts_hourlyTotal, order = c(3,0,3), seasonal = c(0, 1, 1, 24))
model_new_8 = arima(ts_hourlyTotal, order = c(3,0,5), seasonal = c(0, 1, 1, 24))
model_new_9 = arima(ts_hourlyTotal, order = c(4,0,3), seasonal = c(0, 1, 1, 24))
model_new_10 = arima(ts_hourlyTotal, order = c(4,0,4), seasonal = c(0, 1, 1, 24))
model_new_11 = arima(ts_hourlyTotal, order = c(5,0,5), seasonal = c(0, 1, 1, 24))
model_new_12 = arima(ts_hourlyTotal, order = c(6,0,0), seasonal = c(0, 1, 1, 24))
model_new_13 = arima(ts_hourlyTotal, order = c(6,0,5), seasonal = c(0, 1, 1, 24))
model_new_14 = arima(ts_hourlyTotal, order = c(7,0,1), seasonal = c(0, 1, 1, 24))
model_new_15 = arima(ts_hourlyTotal, order = c(7,0,3), seasonal = c(0, 1, 1, 24))
model_new_16 = arima(ts_hourlyTotal, order = c(7,0,5), seasonal = c(0, 1, 1, 24))
model_new_17 = arima(ts_hourlyTotal, order = c(0,0,2), seasonal = c(0, 1, 2, 24))
model_new_18 = arima(ts_hourlyTotal, order = c(2,0,1), seasonal = c(0, 1, 2, 24))
model_new_19 = arima(ts_hourlyTotal, order = c(2,0,2), seasonal = c(0, 1, 2, 24))
model_new_20 = arima(ts_hourlyTotal, order = c(2,0,3), seasonal = c(0, 1, 2, 24))
model_new_21 = arima(ts_hourlyTotal, order = c(2,0,4), seasonal = c(0, 1, 2, 24))
model_new_22 = arima(ts_hourlyTotal, order = c(3,0,2), seasonal = c(0, 1, 2, 24))
model_new_23 = arima(ts_hourlyTotal, order = c(3,0,3), seasonal = c(0, 1, 2, 24))
model_new_24 = arima(ts_hourlyTotal, order = c(3,0,5), seasonal = c(0, 1, 2, 24))
model_new_25 = arima(ts_hourlyTotal, order = c(4,0,3), seasonal = c(0, 1, 2, 24))
model_new_26 = arima(ts_hourlyTotal, order = c(4,0,4), seasonal = c(0, 1, 2, 24))
model_new_27 = arima(ts_hourlyTotal, order = c(5,0,5), seasonal = c(0, 1, 2, 24))
model_new_28 = arima(ts_hourlyTotal, order = c(6,0,0), seasonal = c(0, 1, 2, 24))
model_new_29 = arima(ts_hourlyTotal, order = c(6,0,5), seasonal = c(0, 1, 2, 24))
model_new_30 = arima(ts_hourlyTotal, order = c(7,0,1), seasonal = c(0, 1, 2, 24))
model_new_31 = arima(ts_hourlyTotal, order = c(7,0,3), seasonal = c(0, 1, 2, 24))
model_new_32 = arima(ts_hourlyTotal, order = c(7,0,5), seasonal = c(0, 1, 2, 24))

model_new_1$aic
model_new_2$aic
model_new_3$aic
model_new_4$aic
model_new_5$aic
model_new_6$aic
model_new_7$aic
model_new_8$aic
model_new_9$aic
model_new_10$aic
model_new_11$aic
model_new_12$aic
model_new_13$aic
model_new_14$aic
model_new_15$aic
model_new_16$aic
model_new_17$aic
model_new_18$aic
model_new_19$aic
model_new_20$aic
model_new_21$aic
model_new_22$aic
model_new_23$aic
model_new_24$aic
model_new_25$aic
model_new_26$aic
model_new_27$aic
model_new_28$aic
model_new_29$aic
model_new_30$aic
model_new_31$aic
model_new_32$aic




model19 = arima(ts_hourlyTotal, order = c(6, 0, 11), seasonal = c(0, 1, 1, 24))

model19$aic

#nsdiffs(diff(ts(df_hourly$Total_hourly, start = 1), lag = 24, differences = 1))

#ns_diff_df_hourly = diff(df_hourly$Total_hourly, differences = 1)
#diff_df_hourly = diff(ns_diff_df_hourly, lag = 24, differences = 1)
#diff_ts = ts(diff_df_hourly, start = start(df_hourly$Total_hourly), frequency = frequency(df_hourly$Total_hourly))

#auto.arima(ts_hourlyTotal_sdiff, start.p=1, start.q=1, start.P = 1, start.Q = 1, stationary = TRUE)
#auto.arima(ts_hourlyTotal, start.p=1, start.q=1, start.P = 1, start.Q = 1)

# model 1: SARIMA <- (1,0,1) (1,0,1) 24

#model1 <- arima(df_hourly$Total_hourly, order = c(1,0,1), seasonal = c(1, 0, 1, 24))
#aic1 <- 2 * (1 + 1 + 1 + 1 + 1) - 2 * model1$loglik
#aic1

#results <- data.frame(
#  p = numeric(), q = numeric(), AIC = numeric()
#)

#for (p in 0:5) {
#  for (q in 1:5) {
        # Fit the SARIMA model with d=0 for both non-seasonal and seasonal parts
#        model <- arima(ts_hourlyTotal, order = c(p, 0, q), seasonal = c(1, 1, 0, 24))
        
        # Calculate the AIC score
#        aic_score <- model$aic
        
        # Store the results
#        results <- rbind(results, data.frame(
#          p = p, q = q, AIC = aic_score
#        ))
#  }
#}

# Display the results
#print(results)

# Here we will be trying out the following SARIMA models based on observations on ACF, PACF and EACF plots:
# SARIMA(2,0,1)(1,1,0)[24]
# SARIMA(2,0,2)(1,1,0)[24]
# SARIMA(0,0,2)(1,1,0)[24]
# SARIMA(3,0,2)(1,1,0)[24]
# SARIMA(3,0,3)(1,1,0)[24]
# SARIMA(3,0,5)(1,1,0)[24]
# SARIMA(4,0,3)(1,1,0)[24]
# SARIMA(4,0,4)(1,1,0)[24]
# SARIMA(4,0,5)(1,1,0)[24]
# SARIMA(2,0,1)(2,1,0)[24]
# SARIMA(2,0,2)(2,1,0)[24]
# SARIMA(0,0,2)(2,1,0)[24]
# SARIMA(3,0,2)(2,1,0)[24]
# SARIMA(3,0,3)(2,1,0)[24]
# SARIMA(3,0,5)(2,1,0)[24]
# SARIMA(4,0,3)(2,1,0)[24]
# SARIMA(4,0,4)(2,1,0)[24]
# SARIMA(4,0,5)(2,1,0)[24]


#aic_results <- data.frame(
#  p = numeric(), q = numeric(), P = numeric(), Q = numeric(), AIC = numeric()
#)

model1 = arima(ts_hourlyTotal, order = c(2, 0, 1), seasonal = c(0, 1, 1, 24))
model2 = arima(ts_hourlyTotal, order = c(2, 0, 2), seasonal = c(0, 1, 1, 24))
model3 = arima(ts_hourlyTotal, order = c(0, 0, 2), seasonal = c(0, 1, 1, 24))
model4 = arima(ts_hourlyTotal, order = c(3, 0, 2), seasonal = c(0, 1, 1, 24))
model5 = arima(ts_hourlyTotal, order = c(3, 0, 3), seasonal = c(0, 1, 1, 24))
model6 = arima(ts_hourlyTotal, order = c(3, 0, 5), seasonal = c(0, 1, 1, 24))
model7 = arima(ts_hourlyTotal, order = c(4, 0, 3), seasonal = c(0, 1, 1, 24))
model8 = arima(ts_hourlyTotal, order = c(4, 0, 4), seasonal = c(0, 1, 1, 24))
model9 = arima(ts_hourlyTotal, order = c(4, 0, 5), seasonal = c(0, 1, 1, 24))
model10 = arima(ts_hourlyTotal, order = c(2, 0, 1), seasonal = c(0, 1, 2, 24))
model11 = arima(ts_hourlyTotal, order = c(2, 0, 2), seasonal = c(0, 1, 2, 24))
model12 = arima(ts_hourlyTotal, order = c(0, 0, 2), seasonal = c(0, 1, 2, 24))
model13 = arima(ts_hourlyTotal, order = c(3, 0, 2), seasonal = c(0, 1, 2, 24))
model14 = arima(ts_hourlyTotal, order = c(3, 0, 3), seasonal = c(0, 1, 2, 24))
model15 = arima(ts_hourlyTotal, order = c(3, 0, 5), seasonal = c(0, 1, 2, 24))
model16 = arima(ts_hourlyTotal, order = c(4, 0, 3), seasonal = c(0, 1, 2, 24))
model17 = arima(ts_hourlyTotal, order = c(4, 0, 4), seasonal = c(0, 1, 2, 24))
model18 = arima(ts_hourlyTotal, order = c(4, 0, 5), seasonal = c(0, 1, 2, 24))
model19 = arima(ts_hourlyTotal, order = c(1, 0, 1), seasonal = c(0, 1, 1, 24))
model20 = arima(ts_hourlyTotal, order = c(0, 0, 1), seasonal = c(0, 1, 1, 24))
model21 = arima(ts_hourlyTotal, order = c(1, 0, 0), seasonal = c(0, 1, 1, 24))
model22 = arima(ts_hourlyTotal, order = c(1, 0, 1), seasonal = c(0, 1, 2, 24))
model23 = arima(ts_hourlyTotal, order = c(0, 0, 1), seasonal = c(0, 1, 2, 24))
model24 = arima(ts_hourlyTotal, order = c(1, 0, 0), seasonal = c(0, 1, 2, 24))

# Here we observed for SARIMA (4,0,5)(2,1,1)[24] model, we get the lowest AIC value. Hence we will bw considering this model

model1$aic
model2$aic
model3$aic
model4$aic
model5$aic
model6$aic
model7$aic
model8$aic
model9$aic
model10$aic
model11$aic
model12$aic
model13$aic
model14$aic
model15$aic
model16$aic
model17$aic
model18$aic
model19$aic
model20$aic
model21$aic
model22$aic
model23$aic
model24$aic

# Here we see that the AIC value of the model 18 with SARIMA(4, 0, 5)(2, 1, 0)[24] is the lowest. 
# However, since the no of datapoints are just 528, this model my be too complicated for the data 
# to be fitted. Hence we the the 2nd lowest AIC, which is model 1 with SARIMA(2, 0, 1)(1, 1, 0)[24]
print(model_new_27)

tsdiag(model_new_27)




residual_new_27 <- model_new_27$residuals

par(mfrow = c(1, 1))
plot(residual_new_27, main = "Residual plot of SARIMA(5,0,5)(0,1,2)[24]")

par(mfrow = c(2, 1))
acf(residual_new_27, main="ACF of Residual")
pacf(residual_new_27, main="PACF of Residual")

par(mfrow = c(2, 1))
acf(residual_new_27)
pacf(residual_new_27)

par(mfrow = c(1, 1))
qqnorm(residual_new_27)
qqline(residual_new_27)

# forecast18 <- forecast(model18)

print(model_new_27)

# forecast12 <- forecast(model18, h=5)

forecast_new_27 <- predict(model_new_27, n.ahead = 20, type = "both")
# forecast18 <- predict(model18, n.ahead = 12, type = "both")

par(mfrow = c(2, 1))
plot(forecast_new_27$pred)
plot(forecast_new_27$se)

sq_residual_new_27 <- residual_new_27^2

par(mfrow = c(3,1))
plot(sq_residual_new_27, type="l")
acf(sq_residual_new_27)
pacf(sq_residual_new_27)

adf.test(sq_residual_new_27)

ndiffs(sq_residual_new_27)
nsdiffs(sq_residual_new_27)

aic_results_log <- data.frame(
  p = numeric(), q = numeric(), AIC = numeric()
)
for (p in 0:5) {
  for (q in 0:5) {
    model = arima(sq_residual_new_27, order = c(p, 0, q))
    
    # Calculate the AIC score
    aic_score <- model$aic
    
    # Store the results
    aic_results_log <- rbind(aic_results_log, data.frame(
      p = p, q = q, AIC = aic_score
    ))
  }
}

print(aic_results_log)


garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 3)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))
garch_model <- ugarchfit(spec = garch_spec, data = sq_residual_new_27)
summary(garch_model)

sarima_garch_model <- ugarchfit(spec = garch_spec, data = ts_hourlyTotal, 
                                server.args = list(method = "nlminb", 
                                                   algorithm = "ARMA-GARCH"))
summary(sarima_garch_model)
forecast_sarima_garch <- ugarchforecast(sarima_garch_model, n.ahead = 100)
plot(forecast_sarima_garch)




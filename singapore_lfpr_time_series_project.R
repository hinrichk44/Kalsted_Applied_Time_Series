#This project is a univariate time series project
#We are working with the Singapore labor force participation rate
#LFPR is the ratio of people in the work force vs. available
#Typically LFPR is broken down by age

#Here is the Singapore data for LFPR
#"70.19999695 71.09999847 71.69999695 72.30000305 73.09999847 72.90000153 74.40000153 75.40000153 76 76.90000153 77.40000153 78.19999695 78.90000153 78.69999695 79 78 80 79.80000305 80.30000305 80.5 80.69999695 81.09999847 81.5 81.90000153 82.30000305 82.69999695 83.19999695 83.5"

#Here we are importing the data using scan()
#You run scan(), then copy + paste the data in the CONSOLE
#Then click Enter twice
singapore <- scan()


#Here we are converting the raw data into a time series
#Right now there is no time variable
#The ts() function will add it for us. We start at 1980
#There is no seasonality or specific cycles in this data
#If the data was quarterly, use frequency = 4
singapore <- ts(singapore, start = 1980)


#Here we are doing a simple plot of the data
plot(singapore, ylab = "Singapore LFPR")


#Here we are installing the forecast package
install.packages('forecast')
#Here we are downloading the forecast library
library(forecast)


#We are going to use a Holt Linear Trend Model
#Forecasted value = Level Value + Trend Value
#Level value is essentially a constant
#You need a smoothing parameter for both the level and trend
#The closer the parameter is to 1, the more the model relies on more recent data
#Here we are actually applying the Holt Linear Model to the Singapore data
#We are forecasting the next five years (h = 5)
holt_sing <- holt(singapore, h = 5)

#Here we are getting a summary of our forecast
#The smoothing parameters are included in the summary
#The Lo and Hi are the confidence intervals
summary(holt_sing)

#Here we are plotting our forecast
#One thing to keep in mind is the LFPR cannot exceed 100
#You must be aware of thresholds 
plot(holt_sing)


#Because the LFPR cannot exceed 100, we need to install a threshold
#The holt() + damped approach can help
#This damping parameter assumes the trend cannot be constant forever
#If parameter = 1, then basically agrees with trend
#If parameter = 0, the curve becomes flattened
#Generally recommend you set parameter between 0.8 and 0.95
#R can calculate the damped parameter for us, or we can set it manually

#Here we are plotting the damped addition
plot(holt(singapore, h = 15, damped = T))

#Here we are seeing the summary of the damped model
#R set the damped parameter (phi) to 0.9666
summary(holt(singapore, h = 15, damped = T))

#Here we are plotting the damped addition
#This time we are setting phi manually
#As you can see, the curve is getting flatter
#The prediction intervals are also wider over time
plot(holt(singapore, h = 15, damped = T, phi = 0.8))



#Next we will use an ARIMA model for the LFPR data
#ARIMA is also known as Box-Jenkins
#AR: Autoregressive, seasonality and trends are captured
#I: Integration, differencing of the dataset. Helps eliminate "chaos" in dataset
#MA: Moving Average, movement around a constant mean
#In R, P = AR, D = I, and Q = MA
#Pure AR model: ARIMA(1,0,0)
#Pure MA model: ARIMA(0,0,1)
#Remember that trends in data mean autocorrelation is present
#For the LFPR data, there is only a trend
#There is no seasonality, no MA, and then no differencing is needed


#Here we are officially applying the ARIMA model to the data
arima_sing <- auto.arima(singapore)

#Here we are seeing the summary of the ARIMA model
#It says ARIMA (1,1,0) with drift. Meaning AR and I were applied
summary(arima_sing)

#Here we are plotting the ARIMA model forecast
#We are forecasting 5 years ahead (h = 5)
plot(forecast(arima_sing, h = 5))

#Here we are doing an "exact" calculation of the ARIMA parameters
#Looks like nothing changed this time, which is good!
auto.arima(singapore, stepwise = F, approximation = F)



#autoplot() and forecast::autolayer() are improved ways of plotting forecasts
#We are going to use these functions to plot all three models we used above
#Here we set up three forecasts of 10 years
holttrend <- holt(singapore, h = 10)
holtdamp <- holt(singapore, h = 10, damped = T)
arima_sing <- forecast(auto.arima(singapore), h = 10)

#Here we are importing the ggplot2 library
library(ggplot2)

#Now we will plot the three forecasts
#The first part autoplot will plot the basic LFPR data
#The autolayers are ADDITIONS. We add the forecasts to the basic time series plot
#The $mean extracts the forecasted values from the models
autoplot(singapore) +
  forecast::autolayer(holttrend$mean, series = "Holt Linear Trend") +
  forecast::autolayer(holtdamp$mean, series = "Holt Damped Trend") +
  forecast::autolayer(arima_sing$mean, series = "ARIMA") +
  xlab("Year") +
  ylab("LFPR") +
  guides(color = guide_legend(title = "Forecast Methods")) +
  theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Singapore LFPR Forecast") +
  theme(plot.title = element_text(family = "Serif", hjust = 0.5, color = "blue",
                                  face = "bold", size = 15))



#Here we are running the same plot again, but with thicker lines
autoplot(singapore) + geom_line(size = 2) +
  forecast::autolayer(holttrend$fitted, series = "Holt Linear Trend", size = 1.1) +
  forecast::autolayer(holtdamp$fitted, series = "Holt Damped Trend", size = 1.1) +
  forecast::autolayer(arima_sing$fitted, series = "ARIMA", size = 1.1) +
  xlab("Year") +
  ylab("LFPR") +
  guides(color = guide_legend(title = "Forecast Methods")) +
  theme(legend.position = c(0.8, 0.2)) +
  ggtitle("Singapore LFPR Forecast") +
  theme(plot.title = element_text(family = "Serif", hjust = 0.5, color = "blue",
                                  face = "bold", size = 15))

  
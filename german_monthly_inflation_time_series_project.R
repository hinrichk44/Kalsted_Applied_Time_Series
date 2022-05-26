#For this project we will be reviewing inflation
#Specifically, the monthly inflation rate in Germany
#The data is from Jan 2008 to Oct 2017
#No trend is present, the mean stays constant
#There is however seasonality, so decomposition needed
#We can use Seasonal ARIMA or Exponential Smoothing
#There are negative values in the dataset
#There seems to be stable amplitudes in the data, so stable variance?

#Here we are importing the German inflation data
inflation <- scan()

library(forecast)

#Here we are converting the data to a time series
#Then we are plotting the data
plot.ts(inflation)


#Here we are officially creating a time series from our data
#We start in 2008 and go monthly (frequency = 12)
inflation <- ts(inflation, start = 2008, frequency = 12)

#Here is a plot of the data
plot(inflation)


#For our first analysis of the inflation data, we will use seasonal decomposition
#The decompose() function can help see the components of the time series
#Trend, seasonality, and remainder/white noise are broken down
#You can then choose an additive or multiplicative model
#However, there is no forecast part of it
#Some other drawbacks are first few values being null
#It is slow to catch fast rises, and it adopts a constant seasonal component
#Here we will run decompose() on our data
#We get normal data, seasonal data, trend data, and random data
decompose(inflation)

#Here we are plotting the decomposition
plot(decompose(inflation))


#Now we will use the stl() method
#STL stands for "seasonal and trend decomposition with LOESS"
#More suited towards additive model
#There are no NA values with the stl() method
#The seasonal parts can be adjusted too
#It is fairly robust towards outliers, so outliers won't dramatically alter forecast
#The window argument is the seasonal CYCLES to calculate changes in trends
#As you can see, the amplitude gets higher with more recent data
plot(stl(inflation, s.window = 7))

#Here we are performing stl forecasting
#Method = 'ets' for exponential smoothing
#Default forecast is frequency x2
#So, frequency = 12, so forecast is for 24 months
plot(stlf(inflation, method = "ets"))

#Here is a different way to use ETS forecasting
#The amplitudes are a little smaller
plot(forecast(ets(inflation), h = 24))

#Finally we will use autoplot for ETS
library(ggplot2)
autoplot(stlf(inflation, method = 'ets'))




#Now we will move on to a Seasonal ARIMA analysis of the inflation data
#Seasonal ARIMA models must have two sets of parameters
#A regular set, and a second set for the seasonal part
#ARIMA(p,d,q)(P,D,Q)[m]
#Lowercase = regular, uppercase = seasonal, m = frequency
#We need to multiply the seasonal and non-seasonal parts (Pxp, Dxd, Qxq)

#Here we are actually applying the ARIMA model to the data
#The Best Model is ARIMA(1,0,2)(0,1,1)[12]
#For non-seasonal, we have AR of order 1. I of order 0. MA of order 2
#For seasonal, we have AR order 0, I of order 1, and MA of order 1
arima_inflation <- auto.arima(inflation, stepwise = T,
                              approximation = F, trace = T)


#Here we are forecasting inflation with ARIMA modeling
inflation_forecast <- forecast(arima_inflation)
plot(inflation_forecast)


#For the last model, we will use Exponential Smoothing with ETS
#Exponential smoothing can be done with Holt-Winters Seasonal Method hw()
#Can also be done with ETS method ets()
#Error (alpha), Trend (beta), and Seasonality (gamma) all have smoothing coefficients
#Coefficient closer to 1 means model is reactive, relies heavily on recent data
#Coefficient closer to 0 means model is smoother, relies on old data too
#For ETS, model = "ZZZ"
#Z is auto-generated. If set to "A", it's additive. If set to "M" it's multiplicative. If set to "N" it's non-existent


#Here we are applying the ETS model to the data
ets_inflation <- ets(inflation)

#Here we are plotting the forecast
#We end up with model A,N,A
#Additive error, no trend, additive seasonality
plot(forecast(ets_inflation, h = 60))

#Now we are plotting Holt-Winters to use as a comparison
plot(hw(inflation, h = 60))


#We are going to use Time Series Cross-Validation
#This will help us choose which model is best
#Forecast accuracy can come from MSE (mean-squared error)
#The function tsCV() from the forecast package can be used for cross-validation
#The computation of errors might take long though
#Forecast accuracy can also be average value of all errors in whole time series


#Here we are creating our OWN function to evaluate the ETS forecast
#x = dataset, h = forecast length
forecast_ets = function(x,h)
  {forecast(ets(x), h = h)}


#Here we are creating our OWN function to evaluate the ARIMA forecast
#x = dataset, h = forecast length
forecast_arima = function(x,h)
  {forecast(auto.arima(x), 
           stepwise = T,
           approximation = F,
           h = h)}


#We are feeding the functions we created above into the tsCV() function
#It will TAKE TIME for these values to be calculated
etserror <- tsCV(inflation, forecast_ets, h = 1)
arimaerror <- tsCV(inflation, forecast_arima, h = 1)

#Here we are calculating the mean square error of each forecast
#na.rm stands for "remove NA values"
#The ARIMA model has a lower MSE than the ETS model
mean(etserror^2, na.rm = TRUE)
mean(arimaerror^2, na.rm = TRUE)

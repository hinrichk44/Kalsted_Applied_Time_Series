#Here are the main functions in R for Time Series Analysis:
#decompose() and stl() for seasonal decomposition
#arima() for arima models
#HoltWinters() for exponential smoothing
#acf() and pacf() for autocorrelation
#plot() to plot time series
#ts() to convert data into a time series


#There are also important packages to install for time series
#install.packages('forecast')
#library('forecast')

#install.packacges('ggplot2')
#library('ggplot2')


#Forecasting in R is very simple
#Function forecast() + Standard Model = forecasting
#Standard Models: ARIMA, SDC, ES, Naive, Mean
#The forecast results in the same structure regardless of model


#There is some automation in R forecasting for ARIMA models
#auto.arima() will give most suitable arima model
#You can set the complexity with 'stepwise' or 'approximation'

#Arima() is different from auto.arima()
#The upper case "A" is mandatory
#This allows for manual parameter setting
#Use acf() and pacf() plots to look for autocorrelation
#Add the lags back into the time series until no more ac


#There is also automation for exponential smoothing models
#ets() is the automated function

#ses(), hw(), holt() are manual functions


#Exponential Smoothing and ARIMA are the most popular models



#Plotting is very important in time series
#plot(), monthplot(), seasonplot() are R base functions
#autoplot(), ggmonthplot(), ggseasonplot() are from ggplot2


#Model comparison is important as well
#How do you know which model is best?
#accuracy() gets accuracy of model with train/test sets
#tsCV() is time series cross validation for smaller sets


#Data prepation helps time series run smoothly
#install.packages('quantmod')
#library('quantmod')

#install.packages('xts')
#library('xts)


#Impute missing data with na.locf()


#R Time Series Task View
#Gives you details on many time series tasks
#https://cran.r-project.org/web/views/TimeSeries.html

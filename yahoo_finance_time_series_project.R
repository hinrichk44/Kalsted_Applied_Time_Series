#For this next project we will be analyzing stock data
#We will pull this data from Yahoo Finance
#Specifically, Novartis stock (NVS)

#Here we are installing the quantmod package
install.packages('quantmod')
#Here we are importing the quantmod library
library(quantmod)


#Here we are actually pulling the data from Yahoo Finance
#The data is one year from 2015-2016
#novartis is an "xts" object, which is Extensible Time Series
#It's a multivariate object
#Data is available on trading days only. Must adjust for that
#Frequency cannot be defined and seasonality cannot be identified
#You could insert an NA value into holidays
novartis <- getSymbols(Symbols = "NVS",
                       auto.assign = F,
                       from = "2015-01-01",
                       to = "2016-01-01")


#Here we are plotting the Open column for Novartis stock
plot(as.ts(novartis$NVS.Open))


#The function chartSeries() is specific for plotting quantmod derived financial data
#You can plot candlesticks, matchsticks, and basic plots too
chartSeries(novartis, type = 'line')


#Here we are looking at the acf and pacf for the Open prices of Novartis stock
#There is clearly autocorrelation since most lags exceed the threshold
library(forecast)
ggtsdisplay(novartis$NVS.Open)


#Here we are applying an ARIMA model to our stock data
#The best model is ARIMA(0,1,1)
#"moving average on a differenced dataset"
#The data is so random that there are hardly any patterns in it
novartis_arima <- auto.arima(novartis$NVS.Open,
                             stepwise = T,
                             approximation = F,
                             trace = T)


#Here we are going to use a manual ARIMA model for our stock data
#We are adding an autoregressive part this time
novartis_arima2 <- Arima(novartis$NVS.Open, order = c(1,1,1))
novartis_arima2


#Here we are plotting the forecast of both ARIMA models
#Both forecasts do not look very sophisticated
#Large confidence intervals
plot(forecast(novartis_arima, h = 20))
plot(forecast(novartis_arima2, h = 20))


#Since the ARIMA models weren't that successful, we will try Exponential Smoothing
novartis_ets <- ets(novartis$NVS.Open)


#Here we are plotting a forecast of the ETS model
#Forecast does not look much different than ARIMA
plot(forecast(novartis_ets, h = 20))



#As stated above, stock data is not regular. There are holidays and weekends
#There are several steps we can take to make the data regular
#First we will convert the data into a dataframe
novartis <- as.data.frame(novartis)


#Here we are adding the row names as dates
novartis$Date <- rownames(novartis)
novartis$Date <- as.Date(novartis$Date)
head(novartis)


#Here we are creating a SEPARATE column for dates
#We will convert this column into a dataframe
mydates = seq.Date(from = as.Date("2015-01-01"), 
                   to = as.Date("2016-01-01"), 
                   by = 1)


#Now we are converting mydates into a dataframe as we said
#This is necessary when we merge with the Novartis data
mydates <- data.frame(Date = mydates)


#Here we are merging everything together
#We will identify the missing dates and mark them as NA (done automatically)
#all.y means all ROWS get included
fulldata <- merge(novartis, mydates, by = 'Date', all.y = T)


#Now that we have a full dataset, we must regularize it
#Must remove weekends, and then impute data for holidays
#Here we are removing the first few observations so we can start our dataset on a Monday
#Our first observation is now 01-05-2015, which was a Monday
fulldata <- fulldata[5:366, ]


#Here we are removing Sundays
#We subtract the 7th sequence from each row since 7th day = Sunday
#From = the first observation to be deleted
#To = last observation. We chose the whole dataset since we want it applied fully
#By = step size. So 7 for each 7th day (each Sunday)
#The comma at the end is used to close the operation (deleting rows not columns)
fulldata = fulldata[-(seq(from = 7, to = nrow(fulldata), by = 7)),]


#Here we are removing Saturdays
#Same as above except 6 instead of 7
#After removing both Saturday and Sunday we only have NA values for holidays
fulldata = fulldata[-(seq(from = 6, to = nrow(fulldata), by = 6)),]


#Here we are going to impute values for the NA values
#locf = "last observation carried forward"
fulldata <- na.locf(fulldata)


#Now that our data is regularized, we can apply better time series analysis
#Here we will look at the highest price instead of opening price
highest <- ts(as.numeric(fulldata$NVS.High), frequency = 5)

#Here we are doing a season plot for the highest price
#It compares week by week
#You could use ggseasonplot instead
#There does not seem to be a pattern. The highest price doesn't come on a specific dat
seasonplot(highest, season.labels = c("Mon", "Tue", "Wed", "Thu", "Fri"))

#Next we are using a month plot
#We get a line chart for each of the seasons
#Once again no real pattern emerges
monthplot(highest)

#Here we are doing a monthplot again but with some tweaks
#The default reference point is the mean. We are changing that to the median
#The highs on Th and F are slightly higher than M-W
monthplot(highest, base = median, col.base = "red")


#We will compare the highest price to the lowest price
#Here we are creating the time series for the lowest price
#In order to have two plots together you need mfrow to open
par(mfrow = c(1,2))
lowest = ts(as.numeric(fulldata$NVS.Low), 
                 frequency = 5)

#Here is a monthplot of the lowest price
#We have switched the baseline to median
monthplot(lowest, base = median, col.base = "red")
monthplot(highest, base = median, col.base = "red")
#You need to close out the figure with mfrow
par(mfrow = c(1,1))


#Finally, we will plot a seasonal decomposition of the highest price
#The y-scale is significantly different for season vs. trend
#Seasonality is tiny whereas the trend has more of an impact
plot(stl(highest, s.window = "periodic"))

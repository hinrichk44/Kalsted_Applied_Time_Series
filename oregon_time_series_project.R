#For the final project we will focus on Neural Networks
#We wil clean the data
#Real world data often needs to be cleaned prior to analysis
#Convert to numeric time series, impute missing data, control for outliers
#The data is revenue from an Oregon campsite restaurant
#The revenue is documented by month from 1997-2016
#The restaurant is open all year and peak season is the summer


#We have imported the data using Import Dataset (base) in the Global Environment
#We removed quotes
#We removed "strings as factors"
#We have 240 observations of two variables
#We have two columns, V1 and V2
#V1 seems uselss, V2 has the revenue
#We are changing the name of the dataset to "revenue" for simplicity
revenue <- oregon_time_series_data

#Here we are installing the tidyr package
#The importing the tidyr library
#R was not working so I had to find a workaround
#options(repos="https://CRAN.R-project.org")
#install.packages("tidyr")
library(tidyr)


#We have to get rid of the useless quotes in the V2 column
#The separate() function in tidyr can help us with that
#We are separating two characters FROM the left and three characters FROM the right
#V2 will then convert into three columns
#Rest will have the left characters
#Data will have the actual revenue we need
#Rest2 will have the right characters
revenue <- separate(revenue, col = V2, 
                    sep = c(2, -3), 
                    into = c("rest", "data", "rest2"))

#Here we are viewing the dataset after our initial cleaning
#We now have V1, rest, data, rest2.
#The data column looks clean
head(revenue)


#Here we are converting our dataset to a time series
#We need to convert the data column to numeric
#We will start in 1997 and go monthly (frequency = 12)
revenue_ts <- ts(as.numeric(revenue$data),
           start = 1997, frequency = 12)


#Here we will check the summary of the time series
#We can find null values and outiers here
#There are 4 null values
#The min is 3, but the max is 3334333. That's quite the discrepancy
summary(revenue_ts)


#Here we are importing the forecast library
library(forecast)

#Now we will use the tsclean() function from the forecast library
#It handles outliers and missing values in one swoop
revenue_ts <- tsclean(revenue_ts)


#Here we are checking the summary again
#There are no NA values and the min-max look within range
summary(revenue_ts)


#Here we are plotting the basic revenue time series
#There is definitely seasonality based on the amplitudes
#There is also a slightly upward trend
plot(revenue_ts)


#Lagged values are used as inputs for neural network time series
#Neural Network Autoregression Model
#NNAR(p, k) where p are the lagged values and k are hidden nodes
#NNAR(p. P, k) is a seasonal adjustment where P is a seasonal lag
#nnetar() automatically selects the right parameters for us
#Here we are converting the time series to a NNAR
revenue_nnar <- nnetar(revenue_ts)


#Here we are doing a forecast of three years with our NNAR model
#PI stands for "prediction intervals". The default is FALSE
nnarforecast <- forecast(revenue_nnar, h = 36, PI = T)


#Here we are plotting our forecast
#The model recognizes there is a seasonal pattern
#We have NNAR(3,1,2)[12]
#Uses first three observations as the input
#Plus one observation of the last seasonal cycle
#The seasonal cycle is 12
#Essentially there are four input values
#These values are compressed into two nodes in the hidden layer
library(ggplot2)
autoplot(nnarforecast)



#Here we are preparing a dataframe that we will use for our interactive time series below
#data is the vector of raw data from our forecast
data <- nnarforecast$x
#lower is the lower end of our confidence interval
#In the second column hence [,2]
lower <- nnarforecast$lower[,2]
#upper is the upper end of our confidence interval
#In the second column hence [,2]
upper <- nnarforecast$upper[,2]
#Here are the 36 months of the forecast we made above
pforecast <- nnarforecast$mean

#Now we are combining the raw data, confidence interval boundaries, and 36 months of forecast together
#This is one dataframe called "mydata
mydata <- cbind(data, lower, upper,
                pforecast)

#Here we are viewing the new dataframe
View(mydata)


#Here we are installing the dygraphs package
install.packages('dygraphs')
#Here we are importing the dygraphs library
library(dygraphs)


#Here we are making an interactive time series chart
#The first line is grabbing the dataframe and adding a title
#The %>% is a "pipe" that helps us add layers to the chart
dygraph(mydata, main = "Oregon Campsite Restaurant") %>%
  #RangeSelector allows you to zoom in on the plot
  dyRangeSelector() %>%
  #Here we are adding the time series data (from the data vector we created above)
  dySeries(name = "data", label = "Revenue Data") %>%
  #Here we are adding the confidence intervals and forecast
  dySeries(c("lower","pforecast","upper"), label = "Revenue Forecast") %>%
  #Here we are adding a legend to the chart
  dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
  #Here we are labeling the y-axis
  dyAxis("y", label = "Monthly Revenue USD") %>%
  #Here we are describing how things will be highlighted on our chart
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesOpts = list(strokeWidth = 2)) %>%
  #Here we are setting the axis and gridlines
  dyOptions(axisLineColor = "navy", gridLineColor = "grey") %>%
  #Here we are adding an annotation to a specific point in time on our chart
  dyAnnotation("2010-8-1", text = "CF", tooltip = "Camp Festival", attachAtBottom = T)
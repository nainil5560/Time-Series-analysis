# Installing the neccessary packages for the timeseries analysis
installed.packages("dplyr")
library(dplyr)
library(forecast)


#Extracting the data and gettting the summary report for the data
#setwd("/Users/nainilpatel/Desktop")
tv <- read.csv("tv_data.csv", sep = ",")
head(tv)
summary(tv)

# Retrieving the unique values of network column and daypart column. The purpose of doing this is to use this unique values of daypart and netwrok for call the specific timeseries in the function.
net <- unique(tv$network)
net
dp <- unique(tv$daypart)
dp

# Creating a fucntion arima1 with two parametres i and j, i parameter will be holding network values and j parameter will be holding daypart value.

arima1 <- function(i, j) {
  tv1 <- filter(tv, network == net[i], daypart == dp[j])
  
  # Creating a timseries with the values of the number viewers for each year and month for the specific combination of the daypart and network. 
  
  tv1_ts <- ts(tv1$viewers, start = c(2014,1), end = c(2017,12), frequency = 12)
  plot(tv1_ts)
  
  # Using Seasonal decompostion we will find the trend, seasonality and residuals. From the plot it clearly shows that their is a downward trend and also seasonal component  
  
  stl1 <- stl(tv1_ts, s.window = 12)
  plot(stl1, col = "blue", main="Seasonal Decomposition")
  
  # The seasonal decomposition shows some trend so in order to remove the downward trend we will perform 1st order differencing, and further we will use this differenced series in the Acf and Pacf plot for deciding the parameter of the ar and ma for the seasonal and non-seasonal component.  
  
  des_count <- seasadj(stl1)
  count = diff(des_count, differences = 1)
  plot(count)
  
  # Deseasonalizing the data for getting the conclusion about the general trend of viewership is down. 
  
  des <-  ts(tv1_ts - stl1$time.series[,1], start = c(2014, 01), frequency = 12)
  
  # plotting the Acf(autocorrelation) and pacf(partial correlation) with the stationary series on both mean and variance. The conclusion to be derived from this plot is to identify and AR and MA components
  
  ACF <- acf(count, ylim = c(-0.1,2), col="blue", main="ACF Decomposition")
  Pacf <- pacf(count, ylim = c(-0.1,2), col="blue", main= "PACF Decomposition")
  
  # splitting the deasonalized data into train and test set. 
  
  train <- window(des, start = c(2014,1), end = c(2016,12))
  test <- window(des, start = c(2017,1))    
  
  # Using if, else if and else we will call the network and daypart pair. The if condition executes the network A with the daypart "S,Su 6:00 AM - 8:00 AM" as we have set the paramter i is for network and j is for the daypart.
  # Then we will fit the arima and auto arima model on the train series with transforimg it with the log.
  # For fitting the arima model, the seasoanal and non seasonal paramters needs to be speicified.
  # The non seasonal paramater (p,d,q) are set in accordance with, pacf doesn't show significant lags so it AR(0) or p = 0, acf lags are significant until 4th lag so it MA(4) or q = 4 and as the the train series is not stationary and it is not differcniated so we will set d = 1,seasonal parameter (P,D,Q) are based on the trial and error method.
  
  if (i == 1 && j == 2) {
    fit_auto <- auto.arima(train)
    fit_arima <- arima(train, order = c(0,1,4), seasonal = list(order=c(1,0,0), period = 12))
    
    # Plotting the acf and pacf residuals for the fitted models, for checing the correctness. No spikes outside the blue dotted line for both autocorrelation (acf) and partial correlation (pacf) indicates that their are random residuals and model is working properly.    
    
    acf(ts(fit_arima$residuals),main='ACF Residual')
    pacf(ts(fit_auto$residuals),main='PACF Residual')
  }
  
  # The non seasonal paramater (p,d,q) are set in accordance with, pacf show significant lags till the lag 3 so it AR(3) or p = 3, acf lags are significant until 4th lag so it MA(4) or q = 4 and as the the train series is not stationary and it is not differcniated so we will set d = 1,seasonal parameter (P,D,Q) are based on the trial and error method.  
  
  else if (i == 2 && j == 5) {
    fit_auto <- auto.arima(train)
    fit_arima <- arima(train, order = c(3,1,4), seasonal = list(order=c(1,1,0), period = 12))
    
    # Plotting the acf and pacf residuals for the fitted models, for checing the correctness. No spikes outside the blue dotted line for both autocorrelation (acf) and partial correlation (pacf) indicates that their are random residuals and model is working properly.     
    
    acf(ts(fit_arima$residuals),main='ACF Residual')
    pacf(ts(fit_auto$residuals),main='PACF Residual')
    
  }
  
  # Else will execute the the auto.arima for the user specified network and daypart. User can explicitly call any daypart and network and the else will execute and give sthe auto.arima forecasting for that specific timeseries.  
  
  else {
    fit_auto <- auto.arima(train)
    forecasted_auto <- forecast(fit_auto, 12)
    print(forecasted_auto$mean)
    plot(forecasted_auto, main = "Auto-Arima")
    lines(test, col = "red")
    return(0)
  }
  
  # Displaying the coeeficents and value for the aic for arima and auto.arima model. Which is helpfull for the changing the parameters as we have to trail and error differenet value of the seasonal paramter and choose the best fit model having the lowest aic value. 
  
  print(fit_arima)
  print(fit_auto)
  
  
  # Forecasting the values of the next year by using forecast on the auto.arima model
  # Displaying the mean value of each month for the next year forecasted by the auto.arima model.
  # ploting the forecasted value of auto.arima model by the fiting the tranformed log test series data.  
  
  forecasted_auto <- forecast(fit_auto, 12)
  print(forecasted_auto$mean)
  plot(forecasted_auto, main = "Auto-Arima")
  
  # fitting the actual test value with the forecasted value
  # lines(test, col = "red")
  
  
  # Forecasting the values of the next year by using forecast on the arima model
  # Displaying the mean value of each month for the next year forecasted by the arima model sepcifying the seasonal and non-seasona parameter.
  # ploting the forecasted value of arima model by the fiting the tranformed log test series data.
  
  forecasted_arima <- forecast(fit_arima, 12)
  print(forecasted_arima$mean)
  plot(forecasted_arima, main = "Arima")
  
  # fitting the actual test value with the forecasted value
  # lines(test, col = "red")
  
  
  # Getting the mean absolute error for the forecasted and actual test series data for both arima and auto.arima model.
  
  mae_1 <- 1/12 *sum(abs(forecasted_arima$mean - test))
  print(mae_1)
  
  mae_2 <- 1/12 *sum(abs(forecasted_auto$mean - test))
  print(mae_2)
  
}

# Calling the funnction by passing the parameter. Also a new timeseries can be called explicitly by passing the paramter for the i and j.

x1 <- arima1(1, 2)
x1

x2 <- arima1(2, 5)
x2

# Conclusion:
# forecasted value shows the number of viewrs foe the next year of every month by both arima and auto.arima model.
# the auto.arima model forecasted the viwership for arima(1,2) will be in the range of 70-90 and for arima(2,5) will be in the range of 1100-1200, and the arima model forecasted the viewrship for arima(1,2) will be in the range of 80-90 and for the arima(2,5) will be in the range of 860-904.
# the Selected arima model with selectd paramters for seasonal and non-seasonal componets are accurate as they have the least aic value compared to all other model and the acf and pacf residual plots shows no significant spikes outside the blue dotted zone.

#References:
Retrieved From https://otexts.org/fpp2/seasonal-arima.html 
Retrieved From https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials 
Retrieved from http://ucanalytics.com/blogs/step-by-step-graphic-guide-to-forecasting-through-arima-modeling-in-r-manufacturing-case-study-example/
  
  
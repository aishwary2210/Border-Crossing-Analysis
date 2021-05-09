library(dplyr)
library(fpp)
library(fpp2)

# read the data in R
Border_data <- read.csv("/Users/ash/Desktop/RBS/Business Forecasting")

# filter the data data for US-Mexico Border
Border_data = filter(Border_data, Border_data$Border == 'US-Mexico Border')

# Group by date and sum the all the value
Border_data = Border_data %>% group_by(Date) %>% 
  summarise(Value = sum(Value))

# change the date format 
Border_data$Date = as.Date(Border_data$Date, format = "%m/%d/%Y ")

# sort the data by date
Border_data = arrange(Border_data,Date)

# filter the value from the data
Border_data = Border_data$Value

# change the data in a time-series data
Border_data = ts(Border_data, start = c(1996,1), end = c(2019,3), frequency = 12)

# plot the time series data 
plot(Border_data)

#x = stl(Border_data, s.window = "periodic")
#plot(x)

border_data_train = window(Border_data, start = c(1996,1), end = c(2017,12))
border_data_test = window(Border_data, start = c(2018,1), end = c(2019,3))
plot(border_data_train)
plot(border_data_test)

# Auto correlation
b = Acf(Border_data)
b

Acf(border_data_train)

# meanf method
meanf = meanf(Border_data, h=20)

plot(meanf)
print(meanf)

# naive method
naive = naive(Border_data, h = 20)
plot(naive)

# seasonal naive
seasonal_naive = snaive(Border_data, h = 20)
plot(seasonal_naive)

# drift 
drift = rwf(Border_data, h = 20, drift = T)
plot(drift)

autoplot(Border_data) +
  autolayer(meanf, series = "meanf", PI = F) +
  autolayer(naive, series = "naive", PI = F) +
  autolayer(seasonal_naive, series = "seasonal naive", PI = F) +
  autolayer(drift, series = "drift", PI = F) +
  ggtitle("Forecast for Border Data") +
  xlab("Years") + ylab("Value") + 
  guides(colour = guide_legend(title = "Forecast"))

accuracy(meanf,Border_data)

ggseasonplot(Border_data)


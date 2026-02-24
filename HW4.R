#Jamie Boyd

install.packages(c("dplyr", "lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

weather = read.csv("/cloud/project/activit04/campus_weather.csv", na.strings = "#N/A")

metaDat = read.csv("/cloud/project/activit04/meter_weather_metadata.csv", na.strings = "#N/A")

weather$date = mdy_hm(weather$Date)
weather$dateET = mdy_hm(weather$Date, tz="America/New_York")

weatherCheck = weather %>%
  filter(is.na(weather$dateET))

weather$dateET

weather$date[2] %--% weather$date[3]
int_length(weather$date[2] %--% weather$date[3])

test = weather$date[1:10]
test
test[-1]

# x is a date vector
timeCheck900 = function(x){
  intervals = x[] %--% x[-1]
  interval_times = int_length(intervals)
  intervals[interval_times != 900]
}

timeCheck900(weather$date)


soilFiles = list.files("/cloud/project/activit04/soil")
#set up variable to be used in for loop
soillist = list()

for(i in 1:length(soilFiles))
  {soillist[[i]] = read.csv(paste0("/cloud/project/activit04/soil/", soilFiles [i]))}
str(soillist) #can look at dataframe or data to understand what youre working with

soilData = do.call("rbind", soillist)
#calculate moving average
airMA = numeric()

for(i in 8:length(weather$AirTemp)){airMA[i] = mean(weather$AirTemp[(i-7):i])}

weather$airMA = airMA



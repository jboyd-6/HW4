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

#soilData$date = ymd_hm(soilData$Date)
#soilData$dateET = ymd_hm(soilData$Date, tz = "America/New_York")

airMA = numeric()

for(i in 8:length(weather$AirTemp)){airMA[i] = mean(weather$AirTemp[(i-7):i])}

weather$airMA = airMA

# parse date
weather$dateF <- mdy_hm(weather$Date)
# create a month column
weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)

# examine precipitation using a bar plot
ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=Precip))+
  geom_col(color="royalblue4")+
  theme_classic()

weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021, 
                            # evaluate if the doy is between May 1 and July 7 2021
                            NA, # value if true
                            weather$Precip) # value if false: uses original precipitation observation

weather$FreezeFlag <- ifelse(weather$AirTemp <= 0, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero



#Start of Class Activity ----


#Question 1 - Rolling Average ----

jan22 = weather %>%
  filter(year==2022 & doy<=31)

airtemp = numeric()

for(i in 8:length(jan22$AirTemp)){
  airtemp[i] = mean(jan22$AirTemp[(i-7):i], na.rm = TRUE)}

jan22$airtemprolling = airtemp

ggplot(jan22, aes(x = dateF)) + 
          geom_line(aes(y = AirTemp), color = "coral", alpha = 0.3)+
          geom_line(aes(y = airtemp), color = "royalblue", alpha = 0.3) +
          labs(x= "Date", y= "Air Temperature (Celcius)", title= "Air Temperature in January 2022")+
          theme_classic()

#Prompt 2: ----


#How to do this----
may_june21 = weather %>%
  filter(year == 2021 & doy >= 121 & doy <= 181)

ggplot(may_june21, aes(x = dateF, y = SolRad)) +
  geom_line() +
  theme_classic()

precipt = may_june21 %>%
  filter(Precip > 0)

ggplot(precipt, aes(x = dateF, y = Precip)) +
  geom_point() +
  theme_classic()

#Prompt 3 - Time Change ----

weather$date = mdy_hm(weather$Date)
weather$dateET = mdy_hm(weather$Date, tz="America/New_York")

weatherCheck = weather %>%
  filter(is.na(weather$dateET))

#The times listed are in pacific coast time, so four hours behind ET. You can switch the time and place it in New York (ET) time.

timeCheck900 = function(x){
  intervals = x[] %--% x[-1]
  interval_times = int_length(intervals)
  intervals[interval_times != 900]
}

timeCheck900(weather$date)

#The time check shows day light savings as well as other jumps in time


#Start of HW Questions ----

#Question 1 ----

#No Freezing Temp Data
weather$FreezeFlag = ifelse(weather$AirTemp <= 0, "Yes", "No")

#No difference level greater than 2 between X and Y

weather$xyflag = ifelse(weather$YLevel >= 2, "Yes", "No")

weather$xyflag = ifelse(weather$XLevel >=2, "Yes", "No")

#Dropping/Flagging variables in precip

weather$flagged_precip = weather$Precip

weather$flagged_precip[weather$FreezeFlag == "Yes"] = NA
weather$flagged_precip[weather$xyflag == "Yes"] = NA

#Total Missing Precip Data

missing_precip = sum(is.na(weather$flagged_precip))
#14290 missing values

sum(is.na(weather$Precip))
#1158 missing values

#Question 2 ---- Flag for low battery

weather$batteryflag = ifelse(weather$BatVolt <= 8.5, "Yes", "No")

#Question 3 ----

valuecheck = function(x, minvalue, maxvalue){
  ifelse(x < minvalue | x > maxvalue, "Yes", "No")}

weather$AirTempFlag = valuecheck(weather$AirTemp, -35, 40)
weather$SolRadCheck = valuecheck(weather$SolRad, 0, 1100)
#Used google to help me with values

#Question 4 ----

winter2021 = weather %>%
  filter(year == 2021, doy <= 90)

ggplot(winter2021, aes(x = dateF, y = AirTemp)) +
  geom_line(color = "royalblue") +
  labs(x = "Date",y = "Air Temperature (Degrees C)", title = "Clinton Winter Air Temperature (January to March 2021)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
#Question 5 ----

MA2021 = weather %>%
  filter(year == 2021, doy >= 60 & doy <= 120)

MA2021$precip_degree = MA2021$Precip
MA2021$AirTempF = MA2021$AirTemp * 9/5 + 32

for(i in 1:nrow(MA2021)){
  if(MA2021$AirTempF[i] < 35){
    MA2021$precip_degree[MA2021$doy == MA2021$doy[i]] = NA
    MA2021$precip_degree[MA2021$doy == MA2021$doy[i] - 1] = NA
  }
}

sum(is.na(MA2021$precip_degree))
#4124 Observations dropped

sum(!is.na(MA2021$precip_degree))
#1728 Observations Total

#Question 6----

soilFiles = list.files("/cloud/project/activit04/soil")
#set up variable to be used in for loop
soillist = list()

for(i in 1:length(soilFiles))
{soillist[[i]] = read.csv(paste0("/cloud/project/activit04/soil/", soilFiles [i]))}
str(soillist) #can look at dataframe or data to understand what youre working with

soilData = do.call("rbind", soillist)
#calculate moving average

soilData$date = ymd_hm(soilData$Timestamp)

timeCheck = function(x, interval_ammount){
  intervals = x[] %--% x[-1]
  interval_times = int_length(intervals)
  intervals[interval_times != interval_ammount]
}

# run time check for non 60 min data
timeCheck(soilData$date, 3600)





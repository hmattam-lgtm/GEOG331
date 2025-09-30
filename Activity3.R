#use install.packages to install lubridate
#install.packages(c("lubridate"))

#installing != ability to use
library(lubridate)



#create assert function.
assert <- function(statement,err.message){ 
  if(statement == FALSE){
    print(err.message)
  }
}
#Run this function
assert(1 == 2, "error: unequal values")
assert(2 == 2, "error: unequal values")
#check if vectors are same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")




#read the data

#skip (skip first 3 lines) because we don't want it to be treated as char
#na.strings (that string and spaces are to be seen as NA)
#header (don't take first line as headers for columns)
datW <- read.csv("Z:\\hmattam\\Data\\bewkes\\bewkes_weather.csv", 
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#print(datW[1,])

#read in sensor info (just those top rows)
sensorInfo <- read.csv("Z:\\hmattam\\Data\\bewkes\\bewkes_weather.csv", 
                       na.strings=c("#N/A"), nrows=2)
#print(sensorInfo)

#set column names that are more intuitive
colnames(datW) <-   colnames(sensorInfo)

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#Use lubridate
#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
#datW[1,]


#check what data is missing
length(which(is.na(datW$air.temperature))) #air temp
length(which(is.na(datW$wind.speed))) #wind speed
length(which(is.na(datW$precipitation))) #precipitation
length(which(is.na(datW$soil.moisture))) #soil temp
length(which(is.na(datW$soil.temp))) #soil moisture


#make plot for missing points for soil
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")
#make plot for missing points for air temp
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#how we can convert unreliable data into NA
#its saying if th air temp is <0 it becomes NA and goes into air temp
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,] 

#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)












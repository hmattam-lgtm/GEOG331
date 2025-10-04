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


################################################
##########         Question 5         ########## 
################################################

#shows that there is info for every data point in datW, it is just
#a subset of this data interpreting something else not in the set
assert(length(lightscale) == length(datW$DD), "error: unequal values")

################################################
################################################

#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))


################################################
##########         Question 6         ########## 
################################################

datW$wind.speed2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))


#returns true only if all the values are null in the same in both columns
#use all because without that it was trying to check every row and that gave error
#all reduces it to one true or false statement
assert(all(is.na(datW$air.tempQ2) == is.na(datW$wind.speed2)), "error: unequal values")

################################################
################################################

################################################
##########         Question 7         ########## 
################################################

## precipitation vs soil moisture

#scale soil moisture to precipitation
soilscale <- (max(datW$precipitation)/max(datW$soil.moisture, na.rm = TRUE)) * datW$soil.moisture

#where soil moisture is not NA
soilValid <- !is.na(soilscale)

#empty graph for data up to date of outage
plot(datW$DD[soilValid] , datW$precipitation[soilValid], xlab = "Day of Year", ylab = "Precipitation & Soil Moisture",
     type="n")

#plots the precipitation that is not zero on graph in blue
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plots soil moisture levels in red
points(datW$DD[soilValid], soilscale[soilValid],
       col= "tomato3", pch=19)


##soil temp vs air temp

#scale soil temp to air temp
tempscale <- (max(datW$air.temperature)/max(datW$soil.temp, na.rm = TRUE)) * datW$soil.temp

#where soil temp is not NA
soiltempValid <- !is.na(tempscale)

#empty graph for data up to date of outage
plot(datW$DD[soiltempValid] , datW$air.temperature[soiltempValid], xlab = "Day of Year", ylab = "Air temp & Soil temp",
     type="n")

#plots the air temp on graph in blue
points(datW$DD[soiltempValid], datW$air.temperature[soiltempValid],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        


#plots soil temp in red
points(datW$DD[soiltempValid], tempscale[soiltempValid],
       col= "tomato3", pch=19)

################################################
################################################



################################################
##########         Question 8         ########## 
################################################

temptot <- sum(!is.na(datW$air.tempQ2))
totsoiltemp <- sum(!is.na(datW$soil.temp))
totwindspeed <- sum(!is.na(datW$wind.speed2))
totsoilmoist <- sum(!is.na(datW$soil.moisture))
totPercip <- sum(!is.na(datW$precipitation))

start_date <- datW$timestamp[1]
end_date <- datW$timestamp[nrow(datW)]

table <- data.frame(titles = c("Air Temp", "Wind Speed", "Soil Temp", 
                               "Soil Moist", "Total Percipitation"),
  Averages = c(round(mean(datW$air.tempQ2, na.rm = TRUE),1),
  round(mean(datW$wind.speed2, na.rm = TRUE),2),
  round(mean(datW$soil.temp, na.rm = TRUE),1),
  round(mean(datW$soil.moisture, na.rm = TRUE),3),
  round(sum(datW$precipitation, na.rm = TRUE),3)),
  Observations = c(temptot,totwindspeed,totsoiltemp,totsoilmoist,totPercip)
)
print(paste("Time Period:", start_date, "to", end_date))




################################################
################################################


################################################
##########         Question 9         ########## 
################################################
x_start <- min(datW$DD)
x_end <- max(datW$DD)

plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Moisture (cm3 water per cm3 soil)", 
     main = "Soil Moisture", xlim = c(x_start,x_end))
plot(datW$DD, datW$air.tempQ2, pch=19, type="b", xlab = "Day of Year",
     ylab="Air Temp (Celsius)", 
     main = "Air Temperature", xlim = c(x_start,x_end))
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil Temperature (Celsius)", 
     main = "Soil Temperature", xlim = c(x_start,x_end))
plot(datW$DD, datW$precipitation, pch=19, type="b", xlab = "Day of Year",
     ylab="Precipitation (cm)", 
     main = "Precipitation", xlim = c(x_start,x_end))


################################################
################################################





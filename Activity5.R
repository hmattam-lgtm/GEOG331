#load in lubridate
library(lubridate)
library(dplyr)
library(ggplot2)
#read in streamflow data
datH <- read.csv("Z:\\hmattam\\Data\\hw5_data\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)              

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("Z:\\hmattam\\Data\\hw5_data\\2049867.csv")                            
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))  

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

####################################################
###########          Question 3          ###########
####################################################
tot_obs <- sum(!is.na(datH))+ sum(!is.na(datP))
tot_obs
#4198451 observations

P_freq <- table(datP$HPCP)
H_freq <- table(datH$discharge)
P_freq
H_freq

####################################################

####################################################

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

####################################################

####################################################
###########          Question 5          ###########
####################################################
par(mai=c(1,1,1,1))
#make plot

#### subset of just 2017
year_2017 <- subset(datD, year == 2017)

###getting the daily average of the discharge data
#yrD <- aggregate(year_2017$discharge, by=list(year_2017$doy), FUN="mean", na.rm = "TRUE")
#colnames(yrD) <- c("doy","dailyAve") ##changing data labels

plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,190),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border="NA" #no border
)
lines(year_2017$doy,year_2017$discharge, col = "red", lwd = 2)

##last day of month for tick interval
custom_ticks <- c(0,31,59,90,120,151,181,212,243,273,304,334,365)

#month labels
months<- c("0", "Jan", "Feb", "March", "April", "May", "June", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec" )

axis(1, at = custom_ticks, #tick intervals
     labels = months) #tick labels
axis(2, seq(0,190, by=20),
     seq(0,190, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "discharge 2017"), #legend items
       lwd=c(2,NA,2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "red"),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border
############################################################################

####################################################
###########          Question 7          ###########
####################################################
p_count <- aggregate(datP$HPCP, by=list(datP$year, datP$doy), FUN = "length")
colnames(p_count) <-c("year", "doy", "length")
## Adds a col where if the length is 24 then true nd false if not
p_count$complete <- ifelse(p_count$length == 24, TRUE, FALSE)
##merges the discharge data by matching year and doy
p_count <- merge(p_count, datD[,c("year", "doy", "discharge")],
                  by= c("year", "doy"), all.x = TRUE)
##gets individual dates to label so we can plot by day not year or doy
p_count$date <- as.Date(paste(p_count$year, p_count$doy), format = "%Y %j")

ggplot(data = p_count, mapping = aes(x = date, y = discharge, color = complete)) + 
  geom_point() + 
  theme_classic() + 
  labs(x="Date", y = "Discharge", title = "Comeplete Discharge Data")

####################################################
###########          Question 8          ###########
####################################################

#subset discharge and precipitation within range of interest
hydroD2 <- datD[datD$doy >= 13 & datD$doy < 14 & datD$year == 2012,]
hydroP2 <- datP[datP$doy >= 13 & datP$doy < 14 & datP$year == 2012,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl2 <- floor(min(hydroD2$discharge))-1
#ceiling rounds up to the integer
yh2 <- ceiling(max(hydroD2$discharge))+1
#minimum and maximum range of precipitation to plot
pl2 <- 0
pm2 <-  ceiling(max(hydroP2$HPCP))+.5
#scale precipitation to fit on the 
hydroP2$pscale <- (((yh2-yl2)/(pm2-pl2)) * hydroP2$HPCP) + yl2

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(yl2,yh2), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
  polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
            hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
          c(yl2,hydroP2$pscale[i],hydroP2$pscale[i],yl2),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

####################################################
###########          Question 9          ###########
####################################################


#take just 2016 and 17 data
years16_17 <- datD[datD$year == 2016 | datD$year == 2017,]
years16_17$Year <- as.factor(years16_17$year)

#make a col for season
years16_17$season <- NA

#use doy to assign seasons to each day
years16_17$season[years16_17$doy >= 60 & years16_17$doy <= 151] <- "Spring"
years16_17$season[years16_17$doy >= 152 & years16_17$doy <= 243] <- "Summer"
years16_17$season[years16_17$doy >= 244 & years16_17$doy <= 334] <- "Fall"
##everything w/o season is winter
years16_17$season[is.na(years16_17$season)] <- "Winter"
years16_17$season <- factor(years16_17$season, levels = c("Winter", "Spring", "Summer", "Fall"))

ggplot(data= years16_17, aes(season,discharge, color = Year)) + 
  geom_violin() + 
  labs(x = "Season", y = expression(paste("Discharge ft"^"3 ","sec"^"-1")), fill = "Year",
                       title = "Discharge by Season for 2016 and 2017")

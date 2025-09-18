#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm
heights[1]
heights[2:3]

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#subset the matrix to look at row 1, column2
Mat.bycol[1,2]
#look at all values in row 1
Mat.bycol[1,]
#look at all values in column 2
Mat.bycol[,2]

#read in weather station file from your data folder
datW <- read.csv("Z:\\hmattam\\Data\\noaa_weather\\2011124.csv", stringsAsFactors = T)
str(datW)
#specify a column with a proper date format
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#create a date column by reformatting the date to only include year
datW$year <- as.numeric(format(datW$dateF,"%Y"))

#Example vectors of each data type
test <- factor(c("midterm", "final", "midterm", "midterm", "final"))
colors <- c("maroon", "skyblue", "yellow", "lightgreen", "magenta", "pink", "violet")
exam_scores <- c(98.5,77.68,85.9,92.3,96.6)
class_size <- as.integer(c(25,27,33,35,31))
#find out all unique site names
unique(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
#exclude NAs
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)
#calculate the average daily temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)
#get the mean across all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp
#change the automatic output of column names to be more meaningful MAAT(Mean Annual Air Temperature)
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)
#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#histogram 2
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="skyblue",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#histogram 3
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="pink",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#histogram 4
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="lightyellow",
     border="white")
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#probability density
#taking original histogram and naming it to reference it
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

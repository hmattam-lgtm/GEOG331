head(iris)
install.packages(c("tidyverse"))
library(tidyverse)

#####################################
##### Part 1: for loops         #####
#####################################

#subset versicolor
flower <- iris[iris$Species == "versicolor",]

#list of relationships that we need to make regression tables with
vars <- list(flower$Sepal.Length ~ flower$Sepal.Width, 
          flower$Petal.Length ~ flower$Petal.Width, 
          flower$Sepal.Length ~ flower$Petal.Length)

#empty list to store regressions
mylist <- list()

#empty list to store regression fits
summ <- list()
for(i in 1: 3){
  mylist[[i]] <- lm(vars[[i]])
  summ[[i]] <- summary(mylist[[i]])
}

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

#this takes all the rows from iris ad adds the height info where species matches
i_height <-dplyr::left_join(iris, height)

#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
#geom adds points and aes is the data
ggplot(data = iris, mapping = aes(Sepal.Length,Sepal.Width)) + geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
#theme changes the look
ggplot(data = iris, mapping = aes(Sepal.Length,Sepal.Width)) + geom_point() + theme_classic()

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
# many variations of aes for color, size, etc and labs for labels
ggplot(data = iris, mapping = aes(Sepal.Length,Sepal.Width, colour = Species, size = Petal.Length)) + 
  geom_point() + theme_classic() + labs(x="Sepal Length (cm)", y = "Sepal Width (cm)", title = "Iris Sepal Length by Sepal Width")


#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		
##on google doc
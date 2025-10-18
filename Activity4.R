head(iris)
install.packages(c("tidyverse"))
library(tidyverse)

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
i_height <-dplyr::full_join(iris, height)

head(iris)
install.packages(c("tidyverse"))
library(tidyverse)


flower <- iris[iris$Species == "versicolor",]
vars <- c("flower$Petal.Length","flower$Petal.Length", 
          "flower$Sepal.Length","flower$Sepal.Width", 
          "flower$Sepal.Length","flower$Petal.Length")
mylist <- list()
for(i in 1: 6){
  mylist[[i]] <- lm(vars[i]~vars[i+1])
}

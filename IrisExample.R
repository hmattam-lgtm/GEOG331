rm(list = ls())

#subset the virginica species
flower <- iris[iris$Species == "virginica",]

#make a scatter plot to look at sepal length vs petal length
plot(flower$Sepal.Length, flower$Petal.Length, pch=19,
     xlab = "Sepal length", ylab = "Petal length",
     main = "Iris virginica")

#fit a regression model
fit <- lm(flower$Petal.Length ~ flower$Sepal.Length) #fit a linear model

#plot the residuals
plot(flower$Sepal.Length, summary(fit)$residuals, pch = 19,
     xlab = "Sepal Length", ylab = "Residuals")
abline(h=0)

#check normality of residuals
hist(summary(fit)$residuals, col = "red",
     main = "Residual Distribution", xlab = "Residuals")

#qqnorm or qq line can provide another visual check
qqnorm(summary(fit)$residuals, pch = 19)
qqline(summary(fit)$residuals, pch = 19)

# user shapiro wilks test to check normality
shapiro.test(summary(fit)$residuals)

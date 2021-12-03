library(readr)

# load data
data <- read_csv("Desktop/Fundamental Techniques/CarPrice_Assignment.csv")

#exploring the dataset
summary(data)

# outcome variables
class(data$price)
class(data$enginetype)

#predictor variables
class(data$horsepower)
# compute average mpg
data$average_mpg <- (data$citympg + data$highwaympg)/2
class(data$average_mpg)

# welke bodies zijn er allemaal
class(data$carbody)
unique(data$carbody)
table(data$carbody)

# frequencies table
library(psych)
describe(data)

# Distribution price
hist(data$price)

# distribution horsepower
hist(data$horsepower)

# bivariate correlation
cor(data$price, data$horsepower)

# simple linear regression
model_1 <- lm(price ~ horsepower, data = data)
summary(model_1)

min(data$horsepower)

# confidence interval
confint(model_1)

# plot simple regression
library(ggplot2)
# scatter plot
scatter_model_1 <- ggplot(data = data) + geom_point(aes(x = horsepower , y = price), color ="blue")
scatter_model_1

# regression line
smooth_model_1 <- ggplot(data = data) + geom_smooth(aes(x = horsepower, y = price), method = "lm", color = "purple")
smooth_model_1

# combination of scatter and regression line
total_model_1 <- scatter_model_1 + geom_smooth(aes(x = horsepower, y = price), method = "lm", color = "purple")
total_model_1

# multiple regression
model_2 <- lm(price ~ horsepower + average_mpg + carbody, data = data)
summary(model_2)



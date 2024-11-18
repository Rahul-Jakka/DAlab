1.	 Demonstrate data cleaning - missing values

library(tidyverse)
View(airquality)
 
which(is.na(airquality))
sum(is.na(airquality))
 
# Remove rows with NA values (without modifying the original data)
cleaned_aq <- na.exclude(airquality)

# Calculate the mean of 'Ozone' excluding NA values
ozone_mean <- mean(airquality$Ozone, na.rm = TRUE)

# Fill NA values in 'Ozone' with the calculated mean
newaq <- airquality %>% mutate(Ozone = ifelse(is.na(Ozone), ozone_mean, Ozone))

# View the modified dataset
View(newaq)

 








2.	 Implement data normalization (min-max, z-score)

arr <- c(9.5, 6.2, 8.9, 15.2, 20.0, 10.1, 5.4, 3.2, 1.0, 22.5, 10.0, 16.0)

#min-max
minarr <- min(arr)
maxarr <- max(arr)
arr2 <- arr
for (i in 1:12){
  arr2[i] = round((arr[i]-minarr)/(maxarr-minarr))
  }
print(arr2)

 

#z-score
meanarr <- mean(arr)
sdarr <- sd(arr)
for (i in 1:12){
  arr2[i] = round((arr[i]-meanarr)/sdarr, 2)
  }
print(arr2)

 






3.	 Implement attribute subset selection for data reduction

library(dplyr)
library(leaps)

View(Titanic)
Titanic = Titanic %>% na.omit()

fwd = regsubsets(Freq~., data = Titanic, nvmax = 19, method = "forward")
summary(fwd)
coef(fwd, 3)

bwd = regsubsets(Freq~., data = Titanic, nvmax = 19, method = "backward")
summary(bwd)
coef(bwd, 3)

full = regsubsets(Freq~., data = Titanic, nvmax = 19)
summary(full)
coef(full, 3)

 






4.	 Demonstrate outlier detection

#download dataset: #https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset

file_path<-"/Users/nandinimaharaj/Downloads/bike+sharing+dataset/day.csv"
day<-read.csv(file_path)
View(day)

sum(is.na(day))
boxplot(day[,c("temp","hum","windspeed")])

for(i in c("hum","windspeed"))
{
  data<-unlist(day[i])
  newData<-data[data %in% boxplot.stats(data)$out]
  data[data %in% newData]<-NA
  day[i]<-data
}

sum(is.na(data))
day<-na.exclude(day)
boxplot(day[,c("temp","hum","windspeed")])


OUTPUT:

 





5.	 Perform analytics on any standard data set

 #download dataset:  #https://github.com/datasciencedojo/datasets/blob/master/titanic.csv

titanic <- read.csv("/Users/nandinimaharaj/Downloads/titanic.csv")
library(tidyverse)

head(titanic)
sapply(titanic, class)

#Convert Sex & Survived into factor
titanic$Sex = as.factor(titanic$Sex) 
titanic$Survived = as.factor(titanic$Survived)

summary(titanic)

#Filter rows with missing values
dropnull_titanic = titanic[rowSums(is.na(titanic)) <= 0, ] 

#Splitting based on survival
survivedList = dropnull_titanic[dropnull_titanic$Survived == 1 , ] 
notSurvivedList = dropnull_titanic[dropnull_titanic$Survived == 0, ] 

#Pie chart of Survived & Not Survived
mytable <- table(titanic$Survived)
lbls <- c("Not Survived","Survived")
pie(
  mytable,
  labels = lbls,
  main = "pie chart"
)

#Histogram of Ages
hist(titanic$Age, xlab = "gender", ylab = "frequency")

#Bar plot of Gender Distribution among Non-Survivors
barplot(table(notSurvivedList$Sex), xlab = "gender", ylab = "frequency")

#Density plot of fare of Survivors
temp <- density(table(survivedList$Fare))
plot(temp, type = "n", main = "fare charged")
polygon(temp, col = "lightgray", border = "gray")

#Box plot of Fare
boxplot(titanic$Fare, main = "fare")

OUTPUT:

       





6.	 Implement linear regression

library(caTools)
data <- data.frame(
  Years_Exp = c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7),
  Salary = c(39343.00, 46205.00, 37731.00, 43525.00, 39891.00, 56642.00, 60150.00, 54445.00, 64445.00, 57189.00))

split = sample.split(data$Salary, SplitRatio = 0.7)
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

lm.r = lm(formula = Salary ~ Years_Exp, data = train)
coef(lm.r)

 

library(ggplot2)
ggplot() + 
  geom_point(aes(x = train$Years_Exp, y = train$Salary), col = 'red') +
  geom_line(aes(x = train$Years_Exp, y = predict(lm.r, data = train)), col = "blue") +
  ggtitle("salary vs experience") +
  xlab("Years of Experience") +
  ylab("Salary")
 
# Predict salaries on the test set
test_predictions <- predict(lm.r, newdata = test)

# Calculate Mean Absolute Error (MAE)
mae <- mean(abs(test$Salary - test_predictions))
print(paste("Mean Absolute Error (MAE):", mae))

  




7.	 Implement logistic regression

# Load necessary libraries
install.packages("pROC")
library(ggplot2)
library(pROC)

# Load the iris dataset
data(iris)

# Convert the Species column to a binary outcome (Setosa vs. Non-Setosa)
iris$SpeciesBinary <- ifelse(iris$Species == "setosa", 1, 0)

# Logistic regression model: Predict if the flower is Setosa based on Sepal.Length
logistic_model <- glm(SpeciesBinary ~ Sepal.Length, data = iris, family = "binomial")

# View the summary of the logistic regression model
summary(logistic_model)

 

# Predicted probabilities
iris$predicted_probabilities <- predict(logistic_model, type = "response")

# Add a column for predicted class (0 or 1) based on threshold of 0.5
iris$predicted_class <- ifelse(iris$predicted_probabilities > 0.5, 1, 0)

# Create a confusion matrix
confusion_matrix <- table(Actual = iris$SpeciesBinary, Predicted = iris$predicted_class)
print(confusion_matrix)

 

# Plot the predicted probabilities
ggplot(iris, aes(x = Sepal.Length, y = predicted_probabilities)) +
  geom_point(aes(color = factor(SpeciesBinary)), size = 3) +
  labs(x = "Sepal Length", y = "Predicted Probability of Setosa") +
  ggtitle("Logistic Regression: Predicted Probability of Setosa by Sepal Length") +
  theme_minimal()

 

# Create ROC curve and calculate AUC
roc_curve <- roc(iris$SpeciesBinary, iris$predicted_probabilities)


# Print the AUC value
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

 

# Plot the ROC curve
plot(roc_curve, main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"), col = "blue", lwd = 2)

 







8.	 Construct decision tree for weather data set


install.packages("partykit")

# Load necessary libraries
library(tidyverse)
library(partykit)
 library(caTools)

# download & load dataset:
#https://www.kaggle.com/datasets/petalme/seattle-weather-prediction-dataset

weatherdata <- read.csv("/Users/nandinimaharaj/Downloads/seattle-weather.csv")

# Inspect the dataset
head(weatherdata)
str(weatherdata)

 

# Convert 'weather' to a factor for classification
weatherdata$weather <- as.factor(weatherdata$weather)

# Split the dataset into training and testing sets (80-20 split)
split=sample.split(weatherdata$weather, SplitRatio=0.8)
train=subset(weatherdata, split==TRUE)
test=subset(weatherdata, split==FALSE)
 
# Train a decision tree model to predict 'weather'
model <- ctree(weather ~ precipitation + temp_max + temp_min + wind, data = train)

# Plot the decision tree
plot(model)

 

# Make predictions on the test set
predict_model <- predict(model, test)

# Generate a confusion matrix to evaluate model performance
mat <- table(test$weather, predict_model)
print(mat)

# Calculate the accuracy of the model
accuracy <- sum(diag(mat)) / sum(mat)
print(paste("Accuracy:", accuracy))

 




9.	 Analyze time-series data

# Load necessary libraries
library(lubridate) #converts the starting date into a decimal date
library(forecast) #use it to fit ARIMA models and make forecasts

# Data for positive cases and deaths (as weekly counts)
positiveCases <- c(580, 7813, 28266, 59287, 75700, 87820, 95314, 126214, 218843, 471497, 936851, 1508725, 2072113)

deaths <- c(17, 270, 565, 1261, 2126, 2800, 3285, 4628, 8951, 21283, 47210, 88480, 138475)

# Create a multivariate time series object
# Starting from January 22, 2020, with weekly frequency
mts <- ts(cbind(positiveCases, deaths),
          start = decimal_date(ymd("2020-01-22")),    # Date conversion to decimal format
          frequency = 365.25 / 7)          # Approximate weekly frequency

# Plot the multivariate time series data (positive cases and deaths)
plot(mts, 
     xlab = "Weekly Data", 
     main = "COVID-19 Cases", 
     col.main = "darkgreen")

 
# Create a time series object for positive cases alone
mts1 <- ts(positiveCases, 
           start = decimal_date(ymd("2020-01-22")), 
           frequency = 365.25 / 7)  # Weekly frequency

# Fit an ARIMA model to the positive cases time series
fit <- auto.arima(mts1)

# Generate forecasts for the next 5 periods (weeks)
fit_forecast <- forecast(fit, h = 5)

# Plot the forecast of positive cases for the next 5 weeks
plot(fit_forecast, 
     xlab = "Weekly Data", 
     ylab = "Positive Cases", 
     main = "COVID-19 Forecast", 
     col.main = "green")


 




10.	Work on any data visualization tool

view(airquality)
 
# Bar plot for Ozone concentration
barplot(
  airquality$Ozone,
  main = "Ozone Concentration in Air",
  xlab = "Ozone Levels",
  horiz = TRUE
)
 
# Histogram of Temperature at La Guardia Airport
hist(
  airquality$Temp,
  main = "La Guardia Airport's Maximum Temperature (Daily)",
  xlab = "Temperature (Fahrenheit)",
  xlim = c(50, 125),
  col = "yellow",
  freq = TRUE
)
 

# Box plots for selected air quality parameters
boxplot(
  airquality[, 1:4],
  main = "Box Plots for Air Quality Parameters"
)
 
# Scatter plot for Ozone concentration by Month
plot(
  airquality$Ozone, 
  airquality$Month,
  main = "Scatterplot of Ozone Concentration by Month",
  xlab = "Ozone Concentration (ppb)",
  ylab = "Month of Observation",
  pch = 19
)
 
# Creating a sample matrix and drawing a heatmap
data <- matrix(rnorm(25, 0, 5), nrow = 5, ncol = 5)
colnames(data) <- paste("Col", 1:5)
rownames(data) <- paste("Row", 1:5)
heatmap(data)
 

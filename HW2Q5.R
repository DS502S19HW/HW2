# Author: Tianyu Li
# Created on Feb 18th, 2019
#
# R script for Homework 2 Question 5(Section 4.7, page 171, question 11)
# The College.csv file should be in working direction 
rm(list = ls())
setwd('Z:/R_working_directory/DS502HW2');
library(MASS)
library(class)

# Read the file and set the random seed
ds = read.csv(file = 'Auto.csv', header = TRUE);
set.seed(2)

# Remove missing values
ds[ds == '?'] <- NA;
ds = na.omit(ds);
ds$horsepower = as.numeric(as.character(ds$horsepower));

# (a) Create variable mpg01
mpg01 = rep(0, nrow(ds))
mpg01[ds$mpg > median(ds$mpg)] = 1
ds = data.frame(ds, mpg01)

# (b) Explore data and investigate the association 
#     between "mpg01" and the other features
par(mfrow=c(1,1))
pairs(ds)

par(mfrow=c(3,2))
boxplot(cylinders ~ mpg01, data = ds, main = "Cylinders vs mpg01")
boxplot(displacement ~ mpg01, data = ds, main = "Displacement vs mpg01")
boxplot(horsepower ~ mpg01, data = ds, main = "Horsepower vs mpg01")
boxplot(weight ~ mpg01, data = ds, main = "Weight vs mpg01")
boxplot(acceleration ~ mpg01, data = ds, main = "Acceleration vs mpg01")
boxplot(year ~ mpg01, data = ds, main = "Year vs mpg01")

# (c) Split the data
index = sample(nrow(ds), nrow(ds)/2)
train = ds[index, ]
test = ds[-index, ]

# (d) Pefrome LDA
fit = lda(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration,
          data = train)
fit

pred = predict(fit, test, type = "response")
table(pred$class, test$mpg01)
mean(pred$class != test$mpg01)

# (e) Pefrome QDA
fit = qda(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration,
          data = train)
fit

pred = predict(fit, test, type = "response")
table(pred$class, test$mpg01)
mean(pred$class != test$mpg01)

# (f) Perform logistic regression
fit = glm(mpg01 ~ cylinders + displacement + horsepower + weight + acceleration,
          family = "binomial", data = train)
fit

pred = predict.glm(fit, test, type = "response")
result = rep(0, length(pred))
result[pred > 0.5] = 1
table(result, test$mpg01)
mean(result != test$mpg01)

# (g) Perform KNN
pred = knn(data.frame(train$cylinders, train$displacement, train$horsepower, train$weight, train$acceleration),
           data.frame(test$cylinders, test$displacement, test$horsepower, test$weight, test$acceleration),
           train$mpg01, k = 1)
table(pred, test$mpg01)
mean(pred != test$mpg01)

pred = knn(data.frame(train$cylinders, train$displacement, train$horsepower, train$weight, train$acceleration),
           data.frame(test$cylinders, test$displacement, test$horsepower, test$weight, test$acceleration),
           train$mpg01, k = 5)
table(pred, test$mpg01)
mean(pred != test$mpg01)

pred = knn(data.frame(train$cylinders, train$displacement, train$horsepower, train$weight, train$acceleration),
           data.frame(test$cylinders, test$displacement, test$horsepower, test$weight, test$acceleration),
           train$mpg01, k = 20)
table(pred, test$mpg01)
mean(pred != test$mpg01)

pred = knn(data.frame(train$cylinders, train$displacement, train$horsepower, train$weight, train$acceleration),
           data.frame(test$cylinders, test$displacement, test$horsepower, test$weight, test$acceleration),
           train$mpg01, k = 100)
table(pred, test$mpg01)
mean(pred != test$mpg01)

acc_knn = function(k) {
  pred = knn(data.frame(train$cylinders, train$displacement, train$horsepower, train$weight, train$acceleration),
             data.frame(test$cylinders, test$displacement, test$horsepower, test$weight, test$acceleration),
             train$mpg01, k = k)
  return (mean(pred != test$mpg01))
}

x = vector()
y = vector()
for(i in 1:100) {
  x[i] = i
  y[i] = acc_knn(i)
}

par(mfrow=c(1,1))
plot(x, y, type = "b", xlab = "k", ylab = "error rate", main = "Error rate over K")


# Author: Tianyu Li
# Created on Feb 18th, 2019
#
# R script for Homework 2 Question 4(Section 4.7, page 171, question 10)
# The College.csv file should be in working direction 
rm(list = ls())
setwd('Z:/R_working_directory/DS502HW2');
library(MASS)
library(class)

# Read the file and set the random seed
ds = read.csv(file = 'weekly.csv', header = TRUE);
set.seed(1)

# (a) Produce some numerical and graphical summaries of the Weekly data
summary(ds)
pairs(ds)

# (b) Perform a logistic regression with Direction over Lags and Volume
fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
          family = "binomial", data = ds)
summary(fit)

# (c) Compute the confusion matrix
pred = predict(fit, type = "response")
result = rep("Down", length(pred))
result[pred > 0.5] = "Up"
table(result, ds$Direction)

# (d) Perform a logistic regression with Lag2 as the predictor
fit = glm(Direction ~ Lag2, family = "binomial",
          data = ds, subset = Year < 2009)
fit

pred = predict.glm(fit, subset(ds, Year >= 2009), type = "response")
result = rep("Down", length(pred))
result[pred > 0.5] = "Up"
d_table = table(result, subset(ds, Year >= 2009)$Direction)
d_table

# (e) Perform a LDA with Lag2 as the predictor
fit = lda(Direction ~ Lag2, data = ds, subset = Year < 2009)
fit

pred = predict(fit, subset(ds, Year >= 2009), type = "response")
e_table = table(pred$class, subset(ds, Year >= 2009)$Direction)
e_table

# (f) Perform a QDA Lag2 as the predictor
fit = qda(Direction ~ Lag2, data = ds, subset = Year < 2009)
fit

pred = predict(fit, subset(ds, Year >= 2009), type = "response")
f_table = table(pred$class, subset(ds, Year >= 2009)$Direction)
f_table

# (g) Perform a KNN with Lag2 as the predictor
pred = knn(data.frame(subset(ds, Year < 2009)$Lag2),
          data.frame(subset(ds, Year >= 2009)$Lag2),
          subset(ds, Year < 2009)$Direction, k = 1)
g_table = table(pred, subset(ds, Year >= 2009)$Direction)
g_table

# (h) Compare results
# Helper function to calculate the accuracy from the confusion matrix
accuracy = function(table) {
  result = (table[1, 1] + table[2, 2]) / sum(table)
  return (result)
}

accuracy(d_table)
accuracy(e_table)
accuracy(f_table)
accuracy(g_table)

# (i) Experiments
# logistic Regression
fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
          family = "binomial", data = ds, subset = Year < 2009)
pred = predict.glm(fit, subset(ds, Year >= 2009), type = "response")
result = rep("Down", length(pred))
result[pred > 0.5] = "Up"
glm1 = table(result, subset(ds, Year >= 2009)$Direction)

fit = glm(Direction ~ Lag1 * Lag2 * Lag3 * Lag4 * Lag5 + Volume,
          family = "binomial", data = ds, subset = Year < 2009)
pred = predict.glm(fit, subset(ds, Year >= 2009), type = "response")
result = rep("Down", length(pred))
result[pred > 0.5] = "Up"
glm2 = table(result, subset(ds, Year >= 2009)$Direction)

# LDA
fit = lda(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
          data = ds, subset = Year < 2009)
pred = predict(fit, subset(ds, Year >= 2009), type = "response")
lda1 = table(pred$class, subset(ds, Year >= 2009)$Direction)

fit = lda(Direction ~ Lag1 * Lag2 * Lag3 * Lag4 * Lag5 + Volume,
          data = ds, subset = Year < 2009)
pred = predict(fit, subset(ds, Year >= 2009), type = "response")
lda2 = table(pred$class, subset(ds, Year >= 2009)$Direction)

# QDA
fit = qda(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
          data = ds, subset = Year < 2009)
pred = predict(fit, subset(ds, Year >= 2009), type = "response")
qda1 = table(pred$class, subset(ds, Year >= 2009)$Direction)

fit = qda(Direction ~ Lag1 * Lag2 * Lag3 * Lag4 * Lag5 + Volume,
          data = ds, subset = Year < 2009)
pred = predict(fit, subset(ds, Year >= 2009), type = "response")
qda2 = table(pred$class, subset(ds, Year >= 2009)$Direction)

#KNN
pred = knn(data.frame(subset(ds, Year < 2009)$Lag2),
           data.frame(subset(ds, Year >= 2009)$Lag2),
           subset(ds, Year < 2009)$Direction, k = 10)
knn1 = table(pred, subset(ds, Year >= 2009)$Direction)

pred = knn(data.frame(subset(ds, Year < 2009)$Lag2),
           data.frame(subset(ds, Year >= 2009)$Lag2),
           subset(ds, Year < 2009)$Direction, k = 50)
knn2 = table(pred, subset(ds, Year >= 2009)$Direction)

accuracy(glm1)
accuracy(glm2)
accuracy(lda1)
accuracy(lda2)
accuracy(qda1)
accuracy(qda2)
accuracy(knn1)
accuracy(knn2)
accuracy(d_table)
accuracy(e_table)
accuracy(f_table)
accuracy(g_table)


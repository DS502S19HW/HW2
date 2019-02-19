# Author: Tianyu Li
# Created on Feb 18th  2019
#
# R script for Homework 2 Question 4(Section 4.7, page 171, question 10)
# The College.csv file should be in working direction 
setwd('Z:/R_working_directory/DS502HW2');

# Read the file
ds = read.csv(file = 'weekly.csv', header = TRUE);

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
summary(fit)

pred = predict.glm(fit, subset(ds, Year >= 2009), type = "response")
result = rep("Down", length(pred))
result[pred > 0.5] = "Up"
table(result, subset(ds, Year >= 2009)$Direction)

# (e) Perform a LDA with Lag2 as the predictor
library(MASS)
fit = lda(Direction ~ Lag2, data = ds, subset = Year < 2009)
fit

pred = predict(fit, subset(ds, Year >= 2009), type = "response")
table(pred$class, subset(ds, Year >= 2009)$Direction)

# (f) Perform a QDA Lag2 as the predictor
fit = qda(Direction ~ Lag2, data = ds, subset = Year < 2009)
fit

pred = predict(fit, subset(ds, Year >= 2009), type = "response")
table(pred$class, subset(ds, Year >= 2009)$Direction)

# (g) Perform a KNN with Lag2 as the predictor
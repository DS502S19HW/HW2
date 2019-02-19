# Author: Tianyu Li
# Created on Feb 18th, 2019
#
# R script for Homework 2 Question 8(Section 5.4, page 198, question 5)
# The College.csv file should be in working direction 
rm(list = ls())
setwd('Z:/R_working_directory/DS502HW2');

# Read the file and set the random seed
ds = read.csv(file = 'default.csv', header = TRUE);
set.seed(3)

# (a) Fit a logistic regression model that uses income and balance
#     to predict default
fit = glm(default ~ income + balance, family = binomial, data = ds)
summary(fit)

# (b) Using the validation set approach, estimate the test error of this model
# i. Split the sample set
train = sample(nrow(ds), nrow(ds)/2)

# ii. Fit with training set
train_fit = glm(default ~ income + balance, family = binomial,
                data = ds, subset = train)
summary(train_fit)

# iii. Validate the fit with testing set and classify with 
# if the posterior probability is greater than 0.5.
pred = predict.glm(train_fit, newdata = ds[-train, ], type = 'response')
result = rep("No", length(pred))
result[pred > 0.5] = "Yes"

# iv Compte the validation set error
mean(result != ds[-train, ]$default)

# (c) Repeat the process in (b) three times
# Define the process as a function
error = function(){
  train = sample(nrow(ds), nrow(ds)/2)
  train_fit = glm(default ~ income + balance, family = binomial,
                  data = ds, subset = train)
  summary(train_fit)
  pred = predict.glm(train_fit, newdata = ds[-train, ], type = 'response')
  result = rep("No", length(pred))
  result[pred > 0.5] = "Yes"
  
  error = mean(result != ds[-train, ]$default)
  return(error)
}

# Repeat the function three times
error()
error()
error()

# (d) Add a dummy variable for student and test
error2 = function(){
  train = sample(nrow(ds), nrow(ds)/2)
  train_fit = glm(default ~ income + balance + student, family = binomial,
                  data = ds, subset = train)
  summary(train_fit)
  pred = predict.glm(train_fit, newdata = ds[-train, ], type = 'response')
  result = rep("No", length(pred))
  result[pred > 0.5] = "Yes"
  
  error = mean(result != ds[-train, ]$default)
  return(error)
}

error2()
error2()
error2()


# Author: Tianyu Li
# Created on Feb 18th, 2019
#
# R script for Homework 2 Question 9(Section 5.4, page 199, question 6)
# The College.csv file should be in working direction 
rm(list = ls())
setwd('Z:/R_working_directory/DS502HW2');
library(boot)

# Read the file and set the random seed
ds = read.csv(file = 'default.csv', header = TRUE);
set.seed(4)

# (a) Determine the estimated standard errors
fit = glm(default ~ income + balance, family = "binomial",
          data = ds)
summary(fit)

# (b) Write boot function
boot_fn = function(data, index) {
  fit = glm(default ~ income + balance, family = "binomial",
            data = data, subset = index)
  return (coef(fit))
}

# (c) test with bot function
boot(ds, boot_fn, 1000)

# (d) Comments
# It looks like the estimated standard errors obtained by
# these two methods are pretty close

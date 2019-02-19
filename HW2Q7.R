# Author: Tianyu Li
# Created on Feb 18th, 2019
#
# R script for Homework 2 Question 7(Section 5.4, page 198, question 8)
rm(list = ls())

#(g) Create a plot on probablities of n = 1 to 10000
x = vector()
y = vector()

for(n in 1:10000) {
  x[n] = n
  y[n] = 1 - ((1 - 1/n) ^ n)
}

par(mfrow=c(1,2))
plot(x, y, xlab = "n", ylab = "probablity",
     main = "probablities of n = 1 to 10000")
plot(x[1000:10000], y[1000:10000], xlab = "n", ylab = "probablity",
     main = "probablities of n = 1000 to 10000")
# The probablity that that the jth observation is in the bootstrap sample
# is decreasing as the size of observation n increasing. And it looks like
# The probablity converges to around 0.63

# (h) Probablity on j = 4 by bootstrap samples
for(i in 1:10) {
store = rep(NA, 10000)
  for(n in 1:10000) {
    store[n] = sum(sample(1:100, rep = TRUE) == 4) > 0
  }
  mean[i] = mean(store)
}
mean
# The probablity is about 0.63, which shows that the result
# we got on theory is correct


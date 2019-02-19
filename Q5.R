#(a)solution
Auto=read.csv("C:/WPI/DS502Statistical Methods for data science/HW/HW1/Auto.csv")
View(Auto)
summary(Auto)
attach(Auto)
mpg01=rep(0, length(mpg))
mpg01[mpg > median(mpg)]=1
Auto=data.frame(Auto, mpg01)
##use the data.frame() function to create a single data set containing both “mpg01” and the other “Auto” variables

#(b)solution
##Remove missing values
Auto[Auto == '?']=NA
Auto = na.omit(Auto)
Auto$horsepower = as.numeric(as.character(Auto$horsepower))
##Compute the matrix of correlations 
cor(Auto[,-9])
##Produce a scatterplot matrix
pairs(Auto)
##Produce a boxplot related to Cylinders vs mpg01 
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01")
##Produce a boxplot related to displacement vs mpg01
boxplot(displacement ~ mpg01, data = Auto, main = "displacement vs mpg01")
##Produce a boxplot related to horsepower vs mpg01
boxplot(horsepower ~ mpg01, data = Auto, main = "horsepower vs mpg01")
##Produce a boxplot related to weight vs mpg01
boxplot(weight ~ mpg01, data = Auto, main = "weight vs mpg01")
##Produce a boxplot related to acceleration vs mpg01
boxplot(acceleration ~ mpg01, data = Auto, main = "acceleration vs mpg01")
##Produce a boxplot related to year vs mpg01
boxplot(year ~ mpg01, data = Auto, main = "year vs mpg01")
## Except the categorical data in terms of origin and name,
##From the boxplots, we can tell that cylinders, weight, displacement and horsepower are most associated to mpg01
##While, acceleration and year fail to see any association with mpg01.

#(c)solution
##Split the data into a training set and a test set
train=(year %% 2 == 0)
Auto.train=Auto[train, ]
Auto.test=Auto[!train, ]
mpg01.test=mpg01[!train]

#(d)solution---LDA
##According to the result of (b)cylinders, weight, displacement and horsepower are most associated to mpg01
library(MASS)
fit.lda=lda(mpg01~cylinders+weight+displacement+horsepower,data =Auto,subset=train)
fit.lda
pred.lda=predict(fit.lda, Auto.test)
table(pred.lda$class, mpg01.test)
mean(pred.lda$class!= mpg01.test)
##The test error rate by LDA method is 27.7%.

#(e)solution---QDA
fit.qda=qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.qda
pred.qda=predict(fit.qda, Auto.test)
table(pred.qda$class, mpg01.test)
mean(pred.qda$class!=mpg01.test)
##The test error rate by QDA method is 29.3%.

#(f)solution---Logistic Regression
fit.glm= glm(mpg01~cylinders+ weight+ displacement+horsepower, data = Auto, family= binomial, subset= train)
summary(fit.glm)
probs= predict(fit.glm, Auto.test, type = "response")
pred.glm= rep(0, length(probs))
pred.glm[probs > 0.5]= 1
table(pred.glm, mpg01.test)
mean(pred.glm!= mpg01.test)
##The test error rate by Logistic Regression method is 28.8%.

#(g)solution---KNN
#assume K==1
library(class)
train.X=cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X=cbind(cylinders, weight, displacement, horsepower)[!train, ]
train.mpg01=mpg01[train]
set.seed(1)
pred.knn=knn(train.X, test.X, train.mpg01, k =1)
table(pred.knn, mpg01.test)
mean(pred.knn!=mpg01.test)
##the test error rate for K=1 is nearly 14.1%

#assume K=10
train.X=cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X=cbind(cylinders, weight, displacement, horsepower)[!train, ]
train.mpg01=mpg01[train]
set.seed(1)
pred.knn1=knn(train.X, test.X, train.mpg01, k =10)
table(pred.knn1, mpg01.test)
mean(pred.knn1!=mpg01.test)
##the test error rate for K=10 is nearly 16.3%

#assume K=100
train.X=cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X=cbind(cylinders, weight, displacement, horsepower)[!train, ]
train.mpg01=mpg01[train]
set.seed(1)
pred.knn2=knn(train.X, test.X, train.mpg01, k =100)
table(pred.knn2, mpg01.test)
mean(pred.knn2!=mpg01.test)
##the test error rate for K=10 is nearly 16.8%
##Among testing with different K-value, when K=1, the test error rate is the lowest
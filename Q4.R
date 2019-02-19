library(ISLR)
summary(Weekly)
#(a)solution
cor(Weekly[,-9])
attach(Weekly)
plot(Volume)
##From (a) we can tell that the correlations between the lag variables and
##today's returns are close to zero. In other word, there appears to be 
##little correlation between today's returns and previous days' returns.
##The only substantial correlation is between Year and Volume.
##By plotting the data we see that Volume is increasing over time.

#(b)solution
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Weekly,family = binomial)
summary(glm.fits) 
##From (b), we can tell that Lag2 with the smallest P-value,
##which could be the only predictor in this case significantly.

#(c)solution
glm.probs=predict(glm.fits,type = "response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",length(glm.probs))
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
mean(glm.pred==Direction)
##(54+557)/(54+557+430+48)=0.5610652 
##The percentage of correct predictions on the training data is nearly 56.1%,
##On the opposite, the training error rate could be 100%-56.1%=43.9%, which is often  overly optimistic.
##Or we could also say that for weeks when the market goes up, the model is right 557/(48+557)=92.1%.
##For weeks when the market goes down, the model is right only 54/(54+430)=11.1%.

#(d)solution
train=(Year<2009)
Weekly.2009=Weekly[!train,]
dim(Weekly.2009)
Direction.2009=Direction[!train]
glm.fit2=glm(Direction~Lag2,data = Weekly,family = binomial,subset=train)
summary(glm.fit2)
glm.probs2=predict(glm.fit2,Weekly.2009,type="response")
glm.pred2=rep("Down",length(glm.probs2))
glm.pred2[glm.probs2>.5]="Up"
table(glm.pred2,Direction.2009)
##The percentage of correct predictions on the training data is nearly 62.5%,
##On the opposite, the training error rate could be 100%-62.5%=37.5%.
##Or we could also say that for weeks when the market goes up, the model is right 56/(56+5)=91.8%.
##For weeks when the market goes down, the model is right only 9/(9+34)=20.9%.

#(e)solution
library(MASS)
fit.lda=lda(Direction~Lag2,data = Weekly,family = binomial,subset=train)
fit.lda
pred.lda=predict(fit.lda,Weekly.2009)
table(pred.lda$class,Direction.2009)
##The results of LDA are very close to those obtained with the logistic regression model.

#(f)solution
fit.qda=qda(Direction~Lag2,data = Weekly,family = binomial,subset=train)
fit.qda
pred.qda=predict(fit.qda,Weekly.2009)
table(pred.qda$class,Direction.2009)
##The results of QDA are different from the results getting from LDA and Logistic Regression;
##The percentage of correct predictions on the training data is nearly 58.7%,
##On the opposite, the training error rate could be 100%-58.7%=41.3%.
##Or we could also say that for weeks when the market goes up, the model is right 100%.
##For weeks when the market goes down, the model is right 0%;
##In this case, QDA achieves a correctness of 58.7%, though the model shows an absolute trade when the market goes up

#(g)solution
library(class)
train.X=as.matrix(Lag2[train])
test.X=as.matrix(Lag2[!train])
train.Direction=Direction[train]
set.seed(1)
pred.knn=knn(train.X, test.X, train.Direction, k = 1)
table(pred.knn, Direction.2009)
##The results of KNN are different from the results getting from LDA, QDA and Logistic Regression; 
##The percentage of correct predictions on the training data is nearly 50%%,
##On the opposite, the training error rate could be 100%-50%=50%.
##Or we could also say that for weeks when the market goes up, the model is right 50.8%.
##For weeks when the market goes down, the model is right 48.8%.

#(h)solution
##As an conclusion, within different methods, the Logistic Regression and LDA have the lowest error rates,
##which means those two method in this case could provide the best results.

#(i)solution
###logistic regression cross Lag1 and Lag2
glm.fit3=glm(Direction~Lag1:Lag2,data = Weekly,family = binomial,subset=train)
glm.probs3=predict(glm.fit3,Weekly.2009,type="response")
glm.pred3=rep("Down",length(glm.probs3))
glm.pred3[glm.probs3>.5]="Up"
table(glm.pred3,Direction.2009)
mean(glm.pred3==Direction.2009)
###logistic regression Lag1 plus Lag2
glm.fit4=glm(Direction~Lag1+Lag2,data = Weekly,family = binomial,subset=train)
glm.probs4=predict(glm.fit4,Weekly.2009,type="response")
glm.pred4=rep("Down",length(glm.probs4))
glm.pred4[glm.probs4>.5]="Up"
table(glm.pred4,Direction.2009)
mean(glm.pred4==Direction.2009)
###LDA cross Lag1 and Lag2
fit.lda1=lda(Direction~Lag1:Lag2,data = Weekly,family = binomial,subset=train)
fit.lda1
pred.lda1=predict(fit.lda1,Weekly.2009)
table(pred.lda1$class,Direction.2009)
mean(pred.lda1$class == Direction.2009)
###LDA Lag1 plus Lag2
fit.lda2=lda(Direction~Lag1+Lag2,data = Weekly,family = binomial,subset=train)
fit.lda2
pred.lda2=predict(fit.lda1,Weekly.2009)
table(pred.lda2$class,Direction.2009)
mean(pred.lda2$class == Direction.2009)
###QDA with cross Lag1 and Lag2
fit.qda1=qda(Direction~Lag1:Lag2,data = Weekly,family = binomial,subset=train)
fit.qda1
pred.qda1=predict(fit.qda1,Weekly.2009)
table(pred.qda1$class,Direction.2009)
mean(pred.qda1$class == Direction.2009)
###QDA with Lag1 plus Lag2
fit.qda2=qda(Direction~Lag1+Lag2,data = Weekly,family = binomial,subset=train)
fit.qda2
pred.qda2=predict(fit.qda2,Weekly.2009)
table(pred.qda2$class,Direction.2009)
mean(pred.qda2$class == Direction.2009)
###KNN with K=10 when Lag 2 is the only predictor
train.X1=as.matrix(Lag2[train])
test.X1=as.matrix(Lag2[!train])
train.Direction1=Direction[train]
pred.knn1=knn(train.X1, test.X1, train.Direction1, k = 10)
table(pred.knn1, Direction.2009)
mean(pred.knn1==Direction.2009)
###KNN with K=100 when Lag 2 is the only predictor
train.X2=as.matrix(Lag2[train])
test.X2=as.matrix(Lag2[!train])
train.Direction2=Direction[train]
pred.knn2=knn(train.X2, test.X2, train.Direction1, k = 100)
table(pred.knn2, Direction.2009)
mean(pred.knn2==Direction.2009)
## Conclusion: In this case, after the sensitive analysis, in terms of test error rate,
##the results of logistic Regression and LDA still better than other method;
## We also found that when K value is increasing, the test error rate is also increasing.
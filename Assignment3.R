
############################
#### QUESTION 1 Part a ####
############################
rm(list=ls())
set.seed(5072)


installIfAbsentAndLoad  <-  function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
needed   <-   c("ISLR", "MASS", "corrplot", "class")      #MASS contains the qda() function
installIfAbsentAndLoad(needed)
############################
#### QUESTION 1 Part b ####
############################
library(ISLR)
# response direction 5 lags plus volume as predictors
glm.fitd <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.fitd)
print("Lag2 is the only predictor whose p-value < 0.05, which means Lag2 is statistically significant.")

############################
#### QUESTION 1 Part c ####
############################
glm.pred <- rep('Down', nrow(Weekly)) 
glm.pred[predict(glm.fitd,type='response') > .5] <- 'Up' #find no & yes
fitd.table <- table(Weekly$Direction,glm.pred)
fitd.table

contrasts(Weekly$Direction) 

############################
#### QUESTION 1 Part d ####
############################
# Overall error rate again, this time from the table
print(paste('Error Rate:',(fitd.table["Down","Up"]+fitd.table["Up","Down"])/sum(fitd.table)))
# Type I Error Rate
print(paste('Type I Error Rate:',(fitd.table["Down","Up"]/sum(fitd.table["Down",]))))
# Type II Error Rate
print((fitd.table["Up","Down"]/sum(fitd.table["Up",])))
# Power: 
1-(fitd.table["Up","Down"]/sum(fitd.table["Up",]))
# Precision of the model - correct rate
print((fitd.table["Down","Down"]+fitd.table["Up","Up"])/sum(fitd.table))

############################
#### QUESTION 1 Part e ####
############################
#Lag2 only
# since the data is from 1980 to 2010 set train.data
train.data <- Weekly$Year<2009
head(train.data)                                
tail(train.data)
# Weekly.2009 is a data frame of  test rows
Weekly.2009 <- Weekly[!train.data, ] 
dim(Weekly.2009)
# Direction.2009 is a vector of "Up" and "Down" in the test
# data frame
Direction.2009 <- Weekly$Direction[!train.data]  
# Build a Classification model using the test set (note the
# addition of the last paramater "subset="
glm.Lab2 <- glm(Direction~Lag2, data=Weekly, family=binomial, subset=train.data)

############################
#### QUESTION 1 Part f ####
############################
# Use the model to create test set predictions
prob.Lab2 <- predict(glm.Lab2, Weekly.2009, type="response")
# compute the test error rate
pred.Lab2 <- rep("Down", nrow(Weekly.2009))
pred.Lab2
pred.Lab2[prob.Lab2>.5] <- "Up"
mean(pred.Lab2==Direction.2009)
mean(pred.Lab2!=Direction.2009)
# Create a confusion matrix for the test set and compute the
mytesttable <- table(Direction.2009, pred.Lab2)
mytesttable

# 5 porformance statistics
# test error rate
print((mytesttable["Down","Up"]+mytesttable["Up","Down"])/sum(mytesttable))
# Type I Error Rate
print((mytesttable["Down","Up"]/sum(mytesttable["Down",])))
# Type II Error Rate
print((mytesttable["Up","Down"]/sum(mytesttable["Up",])))
# Power: 
1-(mytesttable["Up","Down"]/sum(mytesttable["Up",]))
# Precision of the model - correct rate
print((mytesttable["Down","Down"]+mytesttable["Up","Up"])/sum(mytesttable))

############################
#### QUESTION 1 Part g ####
############################
# use LDA - Linear Discriminant Analysis
library(MASS)
prob.lda.lab2 <- lda(Direction ~ Lag2, data = Weekly, subset = train.data)
prob.lda.lab2

# create a confusion  matrix for LDA.LAB2 
pred.lda.lab2 <- predict(prob.lda.lab2, Weekly.2009)
lda.lab2table <- table(Direction.2009,pred.lda.lab2$class)
lda.lab2table 
# test error rate
print((lda.lab2table["Down","Up"]+lda.lab2table["Up","Down"])/sum(lda.lab2table))
# Type I Error Rate
print((lda.lab2table["Down","Up"]/sum(lda.lab2table["Down",])))
# Type II Error Rate
print((lda.lab2table["Up","Down"]/sum(lda.lab2table["Up",])))
# Power: 
1-(lda.lab2table["Up","Down"]/sum(lda.lab2table["Up",]))
# Precision of the model - correct rate
print((lda.lab2table["Down","Down"]+lda.lab2table["Up","Up"])/sum(lda.lab2table))

############################
#### QUESTION 1 Part g ####
############################
# USE QDA 
prob.qda.lab2 <- qda(Direction ~ Lag2, data = Weekly, subset = train.data)
prob.qda.lab2

# create a confusion  matrix for qda.LAB2 
pred.qda.lab2 <- predict(prob.qda.lab2, Weekly.2009)
qda.lab2table <- table(Direction.2009,pred.qda.lab2$class)
qda.lab2table 
# test error rate
print((qda.lab2table["Down","Up"]+qda.lab2table["Up","Down"])/sum(qda.lab2table))
# Type I Error Rate
print((qda.lab2table["Down","Up"]/sum(qda.lab2table["Down",])))
# Type II Error Rate
print((qda.lab2table["Up","Down"]/sum(qda.lab2table["Up",])))
# Power: 
1-(qda.lab2table["Up","Down"]/sum(qda.lab2table["Up",]))
# Precision of the model - correct rate
print((qda.lab2table["Down","Down"]+qda.lab2table["Up","Up"])/sum(qda.lab2table))

############################
#### QUESTION 1 Part i ####
############################
#Fit KNN
#The knn() function requires 4 parameters:
#   1. A matrix containing the predictors associated with the training data, here train.lag2
#   2. A matrix containing the predictors associated with the data for which we wish to make predictions, here test.lag2
#   3. A vector containing the class labels for the training observations, here train.Direction
#   4. A value for K, the number of nearest neighbors to be used by the classifier.
train.lag2 <- as.matrix(Weekly$Lag2[train.data])
head(train.lag2)
nrow(train.lag2)
test.lag2 <- as.matrix(Weekly$Lag2[!train.data])
head(test.lag2)
nrow(test.lag2)
train.Direction <- Weekly$Direction[train.data]
head(train.Direction)
length(train.Direction)
#Fit a knn model with k=1 to k =99 odd-number values
knn.pred <- knn(train.lag2, test.lag2, train.Direction, k=1)
mytable <- table(Direction.2009, knn.pred)
mytable

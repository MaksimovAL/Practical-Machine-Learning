setwd(file.path(getwd(),"Practical Machine Learning"))
#install.packages("AppliedPredictiveModeling")
#install.packages("caret")
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(data.table)
#install.packages("e1071")
set.seed(125)
train <- data.table(segmentationOriginal[segmentationOriginal$Case=="Train",])
library(e1071)
firstree <- train(Class ~. , method="rpart", data=train)
print(firstree$finalModel)
#install.packages("rattle")
library("rattle")
#install.packages("rpart.plot")
library("rpart.plot")
fancyRpartPlot(firstree$finalModel)

predict(firstree,a)

a  TotalIntench2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1=2)
b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100
c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100
d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 

#Q2 - Theoretic: as a reference use the Cross validation lecture in week 1

#Q3
#install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))
set.seed(125)
secondtree <- train(Area ~. , method="rpart", data=olive)
predict(secondtree,newdata = as.data.frame(t(colMeans(olive))))

#Q4
#install.packages("ElemStatLearn")
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
logistic <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl , method="glm", family="binomial", data=trainSA)

missClass = function(values,prediction){
        sum(((prediction > 0.5)*1) != values)/length(values)
        }
predtest <- predict(logistic, newdata=testSA)
predtrain <- predict(logistic, newdata=trainSA)
missClass(testSA$chd,predtest)
missClass(trainSA$chd,predtrain)




#Q5
library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
# Set the variable y to be a factor variable in both the training
# and test set. Then set the seed to 33833. Fit a random forest 
# predictor relating the factor variable y to the remaining 
# variables.
# The caret package uses by defualt the Gini importance. 
# Calculate the variable importance using the varImp function 
# in the caret package. What is the order of variable importance? 
vowel.test[,1] <- as.factor(vowel.test[,1]) 
vowel.train[,1] <- as.factor(vowel.train[,1]) 
set.seed(33833)
randomforest <- train(y ~ ., method="rf", data=vowel.train)
fm <- randomforest$finalModel
order(rank(varImp(fm)))

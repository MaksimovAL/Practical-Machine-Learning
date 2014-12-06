#Q1

library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- as.factor(vowel.train$y) 
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
library(caret)
library(randomForest)
rf <- train(y ~ ., data=vowel.train, method = "rf")
gbm <- train(y ~ ., data=vowel.train, method = "gbm")
#in-sample accuracy
model <- c("Random Forest", "Boosting Trees")
accuracy <- c(max(rf$results$Accuracy),
              max(gbm$results$Accuracy))
performance <- rbind(model,accuracy)
performance
#accuracy on the test set
rfpred <- predict(rf,vowel.test)
gbmpred <- predict(gbm,vowel.test)
prediction <- data.frame(rfpred, gbmpred, vowel.test$y)
prediction$rfcorrect <- with(prediction, rfpred == vowel.test$y)
prediction$gbmcorrect <- with(prediction, gbmpred == vowel.test$y)
prediction$agree <- with(prediction, gbmpred == rfpred)
prediction
rfacctest <- sum(prediction$rfcorrect)/length(prediction$rfcorrect) 
gbmacctest <- sum(prediction$gbmcorrect)/length(prediction$gbmcorrect)
#or
#
#confusionMatrix(rfpred, vowel.test$y)
#confusionMatrix(gbmpred, vowel.test$y)
#
rfacctestagree <- sum(prediction$rfcorrect[prediction$agree])/length(prediction$rfcorrect[prediction$agree]) 
print(c(rfacctest,gbmacctest,rfacctestagree))

#Q2

library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

#models specification
rf <- train(diagnosis ~ ., data=training, method = "rf")
gbm <- train(diagnosis ~ ., data=training, method = "gbm")
lda <- train(diagnosis ~ ., data=training, method = "lda")

#models prediction
rfpred <- predict(rf, newdata = testing)
gbmpred <- predict(gbm, newdata = testing)
ldapred <- predict(lda, newdata = testing)

#combo model specification
combo <- data.frame(rfpred, gbmpred, ldapred, diagnosis = testing$diagnosis) 
rfcombo <- train(diagnosis ~ ., data = combo, method = "rf")

#training sample accuracy
model <- c("Random Forest", "Boosting Trees","Linear Discriminant Analysis", "RF combo")
accuracy <- c(max(rf$results$Accuracy),
              max(gbm$results$Accuracy),
              max(lda$results$Accuracy),
              max(rfcombo$results$Accuracy))
performinsample <- rbind(model,accuracy)
performinsample


rfcombopred <- predict(rfcombo,testing$diagnosis)

#test sample accuracy
rfaccutest <- confusionMatrix(rfpred, testing$diagnosis)
gbmaccutest <- confusionMatrix(gbmpred, testing$diagnosis)
ldaaccutest <-confusionMatrix(ldapred, testing$diagnosis)
rfcomboaccutest <- confusionMatrix(rfcombopred, testing$diagnosis)

accuracytest <- c(rfaccutest$overall[1], gbmaccutest$overall[1],ldaaccutest$overall[1], rfcomboaccutest$overall[1])
accuracytest

#Q3

set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
library(caret)
library(elasticnet)
lasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
# To discover wich is the first coefficient to go to zero as the penalty rises 
#the plot coefficient to penalty is analyzed
plot.enet(lasso$finalModel, xvar="penalty", use.color=TRUE)

#Q4
#install.packages("downloader")
#library("downloader")
#gaurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
#gaf <- "gaData.csv"
library(lubridate)
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library("forecast")
bats <- bats(tstrain)
tstest = ts(testing$visitsTumblr)
batsfore <- forecast(bats,h=235)
batsforedf <- data.frame(cbind(lo = batsfore$lower[,2], up = batsfore$upper[,2], test = testing[,3]))
batsforedf$oklow <- with(batsforedf, test > lo )
batsforedf$okup <- with(batsforedf, test < up )
batsforedf$okboth <- with(batsforedf, batsforedf$oklow & batsforedf$okup )
sum(batsforedf$okboth)/235

#Q5
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
svm <- svm(CompressiveStrength ~ ., training)
svmpred <- predict(svm,testing)
svmfore <- forecast(svm,testing)
install.packages("hydroGOF")
library("hydroGOF")
rmse(svmpred,testing$CompressiveStrength)
accuracy(svmpred,testing$CompressiveStrength)



coef1 <- lasso$finalModel$beta
coef1 <- scale(coef1, FALSE, 1/lasso$finalModel$normx)
cnums <- seq(ncol(coef1))
s1 <- switch(xvar, fraction = {
        s1 <- object$L1norm
        s1 <- s1/max(s1)}
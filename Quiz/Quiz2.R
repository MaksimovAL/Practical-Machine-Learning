#Q1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#Q2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


names <- colnames(concrete)
names <- names[-length(names)]

#now let's make a quick feature plot to see if there is any relation between 
#the outcome CompressiveStrength and the rest of the parameters in the data:

featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")

#It is clear from this plot that there is no relation between the 
#outcome and any of the other variables int he data set

#Now we'll make a plot of the outcome as a function of the index

index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() + 
    theme_bw()

#It is clear from this figure that there is a step-like pattern in the data that could be explained
#by one or more variable in the data.
#From this plot we should probably cut the outcome in 4 categories
#install.packages("Hmisc")
library("Hmisc")
cutCS <- cut2(training$CompressiveStrength, g = 4)
summary(cutCS)


#Make a plot of the categorized outcome 

ggplot(data = training, aes(y = index, x = cutCS)) + geom_boxplot() + geom_jitter(col = "blue") + 
        theme_bw()

#Now the step is better seen in the above plot. As we can see this plot the step-like pattern
#is more clear now.

#Now we'll make a plot of the categorized income as function of the rest of the variables

featurePlot(x = training[, names], y = cutCS, plot = "box")

#Q3

histogram(training$Superplasticity)

#Q4

trainingIL <- training[,grep("^IL",colnames(training))]

preProc <- preProcess(trainingIL, method="pca", thresh=0.8)

preProc$rotation

#7 components are needed toexplain the 80% of variance

library(caret)
library(AppliedPredictiveModeling)
library(Hmisc)
#install.packages('e1071', dependencies=TRUE)
library('e1071')
set.seed(3433)
data(AlzheimerDisease)
Data=cbind(diagnosis,predictors)

toMatch <- c("^IL", "diagnosis")
Datas <- Data[,grep(paste(toMatch,collapse="|") , colnames(Data))]
inTrain = createDataPartition(Datas$diagnosis, p = 3/4)[[1]]
train1 = Datas[inTrain, ]
test = Datas[-inTrain, ]
dim(train1)
dim(test1)
model1 <- train(diagnosis ~ ., method = "glm", data = train1)
A1 <- C1$overall[1]


train2 <- preProcess(train[-diagnosis], method="pca", thresh=0.8
                     
model2 <- train(train1$diagnosis ~ ., method = "glm", preProcess = "pca", 
data = train1, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
C2 <- confusionMatrix(test$diagnosis, predict(model2, test))
print(C2)
A2 <- C2$overall[1]
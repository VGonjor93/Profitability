#Installing packages ------

install.packages("caret", dep=TRUE)
install.packages("readr")
install.packages("corrplot")
install.packages("DataExplorer")
install.packages("xfun")
install.packages("highr")
install.packages("ISLR")
install.packages('e1071')

library(caret)
library(readr)
library(corrplot)
library(DataExplorer)
library(ISLR)
library(e1071)



#Importing data -----

exProd <- read_csv("existingproductattributes2017.csv")
View(exProd)
create_report(exProd)

newProd <- read_csv("newproductattributes2017.csv")
View(newProd)
create_report(newProd)

#Data Pre-processing ----- 
na.omit(exProd$Volume)
exProd <- exProd[-c(32, 33, 34, 35, 36, 37, 38, 39, 40, 41),]

str(exProd)
dummyEx <- dummyVars("~ .", data = exProd)
readyData <- data.frame(predict(dummyEx, newdata = exProd))

str(readyData)
summary(readyData)

#Taking out unimportant or high correlation variables
readyData$BestSellersRank <- NULL
readyData$ProfitMargin <- NULL
readyData$ProductNum <- NULL
readyData$ProductTypeExtendedWarranty <- NULL
readyData$x4StarReviews <- NULL
readyData$x2StarReviews <- NULL

create_report(readyData)
View(readyData)

#Looking for correlation between attributes -----

corrData <- cor(readyData)
corrData
corrplot(corrData)


#Preparing for model training -----

set.seed(420)

inTrain <- createDataPartition(readyData$Volume, p = 0.70,  list = FALSE)
trainSet <- readyData[ inTrain,]
testSet <- readyData[ -inTrain,]
nrow(trainSet)
nrow(testSet)


#lm Model training -----

lmModel <- lm(Volume~ ., trainSet)
summary(lmModel)
plot(lmModel)

lmModelpc <- lm(trainSet$ProductTypePC ~ trainSet$x5StarReviews)
summary(lmModelpc)
plot(lmModelpc)

abline(lmModelpc)
#KNN -----

trainX <- trainSet[,names(trainSet) != "Direction"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues


set.seed(420)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
##knnGrid <- expand.grid(k=c(30,31,32,33,34,35,36,37,38,39))
knnFit <- train(Volume ~ ., data = trainSet, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit



postResample(predict(knnFit,trainSet),trainSet$Volume)
postResample(predict(knnFit,testSet),testSet$Volume)

varImp(knnFit)

#SVM -----

svmFit <- svm(Volume ~ ., trainSet, kernel = "radial", fitted = TRUE) 

svmFit

postResample(predict(svmFit,trainSet),trainSet$Volume)
postResample(predict(svmFit,testSet),testSet$Volume)

as.data.frame(predicho=predict(svmFit,testSet),real=testSet$Volume)


ggplot()

summary(svmFit)


#Predictions I guess -----

predictionKNN <- predict(knnFit, testSet)
postResample(predictionKNN, testSet$Volume)

predictionSVM <- predict(svmFit, testSet)
postResample(predictionSVM, testSet$Volume)

 #Preparing new product list for prediction ----- 

na.omit(newProd$Volume)
newProd <- newProd[-c(23),]

dummyNew <- dummyVars("~ .", data = newProd)
newreadyData <- data.frame(predict(dummyNew, newdata = newProd))

newreadyData$BestSellersRank <- NULL
newreadyData$ProfitMargin <- NULL
newreadyData$ProductNum <- NULL
newreadyData$ProductTypeExtendedWarranty <- NULL
newreadyData$x4StarReviews <- NULL
newreadyData$x2StarReviews <- NULL

#NEW PRODUCT PREDICTION ------

FinalVolumePred <- predict(svmFit, newreadyData)
FinalVolumePred

newreadyData$Volume <- FinalVolumePred



#Plots -----

plot(predictionKNN, type = "l", col ="red", ylim = c(0,7000))
lines(testSet$Volume, col="green")


plot(predictionSVM, type = "l", col ="red", ylim = c(0,7000))
lines(testSet$Volume, col="green")

plot(predictionSVM, type = "l", col ="red", ylim = c(0,7000))
lines(predictionKNN, col="green")
lines(testSet$Volume, col="blue")

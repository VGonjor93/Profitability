#Installing packages ------

install.packages("caret", dep=TRUE)
install.packages("readr")
install.packages("corrplot")
install.packages("DataExplorer")
install.packages("xfun")
install.packages("highr")
install.packages("ISLR")
install.packages('e1071')
install.packages("reshape2")

library(caret)
library(readr)
library(corrplot)
library(DataExplorer)
library(ISLR)
library(e1071)
library(reshape2)


#Importing data -----

exProd <- read_csv("C:/Users/poni6/Desktop/Data Analysis/Modulo 2/Task 3/existingproductattributes2017.csv")
View(exProd)
create_report(exProd)

newProd <- read_csv("newproductattributes2017.csv")
View(newProd)
create_report(newProd)

#Data Pre-processing ----- 
na.omit(exProd$Volume)
exProd <- exProd[-c(35, 36, 37, 38, 39, 40, 41),]
exProd[which(exProd[,3]==124.98, arr.ind=TRUE), 3] <- 178.92


str(exProd)
dummyEx <- dummyVars("~ .", data = exProd)
readyData <- data.frame(predict(dummyEx, newdata = exProd))

str(readyData)
summary(readyData)

#Taking out unimportant or high correlation variables
readyData$BestSellersRank <- NULL
readyData$ProfitMargin <- NULL
readyData$ProductNum <- NULL
readyData$x5StarReviews <- NULL
readyData$x3StarReviews <- NULL
readyData$x1StarReviews <- NULL

create_report(readyData)
create_report(exProd)
View(readyData)

#Looking for correlation between attributes -----

corrData <- cor(readyData)
corrData
corrplot(corrData)

create_report(readyData)
create_report(exProd)
View(readyData)

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
lmPrediction <- predict(lmModel, testSet)
plot(lmPrediction, type="l")
points(testSet$Volume)

#KNN -----

ctrl <- trainControl(method="repeatedcv",repeats = 3)
##knnGrid <- expand.grid(k=c(30,31,32,33,34,35,36,37,38,39))
knnFit <- train(Volume ~ ., data = trainSet, method = "knn", trControl = ctrl)
knnFit



postResample(predict(knnFit,trainSet),trainSet$Volume)
postResample(predict(knnFit,testSet),testSet$Volume)

varImp(knnFit)

#SVM -----

svmFit <- svm(Volume ~ ., trainSet, kernel = "radial", fitted = TRUE) 

svmFit

postResample(predict(svmFit,trainSet),trainSet$Volume)
postResample(predict(svmFit,testSet),testSet$Volume)

varImp(svmFit)

summary(svmFit)


#RF -----

rfFit <- train(Volume ~ ., data = trainSet, method = "rf", trControl = ctrl)
rfFit
postResample(predict(rfFit,testSet),testSet$Volume)

varImp(rfFit)

#Predictions I guess -----

predictionKNN <- predict(knnFit, testSet)
postResample(predictionKNN, testSet$Volume)

predictionSVM <- predict(svmFit, testSet)
postResample(predictionSVM, testSet$Volume)

#Preparing new product list for prediction ----- 

na.omit(newProd$Volume)

dummyNew <- dummyVars("~ .", data = newProd)
newreadyData <- data.frame(predict(dummyNew, newdata = newProd))

newreadyData$BestSellersRank <- NULL
newreadyData$ProfitMargin <- NULL
newreadyData$ProductNum <- NULL
newreadyData$x5StarReviews <- NULL
newreadyData$x3StarReviews <- NULL
newreadyData$x1StarReviews <- NULL

#NEW PRODUCT PREDICTION ------

FinalVolumePred <- predict(svmFit, newreadyData)
FinalVolumePred

roundedFinalVolumePred <-round(FinalVolumePred,0)

newreadyData$Volume <- roundedFinalVolumePred
newProd$Volume <- roundedFinalVolumePred


#CALCULATING SALES PROFIT

newreadyData["Profit"] <- newreadyData$Price*newreadyData$Volume*newProd$ProfitMargin

roundedFinalProfit <-round(newreadyData$Profit,0)

newProd["Profit"] <- roundedFinalProfit


#Plots -----

plot(predictionKNN, type = "l", col ="red", ylim = c(0,7000))
lines(testSet$Volume, col="green")


plot(predictionSVM, type = "l", col ="red", ylim = c(0,7000))
lines(testSet$Volume, col="green")

plot(predictionSVM, type = "l", col ="red", ylim = c(0,7000))
lines(predictionKNN, col="green")
lines(testSet$Volume, col="blue")


ggplot(newProd, 
       aes(x=newProd$ProductType, y=newProd$Profit, fill=newProd$ProductType)) + 
       geom_bar(stat="identity") +
       labs(fill = "Product Type" ,title="Total Predicted Profit Per Product Type [SVM]") + 
       scale_x_discrete(name = "Product Type") +
       scale_y_continuous(name = "Profit", breaks = seq(0,500000,50000))


write.table(newProd, file = "FinalPredictionsM2T3.csv", sep = ",")



testData <- exProd
View(testData)
testData$ProductNum <- NULL
testData$Price <- NULL
testData$PositiveServiceReview<-NULL
testData$Volume<-NULL
testData$ProfitMargin<-NULL
testData$ProductHeight<-NULL
testData$ProductWidth<-NULL
testData$NegativeServiceReview<-NULL
testData$ProductDepth<-NULL
testData$ShippingWeight<-NULL
testData$BestSellersRank<-NULL
testData$Recommendproduct<-NULL
View(data.m)
data.m <- melt(testData, id.vars='ProductType')
ggplot(data.m, aes(ProductType, value)) +
        geom_bar(aes(fill = variable), position = "dodge", stat="identity")+
        labs(fill="Customer Reviews", title="Customer Reviews Per Product Type") + 
        scale_x_discrete(name = "Product Type") +
        scale_y_continuous(name = "Number of Reviews")

                           
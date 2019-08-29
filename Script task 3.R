#Installing packages ------

install.packages("caret", dep=TRUE)
install.packages("readr")
install.packages("corrplot")
install.packages("DataExplorer")
install.packages("xfun")
install.packages("highr")

library(caret)
library(readr)
library(corrplot)
library(DataExplorer)


#Importing data -----

exProd <- read_csv("existingproductattributes2017.csv")
View(exProd)
create_report(exProd)

newProd <- read_csv("newproductattributes2017.csv")
View(newProd)
create_report(newProd)

#Data Pre-processing ----- 

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

readyData <- readyData[-c(32, 33, 34, 35, 36, 37, 38, 39, 40, 41)]

create_report(readyData)
View(readyData)

readyData$ProductNum <- as.




#Looking for correlation between attributes -----

corrData <- cor(readyData)
corrData
corrplot(corrData)


#Preparing for model training -----

set.seed(420)

trainSize<-round(nrow(readyData)*0.7)
library(caret)
library(ggplot2)
library(rpart.plot)
library(pROC)
library(ROCR)
library(rpart)
library(randomForest)
library(caTools)
library(e1071)

census=read.csv("E:\\Data Science\\R-Proj\\20180112 -- 0180114 Ivy Data Science Hackshop\\04 DECISION TREES\\CASE STUDY1\\02DATA\\census.csv", stringsAsFactors = T,header = TRUE)
head(census)
str(census)
summary(census)
as.data.frame(colSums(is.na(census)))
census$over50k<-as.factor(census$over50k)
library(caTools)
set.seed(3000)
s1 = sample.split(census$over50k, 0.60)
census_train = subset(census, s1==T)
census_test  = subset(census, s1==F)

dim(census_train)
str(census_train)

dim(census_test)
str(census_test)


CART1<-rpart(over50k~.,data=census_train, method = "class")
prp(CART1)
CART1


predictCART1<-predict(CART1, newdata=census_test, type = "class")
table(census_test$over50k,predictCART1)
(9117+1676)/(1402+1676+596+9117)
confusionMatrix(predictCART1,census_test$over50k)

predictCART2<-predict(CART1, newdata=census_test)#To predict the probabilities for the observations in the test data set


CARTroc<-roc(response=census_test$over50k, predictor = predictCART2[,2],
             level = rev(levels(census_test$over50k)))
CARTroc
plot(CARTroc)


set.seed(3000)
samp = sample.split(census$over50k, SplitRatio = 0.6)
Train11 = subset(census, samp==TRUE)
dim(Train11)
str(Train11)

Test11 = subset(census, samp==FALSE)
dim(Test11)
str(Test11)


PredictForest1<-randomForest(over50k~.,data = Train11,type="class")
PredictForest1
nrow(Test)

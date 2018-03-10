library(randomForest)
library(rpart)
library(rpart.plot)
library(caTools)
library(pROC)
library(caret)
census = read.csv("census.csv", na.string = (""))
View(census)
str(census)
as.data.frame(colSums(is.na(census)))

censusSplit = sample.split(census$over50k,SplitRatio = 0.80 ) 
train = census[censusSplit,]
test = census[!censusSplit,]

bestmtry =tuneRF(train, train$over50k, ntreeTry = 200, stepFactor = 1.2, improve = 0.01, trace = T, plot = T)
f1 = randomForest(over50k ~.,data = train, mtry=3)
f1
importance(f1)
varImpPlot(f1)

pred1 = predict(f1, test, type = "class")
pred1
predtable1 = table(predictions = pred1, actual=test$over50k)
predtable1
test$t1 = pred1
View(test)
confusionMatrix(pred1,test$over50k) ##lib(caret)
sum(diag(predtable1))/sum(predtable1)

predprobs = predict(f1, test, type = 'prob')
predprobs
auc1 = auc(test$over50k, predprobs[,2])
auc2 = auc(test$over50k, predprobs[,1])
plot(roc(test$over50k, predprobs[,2]))
plot(roc(test$over50k, predprobs[,1]))
auc1
auc2


####CART

f2 = rpart(over50k ~., data = census, method = 'class')
prp(f2)
f2
rpart.plot(f2, type = 3, extra = 101, fallen.leaves = T, tweak = 2)
pred2 = predict(f2, test, type='class')
pred2
tablecart = table(test$over50k, pred2)
tablecart
confusionMatrix(test$over50k, pred2)

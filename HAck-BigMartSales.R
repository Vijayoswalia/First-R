library(car)
library(dplyr)
library(zoo)
library(dplyr)
library(randomForest)
train = read.csv("E:\\Data Science\\rWork\\rProjects\\First-R\\Hackathon\\Train_UWu5bXk.csv", na.strings = c(""))
test = read.csv("E:\\Data Science\\rWork\\rProjects\\First-R\\Hackathon\\Test_u94Q5KV.csv", na.strings = c(""))
train
head(train)

#Check for missing value
as.data.frame(colSums(is.na(train)))
str(train)

#reducing factors in Fat_Content
#train$Item_Fat_Content = as.factor(train$Item_Fat_Content)
#train$Outlet_Size = as.factor(train$Outlet_Size)
#train$Outlet_Type = as.factor(train$Outlet_Type)
train$Item_Fat_Content = dplyr::recode(train$Item_Fat_Content,'reg'='Regular','low fat'='LF','Low Fat'='LF')
?recode
View(train)
#finding correlation between train$Outlet_Size,train$Outlet_Type
table(train$Outlet_Size,train$Outlet_Type)
plot(train$Outlet_Size,train$Outlet_Type)
train$Outlet_Size[is.na(train$Outlet_Size) & train$Outlet_Type == "Grocery Store"] = "Small"
train$Outlet_Size[is.na(train$Outlet_Size) & train$Outlet_Type == "Supermarket Type2"] = "Medium"
train$Outlet_Size[is.na(train$Outlet_Size) & train$Outlet_Type == "Supermarket Type3"] = "Medium"
train$Outlet_Size[is.na(train$Outlet_Size ) & train$Outlet_Location_Type == "Tier 2"] = "Small"
as.data.frame(colSums(is.na(train)))

v1 = unique(train$Item_Identifier[is.na(train$Item_Weight)])
v1[1:5]
for (v in v1){
  train$Item_Weight[train$Item_Identifier==v & is.na(train$Item_Weight)] = 
    median(train$Item_Weight[train$Item_Identifier==v],na.rm=T)
}
#RNA = na.aggregate(train[,c(1:2)], by = list(train$Item_Identifier), FUN = first)
#View(RNA)
#train$Item_Identifier = RNA$Item_Identifier
#train$Item_Weight = RNA$Item_Weight
train = train[!is.na(train$Item_Weight),]
as.data.frame(colSums(is.na(train)))

#for test data
as.data.frame(colSums(is.na(test)))
#test$Item_Fat_Content = as.factor(test$Item_Fat_Content)
#test$Outlet_Size = as.factor(test$Outlet_Size)
#test$Outlet_Type = as.factor(test$Outlet_Type)
test$Item_Fat_Content = dplyr::recode(test$Item_Fat_Content,'reg'='Regular','low fat'='LF','Low Fat'='LF')
test$Outlet_Size[is.na(test$Outlet_Size) & test$Outlet_Type == "Grocery Store"] = "Small"
test$Outlet_Size[is.na(test$Outlet_Size) & test$Outlet_Type == "Supermarket Type2"] = "Medium"
test$Outlet_Size[is.na(test$Outlet_Size) & test$Outlet_Type == "Supermarket Type3"] = "Medium"
test$Outlet_Size[is.na(test$Outlet_Size ) & test$Outlet_Location_Type == "Tier 2"] = "Small"
RN = na.aggregate(test[,c(1:2)], by = list(test$Item_Identifier), FUN = first)
test$Item_Identifier = RN$Item_Identifier
test$Item_Weight = RN$Item_Weight
#v2 = unique(test$Item_Identifier[is.na(test$Item_Weight)])
#v2[1:5]
#for (v in v2){
#  test$Item_Weight[test$Item_Identifier==v & is.na(test$Item_Weight)] = 
#    median(test$Item_Weight[test$Item_Identifier==v],na.rm=T)
#}
test = test[!is.na(test$Item_Weight),]
as.data.frame(colSums(is.na(test)))



#Predict tha sales
test$Item_Identifier=NULL
train$Item_Identifier=NULL
#####Random Forest
#rf = randomForest(Item_Outlet_Sales ~., data = train)
#pred = predict(rf,newdata=test,type='class')
#pred[1:5]
#sample = read.csv("E:\\Data Science\\rWork\\rProjects\\First-R\\Hackathon\\SampleSubmission_TmnO39y.csv")
#head(sample)
#sample$Item_Outlet_Sales = pred
#head(sub1)
#write.csv(sub1,'BigMart3.csv',row.names = F)'''


rf1 = randomForest(Item_Outlet_Sales ~ .,data = train)
pred = predict(rf1,newdata=test,type='class')
pred[1:5]
df2 = cbind(test, pred)
write.csv(df2, file='predictions.csv')
sub1 = read.csv("E:\\Data Science\\rWork\\rProjects\\First-R\\Hackathon\\SampleSubmission_TmnO39y.csv")
head(sub1)
sub1$Item_Outlet_Sales = pred
head(sub1)

#model1 = lm(Item_Outlet_Sales ~ Item_Weight+Item_Fat_Content+Item_Visibility+Item_Type+
#              Item_MRP+Outlet_Identifier+Outlet_Establishment_Year+Outlet_Size+Outlet_Location_Type+	
#             Outlet_Type, data = train)
#summary(model1)
#library(rpart)
#rpart(Item_Outlet_Sales ~., data = train, method=anova)

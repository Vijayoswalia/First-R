train = read.csv("E:\\Data Science\\rWork\\rProjects\\First-R\\Hackathon\\Train_UWu5bXk.csv", stringsAsFactors = FALSE, as.is = T, na.strings = c(""))
train
head(train)

#Check for missing value
as.data.frame(colSums(is.na(train)))
View(train)
str(train)

#reducing factors in Fat_Content
train$Item_Fat_Content = as.factor(train$Item_Fat_Content)
train$Outlet_Size = as.factor(train$Outlet_Size)
train$Outlet_Type = as.factor(train$Outlet_Type)
library(car)
library(dplyr)
train$Item_Fat_Content = recode(train$Item_Fat_Content,"'reg'='Regular';'low fat'='LF';'Low Fat'='LF'")

#finding correlation between train$Outlet_Size,train$Outlet_Type
table(train$Outlet_Size,train$Outlet_Type)

train$Outlet_Size[is.na(train$Outlet_Size) & train$Outlet_Type == "Grocery Store"] = "Small"
train$Outlet_Size[is.na(train$Outlet_Size) & train$Outlet_Type == "Supermarket Type2"] = "Medium"
train$Outlet_Size[is.na(train$Outlet_Size) & train$Outlet_Type == "Supermarket Type3"] = "Medium"

train$Outlet_Size[is.na(train$Outlet_Size ) & train$Outlet_Location_Type == "Tier 2"] = "Small"


as.data.frame(colSums(is.na(train)))

library(zoo)
library(dplyr)
RN = na.aggregate(train[,c(1:2)], by = list(train$Item_Identifier), FUN = first)
View(RNA)
train$Item_Identifier = RNA$Item_Identifier
train$Item_Weight = RNA$Item_Weight
train
View(train)

table(train$Outlet_Size,train$Outlet_Location_Type)
table(train$Item_Identifier,train$Item_Weight)
train$Item_Weight = -is.na(train$Item_Weight)



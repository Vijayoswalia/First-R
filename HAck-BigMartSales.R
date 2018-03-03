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
train$Item_Fat_Content=recode(train$Item_Fat_Content,'reg'='Regular','low fat'='LF','Low Fat'='LF')

#finding correlation between train$Outlet_Size,train$Outlet_Type
table(train$Outlet_Size,train$Outlet_Type)

train[train$Outlet_Type == "Grocery Store" & train$Outlet_Size == NA]
if(train$Outlet_Type == 'Grocery Store') {train$Outlet_Size = 'small'}
train$Outlet_Type

train$Outlet_Size[train$Outlet_Size[NA] | train$Outlet_Type["Grocery Store"]] = "small"

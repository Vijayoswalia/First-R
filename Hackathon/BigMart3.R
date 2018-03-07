library(caTools)
library(randomForest)
trn = read.csv("E:\\Data Science\\rWork\\rProjects\\First-R\\Hackathon\\Train_UWu5bXk.csv",na.strings = c(""))
tst = read.csv('E:\\Data Science\\rWork\\rProjects\\First-R\\Hackathon\\Test_u94Q5KV.csv',na.strings = c(""))
str(trn)
str(tst)
colSums(is.na(trn))
colSums(is.na(tst))
target = trn$Item_Outlet_Sales
trn$Item_Outlet_Sales=NULL
c1 = rbind(trn,tst)
str(c1)
colSums(is.na(c1))
View(c1)
plot(c1$Item_Type,c1$Item_Weight)
u1 = unique(c1$Item_Identifier[is.na(c1$Item_Weight)])
u1[1:5]
for (u in u1){
  c1$Item_Weight[c1$Item_Identifier==u & is.na(c1$Item_Weight)] = 
  median(c1$Item_Weight[c1$Item_Identifier==u],na.rm=T)
}
colSums(is.na(c1))
plot(c1$Outlet_Type,c1$Outlet_Size)
levels(c1$Outlet_Type)
c1$Outlet_Size[c1$Outlet_Type=='Grocery Store' & is.na(c1$Outlet_Size)] = 'Small'
c1$Outlet_Size[c1$Outlet_Type=='Supermarket Type1' & is.na(c1$Outlet_Size)] = 'Small'
c1$Outlet_Size[c1$Outlet_Type=='Supermarket Type2' & is.na(c1$Outlet_Size)] = 'Medium'
colSums(is.na(c1))
c1$Item_Identifier=NULL
train = head(c1,nrow(trn))
test = tail(c1,nrow(tst))
train$Item_Outlet_Sales = target

rf1 = randomForest(Item_Outlet_Sales ~ .,data = train)
pred = predict(rf1,newdata=test,type='class')
pred[1:5]
sub1 = read.csv("E:\\Data Science\\rWork\\rProjects\\First-R\\Hackathon\\SampleSubmission_TmnO39y.csv")
head(sub1)
sub1$Item_Outlet_Sales = pred
head(sub1)
write.csv(sub1,'BigMart3.csv',row.names = F)

train = read.csv("E:\\Data Science\\R-Proj\\20180112 -- 0180114 Ivy Data Science Hackshop\\02 LOGISTIC REGRESSION\\CASE STUDY1\\02DATA\\train.csv", stringsAsFactors = FALSE, as.is = TRUE, na.strings = c(""))
summary(train)
train
as.data.frame(colSums(is.na(train)))

library(Amelia)
missmap(train, main = "Missing values vs Observed")

train_titanic_1<-subset(train, select = -c(Cabin,PassengerId,Ticket, Name))
train_titanic_1$Age[is.na(train_titanic_1$Age)]<-mean(train_titanic_1$Age, na.rm = TRUE)

str(train_titanic_1$Embarked)
train_titanic_1$Embarked<-as.factor(train_titanic_1$Embarked)
str(train_titanic_1$Sex)
train_titanic_1$Sex<-as.factor(train_titanic_1$Sex)


contrasts(train_titanic_1$Sex)
contrasts(train_titanic_1$Embarked)


train_titanic_1<-train_titanic_1[!is.na(train_titanic_1$Embarked),]
rownames(train_titanic_1)<-NULL

library(caTools)
set.seed(113)
spl = sample.split(train_titanic_1$Survived, 0.85)
train_titanic_t = subset(train_titanic_1, spl == TRUE)
train_titanic_v = subset(train_titanic_1, spl == FALSE)
nrow(train_titanic_t)
nrow(train_titanic_v)

Model1<-glm(Survived~.,family = binomial(link = 'logit'), data = train_titanic_t)
summary(Model1)

Model2<-glm(Survived~ Pclass+ Sex+ Age+ SibSp+Parch + Embarked,family = binomial(link = 'logit'), data = train_titanic_t)
summary(Model2)

Model3<-glm(Survived~ Pclass+ Sex+ Age+ SibSp + Embarked,family = binomial(link = 'logit'), data = train_titanic_t)
summary(Model3)

Model4<-glm(Survived~ Pclass+ Sex+ Age+ SibSp + I(Embarked == "S"),family = binomial(link = 'logit'), data = train_titanic_t)
summary(Model4)

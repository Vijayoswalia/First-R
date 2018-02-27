train = read.csv("Titenic_train.csv", stringsAsFactors = F, as.is = T, na.strings = c(""))
test = read.csv("Titenic_test.csv", stringsAsFactors = F, as.is = T, na.strings = c(""))
head(titenic)
summary(titenic)
str(titenic)
as.data.frame(colSums(is.na(titenic)))
library(Amelia)
missmap(titenic, main = "main value vs observed")
View(titenic)
data(titenic)
?data
train = subset(train, select = -c(Cabin,PassengerId,Ticket,Name))
train$Age[is.na(train$Age)]=mean(train$Age, na.rm = T)
as.data.frame(colSums(is.na(train)))
train$Embarked = as.factor(train$Embarked)
train$Sex = as.factor(train$Sex)
train = train[!is.na(train$Embarked),]
(rownames(train) = NULL)
contrasts(train$Sex)
contrasts(train$Embarked)

library(caTools)
set.seed(123)
spl = sample.split(train$Survived, 0.85)
train_titenic = subset(train, spl==TRUE)
test_titenic = subset(train, spl==FALSE)
nrow(train_titenic)
nrow(test_titenic)

model1 = glm(Survived~., family=binomial(link = "logit"), data=train_titenic)
model1
summary(model1)

model2 = glm(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked, family = binomial(link = "logit"), data=train_titenic)
summary(model2)

model3 = glm(Survived~Pclass+Sex+Age+SibSp+Embarked, family = binomial(link = "logit"), data = train_titenic)
summary(model3)

model3$coefficients
exp(model3$coefficients)

library(car)
vif(model3)
anova(model3, test = "Chisq")

library(BaylorEdPsych)
PseudoR2(model3)

pred_sur = predict(model3,newdata = test_titenic, type = "response")
pred_sur_n = ifelse(pred_sur>0.5,1,0)

library(caret)
library(lattice)
library(ggplot2)
confusionMatrix(pred_sur_n,test_titenic$Survived)
test_titenic$Survived = as.factor(test_titenic$Survived)
library(gplots)
library(ROCR)
pr <- prediction(pred_sur, test_titenic$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

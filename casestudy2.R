elentra = read.csv("elantra.csv", stringsAsFactors = TRUE, header = TRUE)
elentra
str(elentra)
head(elentra)
library(pastecs)
options(scipen = 100)
options(digits = 2)
stat.desc(elentra)
train = subset(elentra, Year<=2012)
str(train)  #36 observations
test = subset(elentra, Year>2012)
str(test) #14 observations

lm1 = lm(ElantraSales~Unemployment+Queries+CPI_energy+CPI_all, data=train)
summary(lm1)
library(car)
vif(lm1)
lm2 = lm(ElantraSales~Unemployment+Queries+CPI_all, data=train)
summary(lm2)
vif(lm2)
lm3 = lm(ElantraSales~Unemployment+Queries, data=train)
summary(lm3)
vif(lm3)
lm4 = lm(ElantraSales~Queries, data=train)
summary(lm4)
predictlm=predict(lm3, data = test)
summary(abs(predictlm-test$ElantraSales))
(predictlm-test$ElantraSales)
test[which.max(abs(predictlm-test$ElantraSales)),]
elentra$test=predictlm

fit1<-lm(ElantraSales~Unemployment + Queries + CPI_energy + CPI_all + as.factor(Month), data=train)
summary(fit1)
cor(train[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])
fit2<-lm(ElantraSales~Unemployment + CPI_energy + CPI_all + as.factor(Month), data=train)
summary(fit2)

library(vars) # ur.df,
library(forecast)
library(urca)
library(xts)
library(ggplot2)
library(dplyr)
oct_march = read.csv("oct_march.csv", stringsAsFactors = F)
test = oct_march
test = oct_march[-5]
plot(ts1)
test[,1]
str(oct_march)



head(oct_march)
View(oct_march)
oct_march$date = as.Date(oct_march$date, format = "%y-%m-%d")
planes <- group_by(test, test$chid)
planes

a = group_by(oct_march,chid)
  
#adf.test(oct_march, alternative = "stationary")
w = ur.df(oct_march$videoscount,type = "drift", selectlags = "AIC")
summary(w) #w is stationary
x = ur.df(oct_march$views,type = "drift", selectlags = "AIC")
summary(x) #x is stationary
y = ur.df(oct_march$subscriber,type = "drift", selectlags = "AIC")
summary(y) #y is stationary

o_var=VAR(oct_march[,-1], type = "const", lag.max = 10, ic="AIC")













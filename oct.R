library(vars)
library(forecast)
library(urca)
oct_march = read.csv("oct_march.csv", stringsAsFactors = F)
str(oct_march)
View(oct_march)
adf.test(oct_march, alternative = "stationary")
w = ur.df(oct_march$videoscount,type = "drift", selectlags = "AIC")
summary(w) #w is stationary
x = ur.df(oct_march$views,type = "drift", selectlags = "AIC")
summary(x) #x is stationary
y = ur.df(oct_march$subscriber,type = "drift", selectlags = "AIC")
summary(y) #y is stationary
oct = cbind(oct_march$chid, oct_march$views,oct_march$subscriber,oct_march$videoscount)
oct_time = cbind(oct, oct_march$date)
oct_march_TS = ts(oct_time, start = c(2016,10), frequency = 12)


oct1 = cbind(oct_march$views,oct_march$subscriber,oct_march$videoscount)
oct_time1 = cbind(oct1, oct_march$date)
oct_march_TS1 = ts(oct_time1, start = c(2016,10), end = c(2017,03), frequency =12)
class(oct_march_TS1)
?ts
oct_march_TS1
TS = oct_march_TS1[,-5]
colnames(TS) = c( "chid","views","subscriber","videoscount")
TS = ts(TS)
----------------------------------------------
class(TS)

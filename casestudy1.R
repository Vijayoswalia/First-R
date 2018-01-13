statedata = read.csv(file = "E:\\Data Science\\R-Proj\\20180112 -- 0180114 Ivy Data Science Hackshop\\01 MULTIVARIATE LINEAR REGRESSION\\CASE STUDY1\\02DATA\\statedata.csv", stringsAsFactors = FALSE, header = TRUE, as.is = TRUE)
statedata
str(state)
summary(statedata)



statedata_ctn<-subset(statedata, select = c(Population,Income,Illiteracy,Life.Exp,Murder,HS.Grad,Frost,Area,state.area,x,y))
statedata_cat<-subset(statedata, select = -c(Population,Income,Illiteracy,Life.Exp,Murder,HS.Grad,Frost,Area))

library(pastecs)
install.packages("pastecs", dependencies = TRUE)
options(scipen = 100)
options(digits = 2)
stat.desc(statedata_ctn)

as.matrix(sort(table(statedata_cat$state.division), decreasing = TRUE))
as.matrix(sort(table(statedata_cat$state.name), decreasing = TRUE))
as.matrix(sort(table(statedata_cat$state.region), decreasing = TRUE))

plot(statedata$x,statedata$y, xlab = "longtitude of centres", ylab = "latitude of centres", main = "PLOT OF STATE CENTRES")

boxplot(statedata$HS.Grad ~ statedata$state.region, xlab = "Region of USA", ylab = "High School Graduation Rate", main = "Distribution of High School Graduation Rate")

statedata$state.region<-as.factor(statedata$state.region)
statedata$state.division<-as.factor(statedata$state.division)
statedata$state.name<-as.factor(statedata$state.name)

Model1<-lm(Life.Exp~ Population + Income + Illiteracy + Murder + HS.Grad + 
             Frost + Area + x + y + state.division +  + state.region,
           data = statedata)

summary(Model1)

Model2<-lm(Life.Exp~ Population + Income + Illiteracy + Murder + HS.Grad + 
             Frost + Area + x + y + state.division,
           data = statedata)

summary(Model2)

Model3<-lm(Life.Exp~ Population + Income + Illiteracy + Murder + HS.Grad + 
             Frost + Area + x + y,
           data = statedata)

summary(Model3)

Model4<-lm(Life.Exp~ Population + Income + Illiteracy + Murder + HS.Grad + 
              Area + x + y,
           data = statedata)

summary(Model4)

Model5<-lm(Life.Exp~ Population + Income + Murder + HS.Grad + 
             Area + x + y,
           data = statedata)

summary(Model5)

Model5<-lm(Life.Exp~ Population + Income + Murder + 
             Area + x + y,
           data = statedata)

summary(Model5)


Model6<-lm(Life.Exp~ Population + Income + Murder + 
              x + y,
           data = statedata)

summary(Model6)

Model7<-lm(Life.Exp~ Population + Murder + 
             x + y,
           data = statedata)

summary(Model7)
library(car)
as.data.frame(vif(Model7))

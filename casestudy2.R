elentra = read.csv("E:\\Data Science\\R-Proj\\20180112 -- 0180114 Ivy Data Science Hackshop\\01 MULTIVARIATE LINEAR REGRESSION\\CASE STUDY2\\02DATA\\elantra.csv", stringsAsFactors = TRUE, header = TRUE, as.is = TRUE)
elentra
str(elentra)
head(elentra)
train = subset(elentra, Year<=2012)
str(train)  #36 observations
test = subset(elentra, Year>2012)
str(test) #14 observations

lm1 = lm(ElantraSales~Unemployment+Queries+CPI_energy+CPI_all, data=elentra)
summary(lm1)

lm2 = lm(ElantraSales~Unemployment+Queries+CPI_energy+CPI_all, data=train)
summary(lm2)

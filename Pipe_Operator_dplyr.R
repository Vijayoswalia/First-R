View(iris)
iris %>%
  group_by(Species)%>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(avg)

CART
rpart(y~x+x2+x3.... data=d1, method=anova)(regression) or class(classification))
rpart.plot()

to make tree more complex or simple
pruning ---> (CP)complexity parameter
  CP should be min
plotcp(modelname)

gini index()
gi(x1,x2) -----   x1<x2 then x1 will be the root tree

library(ggplot2)

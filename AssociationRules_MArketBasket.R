library(arules)
library(arulesViz)
library(datasets)
data("Groceries")
head(Groceries)
Groceries
summary(Groceries)
inspect(Groceries[1:5])
itemFrequencyPlot(Groceries, topN=20, type = "absolute")
itemFrequencyPlot(Groceries, topN=20, type = "relative")
LIST(Groceries[1:5])
frequentItems = eclat(Groceries, parameter = list(supp = 0.07, maxlen = 15))
frequentItems
inspect(frequentItems)

itemFrequencyPlot(Groceries, topN=20, type = "absolute")
rules = apriori(Groceries, parameter = list(supp = 0.001, conf = 0.5))
inspect(rules)
quality(rules)
head(quality(rules))
inspect(rules[1:5])
options(digits = 2)
inspect(rules[1:5])

rules = sort (rules, by = "confidence", decreasing=TRUE)
rules = sort (rules, by = "lift", decreasing=TRUE)
inspect(rules[1:5])

rules = apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8), appearance = 
                  list (default = "lhs", rhs = "whole milk"), control = list (verbose=F))
inspect(rules[1:5])
rules2 = apriori(Groceries, parameter = list(supp = 0.001, conf = 0.08), 
                 appearance = list (default = "rhs", lhs = "whole milk"), 
                 control = list (verbose=F))
inspect(rules2[1:20])
plot(rules, method = "graph", interective=TRUE, shading = NA)
plot(rules[1:10], method = "graph", interective=TRUE, shading = NA)

Titanic

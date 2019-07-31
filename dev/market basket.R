setwd("C:/Users/Niccolo/Documents/Work life/Deliberate practice/Market Basket Analysis")

groceries <- read.csv('groceries.csv', header = FALSE)
head(groceries)

library(arules)
library(arulesViz)
groceries <- read.transactions('groceries.csv', sep=',')
summary(groceries)

what <- t(cleaned)

inspect(groceries[1:10])

itemFrequency(groceries[, 1:4])

itemFrequencyPlot(groceries, support = 0.078)

itemFrequencyPlot(groceries, topN = 25)

grocery.rules <- apriori(groceries, parameter = list(support = 0.003, confidence = 0.25, minlen = 2))

grocery.rules

summary(grocery.rules)

inspect(grocery.rules[1:10])

inspect(sort(grocery.rules, by = 'lift')[1:20])

ham.rules <- subset(grocery.rules, items %in% 'ham')
inspect(ham.rules)

# Viz
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
plot(grocery.rules, method="grouped", control=list(k=10))
subrules2 <- head(sort(grocery.rules, by="lift"), 10)

# rules as vertices
plot(subrules2, method="graph")
# itemsets as vertices
plot(subrules2, method="graph", control=list(type="itemsets"))


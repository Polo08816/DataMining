library(arules)
library(datasets)
library(Matrix)

data(Adult)

# Explore the data

summary(Adult)
Adult[1:2,]

itemFrequencyPlot(Adult, support=0.1, cex.names = 0.8)

# Apriori Algorithm

rules <- apriori(Adult, parameter = list(support = 0.01, confidence = 0.6))

rulesIncomeSmall <- subset(rules, subset = rhs %in% "income=small" & lift > 1.2)
rulesIncomeLarge <- subset(rules, subset = rhs %in% "income=large" & lift > 1.2)

inspect(sort(rulesIncomeSmall, by = "confidence")[1:3])

# Eclat Algorithm

itemsets <- eclat(Adult, parameter = list(sup = 0.1, maxlen = 15))

fsets <- eclat(Adult, parameter = list(sup = 0.5))

fsets.top5 <- sort(fsets[1:5])
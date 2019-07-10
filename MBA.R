################################
#Market Basket Analysis
## Loading required libraries
library(dplyr)
library(plyr)
library(magrittr)
library(lubridate)
library(chron)
library(ggplot2)
library(cluster)
library(arules)
library(arulesViz)
################################
setwd("B:/PGP/Marketing & Retail/Group Ass")
groc = read.transactions('groceries.csv', format = 'basket', sep=',')
class(groc)
groc
summary(groc)
#Frequency plot - Display Result
itemFrequencyPlot(groc, topN=20, type='absolute')
grocrules <- apriori(groc, parameter = list(supp=0.001, conf=0.8))
grocrules <- sort(grocrules, by='confidence', decreasing = TRUE)
summary(grocrules) 
inspect(grocrules[1:10])
topn <- grocrules[1:5]
plot(topn, method="graph") 

## Restricting the length
grocrules <- apriori(groc, parameter = list(supp=0.001, conf=0.8, maxlen= 3))
grocrules <- sort(grocrules, by='confidence', decreasing = TRUE)
summary(grocrules) 
inspect(grocrules[1:25])
topn <- grocrules[1:5]
plot(topn, method="graph")

## Hnadling redundancy
#subset.matrix <- is.subset(grocrules, grocrules)
#subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
#redundant <- colSums(subset.matrix, na.rm=T) >= 1
#grocrules.pruned <- grocrules[!redundant]
#grocrules<-grocrules.pruned
#grocrules


## Before buying "whole milk", what all customer may buy
whmilk.rules <- apriori(groc, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="whole milk"))
whmilk.rules
summary(whmilk.rules)
whmilk.rules <- sort(whmilk.rules , by='confidence', decreasing = TRUE)
inspect(whmilk.rules[1:10])
topwh <- whmilk.rules[1:10]
plot(topwh, method="graph")

### After buying "whole milk", what all customer may buy
whmilk.rulesaf <- apriori(groc, parameter = list(supp=0.001, conf=0.15),appearance = list(default="rhs",lhs="whole milk"))
whmilk.rulesaf
summary(whmilk.rulesaf)
whmilk.rulesaf <- sort(whmilk.rulesaf , by='confidence', decreasing = TRUE)
inspect(whmilk.rulesaf[1:3])
topwhaf <- whmilk.rulesaf[1:10]
plot(topwhaf, method="graph")


# Bottled beer in lhs
bottbee.rules <- apriori(groc, parameter = list(supp=0.001, conf=0.7),appearance = list(default="lhs",rhs="bottled beer"))
bottbee.rules
summary(bottbee.rules)
bottbee.rules <- sort(bottbee.rules , by='confidence', decreasing = TRUE)
inspect(bottbee.rules[1:1])
topbottbee <- bottbee.rules[1:10]
plot(topbottbee, method="graph")


# Other vegge in lhs
ov.rules <- apriori(groc, parameter = list(supp=0.001, conf=0.5, maxlen = 3),appearance = list(default="rhs",lhs="soda"))
ov.rules
summary(ov.rules)
ov.rules <- sort(ov.rules , by='confidence', decreasing = TRUE)
inspect(ov.rules[1:5])
ov <- ov.rules[1:10]
plot(ov, method="graph")

## Plots
# Filter rules with confidence greater than 0.6 or 60%
subRules<-grocrules[quality(grocrules)$confidence>0.6]
#Plot SubRules
plot(subRules)
grocrules

plot(topn, method = "graph",  engine = "htmlwidget")

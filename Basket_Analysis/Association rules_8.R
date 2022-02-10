##aSSOCIATION RULES

library(tidyverse)

###Loading the package
library(arules)
?apriori

groceries<-read.transactions('groceries.csv',sep=',')

##summary of the dataset
summary(groceries)
inspect(groceries[1:5])

#Examine the frequency of items
itemFrequency(groceries[,1:20])

##plot the frequency of items
itemFrequencyPlot(groceries,topN=10)

#creating the model
gr_rules<-apriori(groceries)
inspect(gr_rules[1:5])

#zero rules created 
#Lets specify the parameters
gr_rules<-apriori(groceries,parameter = list(supp=0.001,
                                             conf=0.9))
inspect(gr_rules[1:5])#if the lift is greater than 1 its a good rule to consider otherwise if, lift less than one bad rules

##sorting the rules
gr_rules<-sort(gr_rules,by='lift',decreasing = T)
inspect(gr_rules[1:5])


##redundant rules
?is.redundant
redundant_rules<-is.redundant(gr_rules)
redundant_rules
summary(redundant_rules)

##removing the redundant rules
gr_rules<-gr_rules[!redundant_rules]
length(gr_rules)

#gr_rules beer
gr_rules_milk<-apriori(groceries,
                       parameter = list(
                         supp=0.001,
                         conf=0.08
                       ),
                       appearance = list(
                         default='rhs',
                         lhs='whole milk'
                       ))
inspect(gr_rules_milk[1:5])
class(gr_rules_milk)

###visualizing the rules
library(arulesViz)
plot(gr_rules_milk,method='graph', engine='interactive')



#install.packages("arulesViz", "datasets")
#install.packages("arules")

library(arules)
library(arulesViz)
library(datasets)

Book1 <- read.transactions("C:/Users/micha/Downloads/Book1-1-1.txt", format = "basket", sep = ",")

itemFrequencyPlot(Book1, topN = 20, type ="absolute")

rules01 <- apriori(Book1, parameter = list(supp = .001, conf = .8))

options(digits = 2)
inspect(rules01[1:10])

summary(rules01)

rules01 <- sort(rules01, by = "confidence", decreasing = TRUE)
rules01
head(rules01)

rulesQrhs <- apriori( Book1, parameter = list(supp = .001, conf = .08, minlen = 2), 
                      appearance = list(default = "lhs", rhs = "3.1"), control = list(verbose = F))
rulesQrhs <- sort(rulesQrhs, decreasing = TRUE, by = 'confidence')
inspect(rulesQrhs)

rulesQlhs <- apriori( Book1, parameter = list(supp = .001, conf = .15, minlen = 2), 
                      appearance = list(default = "rhs", lhs = "3.1"), control = list(verbose = F))
rulesQlhs <- sort(rulesQlhs, decreasing = TRUE, by = 'confidence')
inspect(rulesQlhs)

plot(rulesQlhs, method = 'graph', engine = 'interactive', shading = NA)

subrules2 <- head(sort(rulesQlhs, by = "lift"), 10)
plot( subrules2, method = 'graph')
plot( subrules2, method = 'graph', engine = 'interactive')

rulesBlhs <- apriori( Book1, parameter = list(supp = .001, conf = .08, minlen = 2), 
                      appearance = list(default = "rhs", lhs = '1.1'), control = list(verbose = F))
rulesBlhs <- sort(rulesBlhs, decreasing = TRUE, by = 'confidence')
inspect(rulesBlhs)

#retail <- read.transactions("C:/Users/micha/Downloads/retail.csv", format = "basket", sep = ",")
rules1 <- apriori(Book1, parameter = list(supp = 0.001, conf = 0.8,maxlen=15))
itemFrequencyPlot(Book1, topN = 20, type ="absolute")
rules1 <- sort(rules1, decreasing = TRUE, by = 'confidence')
inspect(rules1)


##  Test for a loop
x <- c("1.1","1.2", "1.3", "1.4", "1.5", "1.6", "1.7", "2.1","2.2", "2.3", "2.4", "2.5", "2.6",
       "3.1", "4.1", "4.2", "4.3", "5.1")
sink('C:/Users/micha/Downloads/score_2_lhsRev20210316.txt')
for (val in x) {
  print(val)
  rulesVallhs <- apriori( Book1, parameter = list(supp = .001, conf = .08, minlen = 2), 
                          appearance = list(default = "rhs", lhs = val), control = list(verbose = F))
  rulesVallhs <- sort(rulesVallhs, decreasing = TRUE, by = 'confidence')
  inspect(rulesVallhs)
  
}
sink()

#append
sink('C:/Users/micha/Downloads/score_data_rhsRev20210316.txt')
for (val in x) {
  print(val)
  rulesValrhs <- apriori( Book1, parameter = list(supp = .001, conf = .08, minlen = 2), 
                          appearance = list(default = "lhs", rhs = val), control = list(verbose = F))
  rulesValrhs <- sort(rulesValrhs, decreasing = TRUE, by = 'confidence')
  inspect(rulesValrhs)
}
sink()

rulesVAlrhs01 <- apriori( Book1, parameter = list(supp = .001, conf = .15, minlen = 2), 
                          appearance = list(default = "rhs", lhs = val), control = list(verbose = F))
rulesValrhs01 <- sort(rulesValrhs, decreasing = TRUE, by = 'confidence')

inspect(rulesValrhs01)

plot(rulesValrhs01, method = 'graph', engine = 'interactive', shading = NA)

subrules2 <- head(sort(rulesValrhs, by = "lift"), 10)
plot( subrules2, method = 'graph')
plot( subrules2, method = 'graph', engine = 'interactive')


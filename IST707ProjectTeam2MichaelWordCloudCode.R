#install.packages("janitor")
library(arules)
library(arulesViz)
library(datasets)
library(ggplot2)
library(proxy)
library(cluster)
library(stringi)
library(Matrix)
library(tidytext)
library(plyr)
library(factoextra)
library(mclust)
library(dplyr)
library(SnowballC)
library(arules)
library(wordcloud)
library(tm)
library(slam)
library(quanteda)
library(janitor)

## Loading file(s)
Q1 <- read.csv("F:\\Syracuse University\\IST 707\\Project\\Comments Only 20210312.csv")
q <- Q1#  Excludes errors 6-11
summary(q)

###  Stopwords
STOPS <-stopwords('english')
myStopwords <- c("and", "the", "that", "she", "client", "case", "apss", "yes", "aps","error", "¿", 
                 "was", "were", "did", "there", "for", "also")

##  Wordcloud

#minTermFreq <-30
#maxTermFreq <-1000

qQuest <- VCorpus(VectorSource(Q1))

qQDTM <- DocumentTermMatrix(qQuest, control = list(removePunctuation = T, tolower = T, 
                                                stemming = T, remove_seperators = T, 
                                                stopwords = STOPS))
qDTM <- DocumentTermMatrix(qQuest, control = list(stopwords = TRUE, wordLengths=c(3, 15),
                                                  removePunctuation = T, tolower = T, 
                                                  stemming = F, remove_seperators = T,
                                                  stopwords = myStopwords,
                                                  removeWords=STOPS,
                                                  removeWords=myStopwords,
                                                  stopwords = STOPS))
DTM <- as.matrix(qDTM)
qDTM <- as.matrix((qQDTM))

totDTM <- colSums(DTM)
qtotDTM <- colSums(qDTM)
wordcloud(colnames(DTM),totDTM,rot.per=0.35,colors=brewer.pal(8, "Dark2"),
          random.order=FALSE, scale = c(5,1))

wordcloud(colnames(qDTM),qtotDTM,rot.per=0.35,colors=brewer.pal(8, "Dark2"),
          random.order=FALSE, scale = c(5,1))
############

#######################

WordFreq <- colSums(as.matrix(DTM))
(head(WordFreq, 20))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord, 20)])
(WordFreq[tail(ord, 20)])

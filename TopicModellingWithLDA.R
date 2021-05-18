library(jsonlite)
library(tibble)
library(quanteda)
library(methods)
library(tidytext)
library(tidyverse)
library(tm)
library(topicmodels)
library(reshape2)
library(corpustools)
library(tidyr)
library(textmineR)
install.packages("topicmodels")
library(magrittr)
install.packages("tidyverse")
install.packages("textmineR")
library(textmineR)
library(dplyr)

###Importing data from json file
oct2020 <- stream_in(file("C:/Users/chait/Documents/688/HW6/OCT2020/OCT2020.json"))
oct2020_flat <- flatten(oct2020)
o2020_tbl <- as.data.frame(oct2020_flat)
titles <- o2020_tbl$data.tagline
titles
text <- gsub("@[[:alpha:]]*","", titles)
text_corpus <- Corpus(VectorSource(titles))
text_corpus[[1]]
text_corpus <- tm_map(text_corpus, tolower)
text_corpus <- tm_map(text_corpus, removeWords, 
                      stopwords("english"))
text_corpus <- tm_map(text_corpus, removePunctuation)

text_corpus <- tm_map(text_corpus, removeNumbers)

text_corpus <- tm_map(text_corpus, stemDocument, language = "en")

text_corpus <- tm_map(text_corpus, stripWhitespace)


###Creating term frequency matrix
dtm<-DocumentTermMatrix(text_corpus)

##selecting small portion as R studio is crashing because pf the large files size
dtm<-dtm[1:1000,]

##checking for row sum to detect nonzero rows
totals<-apply(dtm,1,sum)
dtm<-dtm[totals>0,]

##inspecting
inspect(dtm[1:5,5:10])

###Finding most frequent terms
findFreqTerms(Frequencies,lowfreq = 20)

### removing less frequent
removeSparseTerms(Frequencies,0.997)

##applying lda
lda<- LDA(dtm,k=10)
lda
#attaching the probabilities to topics
topics<- tidy(lda,matrix="beta")
topics
##better visualization
DistribTerm <- dcast(topics,term~topic,value.var="beta")
DistribTerm

##selecting top 20 terms in each doc
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms
##calculating similarity /distance using "eucleadean distance"

lda.similarity <- as.data.frame(topics$beta) %>%
  scale() %>%
  dist(method = "euclidean") %>%
  hclust(method = "ward.D2")
##Plot
par(mar = c(0, 4, 4, 2))
plot(lda.similarity,
     main = "LDA topic similarity by features",
     xlab = "",
     sub = "")

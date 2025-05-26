
library(dplyr)
library(tm)
library(SnowballC) 
library(topicmodels)
library(tidytext)
library(ggplot2)
library(tidyr)
library(wordcloud)
library(RColorBrewer)
library(sentimentr)
library(Rtsne)
library(readr)


data <- read_csv("C:/Github Repos/data-science-tutorial/Lab Test 2 Prep/src/dailystar_articles_with_text.csv", show_col_types = FALSE)

text_data <- data$article_text
print(text_data)

corpus <- Corpus(VectorSource(text_data))

custom_stopwords <- c("said", "year", "will", "bangladesh","s","go","told","al","jazeera",'"',"t","sat","hasn","also","many","says") 

corpus <- tm_map(corpus, content_transformer(tolower))    
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z ]", " ", x))) 
corpus <- tm_map(corpus, removePunctuation)                 
corpus <- tm_map(corpus, removeNumbers)                   
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, custom_stopwords)     
corpus <- tm_map(corpus, stripWhitespace)                 


                     
tokenized_data <- lapply(corpus, function(doc) unlist(strsplit(as.character(doc), "\\s+")))
# print(tokenized_data)


# Remove empty docs
corpus_clean <- sapply(corpus, as.character)
non_empty_indices <- which(nchar(corpus_clean) > 0)
corpus <- corpus[non_empty_indices]



dtm <- DocumentTermMatrix(corpus)
num_topics <- 10
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 42))
top_terms <- terms(lda_model, 10) 


cat("Top Terms for Each Topic:\n")
print(top_terms)

# 
# library(dplyr)
# library(tm)
# library(SnowballC) 
# library(topicmodels)
# library(tidytext)
# library(ggplot2)
# library(tidyr)
# library(wordcloud)
# library(RColorBrewer)
# library(sentimentr)
# library(Rtsne)
# library(readr)
# data <- read_csv("C:/Github Repos/data-science-tutorial/FT/Lab Tasks/saved/dt_articles_with_text.csv", show_col_types = FALSE)
# 
# text_data <- data$article_text
# 
# corpus <- Corpus(VectorSource(text_data))
# 
# custom_stopwords <- c("said", "year", "will", "bangladesh","s","go","told","al","jazeera",'"',"t","sat","hasn","also","many","says") 
# 
# corpus <- tm_map(corpus, content_transformer(tolower))    
# corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z ]", " ", x))) 
# corpus <- tm_map(corpus, removePunctuation)                 
# corpus <- tm_map(corpus, removeNumbers)                   
# corpus <- tm_map(corpus, removeWords, stopwords("english")) 
# corpus <- tm_map(corpus, removeWords, custom_stopwords)     
# corpus <- tm_map(corpus, stripWhitespace)                 
#                      
# tokenized_data <- lapply(corpus, function(doc) unlist(strsplit(as.character(doc), "\\s+")))
# 
# dtm <- DocumentTermMatrix(corpus)
# num_topics <- 6 
# lda_model <- LDA(dtm, k = num_topics, control = list(seed = 42))
# top_terms <- terms(lda_model, 10) 
# 
# 
# cat("Top Terms for Each Topic:\n")
# print(top_terms)















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

# Read CSV
data <- read_csv("C:/Github Repos/data-science-tutorial/FT/Lab Tasks/saved/dt_articles_with_text.csv", show_col_types = FALSE)

# Check column names
print(colnames(data))

# Use actual column name
text_data <- data$article_text

# Clean up the text data
text_data <- as.character(text_data)
text_data <- text_data[!is.na(text_data)]

# Create corpus
corpus <- Corpus(VectorSource(text_data))

# Custom stop words
custom_stopwords <- c("said", "year", "will", "bangladesh", "s", "go", "told", "al", "jazeera", "\"", "t", "sat", "hasn", "also", "many", "says")

# Clean corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z ]", " ", x)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, custom_stopwords)
corpus <- tm_map(corpus, stripWhitespace)

# Create DTM
dtm <- DocumentTermMatrix(corpus)

# Remove empty documents (if any)
row_totals <- apply(dtm, 1, sum)
dtm <- dtm[row_totals > 0, ]

# Run LDA
num_topics <- 10
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 42))
top_terms <- terms(lda_model, 10)

# Output results
cat("Top Terms for Each Topic:\n")
print(top_terms)


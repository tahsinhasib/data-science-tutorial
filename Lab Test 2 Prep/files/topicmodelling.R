
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


data <- read_csv("C:/Github Repos/data-science-tutorial/Lab Test 2 Prep/src/dummy_articles.csv", show_col_types = FALSE)

#-- my own
# Read from tab-separated file (adjust path accordingly)
 # data <- read_tsv("C:/Github Repos/data-science-tutorial/Lab Test 2 Prep/src/dummy_articles.txt", show_col_types = FALSE)



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


# Convert DTM to data frame
dtm_matrix <- as.matrix(dtm)
term_freq <- colSums(dtm_matrix)
term_freq_df <- data.frame(term = names(term_freq), freq = term_freq)

# Get top 20 terms
top_terms <- term_freq_df %>%
  arrange(desc(freq)) %>%
  head(20)

# Plot
ggplot(top_terms, aes(x = reorder(term, freq), y = freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Terms in Document-Term Matrix",
       x = "Term", y = "Frequency") +
  theme_minimal()



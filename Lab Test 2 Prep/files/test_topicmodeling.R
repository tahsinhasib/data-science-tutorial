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
library(reshape2)



# Data preparation
# Read the data
data <- read_csv("C:/Github Repos/data-science-tutorial/Lab Test 2 Prep/src/dailystar_articles_with_text.csv", show_col_types = FALSE)

# Drop NA or empty texts
data <- data[!is.na(data$article_text) & data$article_text != "", ]

# Custom stopwords (domain-specific)
custom_stopwords <- c("said", "year", "will", "bangladesh", "s", "go", "told", "al", "jazeera", "t", "sat", "hasn", "also", "many", "says")


# Creating the Corpus & Document-Term Matrix
# Build corpus
corpus <- Corpus(VectorSource(data$article_text))


# Preprocess text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z ]", " ", x)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, custom_stopwords)
corpus <- tm_map(corpus, stripWhitespace)


# Remove empty documents
corpus_clean <- sapply(corpus, as.character)
non_empty_indices <- which(nchar(corpus_clean) > 0)
corpus <- corpus[non_empty_indices]


# After applying all tm_map transformations:
corpus <- tm_map(corpus, stripWhitespace)

# Remove empty documents
non_empty_idx <- sapply(corpus, function(x) {
  content <- as.character(x)
  content <- gsub("\\s+", "", content)  # remove all whitespace
  nchar(content) > 0                    # keep non-empty content
})

corpus <- corpus[non_empty_idx]



# Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)
print(dtm)

# Calculate TF-IDF (optional: you can filter rare/common terms)
# Inspect tf-idf weights (optional: not used in LDA directly)
dtm_tfidf <- weightTfIdf(dtm)
print(dtm_tfidf)


# Topic Modeling with LDA
# Set number of topics
num_topics <- 6

# Fit LDA model
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 42))


# Examine the Topics
top_terms <- terms(lda_model, 10)  # Top 10 terms per topic
print(top_terms)
topic_probs <- posterior(lda_model)$topics  # Matrix: rows = docs, cols = topics
head(topic_probs)  # Show the topic distribution for the first few docs


# Interpret the Results
library(tidyverse)

# Convert to tidy format
lda_tidy <- tidy(lda_model, matrix = "beta")

# Plot top terms in each topic
lda_tidy %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  ggplot(aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_x_reordered() +
  coord_flip() +
  labs(title = "Top Terms per Topic", x = "", y = "Probability (Beta)")


# Assign most likely topic to each doc
doc_topics <- data.frame(doc_id = rownames(topic_probs),
                         topic = apply(topic_probs, 1, which.max))

head(doc_topics)

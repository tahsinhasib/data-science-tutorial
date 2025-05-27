# Load required libraries
library(tm)
library(tokenizers)
library(SnowballC)
library(stringr)
library(textclean)
library(hunspell)
library(dplyr)
library(readr)

# Load CSV
data <- read_csv("articles.csv")

# Function to expand contractions
expand_contractions <- function(text) {
  text <- str_replace_all(text, "can't", "cannot")
  text <- str_replace_all(text, "won't", "will not")
  text <- str_replace_all(text, "n't", " not")
  text <- str_replace_all(text, "'re", " are")
  text <- str_replace_all(text, "'s", " is")
  text <- str_replace_all(text, "'d", " would")
  text <- str_replace_all(text, "'ll", " will")
  text <- str_replace_all(text, "'t", " not")
  text <- str_replace_all(text, "'ve", " have")
  text <- str_replace_all(text, "'m", " am")
  return(text)
}

# Function to preprocess a single text
clean_article_text <- function(text) {
  text <- tolower(text)                       # Lowercase
  text <- expand_contractions(text)           # Expand contractions
  text <- replace_emoji(text)                 # Replace emojis
  text <- replace_emoticon(text)              # Replace emoticons
  text <- gsub("[^a-z\\s]", "", text)         # Remove punctuation, digits
  text <- stripWhitespace(text)               # Remove extra whitespace
  
  # Remove stop words
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  text <- corpus[[1]]$content
  
  # Tokenize
  tokens <- tokenize_words(text)[[1]]
  
  # Stemming
  stemmed <- wordStem(tokens, language = "en")
  
  # Rejoin to a single string (optional)
  cleaned_text <- paste(stemmed, collapse = " ")
  return(cleaned_text)
}

# Apply the cleaning function to each article
data$cleaned_text <- sapply(data$article_text, clean_article_text)

# View cleaned data
head(data[, c("date_published", "cleaned_text")])

# Optional: Save the cleaned data
write_csv(data, "cleaned_articles.csv")

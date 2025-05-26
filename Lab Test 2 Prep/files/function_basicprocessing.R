# Load necessary libraries
library(readr)
library(stringr)
library(tm)
library(tokenizers)
library(SnowballC)
library(textclean)

# Load CSV
df <- read_csv("C:/Github Repos/data-science-tutorial/Lab Test 2 Prep/src/dstar-copy-test.csv")

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

# Main text processing function
process_text <- function(text) {
  if (is.na(text) || text == "") return(NA)  # Handle missing or empty text
  
  # Expand Contradiction
  cat('\n')
  print("Original")
  print(text)
  cat('\n')
  text <- expand_contractions(text)
  text <- replace_emoji(text)
  text <- replace_emoticon(text)
  print("After contradictions")
  print(text)
  
  # Clean text
  cat('\n')
  clean_text <- tolower(text)                         # Lowercase
  clean_text <- gsub("[^a-z\\s]", "", clean_text)     # Remove punctuation, digits
  clean_text <- stripWhitespace(clean_text)           # Remove extra spaces
  print("Cleaned Text")
  print(clean_text)
  
  # Remove stop words
  cat('\n')
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  text_cleaned <- corpus[[1]]$content
  print("Removed stop words")
  print(text_cleaned)
  
  # Tokenize
  cat('\n')
  tokens <- tokenize_words(text)
  print("Tokens")
  print(tokens[[1]])
  
  # Stemming (optional if you want stemmed version)
  cat('\n')
  tokens_stemmed <- wordStem(tokens, language = "en")
  print("Tokens stemmed")
  print(tokens_stemmed)
  
  # Return processed result
  return(paste(tokens_stemmed, collapse = " "))
}

# Truncate the dataframe to maximum 3 rows
df <- head(df, 3)

# Apply the function to each row of the article_text column
df$processed_text <- sapply(df$article_text, process_text)

# View results
# head(df$processed_text)

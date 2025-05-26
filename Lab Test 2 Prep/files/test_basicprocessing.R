# Load required libraries
library(tm)
library(tokenizers)
library(SnowballC)
library(stringr)
library(textclean)
library(hunspell)
library(dplyr)
library(readr)

# Load your CSV
df <- read_csv("C:/Github Repos/data-science-tutorial/Lab Test 2 Prep/src/dstar-copy-test.csv")  # Use read.csv() if it's not UTF-8

# Function to clean and preprocess a single text
process_text <- function(text) {
  if (is.na(text) || text == "") return(NA)
  
  # 1. Lowercase
  text <- tolower(text)
  
  
  # 2. Expand contractions
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
  
  # 3. Remove emojis and emoticons
  text <- replace_emoji(text)
  text <- replace_emoticon(text)
  
  # 4. Remove punctuation and digits
  text <- gsub("[^a-z\\s]", "", text)
  
  # 5. Remove extra whitespace
  text <- stripWhitespace(text)
  
  # 6. Tokenization
  tokens <- tokenize_words(text)[[1]]
  
  # 7. Stop words removal
  tokens <- tokens[!tokens %in% stopwords("en")]
  
  # 8. Stemming
  tokens <- wordStem(tokens, language = "en")
  
  # 9. Spell check (optional and slow — skip for large data)
  misspelled <- hunspell(tokens)
  suggestions <- sapply(misspelled, hunspell_suggest)
  print(suggestions)  # optional
  
  # 10. Reconstruct cleaned text
  cleaned_text <- paste(tokens, collapse = " ")
  return(cleaned_text)
}

# Apply to article_text column
df$processed_text <- sapply(df$article_text, process_text)

# View result
head(df$processed_text, 2)

# Optional: Save cleaned output
write_csv(df, "C:/Github Repos/data-science-tutorial/Lab Test 2 Prep/src/test/cleaned-copy.csv")

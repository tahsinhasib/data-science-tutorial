# 1. Text Cleaning
library(tm)

text <- "The Price of THIS product is $1,000!!! WOW."
clean_text <- tolower(text)                         # Lowercase
clean_text <- gsub("[^a-z\\s]", "", clean_text)     # Remove punctuation, digits
clean_text <- stripWhitespace(clean_text)           # Remove extra spaces

cat(clean_text)
# Output: "the price of this product is wow"

# 2. Tokenization
library(tokenizers)

text <- "Natural Language Processing with R is powerful."
tokens <- tokenize_words(text)

print(tokens[[1]])
# Output: [1] "natural" "language" "processing" "with" "r" "is" "powerful"

#  3. Stop Words Removal
library(tm)

text <- "This is an example sentence for stop word removal."
text_corpus <- Corpus(VectorSource(text))
text_corpus <- tm_map(text_corpus, removeWords, stopwords("en"))

cat(text_corpus[[1]]$content)
# Output: "This   example sentence   stop word removal."

# 4. Stemming and Lemmatization
library(SnowballC)

words <- c("running", "runs", "ran", "easily", "fairly")
stemmed <- wordStem(words, language = "en")

print(stemmed)
# Output: [1] "run"  "run"  "ran"  "easili"  "fairli"


# 5. Handling Contractions
library(stringr)

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

# Example usage:
text <- "I can't go. He's not ready. They've left already."
expand_contractions(text)


# 6. Handling Emojis and Emoticons
library(textclean)

text <- "I love this! ???????? It's awesome :)"
text <- replace_emoji(text)       # Converts emojis to words
text <- replace_emoticon(text)    # Converts emoticons to words

cat(text)
# Output: "I love this! smiling_face_with_heart_eyes thumbs_up It's awesome smile"


# 7. Spell Checking
library(hunspell)

text <- "This is a smple text with erors."
words <- unlist(strsplit(text, " "))
misspelled <- hunspell(words)

print(misspelled)
# Output: List of incorrect words
# Suggest corrections:
sapply(misspelled, hunspell_suggest)





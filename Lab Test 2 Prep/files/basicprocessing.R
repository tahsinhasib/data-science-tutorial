

# Load your CSV ----- my codes
df <- read_csv("C:/Github Repos/data-science-tutorial/Lab Test 2 Prep/src/dstar-copy-test.csv")  # Use read.csv() if it's not UTF-8
# if i want to read from a text file
# text <- readLines("C:/path/to/your/file.txt", warn = FALSE)
#--------------------------------------------



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


# 1. Text Cleaning
library(tm)

# article_text[n] if i change n it will work on the nth row
text <- df$article_text[1]

clean_text <- tolower(text)                         # Lowercase
clean_text <- gsub("[^a-z\\s]", "", clean_text)     # Remove punctuation, digits
clean_text <- stripWhitespace(clean_text)           # Remove extra spaces

cat(clean_text)

# 2. Tokenization
library(tokenizers)

# text <- "Natural Language Processing with R is powerful."
tokens <- tokenize_words(text)

print(tokens[[1]])


#  3. Stop Words Removal
library(tm)

# text <- "This is an example sentence for stop word removal."
text_corpus <- Corpus(VectorSource(text))
text_corpus <- tm_map(text_corpus, removeWords, stopwords("en"))

cat(text_corpus[[1]]$content)


# 4. Stemming and Lemmatization
library(SnowballC)

words <- c("running", "runs", "ran", "easily", "fairly")
stemmed <- wordStem(words, language = "en")

print(stemmed)



# Example usage:
# text <- "I can't go. He's not ready. They've left already."
expand_contractions(text)


# 6. Handling Emojis and Emoticons
library(textclean)

# text <- "I love this! ???????? It's awesome :)"
text <- replace_emoji(text)       # Converts emojis to words
text <- replace_emoticon(text)    # Converts emoticons to words

cat(text)


# # 7. Spell Checking
# library(hunspell)
# 
# # text <- "This is a smple text with erors."
# words <- unlist(strsplit(text, " "))
# misspelled <- hunspell(words)

# print(misspelled)
# Output: List of incorrect words
# Suggest corrections:
# sapply(misspelled, hunspell_suggest)





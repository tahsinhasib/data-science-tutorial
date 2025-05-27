# Md. Tahsin Hasib
# 22 - 46026 - 1
# Sec G


################################# Reading the file from a csv first Part - 1
# Load necessary libraries
library(readr)
library(stringr)
library(tm)
library(tokenizers)
library(SnowballC)
library(textclean)

# Load CSV
df <- read_csv("C:/Github Repos/data-science-tutorial/LAB TASK - 2/scraped_articles.csv")




####################################################### Perform basic cleaning Part - 2
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
# Apply the function to each row of the content column
df$processed_text <- sapply(df$content, process_text)

# View results
# head(df$processed_text)


############################################### Generate word cloud Part - 3
# Load required libraries
library(readr)
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Doing this step each time
# Step 1: Read the CSV file
data <- read_csv("C:/Github Repos/data-science-tutorial/LAB TASK - 2/scraped_articles.csv")

# Step 2: Extract article_text column and combine all text into one string
text_data <- data$content
text_corpus <- paste(text_data, collapse = " ")

# Step 3: Create a corpus and clean the text
corpus <- Corpus(VectorSource(text_corpus))
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)

# Step 4: Create term-document matrix
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)

# Step 5: Draw the word cloud
set.seed(123)
# Save word cloud to a PNG file
png("C:/Github Repos/data-science-tutorial/LAB TASK - 2/labtask2wordcloud.png", width = 1000, height = 800)
wordcloud(words = df$word,
          freq = df$freq,
          min.freq = 3,
          max.words = 100,
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
dev.off()

cat("Word cloud saved\n")



################################################# Creation of DTM (Document Term Matrix) Part - 4


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


data <- read_csv("C:/Github Repos/data-science-tutorial/LAB TASK - 2/scraped_articles.csv", show_col_types = FALSE)

#-- my own
# Read from tab-separated file (adjust path accordingly)
# data <- read_tsv("C:/Github Repos/data-science-tutorial/Lab Test 2 Prep/src/dummy_articles.txt", show_col_types = FALSE)



text_data <- data$content
print(text_data)

corpus <- Corpus(VectorSource(text_data))

custom_stopwords <- c("said", "year", "weather", "will", "said", "property", "management", "what", "will", "bangladesh","s","go","told","al","jazeera",'"',"t","sat","hasn","also","many","says") 

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


###################################################### DTM Happens here
dtm <- DocumentTermMatrix(corpus)

print("DTM Matrix: ")
print(dtm)


###################################################### LDA with 5 topics here Part - 5
num_topics <- 5
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 42))
top_terms <- terms(lda_model, 10) 


cat("Top Terms for Each Topic:\n")
print(top_terms)


###################### Top words assosiated with topics on a barchart Part - 6

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


# Assigning most likely topic to each doc
doc_topics <- data.frame(doc_id = rownames(topic_probs),
                         topic = apply(topic_probs, 1, which.max))

head(doc_topics)

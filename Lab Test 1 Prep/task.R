# Load required libraries
library(rvest)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)

# Step 1: Scrape all text from <p> tags on pages 1-3
base_url <- "https://www.health.harvard.edu/blog?page="
pages <- 1:3

get_paragraph_text <- function(url) {
  tryCatch({
    page <- read_html(url)
    text <- page %>% html_nodes("p") %>% html_text(trim = TRUE)
    return(text)
  }, error = function(e) {
    message(paste("Error reading:", url))
    return(NULL)
  })
}

all_paragraphs <- unlist(lapply(paste0(base_url, pages), get_paragraph_text))
cat("Total paragraphs scraped:", length(all_paragraphs), "\n")

# Combine all text
all_text <- paste(all_paragraphs, collapse = " ")

# Step 2: Clean text

## a. Convert to lowercase
clean_text <- tolower(all_text)
cat("\nSample after lowercasing:\n", substr(clean_text, 1, 300), "\n")

## b. Remove punctuation and numbers
clean_text <- gsub("[^a-z\\s]", " ", clean_text)
cat("\nSample after removing punctuation and numbers:\n", substr(clean_text, 1, 300), "\n")

## c. Remove short words (less than 4 characters)
clean_text <- gsub("\\b\\w{1,3}\\b", "", clean_text)
cat("\nSample after removing short words:\n", substr(clean_text, 1, 300), "\n")

## d. Remove extra whitespace
clean_text <- str_squish(clean_text)

## e. Tokenize
tokens <- data.frame(word = unlist(strsplit(clean_text, "\\s+")), stringsAsFactors = FALSE)

## f. Remove stopwords
data("stop_words")
tokens <- tokens %>% anti_join(stop_words, by = "word")
cat("\nSample after removing stop words:\n", head(tokens$word, 20), "\n")

# Step 3: Word frequency
word_freq <- tokens %>%
  count(word, sort = TRUE)

# Step 4: Plot top 15 most frequent words
top_words <- word_freq %>% top_n(15, n)

ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Most Frequent Words",
       x = "Words", y = "Frequency") +
  theme_minimal()

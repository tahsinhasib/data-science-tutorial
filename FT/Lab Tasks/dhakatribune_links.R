library(rvest)
library(dplyr)
library(readr)

# Base URL
base_url <- "https://www.dhakatribune.com/"

# Read homepage
homepage <- read_html(base_url)

# Extract <a> tags
a_nodes <- homepage %>% html_nodes("a")

# Get href and aria-label
links_data <- data.frame(
  href = a_nodes %>% html_attr("href"),
  aria_label = a_nodes %>% html_attr("title"),
  stringsAsFactors = FALSE
)

# Filter: keep only valid hrefs and aria-labels (non-NA)
links_data <- links_data %>%
  filter(!is.na(href), !is.na(aria_label)) %>%
  mutate(
    href = ifelse(grepl("^http", href), href, paste0(base_url, href))
  ) %>%
  distinct()

# Save to CSV
write_csv(links_data, "C:/Github Repos/data-science-tutorial/FT/Lab Tasks/saved/dt_links_with_labels.csv")

cat("Saved", nrow(links_data), "links with aria-labels to dt_links_with_labels.csv\n")





# Example: Scrape links and alt text from similar HTML structure

# Suppose 'html_content' contains the HTML snippet as a string
library(rvest)
library(dplyr)

# Parse the HTML (replace 'html_content' with your HTML string or file)
# html <- read_html(html_content)

# For demonstration, let's assume you have already read the HTML as 'html'
# Extract <a> tags inside <div class="h-full">
links <- html %>%
    html_nodes("div.h-full a")

# Get href and image alt text
scraped_data <- data.frame(
    href = links %>% html_attr("href"),
    img_alt = links %>% html_node("img") %>% html_attr("alt"),
    stringsAsFactors = FALSE
)

print(scraped_data)

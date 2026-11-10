#Q3
#Libraries
library(rvest)
library(dplyr)
library(ggplot2)
library(textrank)
library(tidytext) 
library(tm)

#helper function for text cleaning
clean_text_simple <- function(text_vec) {
  text_vec %>%
    tolower() %>%
    gsub("http\\S+|www\\S+", "", .) %>%
    gsub("<.*?>", "", .) %>%
    gsub("[^a-z\\s]", "", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
}

#Q3.1
target_url <- "https://ourworldindata.org/"
web_page <- read_html(target_url)

extracted_paragraphs <- web_page %>%
  html_nodes("p") %>%
  html_text()

webtext_df <- data.frame(
  paragraph_id = seq_along(extracted_paragraphs),
  raw_text = extracted_paragraphs,
  stringsAsFactors = FALSE
) %>% filter(nchar(trimws(raw_text)) > 50)

cat(" 3.1 SCRAPED DATA FRAME SAMPLE \n")
print(head(webtext_df, 3))


#Q3.2
webtext_df$clean_text <- clean_text_simple(webtext_df$raw_text)


#Q3.3 
if (!dir.exists("data")) {
  dir.create("data")
}

# Helper function for text cleaning (fixed regex for base R whitespace safety)
clean_text_simple <- function(text_vec) {
  text_vec %>% 
    tolower() %>% 
    gsub("http\\S+|www\\S+", "", .) %>% 
    gsub("<.*?>", "", .) %>% 
    gsub("[^a-z[:space:]]", "", .) %>% 
    gsub("\\s+", " ", .) %>% 
    trimws()
}

#Q3.1: Data Collection & Web Scraping
target_url <- "https://ourworldindata.org/"
web_page <- read_html(target_url)

extracted_paragraphs <- web_page %>% html_nodes("p") %>% html_text()
webtext_df <- data.frame(
  paragraph_id = seq_along(extracted_paragraphs), 
  raw_text = extracted_paragraphs, 
  stringsAsFactors = FALSE
) %>% filter(nchar(trimws(raw_text)) > 50)

cat("\n 3.1 SCRAPED DATA FRAME SAMPLE \n")
print(head(webtext_df, 3))


# Q3.2: Text Denoising & Preprocessing
webtext_df$clean_text <- clean_text_simple(webtext_df$raw_text)


# Q3.3: Extractive Summarization & Frequency Scoring
full_corpus_text <- paste(webtext_df$clean_text, collapse = " ")

#extract individual sentences cleanly
sentences_vec <- unlist(strsplit(paste(webtext_df$raw_text, collapse = " "), "(?<=[.!?])\\s+", perl = TRUE))
sentences_vec <- sentences_vec[nchar(sentences_vec) > 20]

#build sentence dataframe for TextRank input
sentences_df <- data.frame(
  textrank_id = seq_along(sentences_vec),
  sentence = sentences_vec,
  stringsAsFactors = FALSE
)

#tokenize words per sentence for TextRank
words_per_sentence <- sentences_df %>%
  mutate(clean = clean_text_simple(sentence)) %>%
  tidytext::unnest_tokens(word, clean) %>%
  filter(!word %in% tm::stopwords("en"), nchar(word) > 2) %>%
  select(textrank_id, word)

# Method 1: TextRank Graph-Based Summary
tr_model <- textrank_sentences(data = sentences_df, terminology = words_per_sentence)
tr_summary_df <- summary(tr_model, n = 3)
summary_textrank <- as.character(tr_summary_df)

# Method 2: Frequency-Scored Summary
words_vec <- unlist(strsplit(full_corpus_text, "\\s+"))
words_vec <- words_vec[!(words_vec %in% tm::stopwords("en"))]
freq_table <- sort(table(words_vec), decreasing = TRUE)

score_sentence <- function(sentence) {
  s_words <- unlist(strsplit(clean_text_simple(sentence), "\\s+"))
  s_words <- s_words[s_words %in% names(freq_table)]
  if(length(s_words) == 0) return(0)
  sum(freq_table[s_words]) / length(s_words)
}

sent_scores <- sapply(sentences_vec, score_sentence)
summary_freq <- names(sort(sent_scores, decreasing = TRUE)[1:3])

cat("\n QUESTION 3.3: EXTRACTIVE SUMMARIES \n")
cat("\n TextRank Graph-Based Summary \n"); print(summary_textrank)
cat("\n Frequency-Scored Summary \n"); print(summary_freq)



#visualizations & saving graphs

#visualization 1: Top 10 Most Frequent Terms in Scraped Corpus
top_freq_df <- data.frame(
  word = names(freq_table[1:10]), 
  count = as.numeric(freq_table[1:10])
)

p2 <- ggplot(top_freq_df, aes(x = reorder(word, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Most Frequent Terms in Scraped Web Corpus",
    x = "Extracted Terms", 
    y = "Frequency Count"
  ) +
  theme_classic()

print(p2)
ggsave(file.path("data", "q3_top_frequent_terms.png"), plot = p2, width = 8, height = 4.5, dpi = 300)


#visualization 2: Distribution of Sentence Character Lengths
sent_df <- data.frame(length = nchar(sentences_vec))

p3 <- ggplot(sent_df, aes(x = length)) +
  geom_histogram(binwidth = 30, fill = "darkseagreen", color = "black") +
  labs(
    title = "Distribution of Sentence Character Lengths",
    x = "Character Count per Sentence", 
    y = "Number of Sentences"
  ) +
  theme_light()

print(p3)
ggsave(file.path("data", "q3_sentence_length_distribution.png"), plot = p3, width = 8, height = 4.5, dpi = 300)


#visualization 3: Top 10 Sentences by Frequency Score
top_sent <- sort(sent_scores, decreasing = TRUE)[1:10]
top_sent_df <- data.frame(
  sentence_no = paste0("S", match(names(top_sent), sentences_vec)), 
  score = as.numeric(top_sent)
)

p4 <- ggplot(top_sent_df, aes(x = reorder(sentence_no, score), y = score)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Sentences by Frequency Score", 
    x = "Sentence Number", 
    y = "Average Word Frequency Score"
  ) +
  theme_classic()

print(p4)
ggsave(file.path("data", "q3_top_sentence_scores.png"), plot = p4, width = 8, height = 4.5, dpi = 300)
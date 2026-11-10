#Q2
#Libraries
library(tm)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(topicmodels)
library(tidytext)
library(wordcloud)
library(RColorBrewer)

set.seed(42)

#Q2.1
q2_zip <- file.path("data", "news.zip")

if (!file.exists(q2_zip)) {
  stop("Archive not found at: ", q2_zip, "\nEnsure 'news.zip' is inside your 'data/' folder.")
}

temp_news <- file.path(tempdir(), "q2_news_corpus")
dir.create(temp_news, showWarnings = FALSE, recursive = TRUE)
unzip(q2_zip, exdir = temp_news)

news_files <- list.files(temp_news, full.names = TRUE, recursive = TRUE)
news_files <- news_files[!dir.exists(news_files)]

news_df <- data.frame(
  file_name = basename(news_files),
  content = rep(NA_character_, length(news_files)),
  stringsAsFactors = FALSE
)

for (i in seq_along(news_files)) {
  news_df$content[i] <- paste(readLines(news_files[i], warn = FALSE, encoding = "UTF-8"), collapse = " ")
}
unlink(temp_news, recursive = TRUE)

#tokenize into individual words and removing stop words 
tokenized_words <- news_df %>%
  mutate(doc_id = row_number()) %>%
  #break blocks into individual words
  unnest_tokens(word, content) %>%
  #strip numbers and symbols
  mutate(word = gsub("[^a-z]", "", tolower(word))) %>%
  filter(nchar(word) > 2) %>%
  #remove English stop words
  anti_join(stop_words, by = "word")

#generate document word frequencies
word_counts <- tokenized_words %>%
  count(doc_id, word, sort = TRUE)

cat("2.1 DENOISED TOKENIZED SAMPLE\n")
print(head(word_counts, 10))

#Q2.2
dtm <- word_counts %>%
  cast_dtm(doc_id, word, n)

n_topics <- 2
k_words <- 5
lda_model <- LDA(dtm, k = n_topics, method = "Gibbs", 
                 control = list(seed = 42, burnin = 100, iter = 500))

#extracting Beta probability matrix
lda_topics <- tidy(lda_model, matrix = "beta")

q2_top_terms <- lda_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = k_words) %>%
  ungroup() %>%
  arrange(topic, -beta)

cat("\n 2.2 TOP K WORDS PER TOPIC (BETA PROBABILITIES) \n")
print(q2_top_terms)

#Q2.3
p_bar <- q2_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.4f", beta)), 
            hjust = -0.15, size = 3.8, fontface = "bold") +
  facet_wrap(~ paste("Topic", topic), scales = "free") +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(
    title = "Top Keywords by Posterior Beta Probability per Topic",
    subtitle = "Latent Dirichlet Allocation (Gibbs Sampling)",
    x = "Per-Topic Word Probability (Beta)", 
    y = "Mined Term"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold", size = 11)
  )

#display
print(p_bar)

#save as png
ggsave(file.path("data", "topic_beta_bar_chart.png"), plot = p_bar, width = 8.5, height = 4.5, dpi = 300)

#visualization 2: word clouds
wordcloud_path <- file.path("data", "topic_wordclouds.png")
png(filename = wordcloud_path, width = 900, height = 450, res = 120)

par(mfrow = c(1, n_topics), mar = c(2, 2, 3, 2))

for (i in 1:n_topics) {
  topic_words <- lda_topics %>% 
    filter(topic == i) %>% 
    arrange(desc(beta)) %>%
    head(10)
  
  norm_freq <- round((topic_words$beta / max(topic_words$beta)) * 100)
  
  wordcloud(
    words        = topic_words$term, 
    freq         = norm_freq,
    min.freq     = 1,
    max.words    = 10, 
    scale        = c(2.5, 0.8),
    random.order = FALSE,
    rot.per      = 0,
    colors       = brewer.pal(8, "Dark2")
  )
    title(main         = paste("Topic", i))
}

dev.off()
par(mfrow = c(1, 1))

cat("\nSUCCESS: Saved both visualizations to your 'data/' folder:\n")
cat(" 1. Bar Chart (with exact numeric values): data/topic_beta_bar_chart.png\n")
cat(" 2. Scaled Word Cloud Graphic:              data/topic_wordclouds.png\n")
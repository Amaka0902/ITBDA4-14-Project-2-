#Q4 ETL: libraries
library(rvest)
library(stringr)
library(dplyr)
library(tibble)

#Q4.1 
months     <- "(?:January|February|March|April|May|June|July|August|September|October|November|December)"
date_pat   <- paste0(months, " \\d{1,2}, \\d{4}")
name_pat   <- "[A-Z][A-Za-z'’.-]+(?: [A-Z][A-Za-z'’.-]+)+"
byline_pat <- paste0("^", name_pat, "(?:(?:,| and|, and) ", name_pat, ")*(?: and)?(?: ", date_pat, ")?$")
date_only  <- paste0("^", date_pat, "$")
tail_pat   <- paste0("(?<=[.!?])\\s+", name_pat, "(?:(?:,| and|, and) ", name_pat, ")*(?: ", date_pat, ")?$")

#normalised key for filtering
norm_key <- function(x) x %>% iconv(to = "ASCII//TRANSLIT") %>% tolower() %>% gsub("[^a-z0-9]", "", .)

scrape_owid <- function(url) {
  doc   <- read_html(url)
  nodes <- doc %>% html_nodes("h2, h3, h4, p")
  records <- list(); current_title <- NA_character_
  buf <- character(); auth <- character(); dts <- character()
  
  flush_record <- function() {
    if (!is.na(current_title) && length(buf) > 0) {
      records[[length(records) + 1]] <<- list(
        title_raw   = current_title,
        content_raw = paste(buf, collapse = " "),
        author      = if (length(auth)) paste(unique(auth), collapse = " and ") else NA_character_,
        pub_date    = if (length(dts)) dts[1] else NA_character_,
        source_page = url)
    }
  }
  
  for (nd in nodes) {
    tag <- html_name(nd)
    txt <- html_text(nd) %>% str_replace_all("[\\u00a0\\u200b]", " ") %>% str_squish()
    if (nchar(txt) == 0) next
    if (tag %in% c("h2", "h3", "h4")) {
      flush_record(); current_title <- txt
      buf <- character(); auth <- character(); dts <- character()
    } else if (!is.na(current_title)) {
      if (grepl(date_only, txt) || grepl(byline_pat, txt, perl = TRUE)) {
        dts  <- c(dts, na.omit(str_extract(txt, date_pat)))
        auth <- c(auth, unlist(str_extract_all(str_remove(txt, date_pat), name_pat)))
      } else buf <- c(buf, txt)
    }
  }
  flush_record()
  bind_rows(lapply(records, as.data.frame))
}

urls <- c("https://ourworldindata.org/climate-change",
          "https://ourworldindata.org/co2-and-greenhouse-gas-emissions",
          "https://ourworldindata.org/energy",
          "https://ourworldindata.org/renewable-energy",
          "https://ourworldindata.org/air-pollution",
          "https://ourworldindata.org/natural-disasters",
          "https://ourworldindata.org/food-supply",
          "https://ourworldindata.org/forests-and-deforestation")

safe_scrape <- function(u) {
  out <- tryCatch(scrape_owid(u), error = function(e) {
    cat("FAILED:", u, "-", conditionMessage(e), "\n"); NULL })
  cat(u, "->", ifelse(is.null(out), 0, nrow(out)), "raw records\n")
  out
}
scraped_df <- bind_rows(lapply(urls, safe_scrape))

scraped_df <- scraped_df %>%
  mutate(key = norm_key(title_raw)) %>%
  distinct(key, .keep_all = TRUE) %>%
  filter(!str_detect(key, "^(citethiswork|reusethiswork|morearticles|relatedwork|subscribe|endnotes|licen|donate)"),
         !str_detect(content_raw, "Creative Commons|Reg\\. Charity|open access under")) %>%
  mutate(
    tail_by     = str_extract(content_raw, tail_pat),
    author      = coalesce(author, str_squish(str_remove(tail_by, paste0(" ", date_pat, "$")))),
    pub_date    = coalesce(pub_date, str_extract(tail_by, date_pat)),
    content_raw = str_squish(str_remove(content_raw, tail_pat))
  ) %>%
  select(-tail_by, -key) %>%
  filter(nchar(content_raw) > 0)

cat("Unique records:", nrow(scraped_df), "\n")
print(scraped_df$title_raw)
cat("Authors found:", sum(!is.na(scraped_df$author)),
    "| Dates found:", sum(!is.na(scraped_df$pub_date)), "\n")
if (nrow(scraped_df) < 20) stop("Only ", nrow(scraped_df),
                                " records. Add more URLs to 'urls' and re-run.")

set.seed(42)
min_len <- min(nrow(scraped_df), 25)

raw_etl_df <- scraped_df[seq_len(min_len), ] %>%
  mutate(
    record_id      = row_number(),
    inferred_views = paste0("  ", sample(100:10000, min_len), " views! ")
  ) %>%
  select(record_id, title_raw, content_raw, author, pub_date, inferred_views)

#synthetic noise injection (spurious / inconsistent / incomplete)
raw_etl_df$inferred_views[c(3, 11)] <- "UNKNOWN"
raw_etl_df$content_raw[c(5, 14)]    <- ""
raw_etl_df$title_raw[7] <- paste0("<b>", raw_etl_df$title_raw[7], "</b> &amp; more")

cat("4.1 RAW NOISY DATASET\n")
print(head(raw_etl_df, 8))
print(summary(nchar(scraped_df$content_raw)))

#Q4.2
clean_text_simple <- function(text_vec) {
  text_vec %>%
    tolower() %>%
    iconv(to = "ASCII//TRANSLIT") %>%
    gsub("http\\S+|www\\S+", " ", .) %>%
    gsub("<.*?>", " ", .) %>%
    gsub("&[a-z]+;", " ", .) %>%
    gsub("[^a-z ]", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
}

invisible(Sys.setlocale("LC_TIME", "C"))   # English month names

cleaned_etl_df <- raw_etl_df %>%
  mutate(
    title_clean   = clean_text_simple(title_raw),
    content_clean = clean_text_simple(content_raw),
    numeric_views = suppressWarnings(as.numeric(str_extract(inferred_views, "\\d+"))),
    pub_date      = as.Date(pub_date, format = "%B %d, %Y"),
    author        = str_squish(author)
  )

median_views <- round(median(cleaned_etl_df$numeric_views, na.rm = TRUE))
cat("Median used for imputation:", median_views, "\n")

cleaned_etl_df <- cleaned_etl_df %>%
  mutate(
    numeric_views = as.integer(ifelse(is.na(numeric_views), median_views, numeric_views)),
    content_clean = ifelse(is.na(content_clean) | content_clean == "", "no content available", content_clean),
    title_clean   = ifelse(is.na(title_clean)   | title_clean   == "", "untitled", title_clean),
    author        = ifelse(is.na(author) | author == "", "unknown", author)
  ) %>%
  select(record_id, title_clean, content_clean, numeric_views, author, pub_date)

#data quality audit
audit <- tibble(
  Issue  = c("Non-numeric views", "Empty content", "HTML markup in titles",
             "Missing authors", "Missing dates"),
  Before = c(sum(!str_detect(raw_etl_df$inferred_views, "\\d")),
             sum(is.na(raw_etl_df$content_raw) | raw_etl_df$content_raw == ""),
             sum(str_detect(raw_etl_df$title_raw, "<[^>]+>|&[a-z]+;")),
             sum(is.na(raw_etl_df$author)),
             sum(is.na(raw_etl_df$pub_date))),
  After  = c(sum(is.na(cleaned_etl_df$numeric_views)),
             sum(cleaned_etl_df$content_clean == "no content available"),
             sum(str_detect(cleaned_etl_df$title_clean, "[<>&;]")),
             sum(cleaned_etl_df$author == "unknown"),
             sum(is.na(cleaned_etl_df$pub_date)))
)
print(audit)
print(colSums(is.na(cleaned_etl_df)))
str(cleaned_etl_df)

dir.create("data", showWarnings = FALSE)
export_path <- file.path("data", "cleaned_web_data.csv")
write.csv(cleaned_etl_df, export_path, row.names = FALSE, na = "")

cat("\n4.2 CLEANED ETL DATASET EXPORTED TO:", export_path, "\n")
print(head(cleaned_etl_df, 8))
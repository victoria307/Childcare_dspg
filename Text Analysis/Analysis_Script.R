
# Install Packages --------------------------------------------------------

library(tidyverse)   
library(tidytext)
library(tm) #textmining
library(quanteda) #quantitative analysis of text data
library(textdata) 
library(syuzhet) #sentiment modeling
library(RColorBrewer)
library(topicmodels) #topic modeling
library(SnowballC) #stemming
library(pdftools)
library(stringr)
library(reshape2)
library(dplyr)
# Load Text ---------------------------------------------------------------

corpus_df <- list.files("/Users/gracemullins/DPSG/DPSG dir/Timeline App/www", full.names = TRUE)
View(corpus_df)  # capital V in View()
document_ids <- tools::file_path_sans_ext(basename(corpus_df))
print(document_ids)

# Extract text from all PDFs
corpus_text <- lapply(corpus_df, pdf_text)

# Collapse multiple pages into one string per document
corpus_text <- sapply(corpus_text, paste, collapse = " ")

# Create a data frame with IDs and text
corpus_df_clean <- tibble(
  document_id = document_ids,
  text = corpus_text
)
view(corpus_df_clean)
#Stop Words
data("stop_words")  

my_stop_words <- tibble(
  word = c("childcare", "policy", "va", "virginia", "department", "moa", "fiscal", "Virginia", "child", "fiscal year", "care", "vendor","children", "day","rate","county","survey","VDOE","VDSS", "market","ccdf","mrr","crrsa","vdoe","vdss","january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "appendix", "november", "december")
)
custom_stop_words <- bind_rows(stop_words, my_stop_words)

corpus_df_clean <- corpus_df_clean %>%
  mutate(year = str_extract(document_id, "^\\d{4}")) %>%
  mutate(year = as.integer(year)) # optional: make it numeric

tidy_corpus <- corpus_df_clean %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "\\d")) %>%
  filter(str_length(word) > 2) %>%
  anti_join(custom_stop_words, by = "word") %>%
  select(document_id, word, year)

doc_word_counts <- tidy_corpus %>%
  count(year, document_id, word, sort = TRUE)


#Basic Overall Analysis--------------------------------------------------------------
tidy_corpus <- corpus_df_clean %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "\\d")) %>%        # ❗ filters out words with numbers
  filter(str_length(word) > 2) %>%            # optional: remove short words
  anti_join(custom_stop_words, by = "word") 

 word_counts <- tidy_corpus %>%
   count(word, sort = TRUE)
 
 # Top 20 words
 word_counts %>%
   top_n(20) %>%
   ggplot(aes(x = reorder(word, n), y = n)) +
   geom_col(fill = "steelblue") +
   coord_flip() +
   labs(title = "Most Common Words in Policy Documents", x = "Word", y = "Frequency")

view(tidy_corpus)

#2018 Mr Survey-------------------------------------------------------------
MRSurvey <- tidy_corpus %>%
  filter(document_id == "2018-06-MR-Survey.qmd") 

MRSurvey %>% #Top 20 Words
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top Words in MR Survey", x = "Word", y = "Frequency")

#TF-IDF----------------------------------------------------
doc_word_counts <- tidy_corpus %>%
  count(document_id, word, sort = TRUE)

tf_idf_df <- doc_word_counts %>%
  bind_tf_idf(term = word, document = document_id, n = n)

tf_idf_df %>%
  group_by(document_id) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(x = tf_idf, y = reorder_within(word, tf_idf, document_id), fill = document_id)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ document_id, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top TF-IDF Words by Document", x = "TF-IDF", y = "Word")


#Topic Modeling ----------------------------------------------
dtm <- doc_word_counts %>%
  cast_dtm(document_id, word, n)

lda_model <- LDA(dtm, k = 3, control = list(seed = 1234))

topics <- tidytext::tidy(lda_model, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Plot
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms in Each Topic", x = "Term", y = "Beta")

# Bigram Analysis ---------------------------------------------------------

# Generate bigrams from the clean corpus
bigrams_raw <- corpus_df_clean %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!str_detect(bigram, "\\d")) %>% # Filter out bigrams containing numbers
  filter(str_length(bigram) > 2) # Remove very short bigrams (e.g., "a b")

# Separate bigrams into individual words for filtering
bigrams_separated <- bigrams_raw %>%
  separate(bigram, c("word1", "word2"), sep = " ", remove = FALSE) # Keep original bigram column

# Count the clean bigrams
bigram_counts %>%
  count(bigram, sort = TRUE)

# Visualize the top 20 bigrams
bigram_counts %>%
  top_n(20) %>%
  ggplot(aes(x = reorder(bigram, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Most Common Bigrams in Policy Documents", x = "Bigram", y = "Frequency")

view(bigram_counts)
#Year Data---------------------------

tidy_corpus %>%
  count(year, word, sort = TRUE) %>%
  group_by(year) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder_within(word, n, year), y = n, fill = factor(year))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ year, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Words by Year", x = "Word", y = "Count")

#Year Analysis--
year_to_analyze <- 2024

doc_word_counts_year <- doc_word_counts %>%
  filter(year == year_to_analyze)

dtm_year <- doc_word_counts_year %>%
  cast_dtm(document_id, word, n)

lda_model_year <- LDA(dtm_year, k = 3, control = list(seed = 1234))

topics_year <- tidytext::tidy(lda_model_year, matrix = "beta")

top_terms <- topics_year %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = paste(year_to_analyze,"Top Terms in Each Topic"), x = "Term", y = "Beta")


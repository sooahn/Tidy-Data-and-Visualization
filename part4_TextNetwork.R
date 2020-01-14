########################
### Sooahn Shin
## SNU PolMeth Boot Camp
## [Day 3] Data preprocess & visualization
## Part 4. Text & Network

# specify the working directory
setwd("~/Google Drive/Sooahn/SNU Method Camp/")

# Original codes from Silge and Robinson (2017) https://www.tidytextmining.com/

library(tidyverse)
library(topicmodels)
library(tidytext)
library(wordcloud)
library(reshape2)

# Text data in tidy format: one-token-per-row
library(janeaustenr)
austen_books()
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()
original_books
tidy_books <- original_books %>%
  unnest_tokens(word, text) # tokenization
tidy_books
data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words) # remove stopwords
tidy_books

# Import data
data("AssociatedPress")
AssociatedPress

# tidytext::tidy: dtm to tidy
ap_tidy <- tidy(AssociatedPress)
ap_tidy
# cast_dtm: tidy to dtm
ap_dtm <- cast_dtm(data = ap_tidy, document = "document", term = "term", value = "count")
ap_dtm

# Wordcloud
colnames(ap_tidy)[2] <- "word"
set.seed(1)
ap_tidy %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, scale = c(2,.5)))

ap_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100, scale = c(2,.5))

# tf-idf: a statistic that reflects how important a word is to a document in a collection/corpus
total_words <- ap_tidy %>% 
  group_by(document) %>% 
  summarize(total = sum(count))

ap_tidy <- left_join(ap_tidy, total_words)
ap_tidy

ap_tidy <- ap_tidy %>%
  bind_tf_idf(word, document, count)
ap_tidy

ap_tidy %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# Topic Modeling: LDA
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda
ap_topics <- tidy(ap_lda, matrix = "beta") # per-topic-per-word probabilities
ap_topics
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Word Embedding
#-- Check text2vec package (examplary code: https://rpubs.com/alyssafahringer/176732)

# n-gram
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
austen_bigrams

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts

# Network plot
library(igraph)
# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph

library(ggraph)
set.seed(2020)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# See sna package for measuring network centrality
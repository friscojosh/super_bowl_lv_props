library(tidyverse)
library(tidytext)
library(textdata)
library(tm)
library(SnowballC)
library(wordcloud)

# Get stop words from the text mining package
stop_words <- stopwords(lang)

the_hill_we_climb <- read_delim("poems/the_hill_we_climb.txt", "\n", col_names = FALSE)
colnames(the_hill_we_climb) <- c("lines")
the_hill_we_climb_word_tokens <- the_hill_we_climb %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, lines, to_lower = TRUE) %>%
  filter(!word %in% stop_words)

poem_place <- read_delim("poems/theres_a_poem_in_this_place.txt", "\n", col_names = FALSE)
colnames(poem_place) <- c("lines")
poem_place_word_tokens <- poem_place %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, lines, to_lower = TRUE) %>%
  filter(!word %in% stop_words)

bing <- get_sentiments("bing")
# "How were these sentiment lexicons put together and validated? They were 
# constructed via either crowdsourcing (using, for example, Amazon Mechanical 
# Turk) or by the labor of one of the authors, and were validated using some 
# combination of crowdsourcing again, restaurant or movie reviews, or Twitter 
# data. Given this information, we may hesitate to apply these sentiment lexicons 
# to styles of text dramatically different from what they were validated on, such 
# as narrative fiction from 200 years ago."

the_hill_we_climb_sentiment <- the_hill_we_climb_word_tokens %>%
  inner_join(bing, by = "word") %>%
  mutate(sentiment_value = ifelse(sentiment == "positive", 1, 0))

# Quick overall trend
the_hill_we_climb_sentiment %>%
  ggplot(aes(x = linenumber, y = sentiment_value)) +
  geom_smooth(se = FALSE, size = 4) +
  geom_point(size = 4) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  labs(x = "Line number", y = "Sentiment")

the_hill_we_climb_word_tokens %>%
  group_by(word) %>%
  summarize(freq = n()) %>%
  arrange(-freq) %>%
  head()

# ---

poem_place_sentiment <- poem_place_word_tokens %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  mutate(sentiment_value = ifelse(sentiment == "positive", 1, 0))

# Quick overall trend
poem_place_sentiment %>%
  ggplot(aes(x = linenumber, y = sentiment_value)) +
  geom_smooth(se = FALSE, size = 4) +
  geom_point(size = 4) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  labs(x = "Line number", y = "Sentiment")

poem_place_word_tokens %>%
  group_by(word) %>%
  summarize(freq = n()) %>%
  arrange(-freq) %>%
  head()


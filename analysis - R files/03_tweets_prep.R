
####################################  load packages  ####################################

library(tidyverse)
library(tidytext)
library(lubridate)
library(glue)
library(here)

####################################  read data  ####################################


path = "D:\\Omer - Main\\University\\Year 2\\Semester B\\Advanced Programming\\Project"
trump_tweets <- read_rds(glue("{path}\\data\\trump.rds"))

####################################  new columns  ####################################


clean_tweets <- function(text) {
  text %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT:? ") %>%
    sapply(function(row) iconv(row, "latin1", "ASCII", sub="")) %>% 
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]]+") %>%
    str_remove_all(":") %>%
    # Remove puntucation, using a standard character class
    # str_remove_all("[[:punct:]]") %>%
    # Make everything lowercase
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    str_replace_all("\\s+", " ") %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}

trump_tweets <- trump_tweets %>%
  mutate(
    #split timestamp into time and date
    time = format(as.POSIXct(date), format = "%H:%M:%S"),
    date = as_date(date),
    # name of day
    wday = wday(date, label = TRUE, locale = "US"),
    # weekend/weekday
    weekend = if_else(wday %in% c("Sat", "Sun"), "Weekend", "Weekday"),
    text = text %>% clean_tweets()
    ) %>%
  # count words
  rowwise() %>%
  mutate(n_words = text %>%
                   # str_extract("(?!RT\\b)\\b.+") %>%
                   str_count("\\w+") %>%
                   sum()) %>%
  ungroup() %>% 
  select(date, time, wday, weekend,favorites:text, n_words)


####################################  tokenize words  ####################################

trump_tweets_words <- trump_tweets %>%
  mutate(
    text = str_replace_all(text, "covid-19", "COVID_19"),
    text = str_replace_all(text, "covid 19", "COVID_19"),
    text = str_replace_all(text, "covid19", "COVID_19"),
  ) %>%
  unnest_tokens(word, text)

####################################  tokenize 2-grams  ####################################

trump_tweets_words_bigrams <- trump_tweets %>%
  mutate(
    text = str_replace_all(text, "covid-19", "COVID_19"),
    text = str_replace_all(text, "covid 19", "COVID_19"),
    text = str_replace_all(text, "covid19", "COVID_19"),
  ) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  # drop bigrams with stopwords
  mutate(i = row_number()) %>%    # add index for later grouping
  unnest_tokens(word, bigram, drop = FALSE) %>%    # tokenize bigrams into words
  anti_join(stop_words) %>%    # drop rows with stop words
  group_by(i) %>%    # group by bigram index
  filter(n() == 2) %>%    # drop bigram instances where only one word left
  summarise(bigram = unique(bigram), .groups = "drop")

####################################  save data  ####################################

write_rds(trump_tweets, file = glue("{path}\\data\\trump-tweets.rds"), compress = "xz")
write_rds(trump_tweets_words_bigrams, file = glue("{path}\\data\\trump_tweets-words-bigrams.rds"), compress = "xz")
write_rds(trump_tweets_words, file = glue("{path}\\data\\trump_tweets-words.rds"), compress = "xz")




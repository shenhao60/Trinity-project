library(tidyverse)
library(tidytext)
library(lubridate)
library(RSQLite)
library(DBI)

dbpathR="../COVID-Trends-on-Twitter/Covid-reddit-en.db"
connR=dbConnect(SQLite(),dbpathR)

DF=dbGetQuery(connR,'select * from coronavirusReddit')

wordDF <- DF %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 


scoreDF <- wordDF %>%
  inner_join(get_sentiments("bing")) %>%
  count(status_id, sentiment) %>%
  spread(sentiment, n)

if(!"positive" %in% colnames(scoreDF))
  scoreDF$positive <- NA
if(!"negative" %in% colnames(scoreDF))
  scoreDF$negative <- NA

scoreDF[is.na(scoreDF)] <- 0
scoreDF <- scoreDF %>%
  mutate(sentiment_score=(positive-negative)/(positive+negative)) 

RedditDF <- right_join(DF, scoreDF, by="status_id")


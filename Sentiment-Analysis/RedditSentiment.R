library(RedditExtractoR)
covid=get_reddit(search_terms="covid",page_threshold=5,wait_time=10)
coronavirus=get_reddit(search_terms="coronavirus",page_threshold=5,wait_time=10)
ncov19=get_reddit(search_terms="ncov19",page_threshold=5,wait_time=10)
ncov2019=get_reddit(search_terms="ncov2019",page_threshold=5,wait_time=10)


library(tidyverse)
library(tidytext)
library(lubridate)
library(RSQLite)
library(DBI)

dbpathR="Covid-reddit-en.db"
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


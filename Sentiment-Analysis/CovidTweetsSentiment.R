library(tidyverse)
library(tidytext)
library(lubridate)

setwd("~/Downloads/MSSP/Trinity Project")

# Functions
# Step by step
getWordDF <- function(DF, stopWords){
  wordDF <- DF %>%
    unnest_tokens(word, text) %>%
    filter(is.na(as.double(word))) %>%
    anti_join(stopWords) 
  return(wordDF)
}

getTweetsWithSentiScoreDF <- function(DF, wordDF, custom_sentiment_score_lexicon){
  
  scoreDF <- wordDF %>%
    inner_join(custom_sentiment_score_lexicon) %>%
    count(status_id, sentiment) %>%
    spread(sentiment, n)
  scoreDF[is.na(scoreDF)] <- 0
  scoreDF <- scoreDF %>%
    mutate(sentiment_score=(positive-negative)/(positive+negative)) 
  
  tweetScoreDF <- right_join(DF, scoreDF, by="status_id")
  
  return(tweetScoreDF)
}

getKeywordScore <- function(scoreDF, keyword){
  if(length(keyword)!=1){
    keyword <- paste(keyword,collapse="|")
  }
  keywordDF <- scoreDF %>%
    filter(grepl(keyword, text))
  return(mean(keywordDF$sentiment_score))
}

# All in one
getKeywordScoreLite <- function(DF, keyword){
  DF <- DF %>%
    filter(lang=="en"&country_code=="US")
  
  wordDF <- DF %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) 
  
  scoreDF <- wordDF %>%
    inner_join(get_sentiments("bing")) %>%
    count(status_id, sentiment) %>%
    spread(sentiment, n)
  scoreDF[is.na(scoreDF)] <- 0
  scoreDF <- scoreDF %>%
    mutate(sentiment_score=(positive-negative)/(positive+negative)) 
  
  tweetScoreDF <- right_join(DF, scoreDF, by="status_id")
  
  if(length(keyword)!=1){
    keyword <- paste(keyword, collapse="|")
  }
  keywordDF <- tweetScoreDF %>%
    filter(grepl(keyword, text))
  
  return(mean(keywordDF$sentiment_score))
}

# Sample
# Daily
tweet0329 <- read.csv("CovidTweetsData/2020-03-29 Coronavirus Tweets.CSV", header=TRUE)
tweet0329en <- tweet0329 %>%
  filter(lang=="en")
wordEn0329 <- getWordDF(tweet0329en, stop_words)
tweetScore0329en <- getTweetsWithSentiScoreDF(tweet0329en, wordEn0329, get_sentiments("bing"))
getKeywordScore(tweetScore0329en, "mask")
getKeywordScoreLite(tweet0329, "mask")

# Use Tweets from 03-29 to 03-31 as an example
tweet0330 <- read.csv("CovidTweetsData/2020-03-30 Coronavirus Tweets.CSV", header=TRUE)
tweet0331 <- read.csv("CovidTweetsData/2020-03-31 Coronavirus Tweets.CSV", header=TRUE)
tweetMarch <- list(tweet0329, tweet0330, tweet0331)
maskScoreMarch <- data_frame(date=c("03-29", "03-30", "03-31"), score=unlist(lapply(tweetMarch, getKeywordScoreLite, keyword="mask")))
ggplot(maskScoreMarch, aes(date, score))+
  geom_line(aes(group=1))

# Apply to all datasets
# This does not work well
start <- as.Date("03-29", "%m-%d")
end <- as.Date("04-30", "%m-%d")
days <- start:end
tweetList <- list()
for(i in 1:length(days)){
  tweetList[i] <- read.csv(paste("CovidTweetsData/", as.character(start+i-1), " Coronavirus Tweets.CSV", sep=""), header=TRUE)
}
maskScore <- data_frame(date=days, score=unlist(lapply(tweetList, getKeywordScoreLite, keyword="mask")))
ggplot(maskScore, aes(date, score))+
  geom_line(aes(group=1))

# Using twitteR data
library(twitteR)
library(ROAuth)
library(httr)

setup_twitter_oauth(consumer_key     = "GAD6tFVsvd8oAeC0jhqKQY4QD", 
                    consumer_secret  = "0j3kRuuEiYAKmyEf8xE6u5aOqhUEaQZEdNZsKaWboFUsbuD7ba", 
                    access_token     = "1328377313562509313-Z0a5NV4H3RptehbkLDkplODK5TvKMh",
                    access_secret    = "0SDgW2lB0aAfpbeNMPsKn6amCjpB5Ic4uNVGmcSXddXP1")

# Adding .httr-oauth to .gitignore
# Error in curl::curl_fetch_memory(url, handle = handle) : 
#   Failed to connect to api.twitter.com port 443: Operation timed out

maskTweets <- twListToDF(searchTwitter("mask", lang="en", 
                                       geocode=lookup_coords("usa"),
                                       since="2020-01-01", until="2020-11-22"))

# Use the getWordDF and getTweetsWithSentiScoreDF functions in the previous steps
maskWords <- getWordDF(maskTweets)
maskTScore <- getTweetsWithSentiScoreDF(maskTweets, maskWords, get_sentiments("bing"))
# Then we need a new function to calculate the daily sentiment score
getDailyScore <- function(scoreDF){
  countDF <- scoreDF %>%
    count(date)
  colnames(countDF)[2] <- "number_of_tweets"
  dailyScoreDF <- scoreDF %>%
    group_by(date) %>%
    summarize(daily_score=mean(sentiment_score))
  dailyScoreDF <- left_join(dailyScoreDF, countDF, by="date")
  gather(dailyScoreDF, val_type, value, daily_score:number_of_tweets, factor_key=TRUE)
}
maskDailyScore <- getDailyScore(maskScore)
ggplot(maskDailyScore, aes(date, value))+
  geom_line(aes(linetype=val_type))

# Comparing several keywords
lockdownTweets <- twListToDF(searchTwitter("lockdown", lang="en", 
                                           geocode=lookup_coords("usa"),
                                           since="2020-01-01", until="2020-11-22"))
lockdownWords <- getWordDF(lockdownTweets)
lockdownScore <- getTweetsWithSentiScoreDF(lockdownTweets, lockdownScore, get_sentiments("bing"))
lockdownDailyScore <- getDailyScore(lockdownScore)

maskDailyScore$keyword <- "mask"
lockdownDailyScore$keyword <- "lockdown"
keywordDailyScore <- bind_rows(maskDailyScore, lockdownDailyScore)
ggplot(keywordDailyScore, aes(date, daily_score, color=keyword))+
  geom_line(aes(linetype=val_type))



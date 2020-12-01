library(tidyverse)
library(tidytext)
library(lubridate)
library(rjson)

setwd("~/Downloads/MSSP/Trinity Project")

# For English tweets in United States
getEnScore <- function(DF, keyword){
  DFsub <- DF %>%
    filter(lang=="en"&country_code=="US")
  
  DFsub$date <- gsub("T.*", "", DFsub$created_at)
  
  wordDF <- DFsub %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) 
  
  scoreDF <- wordDF %>%
    inner_join(get_sentiments("bing")) %>%
    count(status_id, sentiment) %>%
    spread(sentiment, n)
  scoreDF[is.na(scoreDF)] <- 0
  scoreDF <- scoreDF %>%
    mutate(sentiment_score=(positive-negative)/(positive+negative)) 
  
  tweetScoreDF <- right_join(DFsub, scoreDF, by="status_id")
  
  if(length(keyword)!=1){
    keyword <- paste(keyword, collapse="|")
  }
  keywordDF <- tweetScoreDF %>%
    filter(grepl(keyword, text))
  
  return(
    keywordDF %>%
      group_by(date) %>%
      summarize(overall_sentiment=mean(sentiment_score))
  )
}

# For all languages
# Create sentiment lexicon dictionary
# https://www.kaggle.com/rtatman/sentiment-lexicons-for-81-languages

langCode <- read.csv("SentimentLexicons/correctedMetadata.csv", header=TRUE)$`Wikipedia.Language.Code`

negTerms <- data_frame(lang=vector(), word=vector())
posTerms <- data_frame(lang=vector(), word=vector())

for(i in 1:length(langCode)){
  negTerms <- rbind(negTerms, data_frame(lang=langCode[i], word=read.delim(file=paste0("SentimentLexicons/negative_words_", langCode[i], ".txt", sep=""), header=FALSE, check.names = FALSE)))
  posTerms <- rbind(posTerms, data_frame(lang=langCode[i], word=read.delim(file=paste0("SentimentLexicons/positive_words_", langCode[i], ".txt", sep=""), header=FALSE, check.names = FALSE)))
}
negTerms$sentiment <- "negative"
posTerms$sentiment <- "positive"

mySentimentLexicon <- bind_rows(negTerms, posTerms)
mySentimentLexicon <- as.data.frame(mySentimentLexicon)

# colnames(mySentimentLexicon) <- c("lang", "word", "sentiment")
# rownames(mySentimentLexicon) <- 1:nrow(mySentimentLexicon)

# Function
getScore <- function(DF, selectedLang, keyword){
  DFsub <- DF %>%
    filter(lang==selectedLang)
  
  DFsub$date <- gsub("T.*", "", DFsub$created_at)
  
  wordDF <- DFsub %>%
    unnest_tokens(word, text) %>%
    # anti_join(fromJSON(file=paste0("stopwords-json-master/dist/", selectedLang, ".json", sep="")))
    anti_join(stopwords(language = selectedLang, source = "stopwords-iso"))
  
  scoreDF <- wordDF %>%
    inner_join(mySentimentLexicon, by=c("lang", "word")) %>%
    count(status_id, sentiment) %>%
    spread(sentiment, n)
  scoreDF[is.na(scoreDF)] <- 0
  scoreDF <- scoreDF %>%
    mutate(sentiment_score=(positive-negative)/(positive+negative)) 
  
  tweetScoreDF <- right_join(DFsub, scoreDF, by="status_id")
  
  if(length(keyword)!=1){
    keyword <- paste(keyword, collapse="|")
  }
  keywordDF <- tweetScoreDF %>%
    filter(grepl(keyword, text))
  
  return(
    keywordDF %>%
      group_by(date) %>%
      summarize(overall_sentiment=mean(sentiment_score))
  )
}

# Sample
tweet0329 <- read.csv("CovidTweetsData/2020-03-29 Coronavirus Tweets.CSV", header=TRUE)
tweet0330 <- read.csv("CovidTweetsData/2020-03-30 Coronavirus Tweets.CSV", header=TRUE)
tweet0331 <- read.csv("CovidTweetsData/2020-03-31 Coronavirus Tweets.CSV", header=TRUE)
tweetMarch <- list(tweet0329, tweet0330, tweet0331)
tweetMarch <- data.frame(Reduce(rbind, tweetMarch))
getEnScore(tweetMarch, "mask")
getEnScore(tweetMarch, c("mask", "N95"))
# getScore does not work well
getScore(tweetMarch, "ja", "マスク")

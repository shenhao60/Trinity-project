library(tidyverse)
library(tidytext)
library(stopwords)
library(lubridate)
library(rjson)

setwd("~/Downloads/MSSP/Trinity Project")

# Functions
# getEnScore(DF, keyword): Return daily score of keyword(s) in English
# getLangScore(DF, selectedLang, keyword): Return daily score of keyword(s) in selected languages
# getScore(DF, keyword): Return daily score of keyword(s) in all languages
# getAllEnScore(DF): Returns all status ids with their scores (for English only)
# getAllLangScore(DF, selectedLang): Returns all status ids with their scores (for selected language)
# getAllScore(DF): Returns all status ids with their scores (for all languages)

# Return daily score of keyword(s) in English
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
  
  if(!"positive" %in% colnames(scoreDF))
    scoreDF$positive <- NA
  if(!"negative" %in% colnames(scoreDF))
    scoreDF$negative <- NA
  
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

mySentimentLexicon$word <- mySentimentLexicon$word$V1

# Return daily score of keyword(s) in selected languages
getLangScore <- function(DF, selectedLang, keyword){
  # Select the tweets in a specific language
  DFsub <- DF %>%
    filter(lang==selectedLang)
  
  # Create a date column
  DFsub$date <- gsub("T.*", "", DFsub$created_at)
  
  stopwd <- data_frame(word=stopwords(language = selectedLang, source = "stopwords-iso"), lexicon="custom")
  
  # Split the tweets into words
  wordDF <- DFsub %>%
    unnest_tokens(word, text) %>%
    # Here I use the stopwords package
    anti_join(stopwd)
  
  if(langs[i] %in% unique(mySentimentLexicon$lang)){
    senti <- mySentimentLexicon %>%
      filter(lang==langs[i]) %>%
      select(-lang)
  } else {senti <- get_sentiments("bing")}
  
  scoreDF <- wordDF %>%
    inner_join(senti) %>%
    count(status_id, sentiment) %>%
    spread(sentiment, n)
  
  if(!"positive" %in% colnames(scoreDF))
    scoreDF$positive <- NA
  if(!"negative" %in% colnames(scoreDF))
    scoreDF$negative <- NA
  
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
# Return daily score of keyword(s) in all languages
getScore <- function(DF, keyword){
  DF_split <- split(DF, DF$lang)
  langs <- names(DF_split)
  score_split <- list()
  
  for(i in 1:length(langs)){
    DFsub <- DF_split[[i]]
    
    if(langs[i] %in% stopwords_getlanguages("stopwords-iso")){
      stopwd <- data.frame(word=stopwords(language = langs[i], source = "stopwords-iso"), lexicon="custom")
    } else if(langs[i] %in% stopwords_getlanguages("snowball")){
      stopwd <- data.frame(word=stopwords(language = langs[i], source = "snowball"), lexicon="custom")
    } else if(langs[i] %in% stopwords_getlanguages("nltk")){
      stopwd <- data.frame(word=stopwords(language = langs[i], source = "nltk"), lexicon="custom")
    } else {
      stopwd <- data.frame(word="", lexicon="custom")
    }
    
    wordDF <- DFsub %>%
      unnest_tokens(word, text) %>%
      anti_join(stopwd) 
    
    if(langs[i] %in% unique(mySentimentLexicon$lang)){
      senti <- mySentimentLexicon %>%
        filter(lang==langs[i]) %>%
        select(-lang)
    } else {senti <- get_sentiments("bing")}
    
    
    scoreDF <- wordDF %>%
      inner_join(senti) %>%
      count(status_id, sentiment) %>%
      spread(sentiment, n)
    
    if(!"positive" %in% colnames(scoreDF))
      scoreDF$positive <- NA
    if(!"negative" %in% colnames(scoreDF))
      scoreDF$negative <- NA
    
    scoreDF[is.na(scoreDF)] <- 0
    scoreDF <- scoreDF %>%
      mutate(sentiment_score=(positive-negative)/(positive+negative)) 
    
    DFsub <- left_join(DFsub, scoreDF)
    
    score_split[[i]] <- select(DFsub, c(status_id, sentiment_score, lang))
  }
  scoreDF <- data.frame(matrix(unlist(score_split), nrow=nrow(DF), byrow=T),stringsAsFactors=FALSE)
  
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
getLangScore(tweetMarch, "ja", "マスク")
getScore(tweetMarch, "mask")


conn <- dbConnect(SQLite(), "Covid-tweets-en.db")
CovidTweetEn <- dbGetQuery(conn, "SELECT status_id, text FROM CoronavirusTweets")
# Returns all status ids with their scores (for English only)
getAllEnScore <- function(DF){
  DFsub <- DF %>%
    filter(lang=="en")
  
  wordDF <- DFsub %>%
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
  
  DFsub <- left_join(DFsub, scoreDF)
  
  DFsub$sentiment_score <- NA
  
  DFsub <- DFsub %>%
    mutate(sentiment_score=(positive-negative)/(positive+negative))
  
  DFsub$sentiment_score[is.na(DFsub$sentiment_score)] <- 0
  
  return(
    select(DFsub, c(status_id, sentiment_score))
  )
}
# Returns all status ids with their scores (for selected language)
getAllLangScore <- function(DF, selectedLang){
  # Select the tweets in a specific language
  DFsub <- DF %>%
    filter(lang==selectedLang)
  
  # Create a date column
  DFsub$date <- gsub("T.*", "", DFsub$created_at)
  
  if (langs[i] %in% stopwords_getlanguages("stopwords-iso")){
    stopwd <- data.frame(word=stopwords(language = langs[i], source = "stopwords-iso"), lexicon="custom")
  } else if (langs[i] %in% stopwords_getlanguages("snowball")){
    stopwd <- data.frame(word=stopwords(language = langs[i], source = "snowball"), lexicon="custom")
  } else if (langs[i] %in% stopwords_getlanguages("nltk")){
    stopwd <- data.frame(word=stopwords(language = langs[i], source = "nltk"), lexicon="custom")
  } else {
    stopwd <- data.frame(word="", lexicon="custom")
  }
  # Split the tweets into words
  wordDF <- DFsub %>%
    unnest_tokens(word, text) %>%
    # Here I use the stopwords package
    anti_join(stopwd)
  
  if (langs[i] %in% unique(mySentimentLexicon$lang)){
    senti <- mySentimentLexicon %>%
      filter(lang==langs[i]) %>%
      select(-lang)
  } else {senti <- get_sentiments("bing")}
  
  scoreDF <- wordDF %>%
    inner_join(senti) %>%
    count(status_id, sentiment) %>%
    spread(sentiment, n)
  
  if(!"positive" %in% colnames(scoreDF))
    scoreDF$positive <- NA
  if(!"negative" %in% colnames(scoreDF))
    scoreDF$negative <- NA
  
  scoreDF[is.na(scoreDF)] <- 0
  
  DFsub <- left_join(DFsub, scoreDF)
  
  DFsub$sentiment_score <- NA
  
  DFsub <- DFsub %>%
    mutate(sentiment_score=(positive-negative)/(positive+negative))
  
  DFsub$sentiment_score[is.na(DFsub$sentiment_score)] <- 0
  
  return(
    select(DFsub, c(status_id, sentiment_score))
  )
}
# Returns all status ids with their scores (for all languages)
getAllScore <- function(DF){
  DF_split <- split(DF, DF$lang)
  langs <- names(DF_split)
  score_split <- list()
  
  for(i in 1:length(langs)){
    DFsub <- DF_split[[i]]
    
    if(langs[i] %in% stopwords_getlanguages("stopwords-iso")){
      stopwd <- data.frame(word=stopwords(language = langs[i], source = "stopwords-iso"), lexicon="custom")
    }else if(langs[i] %in% stopwords_getlanguages("snowball")){
      stopwd <- data.frame(word=stopwords(language = langs[i], source = "snowball"), lexicon="custom")
    }else if(langs[i] %in% stopwords_getlanguages("nltk")){
      stopwd <- data.frame(word=stopwords(language = langs[i], source = "nltk"), lexicon="custom")
    }else{
      stopwd <- data.frame(word="", lexicon="custom")
    }
    
    wordDF <- DFsub %>%
      unnest_tokens(word, text) %>%
      anti_join(stopwd) 
    
    if(langs[i] %in% unique(mySentimentLexicon$lang)){
      senti <- mySentimentLexicon %>%
        filter(lang==langs[i]) %>%
        select(-lang)
    }else{senti <- get_sentiments("bing")}
    
    
    scoreDF <- wordDF %>%
      inner_join(senti) %>%
      count(status_id, sentiment) %>%
      spread(sentiment, n)
    
    if(!"positive" %in% colnames(scoreDF))
      scoreDF$positive <- NA
    if(!"negative" %in% colnames(scoreDF))
      scoreDF$negative <- NA
    
    scoreDF[is.na(scoreDF)] <- 0
    scoreDF <- scoreDF %>%
      mutate(sentiment_score=(positive-negative)/(positive+negative)) 
    
    DFsub <- left_join(DFsub, scoreDF)
    
    score_split[[i]] <- select(DFsub, c(status_id, sentiment_score, lang))
  }
  return(data.frame(matrix(unlist(score_split), nrow=nrow(DF), byrow=T),stringsAsFactors=FALSE))
}

# Test
test <- getAllLangScore(tweet0329, "en")
test2 <- getAllScore(tweet0329)


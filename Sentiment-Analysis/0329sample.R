library(tidyverse)
library(tidytext)
library(wordcloud)
library(rjson)

# Define custom stop words
custom_stop_words <- fromJSON(file="stopwords-all.json")

custom_stop_words <- data_frame(word=unlist(custom_stop_words), lexicon="custom")

# Since all data are about covid-19. so I delete these words. 
covid_stop_words <- data_frame(word=c("t.co", "http", "https", "covid19", "coronavirus", "covid_19", "covid","corona","coronaviruspandemic"), lexicon="custom")

custom_stop_words <- bind_rows(custom_stop_words, covid_stop_words)


# Functions
getTokenizedDF <- function(df, stopWords){
  tokenizedDF <- df %>%
    unnest_tokens(word, text) %>%
    filter(is.na(as.double(word))) %>%
    anti_join(stopWords) 
  tokenizedDF <- left_join(tokenizedDF, countryCodes, by="country_code")
  return(tokenizedDF)
}

getCountDF <- function(tokenDF, keyword){
  countDF <- tokenDF %>%
    count(word, sort=TRUE)
  
  return(countDF)
}
getWordCount <- function(countDF, keyword){
  wordcount <- filter(countDF, word==keyword)[2]
  
  return(wordcount)
}

getWordCloud <- function(countDF){
  return(
    wordcloud(
      word=countDF$word,
      freq=countDF$n,
      max.words=50
    )
  )
}

getSentimentScore <- function(tokenDF){
    scoreDF <- tokenDF %>%
        filter(!is.na(country)) %>%
        inner_join(mySentimentLexicon) %>%
        count(country, sentiment) %>%
        spread(sentiment, n) 
    
    scoreDF[is.na(scoreDF)] <- 0
    
    scoreDF <- scoreDF %>%
      mutate(sentiment_score=(positive-negative)/(positive+negative)) %>%
      mutate(country=fct_reorder(country, sentiment_score))
    
    return(scoreDF)
}

get300ScoreCountries <- function(df, scoreDF){
  countrySub <- df %>%
    count(country_code, sort=TRUE) %>%
    filter(n>300 & country_code!="")
  
  countrySub <- left_join(countrySub,countryCodes,by="country_code")$country
  
  scoreDF %>%
    filter(country %in% countrySub) 
}

plotSentimentScore <- function(scoreDF){
  ggplot(scoreDF, aes(country, sentiment_score, fill=as.factor(country)))+
    geom_col(show.legend=FALSE)+
    coord_flip()
    # theme(axis.text.y=element_text(size=5, face="bold"))
}
  
getSentimentNRC <- function(tokenDF){
  tokenDF %>%
    inner_join(get_sentiments("nrc")) %>%
    count(word, sentiment) %>%
    group_by(sentiment) %>%
    top_n(10,n) %>%
    ungroup() %>%
    mutate(words=fct_reorder(word,n))
}

plotSentimentNRC <- function(sentimentNrcDF){
  ggplot(sentimentNrcDF, aes(words, n, fill=sentiment))+
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales="free")+
    coord_flip()
}


# Import data
tweet0329 <- read.csv("CovidTweetsData/2020-03-29 Coronavirus Tweets.CSV", header=TRUE)
mySentimentLexicon <- read.csv("mySentimentLexicon.csv")
mySentimentLexicon <- select(mySentimentLexicon, -X)
countryCodes <- read.csv("country.csv")
colnames(countryCodes) <- c("country", "country_code")

# Apply functions
token0329 <- getTokenizedDF(tweet0329, custom_stop_words)

count0329 <- getCountDF(token0329)
getWordCount(count0329, "mask")

getWordCloud(count0329)

score0329 <- getSentimentScore(token0329)
score0329_sub <- get300ScoreCountries(tweet0329, score0329)
plotSentimentScore(score0329_sub)
plotSentimentScore(score0329)

nrc0329 <- getSentimentNRC(token0329)
plotSentimentNRC(nrc0329)

# Draft
# tweet0329 %>%
#   count(lang, sort=TRUE)

# tidy0329 <- tweet0329 %>%
#   unnest_tokens(word, text) %>%
#   filter(is.na(as.double(word))) %>%
#   anti_join(custom_stop_words) 
#   
# 
# word_count0329 <- tidy0329 %>%
#   count(word) %>%
#   arrange(desc(n))
# 
# wordcloud(
#   word=word_count0329$word,
#   freq=word_count0329$n,
#   max.words=50
# )
# 
# # Sentiment analysis using NRC
# sentimentnrc0329 <- tidy0329 %>%
#   inner_join(get_sentiments("nrc")) %>%
#   count(sentiment, sort=TRUE)
# 
# ggplot(sentiment0329nrc, aes(sentiment, n, fill=sentiment))+
#   geom_col(show.legend=FALSE)
# 
# wordsentimentnrc0329 <- tidy0329 %>%
#   inner_join(get_sentiments("nrc")) %>%
#   count(word, sentiment) %>%
#   group_by(sentiment) %>%
#   top_n(10,n) %>%
#   ungroup() %>%
#   mutate(word2=fct_reorder(word,n))
# 
# ggplot(wordsentimentnrc0329, aes(word2, n, fill=sentiment))+
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~sentiment, scales="free")+
#   coord_flip()
# 
# # Sentiment score using bing
# sentimentbing0329 <- tidy0329 %>%
#   filter(country_code!="") %>%
#   inner_join(get_sentiments("bing")) %>%
#   count(country_code, sentiment) %>%
#   spread(sentiment, n) %>%
#   mutate(overall_sentiment=positive-negative) %>%
#   arrange(overall_sentiment)
# 
# ggplot(sentimentbing0329, aes(country_code, overall_sentiment, fill=as.factor(country_code)))+
#   geom_col(show.legend=FALSE)+
#   coord_flip()
# 
# word_count0329 %>%
#   filter(word=="mask")

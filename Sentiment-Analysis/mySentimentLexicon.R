library(tidyverse)

# tweet0329 <- read.csv("CovidTweetsData/2020-03-29 Coronavirus Tweets.CSV", header=TRUE)

# uniLang <- unique(tweet0329$lang)
# 
# uniLang

langCode <- read.csv("SentimentLexicons/correctedMetadata.csv", header=TRUE)$`Wikipedia.Language.Code`

negTerms <- vector(mode = "list", length = length(langCode))
posTerms <- vector(mode = "list", length = length(langCode))

for(i in 1:length(langCode)){
  # if(!(uniLang[i] %in% c("en","und","in","ne", "si", "sd", "ml", "ps", "or", "ckb", "am", "lo", "pa", "dv", "iw", "my", "bo"))){
    negTerms[[i]]$Type <- langCode[i]
    negTerms[[i]]$Value <- read.delim(file=paste0("SentimentLexicons/negative_words_", langCode[i], ".txt", sep=""))
    posTerms[[i]]$Type <- langCode[i]
    posTerms[[i]]$Value <- read.delim(file=paste0("SentimentLexicons/positive_words_", langCode[i], ".txt", sep=""))
  # }
}

negTerms <- unlist(negTerms)
# negTerms <- rbind(read.delim(file="~/Downloads/archive-3/sentiment-lexicons/negative_words_ms.txt"), 
#                   read.delim(file="~/Downloads/archive-3/sentiment-lexicons/negative_words_sw.txt"),
#                   negTerms)
posTerms <- unlist(posTerms)

neg <- data_frame(word=negTerms, sentiment="negative")
pos <- data_frame(word=posTerms, sentiment="positive")

mySentimentLexicon <- bind_rows(neg,pos,get_sentiments("bing"))

write.csv(mySentimentLexicon, "mySentimentLexicon.csv")

library(shiny)



library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)
library(cartogram)
library(revgeo)
library(rtweet)
library(tidytext)
library(rjson)
library(plotly)
library(pacman)
library(maps)
library(tmap)
library(sp)
library(DT)

dbpath="Covid-tweets-en.db"
conn=dbConnect(SQLite(),dbpath)


# Select all tweets in April 2020
if(F){
  paste("CREATE TABLE CoronavirusTweets",
        " AS SELECT * FROM CoronavirusTweetsCsv",
        " WHERE (strftime('%Y-%m-%d %H:%M:%S',created_at)>=",
        "strftime('%Y-%m-%d %H:%M:%S','2020-03-29 00:00:00'))",
        " AND (strftime('%Y-%m-%d %H:%M:%S',created_at)<=",
        "strftime('%Y-%m-%d %H:%M:%S','2020-04-29 23:59:59'))",sep='')%>%
    dbSendQuery(conn,.)
  
  April_tweet=paste("SELECT Tweet_ID FROM CoronavirusTweets",sep='')%>%
    dbGetQuery(conn,.)
  # Set Twitter developer account
  create_token(app='MSSP-An-Auxiliary-Tool',
               consumer_key='ORvbA3CEOP06hi9MHfz7yknwV',
               consumer_secret='nAy2PRkiV4AYZ0NvHAF6Iw0IBFrttWMKTuxXbUWN4bcZnMpTQR',
               access_token='1328377313562509313-j1iSFuJLLo3FL768jdnHKe1fzmcWnS',
               access_secret='oJIhGoThBNSBSMLQBo3AxS5kcLrqq8sCjx6OIPX9NRmPT')
  for(i in 1:ceiling(nrow(April_tweet)/90000)) {
    rl=rate_limit("lookup_statuses")
    if(rl%>%select(remaining)!=900){
      rl%>%select(reset)*60%>%ceiling()%>%Sys.sleep()
    }
    april_tweet=lookup_statuses(April_tweet$Tweet_ID[(900*i):nrow(April_tweet)])
    if(i==1){April_tweet=april_tweet}else{April_tweet=rbind(April_tweet,april_tweet)}
  }
  April_tweet%>%
    select(status_id,user_id,screen_name,created_at,text,is_quote,
           is_retweet,favourites_count,retweet_count,followers_count,
           friends_count,lang)%>%
    dbWriteTable(conn,'CoronavirusTweets',.)
}
# Select all tweets with geo information from 202001 to 202011
if(F){
  paste("CREATE TABLE CoronavirusTweetsGeo",
        " AS SELECT * FROM CoronavirusTweetsCsv",
        " WHERE Geolocation_coordinate='YES'",sep='')%>%
    dbSendQuery(conn,.)
  
  Geo_tweet=paste("SELECT Tweet_ID FROM CoronavirusTweetsGeo",sep='')%>%
    dbGetQuery(conn,.)
  
  
  for(i in 1:ceiling(nrow(Geo_tweet)/90000)) {
    rl=rate_limit("lookup_statuses")
    if(rl%>%select(remaining)!=900){
      rl%>%select(reset)*60%>%ceiling()%>%Sys.sleep()
    }
    geo=lookup_statuses(Geo_tweet$Tweet_ID[(900*i):nrow(Geo_tweet)])
    if(i==1){Geo=geo}else{Geo=rbind(Geo,geo)}
  }
  
  lat_lng(Geo)%>%
    select(status_id,user_id,screen_name,created_at,text,is_quote,
           is_retweet,favourites_count,retweet_count,followers_count,
           friends_count,lang,place_full_name,place_type,country_code,
           place_name,country,lat,lng)%>%
    dbWriteTable(conn,'CoronavirusTweetsGeo',.)
  
  # Delete initial collection of covid tweets csv files table
  "DROP TABLE CoronavirusTweetsCsv" %>%
    dbSendQuery(conn,.)
  # Create index to accelerate query
  paste("CREATE INDEX CT_status_id ON CoronavirusTweets(status_id);",
        "CREATE INDEX CTG_status_id ON CoronavirusTweetsGeo(status_id);",
        "CREATE INDEX TS_status_id ON TweetsSentiment(status_id);",
        "CREATE INDEX TGS_status_id ON TweetsGeoSentiment(status_id);",
        "CREATE INDEX CTG_lat_long ON CoronavirusTweetsGeo(lat,lng);",
        "CREATE UNIQUE INDEX GD_lat_long ON GeoDetail(lat,lng)")%>%
    dbSendQuery(conn,.)
}
dbDisconnect(conn)


getTwitterData=function(conn,geoinfo=T,keywords=NULL,
                        period=c('2020-03-29 00:00:00','2020-04-30 23:59:59')){
  # Select table of database according to 'geoinfo'
  if(geoinfo){
    geoinfo_query=paste("SELECT CoronavirusTweetsGeo.*,",
                        "city,state,country,sentiment_score ",
                        "FROM CoronavirusTweetsGeo ",
                        "LEFT JOIN TweetsGeoSentiment ON ",
                        "CoronavirusTweetsGeo.status_id=",
                        "TweetsGeoSentiment.status_id ",
                        "LEFT JOIN GeoDetail ON ",
                        "CoronavirusTweetsGeo.lat=GeoDetail.lat ",
                        "AND CoronavirusTweetsGeo.lng=GeoDetail.lng",sep="")
  }
  else{
    geoinfo_query=paste("SELECT CoronavirusTweets.*,sentiment_score ",
                        "FROM CoronavirusTweets ",
                        "LEFT JOIN TweetsSentiment ON ",
                        "CoronavirusTweets.status_id=",
                        "TweetsSentiment.status_id",sep="")
  }
  # Add keywords conditions according to 'keywords' 
  if(length(keywords==0)){
    keywords_query=''
  }
  else{
    for(i in 1:length(keywords)){
      if(i==1){
        keywords_query=paste(" ((text LIKE '%",keywords[i],"%')",sep="")
      }
      else{
        keywords_query=keywords_query%>%
          paste("OR (text LIKE '%",keywords[i],"%')",sep="")
      }
    }
    keywords_query=paste(keywords_query,") ",sep="")
  }
  # Add period conditions according to 'period'
  if(length(period)!=2){
    period_query=''
  }
  else{
    period_query=paste(" (strftime('%Y-%m-%d %H:%M:%S',created_at)>=",
                       "strftime('%Y-%m-%d %H:%M:%S','",period[1],"') ",
                       "AND strftime('%Y-%m-%d %H:%M:%S',created_at)<=",
                       "strftime('%Y-%m-%d %H:%M:%S','",period[2],"')) ",
                       sep="")
  }
  # Write SQL
  if(period_query==''){
    if(keywords_query==''){
      query=paste(geoinfo_query,sep="")
    }
    else{
      query=paste(geoinfo_query," WHERE",keywords_query,sep="")
    }
  }
  else{
    if(keywords_query==''){
      query=paste(geoinfo_query," WHERE",period_query,sep="")
    }
    else{
      query=paste(geoinfo_query," WHERE",
                  period_query,"AND",keywords_query,sep="")
    }
  }
  # Obtain Data
  dbGetQuery(conn,query)
}



## Get trend function

getTwitterTrend=function(conn,geoinfo='country',trend='day',keywords=NULL,
                         period=c('2020-03-29 00:00:00','2020-04-30 23:59:59')){
  # Add trend cconditions according to 'trend'
  if(trend=='day'){
    trend_query=c("'%Y-%m-%d'","date")
  }
  else{
    if(trend=='week'){
      trend_query=c("'%W'","week")
    }
    else{
      if(trend=='month'){
        trend_query=c("'%m'","month")
      }
      else{
        stop("The trend can only be 'day', 'week' or 'month'.") 
      }
    }
  }
  # Select table of database according to 'geoinfo'
  if(is.null(geoinfo)){
    geoinfo_query=paste("SELECT strftime(",trend_query[1],
                        ",created_at) AS ",trend_query[2],", ",
                        "count(*) AS number, ",
                        "avg(sentiment_score) AS sentiment_score ",
                        "FROM CoronavirusTweets ",
                        "LEFT JOIN TweetsSentiment ON ",
                        "CoronavirusTweets.status_id=",
                        "TweetsSentiment.status_id",sep="")
    group_query=paste(" GROUP BY strftime(",trend_query[1],
                      ",created_at)",sep="")
  }
  else{
    if(geoinfo=='country'){
      geoinfo_query=paste("SELECT strftime(",trend_query[1],
                          ",created_at) AS ",trend_query[2],", ",
                          "count(*) AS number, country, ",
                          "avg(sentiment_score) AS sentiment_score ",
                          "FROM CoronavirusTweetsGeo ",
                          "LEFT JOIN TweetsGeoSentiment ON ",
                          "CoronavirusTweetsGeo.status_id=",
                          "TweetsGeoSentiment.status_id ",
                          "LEFT JOIN GeoDetail ON ",
                          "CoronavirusTweetsGeo.lat=GeoDetail.lat ",
                          "AND CoronavirusTweetsGeo.lng=GeoDetail.lng",sep="")
      group_query=paste(" GROUP BY strftime(",trend_query[1],
                        ",created_at),country",sep="")
    }
    else{
      if(geoinfo=='state'){
        geoinfo_query=paste("SELECT strftime(",trend_query[1],
                            ",created_at) AS ",trend_query[2],", ",
                            "count(*) AS number, country, state, ",
                            "avg(sentiment_score) AS sentiment_score ",
                            "FROM CoronavirusTweetsGeo ",
                            "LEFT JOIN TweetsGeoSentiment ON ",
                            "CoronavirusTweetsGeo.status_id=",
                            "TweetsGeoSentiment.status_id ",
                            "LEFT JOIN GeoDetail ON ",
                            "CoronavirusTweetsGeo.lat=GeoDetail.lat ",
                            "AND CoronavirusTweetsGeo.lng=GeoDetail.lng",sep="")
        group_query=paste(" GROUP BY strftime(",trend_query[1],
                          ",created_at),country,state",sep="")
      }
      else{
        if(geoinfo=='city'){
          geoinfo_query=paste("SELECT strftime(",trend_query[1],
                              ",created_at) AS ",trend_query[2],", ",
                              "count(*) AS number, country, state, city, ",
                              "avg(sentiment_score) AS sentiment_score ",
                              "FROM CoronavirusTweetsGeo ",
                              "LEFT JOIN TweetsGeoSentiment ON ",
                              "CoronavirusTweetsGeo.status_id=",
                              "TweetsGeoSentiment.status_id ",
                              "LEFT JOIN GeoDetail ON ",
                              "CoronavirusTweetsGeo.lat=GeoDetail.lat ",
                              "AND CoronavirusTweetsGeo.lng=GeoDetail.lng",
                              sep="")
          group_query=paste(" GROUP BY strftime(",trend_query[1],
                            ",created_at),country,state,city",sep="")
        }
        else{
          stop("The geoinfo can only be 'NULL', 'city', 'state' or 'country'.")
        }
      }
    }
  }
  
  # Add keywords conditions according to 'keywords' 
  if(is.null(keywords)){
    keywords_query=''
  }
  else{
    for(i in 1:length(keywords)){
      if(i==1){
        keywords_query=paste(" ((text LIKE '%",keywords[i],"%')",sep="")
      }
      else{
        keywords_query=keywords_query%>%
          paste("OR (text LIKE '%",keywords[i],"%')",sep="")
      }
    }
    keywords_query=paste(keywords_query,") ",sep="")
  }
  # Add period conditions according to 'period'
  if(is.null(period)){
    period_query=''
  }
  else{
    if(length(period)==2){
      period_query=paste(" (strftime('%Y-%m-%d %H:%M:%S',created_at)>=",
                         "strftime('%Y-%m-%d %H:%M:%S','",period[1],"') ",
                         "AND strftime('%Y-%m-%d %H:%M:%S',created_at)<=",
                         "strftime('%Y-%m-%d %H:%M:%S','",period[2],"')) ",
                         sep="")
    }
    else{
      stop("The time period should be a vector with length 2.") 
    }
  }
  # Write SQL
  if(period_query==''){
    if(keywords_query==''){
      query=paste(geoinfo_query,group_query,sep="")
    }
    else{
      query=paste(geoinfo_query," WHERE",keywords_query,group_query,sep="")
    }
  }
  else{
    if(keywords_query==''){
      query=paste(geoinfo_query," WHERE",period_query,group_query,sep="")
    }
    else{
      query=paste(geoinfo_query," WHERE",period_query,"AND",keywords_query,
                  group_query,sep="")
    }
  }
  # Obtain Data
  dbGetQuery(conn,query)
}


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

langCode <- read.csv("correctedMetadata.csv", header=TRUE)$`Wikipedia.Language.Code`

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

# Firstly make a word frequency plot
# connect to data base
conn=dbConnect(SQLite(),dbpath)
# get twitter data with geo information
tweetsGeo=getTwitterData(conn,period = NULL)
# get twitter data with giving keywords and time

# load spread data



daily <- read.csv(file= "us_covid19_daily.csv", header=TRUE)
spread_data <- select(daily,date,hospitalizedCumulative,death,deathIncrease,negativeIncrease,positiveIncrease)%>%
  mutate(date_new=ymd(date))%>%
  arrange(daily, desc(date_new))
spread_data <- spread_data[68:100,]


# using plotly package to make a plot that both have death, sentiment and frequency
positiveIncrease <- spread_data[,6]


date <-spread_data$date_new
spread_data$date<- 1:33



tweetsGeo <- tweetsGeo %>%
  group_by(state)%>%
  mutate(sum = n(),
         long=lng)
tweetsGeo_En <- filter(tweetsGeo, country == 'United States')

# summary(tweetsGeo_En$sum)
# divide sum of tweets in each state into 4 parts which is 
# tweetsGeo_En$cut <- cut(tweetsGeo_En$sum,
#                      breaks=c(0,1658,7890,15064,18206),
#                     include.lowest = T)
tweetsGeo_En_1 <- tweetsGeo_En%>%
  mutate(long=as.double(long),
         lat=as.double(lat))%>%
  group_by(state)%>%
  summarize(sum_states=n(), mean_sentiment=mean(sentiment_score))


Apr40 <- read.csv("us_states_daily.csv")
Geoplot <- merge(Apr40,tweetsGeo_En_1 , by=c("state"))
Geoplot$hover <- with(Geoplot, paste(state, '<br>',  '<br>', "Positive:", positive, "<br>","death:", death,"<br>", "number of tweets", sum_states,'<br>', "sentiment score of this state", mean_sentiment)) #put data








# Define UI for application that draws a histogram
ui=fluidPage(
  headerPanel('What is related to COVID-19'),
  # Sidebar
  sidebarPanel(
    textInput("text", "Enter Text",  value = "mask"),
    ),
  # Main panel
  mainPanel(
    h3('The frequency of the word with the COVID-19 trend'),
    plotlyOutput('fig1'),
    h3('Sentiment Score of each state'),
    plotlyOutput('fig2'),
  )
)

# Define server logic required to draw a histogram
server <- function(input,output) {

  # get twitter data with giving keywords and time
  df= reactive({getTwitterTrend(conn,geoinfo = NULL,keywords = input$text)}) 
  # using plotly package to make a plot that both have death, sentiment and frequency
  
  data = reactive({
     cbind('date'=spread_data$date, 'positiveIncrease number'=spread_data$positiveIncrease, 'frequency'=df()$number,  'sentiment'=df()$sentiment_score)
    }) 
  
  ay =reactive({list(
    tickfont = list(color = "red"),
    overlaying = "y",
    side = "right",
    title = "frequency"
  )}) 
  
  fig1=reactive({
    fig1= plot_ly(data=data.frame(data()), x = ~date, y = ~positiveIncrease, name = 'positiveIncrease', type = 'scatter', mode = 'lines') %>%
        add_trace(data=data.frame(data()),y = ~frequency, x=~date, name = 'text',  mode = 'lines+markers',yaxis = "y2") %>%
        layout(title = "Increase and the frequency of the text", yaxis2 = ay(),xaxis = list(title="x")) %>%
        add_trace(data=data.frame(data()), y = ~sentiment, name = 'sentiment', mode = 'markers')
    fig1
  })
  fig2 =reactive({
    fig2 =  plot_geo(Geoplot, locationmode = 'USA-states') %>% 
      add_trace(Geoplot, locations = ~state,type='choropleth',z= ~mean_sentiment,text = ~hover,colors="Reds")%>% 
      layout(title = "sentiment score of each state")
    fig2
  }) 

  output$fig1=renderPlotly(fig1())
  output$fig2=renderPlotly(fig2())
}    


# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)
library(plotly)

# get Twitter Data function
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


# get reddit Data function
getRedditData=function(conn,keywords=NULL,
                        period=c('2020-03-29','2020-04-30')){
  dbquery=paste("SELECT CoronavirusReddit.*,sentiment_score ",
                "FROM CoronavirusReddit ",
                "LEFT JOIN RedditSentiment ON ",
                "CoronavirusReddit.status_id=",
                "RedditSentiment.status_id ",sep="")

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
    period_query=paste(" (strftime('%Y-%m-%d',created_at)>=",
                       "strftime('%Y-%m-%d','",period[1],"') ",
                       "AND strftime('%Y-%m-%d',created_at)<=",
                       "strftime('%Y-%m-%d','",period[2],"')) ",
                       sep="")
  }
  # Write SQL
  if(period_query==''){
    if(keywords_query==''){
      query=paste(dbquery,sep="")
    }
    else{
      query=paste(dbquery," WHERE",keywords_query,sep="")
    }
  }
  else{
    if(keywords_query==''){
      query=paste(dbquery," WHERE",period_query,sep="")
    }
    else{
      query=paste(dbquery," WHERE",period_query,"AND",keywords_query,sep="")
    }
  }
  # Obtain Data
  dbGetQuery(conn,query)
}



## Get trend function

getRedditTrend=function(conn,keywords=NULL,
                         period=c('2020-03-29','2020-04-30')){
  dbquery=paste("SELECT strftime('%Y-%m-%d',created_at) AS date,",
                "count(*) AS number, ",
                "avg(sentiment_score) AS sentiment_score ",
                "FROM CoronavirusReddit ",
                "LEFT JOIN RedditSentiment ON ",
                "CoronavirusReddit.status_id=",
                "RedditSentiment.status_id",sep="")
  group_query="GROUP BY strftime('%Y-%m-%d',created_at)"
  
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
      period_query=paste(" (strftime('%Y-%m-%d',created_at)>=",
                         "strftime('%Y-%m-%d','",period[1],"') ",
                         "AND strftime('%Y-%m-%d',created_at)<=",
                         "strftime('%Y-%m-%d','",period[2],"')) ",
                         sep="")
    }
    else{
      stop("The time period should be a vector with length 2.") 
    }
  }
  # Write SQL
  if(period_query==''){
    if(keywords_query==''){
      query=paste(dbquery,group_query,sep="")
    }
    else{
      query=paste(dbquery," WHERE",keywords_query,group_query,sep="")
    }
  }
  else{
    if(keywords_query==''){
      query=paste(dbquery," WHERE",period_query,group_query,sep="")
    }
    else{
      query=paste(dbquery," WHERE",period_query,"AND",keywords_query,
                  group_query,sep="")
    }
  }
  # Obtain Data
  dbGetQuery(conn,query)
  
}

dbpathr="Covid-reddit-en.db"
connr=dbConnect(SQLite(),dbpathr)

# connect to database

dbpath="Covid-tweets-en.db"
conn=dbConnect(SQLite(),dbpath)

# load spread data
spread_data=read.csv("us_covid19_daily.csv",header=TRUE)%>%
  transmute(date=ymd(date),
            positiveIncrease=positiveIncrease,
            deathIncrease=deathIncrease)%>%
  arrange(desc(date))%>%
  .[68:100,]

Geoplot=getTwitterTrend(conn,geoinfo='state',period=NULL)%>%
  filter(country=='United States')%>%
  {merge(read.csv("us_states_daily.csv",header=TRUE),.,by="state")}%>%
  {mutate(.,hover=with(.,paste(state,
                            "<br> <br> Positive:",positive,
                            "<br> death:",death,
                            "<br> number of tweets",number,
                            "<br> sentiment score of this state",sentiment_score)))}

ay=list(tickfont=list(color="red"),overlaying="y",side="right",title="frequency")

# using plotly package to make a plot that both have death, sentiment and frequency

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
  # using plotly package to make a plot that both have death, sentiment and frequency
  fig1=reactive({
    input=input$text%>%
      str_split('#')%>%
      .[[1]]
    data=getTwitterTrend(conn,geoinfo = NULL,keywords=input)%>%
      {data.frame(date=spread_data$date,
                  positiveIncrease=spread_data$positiveIncrease,
                  frequency=.$number,
                  sentiment=.$sentiment_score)}

    fig=plot_ly(data,x=~date,y=~positiveIncrease,color=I('black'),
                name='positiveIncrease',type ='scatter',mode='lines+markers') 
    n=nrow(data)
    color=data$sentiment%>%
      {(.[1:(n-1)]+.[2:n])/2}%>%
      {ifelse(.>0,'green','red')}
    
    fig=fig%>%
      add_trace(x=data$date[1:2],y=data$frequency[1:2],color=I('blue'),
                name=input[1],mode='lines+markers',yaxis="y2",
                visible='legendonly')
    for(i in 1:(n-1))
      fig=fig%>%
        add_trace(x=data$date[i:(i+1)],y=data$frequency[i:(i+1)],color=I(color[i]),
                  showlegend=F,mode='lines+markers',yaxis="y2")
    fig%>%layout(title="Twitter Sentiment trends",yaxis2=ay,xaxis=list(title="x"))
    
  })
  fig2 =reactive({
    plot_geo(Geoplot,locationmode='USA-states')%>% 
      add_trace(Geoplot,locations=~state,type='choropleth',
                z=~sentiment_score,text=~hover,colors="Reds")%>% 
      layout(title = "sentiment score of each state")
  }) 

  output$fig1=renderPlotly(fig1())
  output$fig2=renderPlotly(fig2())

  
}    

# Run the application 
shinyApp(ui = ui, server = server)

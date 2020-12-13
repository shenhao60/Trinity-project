# There are four functions in this document:
# 'getTwitterData', 'getTwitterTrend', 'getRedditData', 'getRedditTrend'

# Get tweets data function
getTwitterData=function(conn,geoinfo=T,keywords=NULL,
                        period=c('2020-03-29 00:00:00','2020-04-01 23:59:59')){
  
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

# Get tweets trend function
getTwitterTrend=function(conn,geoinfo='country',trend='day',keywords=NULL,
                       period=c('2020-03-29 00:00:00','2020-04-01 23:59:59')){
  
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

# Get reddit data function
getRedditData=function(conn,keywords=NULL,
                        period=c('2020-03-29','2020-04-30')){
  # Initial tables connection
  dbquery=paste("SELECT CoronavirusReddit.*,sentiment_score ",
                "FROM CoronavirusReddit ",
                "LEFT JOIN RedditSentiment ON ",
                "CoronavirusReddit.status_id=",
                "RedditSentiment.status_id ",sep="")

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

# Get reddit trend function
getRedditTrend=function(conn,keywords=NULL,
                         period=c('2020-03-29','2020-04-30')){
  # Initial query of tables connection
  dbquery=paste("SELECT strftime('%Y-%m-%d',created_at) AS date,",
                "count(*) AS number, ",
                "avg(sentiment_score) AS sentiment_score ",
                "FROM CoronavirusReddit ",
                "LEFT JOIN RedditSentiment ON ",
                "CoronavirusReddit.status_id=",
                "RedditSentiment.status_id ",sep="")
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
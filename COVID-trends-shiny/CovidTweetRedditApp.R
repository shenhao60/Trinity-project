# load packages
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(lubridate)
library(DBI)
library(RSQLite)
library(plotly)

# load dependency
source('function-db.R',encoding = 'UTF-8')
source('function-pt.R',encoding = 'UTF-8')
dbpathT='Covid-tweets-en.db'
dbpathR='Covid-reddit-en.db'
connT=dbConnect(SQLite(),dbpathT)
connR=dbConnect(SQLite(),dbpathR)

# load COVID data
covid <- read.csv('us_covid19_daily.csv')%>%select(date,positiveIncrease)
covidGeo=read.csv('us_states_covid19_daily.csv')%>%
  select(date,positiveIncrease,state)%>%
  mutate(month=month(ymd(date)))%>%
  {aggregate(positiveIncrease~month,.,sum)}
covidGeoM=read.csv('us_states_covid19_daily.csv')%>%
  select(date,positiveIncrease,state)%>%
  mutate(month=month(ymd(date)))%>%
  {aggregate(positiveIncrease~state+month,.,sum)}

# define layout
header <- dashboardHeader(title="Trends of Keywords")
sidebar <- dashboardSidebar(sidebarMenu(menuItem("Trends", tabName="tweetsTrend"),
                                       menuItem("Tweets with geo info", tabName="tweetsGeo")))

body <- dashboardBody(
  tabItems(
    tabItem("tweetsTrend",
            fluidRow(
              column(3,
                     box(width=NULL,
                         textInput("keywordTweet", label=h4("Enter keywords"), value=""),
                         actionButton("addButtonTweet", "Add"),
                         p(class = "text-muted",
                           paste('Note: type in the group of keywords here. If you have more than one keyword for one group, use # to separate them. For example, type in "mask#covid" for analyzing tweets containing mask and covid in their text. Every group generates one frequency and sentiment line plot. You can compare at most two groups of keywords at the same time. The app could take a few seconds to respond. Please do not click the add button repeatedly. ')
                         )
                     ),
                     box(width=NULL,
                         uiOutput("keywordSelectTweet")
                     )
              ),
              column(9,
                     plotlyOutput("figTrend")
              )
            )
    ),
    tabItem("tweetsGeo",
            fluidRow(
              column(3,
                     box(width=NULL,
                         textInput("keywordTweetGeo", label=h4("Enter keywords"), value=""),
                         actionButton("addButtonTweetGeo", "Add"),
                         p(class = "text-muted",
                           paste('Note: type in the group of keywords here. If you have more than one keyword for one group, use # to separate them. For example, type in "mask#covid" for analyzing tweets containing mask and covid in their text. The app could take a few seconds to respond. Please do not click the add button repeatedly. ')
                         )
                     )
              ),
              column(9,
                     plotlyOutput("figTweetGeo"),
                     br(),
                     plotlyOutput("figTrendTweetGeo")
              )
              #"figTrendTweetGeo"
            )
            
    )
  )
)

ui <- dashboardPage(
  header,
  sidebar,
  body
)

# Server
server <- function(input, output) {
  # plot for normal tweets
  # define overall variables
  # for twitter
  dataTH=reactiveVal(NULL)
  dataTP=reactiveVal(NULL)
  # for reddit
  dataRH=reactiveVal(NULL)
  dataRP=reactiveVal(NULL)
  # response to click action
  observeEvent(input$addButtonTweet,{
    # for twitter
    keywordT=input$keywordTweet%>%str_split('#')%>%.[[1]]
    trendT=getTwitterTrend(connT,geoinfo=NULL,keywords=keywordT,period=NULL)
    dataT1=list(keywordT,trendT)
    dataT2=dataTH()
    dataTH(dataT1)
    dataTP(list(dataT1,dataT2))
    # for reddit
    trendR=getTwitterTrend(connR,geoinfo=NULL,keywords=keywordT,period=c('2020-03-29 00:00:00','2020-04-30 23:59:59'))
    dataR1=list(keywordT,trendR)
    dataR2=dataRH()
    dataRH(dataR1)
    dataRP(list(dataR1,dataR2))
  })
  # output ui
  output$keywordSelectTweet=renderUI({
    if(!is.null(dataTP()[[1]])){
      if(is.null(dataTP()[[2]])){
        choice=list(paste(legendName(dataTP()[[1]][[1]]),"- twitter",sep=""),
                    paste(legendName(dataRP()[[1]][[1]]),"- reddit",sep=""))
        value=list('p1','p2')
      }
      else{
        choice=list(paste(legendName(dataTP()[[1]][[1]]),"- twitter",sep=""),
                    paste(legendName(dataTP()[[2]][[1]]),"- twitter",sep=""),
                    paste(legendName(dataRP()[[1]][[1]]),"- reddit",sep=""),
                    paste(legendName(dataRP()[[2]][[1]]),"- reddit",sep=""))
        value=list('p1','p2','p3','p4')
      }
    }
    validate(need(exists("choice"),"Please add a group of keywords."))
    checkboxGroupInput("lineSelectedTweet", "Select line(s) to plot: ",
                       choiceNames=choice,choiceValues=value,selected='p1')
    
  })
  # plot output
  output$figTrend=renderPlotly({
    validate(need(length(input$lineSelectedTweet)>2,"Please select no more than 2 lines. "))
    
    if(!is.null(dataTP()[[1]])){
      if(is.null(dataTP()[[2]])){
        # Only enter one group of keywords
        if('p1'%in%input$lineSelectedTweet){
          if('p3' %in%input$lineSelectedTweet){
            # if plot keyword group1 for t and r
            trendsPlot(covid,list(dataTP()[[1]][[1]],dataRP()[[1]][[1]]),
                       list(dataTP()[[1]][[2]],dataRP()[[1]][[2]]))
          }else{
            # if plot keyword group1 for t
            trendPlot(covid,dataTP()[[1]][[1]],dataTP()[[1]][[2]])
          }
          
        }else if('p3' %in%input$lineSelectedTweet){
          # if plot keyword group1 for r
          trendPlot(covid,dataRP()[[1]][[1]],dataRP()[[1]][[2]])
        }
      }else{
        # Enter two groups of keywords
        if('p1'%in%input$lineSelectedTweet){
          if('p2'%in%input$lineSelectedTweet){
            # if plot group 1 and 2 for tweet
            trendsPlot(covid,list(dataTP()[[1]][[1]],dataTP()[[2]][[1]]),
                       list(dataTP()[[1]][[2]],dataTP()[[2]][[2]]))
          }else if('p3'%in%input$lineSelectedTweet){
            # if plot group 1 for t and r
            trendsPlot(covid,list(dataTP()[[1]][[1]],dataRP()[[1]][[1]]),
                       list(dataTP()[[1]][[2]],dataRP()[[1]][[2]]))
          }else if('p4'%in%input$lineSelectedTweet){
            # if plot group 1 t and group 2 r
            trendsPlot(covid,list(dataTP()[[1]][[1]],dataRP()[[2]][[1]]),
                       list(dataTP()[[1]][[2]],dataRP()[[2]][[2]]))
          }else{
            # if plot group 1 t
            trendPlot(covid,dataTP()[[1]][[1]],dataTP()[[1]][[2]])
          }
        }else if('p2'%in%input$lineSelectedTweet){
          if('p3'%in%input$lineSelectedTweet){
            # if plot group 2 t and group 1 r
            trendsPlot(covid,list(dataTP()[[2]][[1]],dataRP()[[1]][[1]]),
                       list(dataTP()[[2]][[2]],dataRP()[[1]][[2]]))
          }else if('p4'%in%input$lineSelectedTweet){
            # if plot group 2 t and r
            trendsPlot(covid,list(dataTP()[[2]][[1]],dataRP()[[2]][[1]]),
                       list(dataTP()[[2]][[2]],dataRP()[[2]][[2]]))
          }else{
            # if plot group 2 t
            trendPlot(covid,dataTP()[[2]][[1]],dataTP()[[2]][[2]])
          }
        }else if('p3'%in%input$lineSelectedTweet){
          if('p4'%in%input$lineSelectedTweet){
            # if plot group 1 and 2 r
            trendsPlot(covid,list(dataRP()[[1]][[1]],dataRP()[[2]][[1]]),
                       list(dataRP()[[1]][[2]],dataRP()[[2]][[2]]))
          }else{
            # if plot group 1 r
            trendPlot(covid,dataRP()[[1]][[1]],dataRP()[[1]][[2]])
          }
        }else if('p4'%in%input$lineSelectedTweet){
          # if plot group 2 r
          trendPlot(covid,dataRP()[[2]][[1]],dataRP()[[2]][[2]])
        }
        
      }
    }else{plotly_empty()}
  })
  
  # plot for geo tweets 
  # define overall variables
  dataTPG=reactiveVal(NULL)
  # response to click action
  observeEvent(input$addButtonTweetGeo,{
    keywordTG=input$keywordTweetGeo%>%str_split('#')%>%.[[1]]
    trendTGM=keywordTG%>%
      {getTwitterTrend(connT,geoinfo='country',trend='month',keywords=.,period=NULL)}%>%
      filter(country=='United States')%>%
      mutate(month=as.integer(month))%>%
      select(-country)
    
    trendTGP=keywordTG%>%
      {getTwitterTrend(connT,geoinfo='state',trend='month',keywords=.,period=NULL)}%>%
      filter(country=='United States')%>%
      mutate(month=as.integer(month))
    dataTPG(list(keywordTG,trendTGP,trendTGM))
  })
  
  # plot output
  output$figTrendTweetGeo=renderPlotly({
    validate(need(input$addButtonTweetGeo!=0,""))
    geoTrendPlot(covidGeo,dataTPG()[[1]],dataTPG()[[3]])
  })
  output$figTweetGeo=renderPlotly({
    validate(need(input$addButtonTweetGeo!=0,"Please add a group of keywords. "))
    geoTrendMap(covidGeoM,dataTPG()[[2]])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
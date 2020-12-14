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
covidTweet <- read.csv('us_covid19_daily.csv')%>%select(date,positiveIncrease)
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
sidebar <- dashboardSidebar(sidebarMenu(menuItem("Trends of Tweets", tabName="tweetsTrend"),
                                        menuItem("Tweets with geo info", tabName="tweetsGeo")))
body <- dashboardBody(
  tabItems(
    tabItem("tweetsTrend",
            fluidRow(
              column(3,
                     box(width=NULL,
                         textInput("keywordTweet", label=h4("Enter keywords"), value=""),
                         actionButton("addButtonTweet", "Add")
                         #actionButton("resetButtonTweet", "reset")
                     ),
                     box(width=NULL,
                         uiOutput("keywordSelectTweet")
                         #actionButton("plotButtonTweet", "plot") 
                     )
              ),
              column(9,
                     plotlyOutput("figTrendTweet")
              )
            )
    ),
    tabItem("tweetsGeo",
            fluidRow(
              column(3,
                     box(width=NULL,
                         textInput("keywordTweetGeo", label=h4("Enter keywords"), value=""),
                         actionButton("addButtonTweetGeo", "Add")
                         #actionButton("resetButtonReddit", "reset")
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
  dataTH=reactiveVal(NULL)
  dataTP=reactiveVal(NULL)
  # response to click action
  observeEvent(input$addButtonTweet,{
    keywordT=input$keywordTweet%>%str_split('#')%>%.[[1]]
    trendT=getTwitterTrend(connT,geoinfo=NULL,keywords=keywordT,period=NULL)
    dataT1=list(keywordT,trendT)
    dataT2=dataTH()
    dataTH(dataT1)
    dataTP(list(dataT1,dataT2))
  })
  # output ui
  output$keywordSelectTweet=renderUI({
    if(!is.null(dataTP()[[1]])){
      if(is.null(dataTP()[[2]])){
        choice=list(legendName(dataTP()[[1]][[1]]))
        value=list('p1')
      }
      else{
        choice=list(legendName(dataTP()[[1]][[1]]),
                    legendName(dataTP()[[2]][[1]]))
        value=list('p1','p2')
      }
    }
    validate(need(exists("choice"),"Please add a group of keywords."))
    checkboxGroupInput("lineSelectedTweet", "Select line(s) to plot: ",
                       choiceNames=choice,choiceValues=value,selected='p1')
  })
  # plot output
  output$figTrendTweet=renderPlotly({
    if(!is.null(dataTP()[[1]])){
      if(is.null(dataTP()[[2]])){
        trendPlot(covidTweet,dataTP()[[1]][[1]],dataTP()[[1]][[2]])
      }
      else{
        if('p1'%in%input$lineSelectedTweet){
          if('p2'%in%input$lineSelectedTweet){
            trendsPlot(covidTweet,list(dataTP()[[1]][[1]],dataTP()[[2]][[1]]),
                       list(dataTP()[[1]][[2]],dataTP()[[2]][[2]]))
          }
          else{
            trendPlot(covidTweet,dataTP()[[1]][[1]],dataTP()[[1]][[2]])
          }
        }
        else{
          if('p2'%in%input$lineSelectedTweet){
            trendPlot(covidTweet,dataTP()[[2]][[1]],dataTP()[[2]][[2]])
          }
          else{
            plotly_empty()
          }
        }
      }
    }
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
    validate(need(input$addButtonTweetGeo!=0,"Please add a group of keywords."))
    geoTrendPlot(covidGeo,dataTPG()[[1]],dataTPG()[[3]])
  })
  output$figTweetGeo=renderPlotly({
    validate(need(input$addButtonTweetGeo!=0,"Please add a group of keywords."))
    geoTrendMap(covidGeoM,dataTPG()[[2]])
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
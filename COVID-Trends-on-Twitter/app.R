# loadoackages
library(shiny)
library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)
library(plotly)
# load functions
source('function.R', encoding = 'UTF-8')
# connect to database
dbpathr <- "Covid-reddit-en.db"
connr <- dbConnect(SQLite(), dbpathr)
dbpath <- "Covid-tweets-en.db"
conn <- dbConnect(SQLite(), dbpath)
# load spread data
spread_data <- read.csv("us_covid19_daily.csv", header = TRUE) %>%
  transmute(
    date = ymd(date),
    positiveIncrease = positiveIncrease,
    deathIncrease = deathIncrease
  ) 

Geoplot <- getTwitterTrend(conn, geoinfo = "state", period = NULL) %>%
  filter(country == "United States") %>%
  {
    merge(read.csv("us_states_daily.csv", header = TRUE), ., by = "state")
  } %>%
  {
    mutate(., hover = with(., paste(
      state,
      "<br> <br> Positive:", positive,
      "<br> death:", death,
      "<br> number of tweets", number,
      "<br> sentiment score of this state", sentiment_score
    )))
  }

ay <- list(tickfont = list(color = "red"), overlaying = "y", side = "right", title = "frequency")

# using plotly package to make a plot that both have death, sentiment and frequency

# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("What is related to COVID-19"),
  # Sidebar
  sidebarPanel(
    textInput("text", "Enter Text", value = "mask"),
  ),
  # Main panel
  mainPanel(
    h3("The frequency of the word with the COVID-19 trend"),
    plotlyOutput("fig1"),
    h3("Sentiment Score of each state"),
    plotlyOutput("fig2"),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # get twitter data with giving keywords and time
  # using plotly package to make a plot that both have death, sentiment and frequency
  fig1 <- reactive({
  input <- input$text %>%
      str_split("#") %>%
      .[[1]]
  data <- getRedditTrend(connr, keywords = input, period=c('2020-01-22', '2020-09-27')) %>% 
    mutate(date=ymd(date))%>%
    left_join(spread_data,'date')
  fig <- plot_ly(data, x = ~date, y = ~positiveIncrease, color = I("black"),
                 name = "positiveIncrease", type = "scatter", mode = "lines+markers")
  n <- nrow(data)
  color <- data$sentiment %>%
    {(.[1:(n - 1)] + .[2:n]) / 2} %>%
    {ifelse(. > 0, "green", "red")}
  fig <- fig %>%
    add_trace(x = data$date[1:2], y = data$number[1:2], color = I("blue"),
              name = input[1], mode = "lines+markers", yaxis = "y2", visible = "legendonly")
  for (i in 1:(n - 1)) {
    fig <- fig %>%
      add_trace(x = data$date[i:(i + 1)], y = data$number[i:(i + 1)], color = I(color[i]),
                showlegend = F, mode = "lines+markers", yaxis = "y2")
  }
  fig %>% layout(title = "Twitter Sentiment trends", yaxis2 = ay, xaxis = list(title = "x"))
  })
  fig2 <- reactive({
    plot_geo(Geoplot, locationmode = "USA-states") %>%
      add_trace(Geoplot,
        locations = ~state, type = "choropleth",
        z = ~sentiment_score, text = ~hover, colors = "Reds"
      ) %>%
      layout(title = "sentiment score of each state")
  })

  output$fig1 <- renderPlotly(fig1())
  output$fig2 <- renderPlotly(fig2())
}

# Run the application
shinyApp(ui = ui, server = server)
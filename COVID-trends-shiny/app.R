# load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(DBI)
library(RSQLite)
library(plotly)
# load dependencies
#setwd('COVID-Trends-on-Twitter/')
source('function-db.R',encoding = 'UTF-8')
source('function-pt.R',encoding = 'UTF-8')
dbpathT='Covid-tweets-en.db'
dbpathR='Covid-reddit-en.db'
connT=dbConnect(SQLite(),dbpathT)
connR=dbConnect(SQLite(),dbpathR)

# Shiny APP
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
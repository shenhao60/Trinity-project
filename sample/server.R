library(shinydashboard)
library(tidytext)

function(input, output) {
    keywordVal <- reactiveValues(word=vector()) 
    keywordSel <- reactiveValues(word=vector()) 
    observeEvent(input$addButton,{
        keywordDF <- data_frame(text=input$keyword)
        keywordDF <- keywordDF %>%
            unnest_tokens(word, text)
        keywordVal$word <- c(keywordVal$word, keywordDF$word)
    })
    observeEvent(input$resetButton,{
        keywordVal$word <- NULL
    })
    output$keywordSelect <- renderUI({
        checkboxGroupInput("keywordSelected", "Select keywords: ", keywordVal$word)
    })
    observeEvent(input$plotButton,{
        keywordSel$word <- c(keywordSel$word, input$keywordSelected)
    })
    output$value <- renderPrint({ 
        str(keywordSel$word) 
        })
}

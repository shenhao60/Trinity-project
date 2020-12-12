library(shinydashboard)


header <- dashboardHeader(title="Covid Tweet Trend")

body <- dashboardBody(
    fluidRow(
        column(3,
               box(width=NULL,
                   textInput("keyword", label=h4("text input"), value="Enter keyword..."),
                   actionButton("addButton", "add"),
                   actionButton("resetButton", "reset")
               ),
               box(width=NULL,
                   uiOutput("keywordSelect"),
                   actionButton("plotButton", "plot")
               )
        ),
        column(9, verbatimTextOutput("value"))
        # Put plots in this column
    )
)

dashboardPage(
    header,
    dashboardSidebar(disable = TRUE),
    body
)
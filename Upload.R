library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(quanteda)
# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      tags$hr(),
      
      actionButton("plot", "Render a test plot with dates"),
      
      tags$hr(),
      
      textInput("context","Input a word you want to see in context"),
      actionButton("context2","Press to execute the context thing")
    
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      #h4("Table", align = "center"),
      #tableOutput("contents"),
      h4("Plot", align = "center"),
      plotOutput("dates"),
      h4("Context", align = "Center"),
      verbatimTextOutput("context3")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df, 3))
    }
    else {
      return(df)
    }
    
  })
  

  
  observeEvent(input$plot, {
    
    
    data_news <- read_csv(input$file1$datapath, col_names = TRUE)
    output$dates <- renderPlot(qplot(data_news$date))
      
    })
  
  observeEvent(input$context2, {
    data_news <- read_csv(input$file1$datapath, col_names = TRUE)
    news_corp <- corpus(data_news, text_field = "article")
    toks_news <- tokens(news_corp, remove_punct = TRUE)
    test <- input$context
    message(test)
    output$context3 <- renderPrint(
      head(kwic(toks_news, pattern = "virus", window = 3)))
    })
   
    
  
    
  
  
  }
  
  
  
    
    
  

    
  
# Create Shiny app ----
shinyApp(ui, server)

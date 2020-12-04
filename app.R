# Vineyard Sampling app for Minh's Summer Research Scholarship Project

library(shiny)
library(shinydashboard)
library(readxl)
Coombe_map <- read_excel("Coombe_map.xlsx")

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(plotOutput("plot1", height = 250)),
            
            box(
                title = "Controls",
                sliderInput(inputId = "slider", label = "Number of samples:", 
                            min = 1, max = max(Coombe_map$Vine_ID), value = 30)
            )
        )
    )
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    
    
    output$plot1 <- renderPlot({
        print(sample(Coombe_map$Vine_ID, size = input$slider))
        
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
}

shinyApp(ui, server)

# Vineyard Sampling app for Minh's Summer Research Scholarship Project

library(shiny)
library(shinydashboard)
library(readxl)
library(sampling)
library(ggplot2)
Coombe_map <- read_excel("Coombe_map.xlsx")

ui <- dashboardPage(
    dashboardHeader(title = "Vineyard Sampling"),
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "Home", icon("home", lib ="glyphicon")),
            menuItem("Sampling Plan", tabName = "Sampling plan"),
            menuItem("Data entry", tabName = "Data entry"),
            menuItem("Plots", tabName = "Plots/ Analysis"),
            menuItem("Acknowledgements", tabName = "Acknowledgements")
        )
        
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            
            box(plotOutput("plot1", height = 250)),
            
            box(
                title = "Sampling plan",
                selectInput("select", label = h3("Type of Sampling"), 
                            choices = list("Cluster Sampling" = 1, "Stratified Sampling" = 2, "Simple Random Sampling" = 3), 
                            selected = 1),
                uiOutput('slider')
                
            ),
            box(plotOutput('Vineyard_map'))
            
        )
    )
)

server <- function(input, output) {
    #Simple Random Sampling
    
    output$slider <- renderUI (switch (input$select, '3'= {sliderInput(inputId = "samplenumber", label = "Number of samples:", 
                                                                       min = 1, max = max(Coombe_map$Vine_ID), value = 30)},
                                       NULL
    )
    )
    ####histogram plot
    set.seed(122)
    histdata <- rnorm(500)
    #Select type of sampling
    output$value <- renderPrint({ input$select })
    
    output$plot1 <- renderPlot({
        print(sample(Coombe_map$Vine_ID, size = input$samplenumber))
        
        data <- histdata[seq_len(input$samplenumber)]
        hist(data)
    })
    
    output$Vineyard_map <- renderPlot({ggplot(Coombe_map, aes(x= Row, y = Column, color = Rootstock)) + geom_point()
    })
}

shinyApp(ui, server)

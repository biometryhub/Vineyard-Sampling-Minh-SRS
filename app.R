# Vineyard Sampling app for Minh's Summer Research Scholarship Project

library(shiny)
library(shinydashboard)
library(readxl)
library(sampling)
library(ggplot2)

options(shiny.usecairo=T)

Coombe_map <- read_excel("Coombe_map.xlsx")

ui <- dashboardPage(
    dashboardHeader(title = "Vineyard Sampling"),
    sidebar <- dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "Home"),
            menuItem("Sampling Plan", tabName = "Sampling plan"),
            menuItem("Data entry", tabName = "Data entry"),
            menuItem("Plots", tabName = "Plots/ Analysis"),
            menuItem("Acknowledgements", tabName = "Acknowledgements")
        )
        
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            
            box(plotOutput("Vineyard_map", height = 500),
                # Minh compelte
                # Add download button here for plot
            ),
            
            box(
                title = "Sampling plan",
                selectInput("select", label = h3("Type of Sampling"), 
                            choices = list("Cluster Sampling" = 1, "Stratified Sampling" = 2, "Simple Random Sampling" = 3), 
                            selected = 1),
                
                conditionalPanel(
                    # Cluster sampling
                    condition = "input.select == 1",
                    selectInput(
                        "clustervariable", "Cluster Variable",
                        c("Row", "Panel", "Rootstock")
                    ),
                    
                    
                    sliderInput(
                        "clusternumber", "Number of Clusters", min = 1, max = 11, value = 3
                    )
                    
                    
                ),
                # Only show this panel if stratified is selected
                conditionalPanel(
                    condition = "input.select == 2",
                    
                    selectInput(
                        "stratanames", "Stratified on Rootstock based on:",
                        c("Row", "Panel", "Rootstock")
                    ),
                    # sliderInput(
                    #     "stratanumber", "Number of Strata", min = 1, max = 11, value = 3
                    # ),
                    sliderInput(
                        "stratasamplenumber", "Number of Samples within a Strata", min = 1, max = 11, value = 3
                    )
                ),
                
                # Show this for SRS
                conditionalPanel(
                    condition = "input.select == 3",
                    sliderInput(inputId = "samplenumber", label = "Number of samples:", 
                                min = 1, max = nrow(Coombe_map), value = 30)
                ),
                
                checkboxInput("replacement", "Sample with replacement?", FALSE),
                actionButton("submit", label = "Generate Plan") 
                
            )
        ),
        
        fluidRow(
            box(width = 12
                # Add table here to display samples chosen
                
            )
        )
    )
)

server <- function(input, output, session) {
    #Simple Random Sampling
    
    
    # output$slider <- renderUI (switch (input$select, '3'= {sliderInput(inputId = "samplenumber", label = "Number of samples:", 
    #                                                                    min = 1, max = max(Coombe_map$Vine_ID), value = 30)},
    #                                    NULL
    # )
    # )
    ####histogram plot
    # output$clustervariable <- renderUI (switch (input$select, '1' = {selectInput(inputId = "clustervariable1", label = h3("Select box"),
    #                                                                              choices = list("Rootstock" = 1, "Row" = 2, "Panel" = 3, "Column" = 4),
    #                                                                              selected = 1) })
    # )
    set.seed(122)
    histdata <- rnorm(500)
    #Select type of sampling
    output$value <- renderPrint({ input$select })
    
    #Vineyard Map
    # Set up the data frame using an eventReactive() or possibly observeEvent()
    observe({
        
        
        if(input$clustervariable == 'Row') {
            # Row cluster
            updateSliderInput(session, "clusternumber", max = 11)
        }
        else if(input$clustervariable == "Rootstock") {
            # Rootstock
            updateSliderInput(session, "clusternumber", max = 8)
        }
        else if(input$clustervariable == "Panel") { 
            # Panel
            updateSliderInput(session, "clusternumber", max = 16)
        }
        
    })
    
    #For stratified sampling
    observe({
        
        if(input$stratanames == 'Row') {
            # Row stratified
            #updateSliderInput(session, "stratanumber", max = 11)
            updateSliderInput(session, "stratasamplenumber", max = 32)
        }
        else if(input$stratanames == "Panel") {
            # Panel stratified
            # updateSliderInput(session, "stratanumber", max = 176)
            updateSliderInput(session, "stratasamplenumber", max = 2, value = 1)
        }
        else if(input$stratanames == "Rootstock") {
            # Panel stratified
            #pdateSliderInput(session, "stratanumber", max = 8)
            updateSliderInput(session, "stratasamplenumber", max = 44)
        }
    })
    
    
    
    #Sample with replacement?
    output$replacement <- renderText({ input$replacement })
    
    coombe <- eventReactive(#Do stuff in here to the data set
        #To use this we start with the name of the input to react to
        # Then we run other code
        # To use the updated data set, we call it via coombe()
        input$submit, {
            Coombe_map$sample <- 0
            if(input$select == 3) {
                #Generate Simple Random Sample
                # Minh to complete - have to use full if statements here
                if(input$replacement == FALSE )
                {Coombe_map$sample <- srswor(input$samplenumber, length(Coombe_map$Vine_ID))}
                else if(input$replacement == TRUE )
                {Coombe_map$sample <- srswr(input$samplenumber, length(Coombe_map$Vine_ID))}
            }
            else if(input$select == 2) {
                
                #a <- table(Coombe_map$Rootstock)
                
                if(input$stratanames == "Panel") {
                    col <- "PanelCluster"
                }
                else {
                    col <- input$stratanames
                }
                
                
                # Minh to update size to be based on slider
                sample <- strata(Coombe_map, col, 
                                 size=rep(input$stratasamplenumber, nrow(unique(Coombe_map[,col]))), 
                                 method=ifelse(test = input$replacement, "srswr", "srswor"))
                print(sample)
                Coombe_map$sample <- 0
                Coombe_map$sample[sample$ID_unit] <- 1
                
                # Generate Stratified Sample
            }
            else {
                #Generate cluster sample
                # Minh to complete
                sample <- cluster(Coombe_map, input$clustervariable, input$clusternumber, method = ifelse(test = input$replacement, "srswr", "srswor"))
                # Then we need to somehow select all the ID_unit values from Coombe_map so they are listed as 1, and everything else is 0
                Coombe_map$sample <- 0
                Coombe_map$sample[sample$ID_unit] <- 1
            }
            
            Coombe_map
        }
    )
    
    plotOutput <- reactive({
        p <- ggplot(coombe(), aes(x= Row, y = Column, color = Rootstock)) + geom_point(size = 3)  + 
            ggtitle("Map of Coombe Vineyard") + labs(color = 'Rootstock') + 
            geom_point(aes(shape = as.factor(sample), alpha = sample, size = 3*sample), colour = "grey30") + 
            scale_shape_manual(values=c(16, 15)) + guides(size = FALSE, alpha = FALSE, shape = FALSE) + 
            theme_bw() + theme(legend.position = "top")
    })
    
    output$Vineyard_map <- renderPlot({
        print(plotOutput())
    })
    
    # Minh complete
    # Add downloadHandler functions here
    # Try and copy the example I sent from Stack Overflow
    # https://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
    
    
}

shinyApp(ui, server)

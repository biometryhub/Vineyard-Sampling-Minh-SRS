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
            
            box(plotOutput("Vineyard_map", height = 500)),
            
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
                    # Minh complete
                    selectInput(
                        "stratanames", "Stratified on Rootstock based on:",
                        c("Row", "Panel")
                     ),
                     sliderInput(
                         "stratanumber", "Number of Stratum", min = 1, max = 11, value = 3
                    )
                    # sliderInput(
                    #     "clusternumber", "Number of Clusters", min = 1, max = 11, value = 3
                    # )
                ),
                
                # Show this for SRS
                conditionalPanel(
                    condition = "input.select == 3",
                sliderInput(inputId = "samplenumber", label = "Number of samples:", 
                            min = 1, max = nrow(Coombe_map), value = 30)
                ),
                
                # Minh complete
                # Add a checkbox here to say "Sample with replacement?"

                checkboxInput("replacement", "Sample with replacement", FALSE),
                actionButton("submit", label = "Submit") 
                
        
                
            ),
            #box(plotOutput('Vineyard_map'))#,
            # box( uiOutput('clustervariable'))
            
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
        #  Minh complete
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
        #  Minh complete
        if(input$stratanames == 'Row') {
            # Row stratified
            updateSliderInput(session, "stratanumber", max = 11)
        }
        else if(input$stratanames == "Panel") {
            # Panel stratified
            updateSliderInput(session, "stratanumber", max = 16)
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
                
                a <- table(Coombe_map$Rootstock)
                
                # Minh to update size to be based on slider
                sample <- strata(Coombe_map,c("Rootstock"), size=round(a*0.2), method=ifelse(test = input$replacement, "srswr", "srswor"))
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
    
    output$Vineyard_map <- renderPlot({
        ggplot(coombe(), aes(x= Row, y = Column, color = as.factor(sample))) + geom_point()  + 
            scale_color_manual(values = c("0"= "blue", "1"= "red"), labels = c("Non Sample", "Sample")) + 
            #scale_x_reverse() + 
            ggtitle("Map of Coombe Vineyard") + theme(legend.position = "top") + labs(color = 'Sampling plan')
    })
    
  
    
    
}

shinyApp(ui, server)

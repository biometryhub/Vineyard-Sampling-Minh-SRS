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
                        c("Rootstock", "Row", "Panel", "Column")
                    ),
                    sliderInput(
                        "clusternumber", "Number of Clusters", min = 1, max = 50, value = 10
                    ) 
                ),
                # Only show this panel if stratified is selected
                conditionalPanel(
                    condition = "input.select == 2",
                    sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
                ),
                # Show this for SRS
                conditionalPanel(
                    condition = "input.select == 3",
                sliderInput(inputId = "samplenumber", label = "Number of samples:", 
                            min = 1, max = nrow(Coombe_map), value = 30)
                ),
                actionButton("submit", label = "Submit"), 
                
        
                
            ),
            #box(plotOutput('Vineyard_map'))#,
            # box( uiOutput('clustervariable'))
            
        )
    )
)

server <- function(input, output) {
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
    #Simple Random Sampling
    # output$plot1 <- renderPlot({
    #     print(sample(Coombe_map$Vine_ID, size = input$samplenumber))
    #    
    #     #data(Coombe_map)
    #     Tot= Coombe_map$Vine_ID
    #     name= Coombe_map$Vine_ID
    #     n=input$samplenumber
    #     s=srswor(n, length(Tot))
    #     simplerandom <- as.vector(name[s==1])
    #     print(simplerandom)
    #     Coombe_map$sample <- s
    #     s=srswor(input$samplenumber, length(Coombe_map$Vine_ID))
    #     #sample_df <- data.frame(Coombe_map$Vine_ID, s)
    #     #print(sample_df)
    #     #simplerandom <- srswor(input$samplenumber, N = 352)
    #     #print(simplerandom)
    # #Cluster Sampling
    # 
    # #Render sampling plan
    #     output$submit <- renderPrint({ input$submit })
    #     
    # #Histogram     
    #     data <- histdata[seq_len(input$samplenumber)]
    #     hist(data)
    # })
    # 
    #Vineyard Map
    # Set up the data frame using an eventReactive() or possibly observeEvent()
    
    coombe <- eventReactive(#Do stuff in here to the data set
        #To use this we start with the name of the input to react to
        # Then we run other code
        # To use the updated data set, we call it via coombe()
        input$submit, {
            Coombe_map$sample <- 0
            if(input$select == 3) {
                #Generate Simple Random Sample
                Coombe_map$sample <- srswor(input$samplenumber, length(Coombe_map$Vine_ID))
            }
            else if(FALSE) {
                s=strata(Coombe_map,c(input$strata), size=round(a*0.2), method="srswor")
                # Generate Stratified Sample
            }
            else {
                #Generate cluster sample
                sample <- cluster(Coombe_map, "Row", input$clusternumber, method = "srswor")
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

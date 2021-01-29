# Vineyard Sampling app for Minh's Summer Research Scholarship Project

library(shiny)
library(shinydashboard)
library(readxl)
library(sampling)
library(ggplot2)
library(Cairo)
library(dplyr)

options(shiny.usecairo=T)

Coombe_map <- read_excel("Coombe_map.xlsx")

ui <- dashboardPage(
    dashboardHeader(title = "Vineyard Sampling"),
    sidebar <- dashboardSidebar(
        sidebarMenu(id = "tabs",
                    menuItem("Home", tabName = "Home"),
                    menuItem("Sampling Plan", tabName = "Sampling_plan"),
                    menuItem("Data entry", tabName = "Data_entry"),
                    menuItem("Plots", tabName = "Plots/ Analysis"),
                    menuItem("Acknowledgements", tabName = "Acknowledgements")
        )
        
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        tabItems(
            tabItem( tabName = "Sampling_plan",
                     
                     fluidRow(
                         
                         box(plotOutput("Vineyard_map", height = 500),
                             # Minh compelte
                             # Add download button here for plot
                             downloadButton('downloadMap', 'Download Map')
                         ),
                         
                         box(
                             title = "Sampling plan",
                             selectInput("select", label = h3("Type of Sampling"), 
                                         choices = list("Cluster Sampling" = 1, "Stratified Sampling" = 2, "Simple Random Sampling" = 3), 
                                         selected = 1),
                             
                             conditionalPanel(
                                 # Cluster sampling
                                 condition = "input.tabs == 'Sampling_plan' & input.select == 1",
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
                                 condition = "input.tabs == 'Sampling_plan' & input.select == 2",
                                 
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
                                 condition = "input.tabs == 'Sampling_plan' & input.select == 3",
                                 sliderInput(inputId = "samplenumber", label = "Number of samples:", 
                                             min = 1, max = nrow(Coombe_map), value = 30)
                             ),
                             
                             checkboxInput("replacement", "Sample with replacement?", FALSE),
                             actionButton("submit", label = "Generate Plan") 
                             
                         )
                     ),
                     
                     fluidRow(
                         box(width = 12,
                             downloadButton('downloadPlan', 'Download Sampling Plan'),
                             # Add table here to display samples chosen
                             DT::dataTableOutput("plan")
                         )
                     )
            ),
            tabItem( tabName = "Data_entry",
                     fluidRow(
                         
                         box(title = "Enter your measurements here",
                             numericInput("Vine_ID", "Vine ID:", 1, min = 1, max = 352),
                             selectInput("Rootstock", "Rootstock",
                                         c("Ramsey","SO 4","BVRC12","Teleki 5C","Schwarzmann","K 51-40" ,"Ruggeri 140" ,"420 A")),
                             #textInput("caption", "Caption", "Data Summary"),
                             numericInput("trunk", "Trunk Circumference", 1, min = 1, max = 352),
                             numericInput("cane", "Cane Count", 1, min = 1, max = 352),
                             numericInput("diameter", "Cane Diameter", 1, min = 1, max = 352),
                             numericInput("internode", "Internode Distance", 0.1, min = 1, max = 352),
                             actionButton("submit_data", label = "Submit Data") 
                             
                         )
                     ),
                     fluidRow(
                         box(width = 12,
                             downloadButton('downloadData', 'Download Sampling Data'),
                             #dataTableOutput('sampled_coombe'),
                             # Add table here to display samples chosen
                             DT::dataTableOutput("sampled_coombe")
                         )
                     )  
                     
            )
        )
    )
)

server <- function(input, output, session){
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
            theme_bw() + theme(plot.title = element_text(size = 16,hjust = 0.5),legend.position = "top")
    })
    
    output$Vineyard_map <- renderPlot({
        print(plotOutput())
    })
    
    #Set up Sampling table
    output$plan <- DT::renderDataTable({
        data <- coombe()[coombe()$sample == 1,]
        DT::datatable(data, rownames = F, extensions = "Responsive", plugins = 'natural',
                      options = list(lengthMenu = list(c(3, 10, -1), c('3', '10', 'All')),
                                     pageLength = 3, scrollX = TRUE))
        # return(data_table)
    })
    
    
    output$downloadMap <- downloadHandler(
        filename = function() { "sampling_plan.png" },
        content = function(file) {
            ggsave(file,plotOutput())
        }
    )
    
    output$downloadPlan <- downloadHandler(
        filename = function() { "sampling_plan.csv" },
        content = function(file) {
            write.csv( x =coombe(), file, row.names = FALSE)
        }
    )
    #2. Data entry
    #Output for Data entry - collect data into a data frame
    #Create blank data frame first
    # data_input <- data.frame(VineID = numeric(), Trunk_Circumference = numeric(), Cane = numeric(),
    # Diameter = numeric(), circumference = numeric())
    #data_input <- rbind(data_input, data.frame(VineID = input$Vine_ID, Trunk_Circumference = input$trunk, Cane = input$cane,Diameter = input$diameter))
    #eventReactive(input$submit_data, merge(data_input))
    
    
    #coombe_sampled <- data.frame(Vine_ID = as.numeric(), Rootstock = as.character(),cane = as.numeric(),
    # diameter = as.numeric(), circumference = as.numeric())
    coombe_sampled <- eventReactive(input$submit_data, {
        input_data <- data.frame(Vine_ID = input$Vine_ID, 
                                 cane_count = input$cane, 
                                 trunk_circumference = input$trunk,
                                 cane_diameter = input$diameter,
                                 internode_length = input$internode)
        results <- merge(coombe(), input_data, by = "Vine_ID", all.x = TRUE)
    }
    )
    #coombe_sampled <- eventReactive(input$submit_data, {data.frame("Vine ID" = input$Vine_ID, "Rootstock"= input$Rootstock, "Trunk Circumference" = input$trunk)}
    #)
    #output$sampled_coombe <- renderDataTable(coombe_sampled)
    
    output$sampled_coombe <- DT::renderDataTable({
        output <- coombe_sampled() %>% 
            select(Vine_ID, Rootstock, sample, cane_count, trunk_circumference, cane_diameter, internode_length) %>% 
            filter(!is.na(trunk_circumference))
        DT::datatable(output, rownames = F, extensions = "Responsive", plugins = 'natural',
                      options = list(lengthMenu = list(c(3, 10, -1), c('3', '10', 'All')),
                                     pageLength = 3, scrollX = TRUE))
        # return(data_table)
    })
    
    #Submit button for data collection
    #observeEvent(input$submit_data, renderPrint(coombe_sampled))
    
}

shinyApp(ui, server)

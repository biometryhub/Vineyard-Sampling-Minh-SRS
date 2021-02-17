# Vineyard Sampling app for Minh's Summer Research Scholarship Project

library(shiny)
library(shinydashboard)
library(readxl)
library(sampling)
library(ggplot2)
library(Cairo)
library(dplyr)
library(rhandsontable)

options(shiny.usecairo = T)

Coombe_map <- read_excel("Coombe_map.xlsx")

ui <- dashboardPage(
    dashboardHeader(title = "Vineyard Sampling"),
    sidebar <- dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            #menuItem("Home", tabName = "Home"),
            menuItem("Sampling Plan", tabName = "Sampling_plan"),
            menuItem("Data entry", tabName = "Data_entry"),
            menuItem("Analysis/Plots", tabName = "Plots_Analysis"),
            menuItem("Acknowledgements", tabName = "Acknowledgements")
        )
        
    ),
    dashboardBody(# Boxes need to be put in a row (or column)
        tabItems(
            tabItem(tabName = "Sampling_plan",
                    
                    fluidRow(
                        box(
                            plotOutput("Vineyard_map", height = 500),
                            
                            downloadButton('downloadMap', 'Download Map')
                        ),
                        
                        box(
                            title = "Sampling plan",
                            selectInput(
                                "select",
                                label = h3("Type of Sampling"),
                                choices = list(
                                    "Cluster Sampling" = 1,
                                    "Stratified Sampling" = 2,
                                    "Simple Random Sampling" = 3
                                ),
                                selected = 1
                            ),
                            
                            conditionalPanel(
                                # Cluster sampling
                                condition = "input.tabs == 'Sampling_plan' & input.select == 1",
                                selectInput(
                                    "clustervariable",
                                    "Cluster Variable",
                                    c("Row", "Panel", "Rootstock")
                                ),
                                
                                
                                sliderInput(
                                    "clusternumber",
                                    "Number of Clusters",
                                    min = 1,
                                    max = 11,
                                    value = 3
                                )
                                
                                
                            ),
                            # Only show this panel if stratified is selected
                            conditionalPanel(
                                condition = "input.tabs == 'Sampling_plan' & input.select == 2",
                                
                                selectInput(
                                    "stratanames",
                                    "Stratified on Rootstock based on:",
                                    c("Row", "Panel", "Rootstock")
                                ),
                                
                                sliderInput(
                                    "stratasamplenumber",
                                    "Number of Samples within a Strata",
                                    min = 1,
                                    max = 11,
                                    value = 3
                                )
                            ),
                            
                            # Show this for SRS
                            conditionalPanel(
                                condition = "input.tabs == 'Sampling_plan' & input.select == 3",
                                sliderInput(
                                    inputId = "samplenumber",
                                    label = "Number of samples:",
                                    min = 1,
                                    max = nrow(Coombe_map),
                                    value = 30
                                )
                            ),
                            
                            checkboxInput("replacement", "Sample with replacement?", FALSE),
                            
                            #Set seed value
                            p("The session's seed value is"),
                            textOutput("random_seed"),
                            numericInput(
                                "customize_seed",
                                "Or Customize seed value here:",
                                NULL,
                                min = 1,
                                max = 1e6
                            ),
                            
                            # Action button for plan generation
                            actionButton("submit", label = "Generate Plan")
                            
                        )
                    ),
                    
                    fluidRow(
                        box(
                            width = 12,
                            downloadButton('downloadPlan', 'Download Sampling Plan'),
                            # Add table here to display samples chosen
                            DT::dataTableOutput("plan")
                        )
                    )),
            tabItem(tabName = "Data_entry",
                    fluidRow(
                        box(
                            width = 12,
                            fileInput(
                                "sampling_plan_file",
                                "Upload your Sampling Plan (.csv) here",
                                accept = c("text/csv",
                                           ".csv")
                            ),
                            actionButton("submit_plan", label = "Upload")),
                            
                            box(
                                width = 12,
                                title = "Enter your measurements here",
                                selectInput("Vine_ID", "Vine ID:", c(1:352)),
                                selectInput(
                                    "Rootstock",
                                    "Rootstock",
                                    c(
                                        "Ramsey",
                                        "SO 4",
                                        "BVRC12",
                                        "Teleki 5C",
                                        "Schwarzmann",
                                        "K 51-40" ,
                                        "Ruggeri 140" ,
                                        "420 A"
                                    )
                                ),
                                numericInput(
                                    "trunk",
                                    "Trunk Circumference",
                                    1,
                                    min = 1,
                                    max = 352
                                ),
                                numericInput("cane", "Cane Count", 1, min = 1, max = 352),
                                numericInput("diameter", "Cane Diameter", 1, min = 1, max = 352),
                                numericInput(
                                    "internode",
                                    "Internode Distance",
                                    0.1,
                                    min = 1,
                                    max = 352
                                ),
                                actionButton("submit_data", label = "Submit Data")
                                
                            )
                        ),
                        fluidRow(
                            box(
                                width = 12,
                                downloadButton('downloadData', 'Download Sampling Data'),
                                
                                # Add table here to display samples chosen
                                rhandsontable::rHandsontableOutput("sampled_coombe")
                            )
                        )
                        
                    ),
            
            tabItem(tabName = "Plots_Analysis",
                    fluidRow(box(
                        width = 6,
                        fileInput(
                            "analysis_file",
                            "Upload your Sampling Data (.csv) here",
                            accept = c("text/csv",
                                       ".csv")
                        ),
                        actionButton("submit_data1", label = "Submit Data")
                        
                    ))),
            tabItem(tabName = "Acknowledgements",
                    fluidRow(box(width = 12)))
            
        ))
)

server <- function(input, output, session) {
    rvs <-
        reactiveValues(coombe_sampled = NULL,
                       coombe = Coombe_map,
                       seed = NULL)
    # coombe <- reactiveValues()
    # seed <- reactiveValues()
    #Simple Random Sampling
    #rvs$coombe
    
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
    
    # Set Seed value
    
    #rvs$seed <- sample(1:1e6, 1)
    output$random_seed <- renderText(rvs$seed)
    # if the seed input box is not blank, use the seed they have entered
    # Otherwise use the random seed
    observe({
        if (!isTruthy(input$customize_seed))
        {
            rvs$seed <- sample(1:1e6, 1)
        }
        else {
            rvs$seed <- input$customize_seed
            #output$random_seed <- renderPrint(seed)
        }
        
        set.seed(rvs$seed)
        #print(rvs$seed)
    })
    
    
    
    #Select type of sampling
    
    observe({
        if (input$clustervariable == 'Row') {
            # Row cluster
            updateSliderInput(session, "clusternumber", max = 11)
        }
        else if (input$clustervariable == "Rootstock") {
            # Rootstock
            updateSliderInput(session, "clusternumber", max = 8)
        }
        else if (input$clustervariable == "Panel") {
            # Panel
            updateSliderInput(session, "clusternumber", max = 16)
        }
        
    })
    
    #For stratified sampling
    observe({
        if (input$stratanames == 'Row') {
            # Row stratified
            #updateSliderInput(session, "stratanumber", max = 11)
            updateSliderInput(session, "stratasamplenumber", max = 32)
        }
        else if (input$stratanames == "Panel") {
            # Panel stratified
            # updateSliderInput(session, "stratanumber", max = 176)
            updateSliderInput(session,
                              "stratasamplenumber",
                              max = 2,
                              value = 1)
        }
        else if (input$stratanames == "Rootstock") {
            # Panel stratified
            #pdateSliderInput(session, "stratanumber", max = 8)
            updateSliderInput(session, "stratasamplenumber", max = 44)
        }
    })
    
    
    
    #Sample with replacement?
    output$replacement <- renderText({
        input$replacement
    })
    
    observeEvent(#Do stuff in here to the data set
        #To use this we start with the name of the input to react to
        # Then we run other code
        # To use the updated data set, we call it via rvs$coombe
        input$submit, {
            Coombe_map$sample <- 0
            if (input$select == 3) {
                #Generate Simple Random Sample
                # Minh to complete - have to use full if statements here
                if (input$replacement == FALSE)
                {
                    Coombe_map$sample <-
                        srswor(input$samplenumber,
                               length(Coombe_map$Vine_ID))
                }
                else if (input$replacement == TRUE)
                {
                    Coombe_map$sample <-
                        srswr(input$samplenumber,
                              length(Coombe_map$Vine_ID))
                }
            }
            else if (input$select == 2) {
                if (input$stratanames == "Panel") {
                    col <- "PanelCluster"
                }
                else {
                    col <- input$stratanames
                }
                
                
                # Minh to update size to be based on slider
                sample <- strata(
                    Coombe_map,
                    col,
                    size = rep(input$stratasamplenumber, nrow(unique(
                        Coombe_map[, col]
                    ))),
                    method = ifelse(test = input$replacement, "srswr", "srswor")
                )
                #print(sample)
                Coombe_map$sample <- 0
                Coombe_map$sample[sample$ID_unit] <- 1
                
                # Generate Stratified Sample
            }
            else {
                sample <-
                    cluster(
                        Coombe_map,
                        input$clustervariable,
                        input$clusternumber,
                        method = ifelse(test = input$replacement, "srswr", "srswor")
                    )
                Coombe_map$sample <- 0
                Coombe_map$sample[sample$ID_unit] <- 1
            }
            
            rvs$coombe <- Coombe_map
            
            #print(rvs$coombe$Vine_ID[rvs$coombe$sample==1])
            updateSelectInput(session, "Vine_ID", choices = rvs$coombe$Vine_ID[rvs$coombe$sample ==
                                                                                   1])
            
            return(rvs$coombe)
        })
    #Use user data input to analyse
    userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$sampling_plan_file, message = FALSE))
        input$sampling_plan_file
    })
    # The user's data, parsed into a data frame
    observeEvent( input$submit_plan  ,{
        File <- reactive({input$sampling_plan_file})
        #inputId = "file"
        data <- read.csv(userFile()$datapath,
                         header = F,
                         # quote = input$quote,
                         #sep = input$sep,
                         #stringsAsFactors = stringsAsFactors
                         )
        print(data)
        rvs$coombe <- data
        updateSelectInput(session, "Vine_ID", choices = rvs$coombe$Vine_ID[rvs$coombe$sample ==
                                                                               1])
    })
    
    plotOutput <- reactive({
        #head(rvs$coombe)
        p <-
            ggplot(rvs$coombe, aes(
                x = Row,
                y = Column,
                color = Rootstock
            )) + geom_point(size = 3)  +
            ggtitle("Map of Coombe Vineyard") + labs(color = 'Rootstock') +
            scale_shape_manual(values = c(16, 15)) + guides(size = FALSE,
                                                            alpha = FALSE,
                                                            shape = FALSE) +
            theme_bw() + theme(plot.title = element_text(size = 16, hjust = 0.5),
                               legend.position = "top")
        
        if ("sample" %in% colnames(rvs$coombe)) {
            p <- p +
                geom_point(aes(
                    shape = as.factor(sample),
                    alpha = sample,
                    size = 3 * sample
                ),
                colour = "grey30")
        }
        return(p)
    })
    
    output$Vineyard_map <- renderPlot({
        print(plotOutput())
    })
    
    #Set up Sampling table
    output$plan <- DT::renderDataTable({
        if ("sample" %in% colnames(rvs$coombe)) {
            data <- rvs$coombe[rvs$coombe$sample == 1,]
        }
        else {
            data <- NULL
        }
        
        DT::datatable(
            data,
            rownames = F,
            extensions = "Responsive",
            plugins = 'natural',
            options = list(
                lengthMenu = list(c(3, 10, -1), c('3', '10', 'All')),
                pageLength = 3,
                scrollX = TRUE
            )
        )
        # return(data_table)1
    })
    
    
    output$downloadMap <- downloadHandler(
        filename = function() {
            "sampling_plan.png"
        },
        content = function(file) {
            ggsave(file, plotOutput())
        }
    )
    
    output$downloadPlan <- downloadHandler(
        filename = function() {
            "sampling_plan.csv"
        },
        content = function(file) {
            write.csv(x = rvs$coombe, file, row.names = FALSE)
        }
    )
    #2. Data entry
    # Upload CSV file of the sampling plan
    eventReactive(input$sampling_plan_file,
                  sampling_csv <-
                      read.csv(input$sampling_plan_file))
    
    
    #Update select input depending on the sampling plan's Vine ID
    #observe(
    
    # Get vine numbers where rvs$coombe$sample == 1
    # But what happens before we generate a sample? Do we need to handle that?
    
    #if("sample" %in% colnames(rvs$coombe)) {
    #
    #}
    
    # if(length(rvs$coombe) == 0) {
    # If coombe$ Vine_ID = NULL
    #updateSelectInput(session, "Vine_ID", choices = c(1:352))
    # }
    # else if (length(rvs$coombe > 0)){
    #     # If there is sample plan
    #     updateSelectInput(session, "Vine_ID", as.list(coombe$Vine_ID))
    # }
    # else if (input$sampling_plan_file) {
    #     updateSelectInput(session, "Vine_ID", as.list(sampling_plan_file$Vine_ID))
    # }
    
    #)
    
    observeEvent(input$submit_data, {
        rvs$coombe_sampled  <- rbind(
            rvs$coombe_sampled,
            data.frame(
                Vine_ID = input$Vine_ID,
                cane_count = input$cane,
                trunk_circumference = input$trunk,
                cane_diameter = input$diameter,
                internode_length = input$internode
            )
            
        )
    })
    
    #print(rvs)
    # Merge at download button
    output$downloadData <- downloadHandler(
        filename = function() {
            "sampling_data.csv"
        },
        content = function(file) {
            sampling_csv <-
                merge(rvs$coombe,
                      rvs$coombe_sampled,
                      by = "Vine_ID",
                      all.x = TRUE)
            write.csv(x = sampling_csv, file, row.names = FALSE)
        }
    )
    
    
    output$sampled_coombe <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(rvs$coombe_sampled, stretchH = "all") %>%
            hot_cols(columnSorting = T)
    })
    
    observeEvent(input$sampled_coombe, {
        rvs$coombe_sampled <- hot_to_r(input$sampled_coombe)
    })
    
    #3. Analysis and Plot
    #Use user data input to analyse
    userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$sampling_plan_file, message = FALSE))
        input$sampling_plan_file
    })
    # The user's data, parsed into a data frame
    
    #User data input for analysis
    observeEvent( input$submit_data1  ,{
        inFile <- reactive({input$analysis_file})
        #inputId = "file"
        data1 <- read.csv(inFile()$datapath,
                         header = F,
                         # quote = input$quote,
                         #sep = input$sep,
                         #stringsAsFactors = stringsAsFactors
        )
        print(data1)
    })
    
    #Create a Box plot
    
}

shinyApp(ui, server)

# Vineyard Sampling app for Minh's Summer Research Scholarship Project

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(readxl)
library(sampling)
library(ggplot2)
library(ragg)
library(dplyr)
library(rhandsontable)

options(shiny.useragg = TRUE)

Coombe_map <- read_excel("Coombe_map.xlsx")

ui <- dashboardPage(
    dashboardHeader(title = "Vineyard Sampling"),
    sidebar <- dashboardSidebar(
        sidebarMenu(
            id = "tabs",
            #menuItem("Home", tabName = "Home"),
            menuItem("Sampling Plan", tabName = "Sampling_plan"),
            menuItem("Data entry", tabName = "Data_entry"),
            menuItem("Plots", tabName = "Plots_Analysis"),
            menuItem("Analysis", tabName = "Stats_Analysis"),
            menuItem("Acknowledgements", tabName = "Acknowledgements")
        )
        
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        tabItems(
            tabItem(tabName = "Sampling_plan",
                    
                    fluidRow(
                        box(
                            plotOutput("Vineyard_map", height = 500),
                            
                            downloadButton('downloadMap', 'Download Map')
                        ),
                        
                        box(
                            title = "Sampling plan",
                            column(
                                1,
                                dropdownButton(
                                    tags$h3("Sampling Plan help"),
                                    helpText(
                                        "The sampling plan is adjustable by choosing types of sampling, other variables and number of samples via dropdown boxes and sliders. Click “Generate Plan” after you have adjusted the parameters."
                                    ),
                                    circle = TRUE,
                                    status = "danger",
                                    icon = icon("question"),
                                    width = "200px",
                                    size = "xs",
                                    right = T,
                                    tooltip = tooltipOptions(title = "Click for help!")
                                )
                            ),
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
                            column(
                                1,
                                dropdownButton(
                                    tags$h3("Seed value help"),
                                    helpText(
                                        "A random seed is a starting point in generating random numbers. A random seed was generated at each new session or you can customize it here to reproduce a previous session."
                                    ),
                                    circle = TRUE,
                                    status = "danger",
                                    icon = icon("question"),
                                    width = "300px",
                                    size = "xs",
                                    right = T,
                                    tooltip = tooltipOptions(title = "Click for help!")
                                )
                            ),
                            
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
                            column(
                                1,
                                dropdownButton(
                                    tags$h3("Download plan help"),
                                    helpText(
                                        "Download the Sampling Plan to upload and use for the Data entry step."
                                    ),
                                    circle = TRUE,
                                    status = "danger",
                                    icon = icon("question"),
                                    width = "300px",
                                    size = "xs",
                                    right = T,
                                    tooltip = tooltipOptions(title = "Click for help!")
                                )
                            ),
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
                            actionButton("submit_plan", label = "Upload")
                        ),
                        
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
                    )),
            
            tabItem(tabName = "Plots_Analysis",
                    fluidRow(
                        box(
                            width = 6,
                            fileInput(
                                "analysis_file",
                                "Upload your Sampling Data (.csv) here",
                                accept = c("text/csv",
                                           ".csv")
                            ),
                            actionButton("submit_data1", label = "Submit Data")
                            
                        ),
                        
                        column(
                            width = 12,
                            box(
                                width = NULL,
                                title = "Plot the data",
                                solidHeader = T,
                                status = "success",
                                
                                dropdown(
                                    tags$h3("Plot Options"),
                                    
                                    selectInput("xval", "X values", choices = ""),
                                    selectInput("yval", "Y values", choices = ""),
                                    selectInput("colours", "Colour by", choices = ""),
                                    selectInput(
                                        "type",
                                        "Plot type",
                                        choices = c("Scatter plot", "Box plot", "Bar plot")
                                    ),
                                    selectInput(
                                        "theme",
                                        "Plot theme",
                                        choices = c(
                                            "ggplot2 Default",
                                            "Black and White" = "bw",
                                            "Classic",
                                            "Minimal",
                                            "Dark"
                                        ),
                                        selected = "bw"
                                    ),
                                    sliderInput(
                                        "size",
                                        "Point size",
                                        min = 1,
                                        max = 20,
                                        value = 4
                                    ),
                                    materialSwitch(
                                        inputId = "interactive",
                                        label = "Interactive plot",
                                        value = FALSE,
                                        status = "success",
                                        width = '95%',
                                        inline = T
                                    ),
                                    
                                    style = "material-circle",
                                    icon = icon("gear"),
                                    status = "danger",
                                    width = "300px",
                                    animate = animateOptions(
                                        enter = animations$fading_entrances$fadeInLeftBig,
                                        exit = animations$fading_exits$fadeOutLeftBig
                                    ),
                                    tooltip = tooltipOptions(title = "Click to change plot options")
                                ),
                                conditionalPanel(condition = 'input.interactive == true',
                                                 plotlyOutput('interactive_plot', height = "70vh")),
                                
                                conditionalPanel(condition = 'input.interactive == false',
                                                 plotOutput('static_plot', height = "70vh")),
                                downloadButton('downloadPlot', 'Download Plot')
                                
                            )
                        )
                        
                        
                    )),
            tabItem(tabName = "Stats_Analysis",
                    
                    fluidRow(
                        box(
                            checkboxGroupInput(
                                "analysis_type",
                                label = h3("Type of Analysis"),
                                choices = c("Estimate Mean", "Estimate Total", "Estimate Variance"),
                                selected = 1
                            ),
                            selectInput(
                                "variable_interest",
                                "Variable of Interest",
                                choices = ""
                            ),
                            actionButton("submit_variable", label = "Go")                            ),
                        ),
                    box(
                    textOutput("result")
                    )
                    ),
                            
                            tabItem(tabName = "Acknowledgements",
                                    fluidRow(
                                        id = "about",
                                        column(width = 2),
                                        column(
                                            width = 8,
                                            box(
                                                width = NULL,
                                                solidHeader = F,
                                                status = "danger",
                                                h3("Information about this app:"),
                                                "This application is a demonstration of some of the capabilities of the R shiny framework. It allows the user to upload a file (or use a built-in dataset), produce a plot, view and edit the data in an output table and then download an example report based on the input to the app. Any data uploaded to this app is not retained in any way once the app is closed, except through any reports you have downloaded to your computer.",
                                                h3("Packages:"),
                                                a(href = "https://shiny.rstudio.com/", "shiny", .noWS = "after"),
                                                ": for the application",
                                                br(),
                                                a(href = "https://rstudio.github.io/shinydashboard/", "shinydashboard", .noWS = "after"),
                                                ", ",
                                                a(href = "https://rinterface.github.io/shinydashboardPlus/", "shinydashboardPlus", .noWS = "after"),
                                                " and ",
                                                a(href = "https://dreamrs.github.io/shinyWidgets/index.html", "shinyWidgets", .noWS = "after"),
                                                ": to customise the appearance of the application",
                                                br(),
                                                a(href = "https://ggplot2.tidyverse.org/", "ggplot2", .noWS = "after"),
                                                ": to produce the static plot in the",
                                                actionLink("switch_to_plot", "Analysis/Plots"),
                                                "tab",
                                                br(),
                                                a(href = "https://plotly.com/r/", "plotly", .noWS = "after"),
                                                ": to produce the interactive plot in the",
                                                actionLink("switch_to_plot", "Analysis/Plots"),
                                                "tab",
                                                br(),
                                                a(href = "https://www.tidyverse.org/blog/2019/07/ragg-0-1-0/", "ragg", .noWS = "after"),
                                                ": to produce higher resolution plots in the",
                                                actionLink("switch_to_plot", "Analysis/Plots"),
                                                "tab",
                                                br(),
                                                a(href = "https://jrowen.github.io/rhandsontable/", "rhandsontable", .noWS = "after"),
                                                ": for producing the editable table in the",
                                                actionLink("switch_to_table", "Data Entry"),
                                                "tab",
                                                br(),
                                                h3("Author:"),
                                                "This app was developed by Minh Vu and Sam Rogers.",
                                                br(),
                                                br(),
                                                h3("More information:"),
                                                "For help with this app or requests to develop a Shiny application please contact",
                                                a(href = "mailto:biometryhub@adelaide.edu.au", "The Biometry Hub"),
                                                br(),
                                                br(),
                                                img(
                                                    src = "Biometry Hub.png",
                                                    height = 140,
                                                    width = 200
                                                ),
                                                img(
                                                    src = "logo_UofA.png",
                                                    height = 140,
                                                    width = 200
                                                ),
                                                
                                                actionBttn(
                                                    inputId = "disclaimer",
                                                    label = "Disclaimer",
                                                    style = "material-flat",
                                                    color = "warning",
                                                    icon = icon("warning"),
                                                    size = "xs"
                                                )
                                            )
                                        ),
                                        column(width = 2)
                                    ))
                            
                        )
                    ))
            
            server <- function(input, output, session) {
                rvs <-
                    reactiveValues(coombe_sampled = NULL,
                                   coombe = Coombe_map,
                                   seed = NULL)
                
                # Set Seed value
                
                output$random_seed <- renderText(rvs$seed)
                observe({
                    if (!isTruthy(input$customize_seed))
                    {
                        rvs$seed <- sample(1:1e6, 1)
                    }
                    else {
                        rvs$seed <- input$customize_seed
                    }
                    
                    set.seed(rvs$seed)
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
                        updateSliderInput(session, "stratasamplenumber", max = 32)
                    }
                    else if (input$stratanames == "Panel") {
                        # Panel stratified
                        updateSliderInput(session,
                                          "stratasamplenumber",
                                          max = 2,
                                          value = 1)
                    }
                    else if (input$stratanames == "Rootstock") {
                        # Panel stratified
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
                observeEvent(input$submit_plan , {
                    File <- reactive({
                        input$sampling_plan_file
                    })
                    #inputId = "file"
                    data <- read.csv(userFile()$datapath,
                                     header = T)
                    #print(head(data))
                    rvs$coombe <- data
                    updateSelectInput(session, "Vine_ID",
                                      choices = rvs$coombe$Vine_ID[rvs$coombe$sample == 1])
                })
                
                plotOutput <- reactive({
                    #head(rvs$coombe)
                    p <-
                        ggplot(rvs$coombe, aes(
                            x = Row,
                            y = Column,
                            color = Rootstock
                        )) + geom_point(size = 3)  +
                        labs(color = 'Rootstock', title = "Map of Coombe Vineyard") +
                        scale_shape_manual(values = c(16, 15)) + guides(size = FALSE,
                                                                        alpha = FALSE,
                                                                        shape = FALSE) +
                        theme_bw() + theme(
                            plot.title = element_text(size = 16, hjust = 0.5),
                            legend.position = "top",
                            text = element_text(size = 18)
                        )
                    
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
                #Update select input depending on the sampling plan's Vine ID
                
                
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
                    if (!is.null(rvs$coombe_sampled)) {
                        rhandsontable::rhandsontable(rvs$coombe_sampled, stretchH = "all") %>%
                            hot_cols(columnSorting = T)
                    }
                })
                
                observeEvent(input$sampled_coombe, {
                    rvs$coombe_sampled <- hot_to_r(input$sampled_coombe)
                })
                
                #3. Analysis and Plot
                #Use user data input to analyse
                # userFile <- reactive({
                #     # If no file is selected, don't do anything
                #     validate(need(input$sampling_plan_file, message = FALSE))
                #     input$sampling_plan_file
                #     print(input)
                # })
                # The user's data, parsed into a data frame
                
                #User data input for analysis
                observeEvent(input$submit_data1  , {
                    inFile <- reactive({
                        input$analysis_file
                    })
                    print(inFile())
                    data1 <- read.csv(inFile()$datapath,
                                      header = T)
                    rvs$data <- data1
                    print(head(data1))
                })
                
                #Create an interactive plot
                observeEvent(rvs$data, {
                    updateSelectInput(session, "xval", choices = colnames(rvs$data))
                    updateSelectInput(
                        session,
                        "yval",
                        choices = colnames(rvs$data),
                        selected = colnames(rvs$data)[2]
                    )
                    updateSelectInput(
                        session,
                        "colours",
                        choices = c("None", colnames(rvs$data)),
                        selected = "None"
                    )
                })
                output$static_plot <- renderPlot({
                    selected_colour <- NULL
                    selected_fill <- NULL
                    if (!is.na(input$colours) | !is.null(input$colours)) {
                        if (input$colours == "None" | input$colours == "") {
                            selected_colour <- NULL
                            selected_fill <- NULL
                        }
                        else {
                            selected_colour <- input$colours
                            selected_fill <- input$colours
                        }
                    }
                    
                    # output$sampled_coombe <- rhandsontable::renderRHandsontable({
                    #     if (!is.null(rvs$coombe_sampled)) {
                    #         rhandsontable::rhandsontable(rvs$coombe_sampled, stretchH = "all") %>%
                    #             hot_cols(columnSorting = T)
                    #     }
                    # })
                    #
                    
                    if (!is.null(rvs$data)) {
                        p <- ggplot(
                            rvs$data,
                            aes_string(
                                input$xval,
                                input$yval,
                                colour = selected_colour,
                                fill = selected_fill
                            )
                        )
                    }
                    switch(
                        input$type,
                        "Scatter plot" = p <-
                            p + geom_point(size = input$size),
                        "Box plot" = p <-
                            p + geom_boxplot(
                                aes_string(
                                    fill = selected_colour,
                                    colour = NULL,
                                    group = selected_colour
                                )
                            ),
                        "Bar plot" = p <-
                            p + geom_bar(stat = "identity", aes_string(
                                fill = selected_colour, colour = NULL
                            ))#,
                    )
                    switch(
                        input$theme,
                        "ggplot2 Default" = p <-
                            p + theme_gray(base_size = 16),
                        bw = p <- p + theme_bw(base_size = 16),
                        Classic = p <- p + theme_classic(base_size = 16),
                        Minimal = p <- p + theme_minimal(base_size = 16),
                        Dark = p <- p + theme_dark(base_size = 16)
                    )
                    rvs$plot <- p
                    return(p)
                })
                output$interactive_plot <- renderPlotly({
                    selected_colour <- NULL
                    selected_fill <- NULL
                    if (!is.na(input$colours) | !is.null(input$colours)) {
                        if (input$colours == "None" | input$colours == "") {
                            selected_colour <- NULL
                            selected_fill <- NULL
                        }
                        else {
                            selected_colour <- input$colours
                            selected_fill <- input$colours
                        }
                    }
                    p <- ggplot(
                        rvs$data,
                        aes_string(
                            input$xval,
                            input$yval,
                            colour = selected_colour,
                            fill = selected_fill
                        )
                    )
                    switch(
                        input$type,
                        "Scatter plot" = p <-
                            p + geom_point(size = input$size),
                        "Box plot" = p <-
                            p + geom_boxplot(
                                aes_string(
                                    fill = selected_colour,
                                    colour = NULL,
                                    group = selected_colour
                                )
                            ),
                        "Bar plot" = p <-
                            p + geom_bar(stat = "identity", aes_string(
                                fill = selected_colour, colour = NULL
                            ))#,
                    )
                    switch(
                        input$theme,
                        "ggplot2 Default" = p <-
                            p + theme_gray(base_size = 16),
                        bw = p <- p + theme_bw(base_size = 16),
                        Classic = p <- p + theme_classic(base_size = 16),
                        Minimal = p <- p + theme_minimal(base_size = 16),
                        Dark = p <- p + theme_dark(base_size = 16)
                    )
                    rvs$plot <- p
                    p <- ggplotly(p)
                    return(p)
                })
                
                output$downloadPlot <- downloadHandler(
                    filename = function() {
                        "sampling_plot.png"
                    },
                    content = function(file) {
                        ggsave(file, rvs$plot)
                    }
                )
                
                #4. Analysis 
                #Update select input colnames(rvs$data)
                observeEvent(rvs$data, {
                    updateSelectInput(session, "variable_interest", choices = colnames(rvs$data))
                })
                
                output$result <- renderText("Estimated Population Mean is
                                            br()
                                            Estimated Population Total is")
                #5.Acknowledgement
                observeEvent(input$disclaimer, {
                    sendSweetAlert(
                        session = session,
                        title = "Disclaimer",
                        text = tags$span(
                            tags$p('Copyright (c) 2020 University of Adelaide Biometry Hub'),
                            tags$p(
                                'Permission is hereby granted, free of charge, to any person obtaining
          a copy of this software and associated documentation files (the
          "Software"), to deal in the Software without restriction, including
          without limitation the rights to use, copy, modify, merge, publish,
          distribute, sublicense, and/or sell copies of the Software, and to
          permit persons to whom the Software is furnished to do so, subject to
          the following conditions:'
                            ),
                            tags$ul(
                                tags$li(
                                    'The above copyright notice and this permission notice shall
                  be included in all copies or substantial portions of the
                  Software.'
                                )
                            ),
                            tags$strong(
                                'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
               EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
               OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
               NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
               HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
               WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
               FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
               OTHER DEALINGS IN THE SOFTWARE.'
                            )
                        ),
                        type = "warning"
                    )
                })
                
            }
            
            shinyApp(ui, server)
            
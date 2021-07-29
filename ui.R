# Define UI for application that draws a histogram
ui <-
  navbarPage(
    title = "Primer Checker Analysis Tool",
    windowTitle = "Welcome to version 1.0 of Primer Checker Analysis Tool",
    theme = shinytheme("flatly"),
    tabPanel(
      "Home",
      
      div(includeHTML("www/description.html")),
      themeelector()
      
    ),
    #tabPanel Start
    tabPanel(
      "Start",
      # Tab title
      h3("Inputs of the Analysis"),
      hr(),
      fluidRow(
        column(3),
        column(6,
               fileInput("primer_file", label = h3("Primer Checker results file"), 
                         buttonLabel = "Choose your file...",
                         width = '100%'),
               numericInput("no_variants", label = h3("The number of variants"),
                            value = 500000,
                            width = '100%'),
               bsTooltip("no_variants", 
                         title = "Input the total number of COVID-19 variants Primer Checker tool used",
                         placement = "right", options = list(container = "body")),
               dateRangeInput("date_range", label = h3("Date Range")
                         ),
               bsTooltip("date_range", 
                         title = "The COVID-19 variants Date Range used by Primer Checker",
                         placement = "right", options = list(container = "body"))
               ),
        column(3)
      ),
      
      
      
      
      
      
    ),# tabPanel Start
    
    tabPanel("Results",
             titlePanel("Results of the Analysis"),
             tabsetPanel(type = "tabs",
                         tabPanel("Data", DT::dataTableOutput("primer_datatable")),
                         tabPanel("Statistics", DT::dataTableOutput("stats"),
                                  hr(),
                                  h3("Position of Signle Mutations"),
                                  DT::dataTableOutput("snps"),
                                  h3("Pivot table of Signle Mutations for fwd primer type"),
                                  pivottablerOutput("fwd"),
                                  h3("Position of Signle Mutations"),
                                  DT::dataTableOutput("fwd2")),
                         tabPanel("Graphs")
                         )
             ),
    
    tabPanel("Help",
             titlePanel("Help for Proteosign ver. 2.0"))
    
    
  ) # navbarPage

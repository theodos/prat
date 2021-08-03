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
                         tabPanel("Original Data", DT::dataTableOutput("primer_datatable")),
                         tabPanel("Statistics", DT::dataTableOutput("stats"),
                                  hr(),
                                  h3("Position of Signle Mutations"),
                                  DT::dataTableOutput("snps"),
                                  h3("Detailed Signle Mutations for fwd primer type"),
                                  DT::dataTableOutput("pivot_fwd"),
                                  h3("Detailed Signle Mutations for rev primer type"),
                                  DT::dataTableOutput("pivot_rev")),
                         tabPanel("Graph for fwd primer",
                                  sidebarLayout(sidebarPanel("Graph parameters",
                                                             textInput("fwdmtitle", label = h3("Main graph title"), value = "Forward primer mutations (%)")
                                                             
                                                             ),
                                                mainPanel("Graphs",
                                                          plotOutput("fwd_graph")
                                                          )
                                                ) # sidebarLaytout end
                                  ), # tabPanel end
                         tabPanel("Graph for rev primer",
                                  sidebarLayout(sidebarPanel("Graph parameters",
                                                             textInput("revmtitle", label = h3("Main graph title"), value = "Reverse primer mutations (%)")
                                                             
                                                            ),
                                                mainPanel("Graphs",
                                                          plotOutput("rev_graph")
                                                         )
                                                ) # sidebarLaytout end
                                 ) # tabPanel end
                         ), #tabsetPanel end
             
             ),
    
    tabPanel("Help",
             titlePanel("Help for Proteosign ver. 2.0"))
    
    
  ) # navbarPage

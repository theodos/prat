# Define UI for application that draws a histogram
ui <-
  navbarPage( div(column(width = 8, tags$p("Primer Checker Analysis")), 
                  column(width = 4, tags$img(src = "logo.png", height = 30))),
   #title = "Primer Checker Analysis Tool",
    windowTitle = "Welcome to version 1.0 of Primer Checker Analysis Tool",
    theme = shinytheme("cerulean"),
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
                            value = 560259,
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
                                  sidebarLayout(sidebarPanel(h3("Graph parameters"),
                                                             hr(),
                                                             h4("Labels for Title and Axis"),
                                                             textInput("fwdmtitle", label = h5("Main graph title"), value = "Forward primer mutations (%)"),
                                                             textInput("fwdxlabel", label = h5("X axis label"), value = "Primer sequence"),
                                                             textInput("fwdylabel", label = h5("Y axis label"), value = "Percent"),
                                                             hr(),
                                                             h4("Colours for A, T, C, G"),
                                                             colourInput("fwdacol", "Select colour for Adenine (A)", "lightgreen", allowTransparent = TRUE),
                                                             colourInput("fwdtcol", "Select colour for Thymine (T)", "darkorange1", allowTransparent = TRUE),
                                                             colourInput("fwdccol", "Select colour for Cytosine (C)", "grey", allowTransparent = TRUE),
                                                             colourInput("fwdgcol", "Select colour for Guanine (G)", "skyblue", allowTransparent = TRUE),
                                                             hr(),
                                                             h4("Cut-off Percent value"),
                                                             numericInput("fwdcutoff", label = h4("Display only percent labels above value:"), value = 4)
                                                             
                                                             ),
                                                mainPanel(h3("Graphs"),
                                                          plotOutput("fwd_graph")
                                                          
                                                          )
                                                ) # sidebarLaytout end
                                  ), # tabPanel end
                         tabPanel("Graph for rev primer",
                                  sidebarLayout(sidebarPanel(h3("Graph parameters"),
                                                             hr(),
                                                             h4("Labels for Title and Axis"),
                                                             textInput("revmtitle", label = h5("Main graph title"), value = "Reverse primer mutations (%)"),
                                                             textInput("revxlabel", label = h5("X axis label"), value = "Primer sequence"),
                                                             textInput("revylabel", label = h5("Y axis label"), value = "Percent"),
                                                             hr(),
                                                             h4("Colours for A, T, C, G"),
                                                             colourInput("revacol", "Select colour for Adenine (A)", "lightgreen", allowTransparent = TRUE),
                                                             colourInput("revtcol", "Select colour for Thymine (T)", "darkorange1", allowTransparent = TRUE),
                                                             colourInput("revccol", "Select colour for Cytosine (C)", "grey", allowTransparent = TRUE),
                                                             colourInput("revgcol", "Select colour for Guanine (G)", "skyblue", allowTransparent = TRUE),
                                                             hr(),
                                                             h4("Cut-off Percent value"),
                                                             numericInput("revcutoff", label = h4("Display only percent labels above value:"), value = 4),
                                                             
                                                             
                                                            ),
                                                mainPanel(h3("Graphs"),
                                                          plotOutput("rev_graph")
                                                         )
                                                ) # sidebarLaytout end
                                 ) # tabPanel end
                         ), #tabsetPanel end
             
             ),
    
    tabPanel("Help",
             titlePanel("Help for Proteosign ver. 2.0"))
    
    
  ) # navbarPage

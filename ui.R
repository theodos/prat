# Define UI for application that draws a histogram
ui <-
  navbarPage( div(column(width = 5, tags$img(src = "prat_logo_header.png", height = 35, style="padding-bottom:7px;")), 
                  column(width = 7, a(href="https://enzyquest.com", tags$img(src = "logo_enzyquest2.png", height = 35, style="padding-bottom:7px;")))),
   #title = "Primer Checker Analysis Tool",
    windowTitle = "Welcome to version 1.0 of PRAT",
    theme = shinytheme("cerulean"),
   id="navbar",
    tabPanel(
      "Home",
      
      div(column(width=3), column(width=6, includeHTML("www/description.html")), column(width=3))
     # themeelector()
      
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
               fileInput("primer_file", label = h3("Primer Checker results file (csv)"), 
                         buttonLabel = "Choose your csv file...",
                         width = '100%'),
               p("You can try an example by first downloading an example GISAID primer checker results csv file and then uploading it to PRAT."),
               a(href="GISAID_Primer1_Results.csv", "Download example csv file. ", donwload=NA, target="_blank"),
               actionLink("example_btn", "Press here to automatically define the number of variants and the date range for the example file."),
               numericInput("no_variants", label = h3("The number of variants"),
                            value = 560259,
                            width = '100%'),
               bsTooltip("no_variants", 
                         title = "Input the total number of COVID-19 variants used by Primer Checker",
                         placement = "right", options = list(container = "body")),
               dateRangeInput("date_range", label = h3("Date Range"),
                              start = "2021-01-01"
                              
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
                         tabPanel("Filter parameters", div(includeHTML("www/filters.html")),
                                  dateRangeInput("date_range_filters", label = h4("Date range:")),
                                  bsTooltip("date_range_filters", 
                                            title = "Input the dates your analysis will be based. The date range must be between the date range of all your data.",
                                            placement = "right", options = list(container = "body")),
                                  selectizeInput("countries_filter", label = h4("Countries to analyse"),
                                              choices = NULL, multiple = TRUE, options = list(placeholder = 'Analyse all countries')),
                                  bsTooltip("countries_filter", 
                                            title = "Leave empty to analyse all countries in the dataset.",
                                            placement = "right", options = list(container = "body")),
                                  ),
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
                                                             numericInput("fwdcutoff", label = h4("Display only percent labels above value:"), value = 4),
                                                             
                                                             hr(),
                                                             h4("Download plot"),
                                                             selectInput("fwdfile_type", label =  "File Type",
                                                                         choices = c("pdf", "png", "jpeg", "tiff")),
                                                             downloadButton("fwddownloadPlot", label = "Download plot")
                                                               
                                                             
                                                             
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
                                                             
                                                             hr(),
                                                             h4("Download plot"),
                                                             selectInput("revfile_type", label =  "File Type",
                                                                         choices = c("pdf", "png", "jpeg", "tiff")),
                                                             downloadButton("revdownloadPlot", label = "Download plot")
                                                             
                                                            ),
                                                mainPanel(h3("Graphs"),
                                                          plotOutput("rev_graph")
                                                         )
                                                ) # sidebarLaytout end
                                 ) # tabPanel end
                         ), #tabsetPanel end
             
             ),
    
    tabPanel("Help",
             titlePanel("Help for Primer Checker Analysis tool for SARS-CoV-2"),
             div(includeHTML("www/help.html"))
             )
    
    
  ) # navbarPage

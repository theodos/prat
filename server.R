
# Define server logic required to draw a histogram
function(input, output, session) {
  
  
  data_pc <- reactive({
    req(input$primer_file)
    
    ext <- tools::file_ext(input$primer_file$name)
    switch(ext,
           csv = vroom::vroom(input$primer_file$datapath, delim = ",",
                              show_col_types = FALSE, col_select = c(!Primer)),
           tsv = vroom::vroom(input$primer_file$datapath, delim = "\t",
                              show_col_types = FALSE),
           txt = vroom::vroom(input$primer_file$datapath, delim = "\t",
                              show_col_types = FALSE),
           validate("Invalid file; Please upload a .csv or .tsv/txt file")
    )
  })
  
  data_diagram <- reactive({
    data_pc() %>% filter(`Total Mutation in Primer Region` == 1) %>% 
      select(Diagram,`Primer Type`, `Accession ID`) %>% 
      separate(col = "Diagram", into=c("Query","pos", "Variant"), sep = "\\s")
      
  })
  output$primer_datatable <- DT::renderDataTable({
    data_pc()
  }, filter='top')
  
  output$stats <- DT::renderDataTable({
    data_pc() %>% group_by(`Primer Type`,`Total Mutation in Primer Region`) %>% 
      count(name="Count") %>% mutate(`Percent of Total in %`=Count/input$no_variants)
  }, filter='top')
    
  output$snps <- DT::renderDataTable({
    data_diagram() %>% mutate(`Mutation index` = str_locate(pos, pattern = "X")[,1]) %>%
      mutate(`Query Nucleotide` = str_sub(Query,start = `Mutation index`, end = `Mutation index`)) %>%
      mutate(`Variant nucleotide` = str_sub(Variant,start = `Mutation index`, end = `Mutation index`)) %>%
      select("Query","Variant","Accession ID", "Primer Type", "Mutation index", "Query Nucleotide", "Variant nucleotide")
  })
  #gisaid %>% group_by(`Primer Type`,`Total Mutation in Primer Region`) %>% count()
  
  output$fwd <- renderPivottabler({
    data_diagram() %>% mutate(`Mutation index` = str_locate(pos, pattern = "X")[,1]) %>%
      mutate(`Query Nucleotide` = str_sub(Query,start = `Mutation index`, end = `Mutation index`)) %>%
      mutate(`Variant nucleotide` = str_sub(Variant,start = `Mutation index`, end = `Mutation index`)) %>%
      select("Primer Type", "Mutation index", "Query Nucleotide", "Variant nucleotide") %>%
      filter(`Primer Type` == "fwd") -> fwd
    pt <- PivotTable$new()
    pt$addData(fwd)
    pt$addColumnDataGroups("Variant nucleotide")
    pt$addRowDataGroups("Mutation index")
    pt$addRowDataGroups("Query Nucleotide", addTotal = FALSE)
    pt$defineCalculation(calculationName="TotalSNPs", summariseExpression="n()")
    pt$evaluatePivot()
    pivottabler(pt)
  })
  
  output$fwd2 <- DT::renderDataTable({
    data_diagram() %>% mutate(`Mutation index` = str_locate(pos, pattern = "X")[,1]) %>%
      mutate(`Query Nucleotide` = str_sub(Query,start = `Mutation index`, end = `Mutation index`)) %>%
      mutate(`Variant nucleotide` = str_sub(Variant,start = `Mutation index`, end = `Mutation index`)) %>%
      select("Primer Type", "Mutation index", "Query Nucleotide", "Variant nucleotide") %>%
      filter(`Primer Type` == "fwd") -> fwd
    pt <- PivotTable$new()
    pt$addData(fwd)
    pt$addColumnDataGroups("Variant nucleotide")
    pt$addRowDataGroups("Mutation index")
    pt$addRowDataGroups("Query Nucleotide", addTotal = FALSE)
    pt$defineCalculation(calculationName="TotalSNPs", summariseExpression="n()")
    pt$evaluatePivot()
    pt$asDataFrame(rowGroupsAsColumns = TRUE)
  })
}


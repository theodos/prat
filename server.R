
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
      separate(col = "Diagram", into=c("Query","pos", "Variant"), sep = "\\s") %>%
      mutate(`Mutation index` = str_locate(pos, pattern = "X")[,1]) %>%
      mutate(`Query Nucleotide` = str_sub(Query,start = `Mutation index`, end = `Mutation index`)) %>%
      mutate(`Variant nucleotide` = str_sub(Variant,start = `Mutation index`, end = `Mutation index`)) %>%
      select("Query","Variant","Accession ID", "Primer Type", "Mutation index", "Query Nucleotide", "Variant nucleotide")
      
  })
  
  data_pivot_fwd <- reactive({
    data_diagram() %>% select("Primer Type", "Mutation index", "Query Nucleotide", "Variant nucleotide") %>%
      filter(`Primer Type` == "fwd") -> fwd
    pt <- PivotTable$new()
    pt$addData(fwd)
    pt$addColumnDataGroups("Variant nucleotide")
    pt$addRowDataGroups("Mutation index")
    pt$addRowDataGroups("Query Nucleotide", addTotal = FALSE)
    pt$defineCalculation(calculationName="TotalSNPs", summariseExpression="n()")
    pt$evaluatePivot()
    pt$asDataFrame(rowGroupsAsColumns = TRUE) -> data_with_nas
    data_with_nas %>% replace(is.na(.), 0) #replace all NAs with "0"
  })
  
  data_pivot_rev <- reactive({
    data_diagram() %>% select("Primer Type", "Mutation index", "Query Nucleotide", "Variant nucleotide") %>%
      filter(`Primer Type` == "rev") -> rev
    pt <- PivotTable$new()
    pt$addData(rev)
    pt$addColumnDataGroups("Variant nucleotide")
    pt$addRowDataGroups("Mutation index")
    pt$addRowDataGroups("Query Nucleotide", addTotal = FALSE)
    pt$defineCalculation(calculationName="TotalSNPs", summariseExpression="n()")
    pt$evaluatePivot()
    pt$asDataFrame(rowGroupsAsColumns = TRUE) -> data_with_nas
    data_with_nas %>% replace(is.na(.), 0) #replace all NAs with "0"
  })
  
  output$primer_datatable <- DT::renderDataTable({
    data_pc()
  }, filter='top')
  
  output$stats <- DT::renderDataTable({
    data_pc() %>% group_by(`Primer Type`,`Total Mutation in Primer Region`) %>% 
      count(name="Count") %>% mutate(`Percent of Total in %`=round(x = Count/input$no_variants*100, digits = 7))
  }, filter='top')
    
  output$snps <- DT::renderDataTable({
    data_diagram()
  })
  #gisaid %>% group_by(`Primer Type`,`Total Mutation in Primer Region`) %>% count()
  
  # output$fwd <- renderPivottabler({
  #   data_diagram() %>% select("Primer Type", "Mutation index", "Query Nucleotide", "Variant nucleotide") %>%
  #     filter(`Primer Type` == "fwd") -> fwd
  #   pt <- PivotTable$new()
  #   pt$addData(fwd)
  #   pt$addColumnDataGroups("Variant nucleotide")
  #   pt$addRowDataGroups("Mutation index")
  #   pt$addRowDataGroups("Query Nucleotide", addTotal = FALSE)
  #   pt$defineCalculation(calculationName="TotalSNPs", summariseExpression="n()")
  #   pt$evaluatePivot()
  #   pivottabler(pt)
  # })
  # 
  output$pivot_fwd <- DT::renderDataTable(
    data_pivot_fwd(), rownames = FALSE)
  
  output$pivot_rev <- DT::renderDataTable(
    data_pivot_rev(), rownames = FALSE)
  
  output$fwd_graph <- renderPlot({
    data_pivot_fwd() -> forbar
    forbar[dim(forbar)[1],dim(forbar)[2]] -> total
    forbar[-nrow(forbar),-ncol(forbar)] -> forbar
    forbar %>% mutate(across(where(~ is.numeric(.x) && !is.integer(.x)), ~ round(.x / {{total}} * 100,5))) -> forbar_percent
    
    data_diagram() -> snps_data
    nchar(snps_data[1,1]) -> query_seq_length
    as.data.frame(list(strsplit(as.character(snps_data[1,1]), fixed = 1, split=""),c(1:query_seq_length)),col.names = c("Query Nucleotide","Mutation index")) -> ori_seq
    colnames(ori_seq) <- c("Query Nucleotide","Mutation index")
    
    merge(x=forbar_percent,y=ori_seq,by="Mutation index",all = TRUE) -> merged_seq
    merged_seq$`Query Nucleotide.x`<-merged_seq$`Query Nucleotide.y`
    merged_seq %>% replace(is.na(.),0) -> merged_seq
    merged_seq[,-dim(merged_seq)[2]] -> merged_seq
    
    merged_seq %>% arrange(as.numeric(`Mutation index`)) -> merged_seq_ordered
    
    merged_seq_ordered %>% pivot_longer(where(is.numeric), names_to = "hit", values_to = "count") -> forbar_percent_long
    
    ceiling(max(forbar_percent_long$count)) -> maxy
    
    ggplot(data=forbar_percent_long, aes(x=`Mutation index`, fill=hit, y = count)) +
      geom_bar(stat="identity") +
      # geom_text(aes(label = ifelse(count>4, count, "")), vjust =1.5) +
      scale_x_discrete(limits = forbar_percent_ordered[,1], labels = forbar_percent_ordered[,2]) +
      ggtitle(input$fwdmtitle) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.title = element_blank()) +
      geom_label_repel(aes(label = ifelse(count>input$fwdcutoff, paste(round(count,2), "%", sep=""), "")), max.overlaps = Inf, box.padding = 1.5, show.legend = FALSE) +
      scale_fill_manual(values = c("A" = input$fwdacol, "T" = input$fwdtcol, "G" = input$fwdgcol, "C" = input$fwdccol)) +
      annotate("text", x = 0, y = -3.5, label = "5'") + coord_cartesian(ylim = c(0, maxy),  clip = "off") + annotate("text", x = 30, y = -3.5, label = "3'") +
      xlab(input$fwdxlabel) + ylab(input$fwdylabel)
    
  })
  
  output$rev_graph <- renderPlot({
    data_pivot_rev() -> forbar
    forbar[dim(forbar)[1],dim(forbar)[2]] -> total
    forbar[-nrow(forbar),-ncol(forbar)] -> forbar
    forbar %>% mutate(across(where(~ is.numeric(.x) && !is.integer(.x)), ~ round(.x / {{total}} * 100,5))) -> forbar_percent
    
    data_diagram() -> snps_data
    nchar(snps_data[1,1]) -> query_seq_length
    as.data.frame(list(strsplit(as.character(snps_data[1,1]), fixed = 1, split=""),c(1:query_seq_length)),col.names = c("Query Nucleotide","Mutation index")) -> ori_seq
    colnames(ori_seq) <- c("Query Nucleotide","Mutation index")
    
    merge(x=forbar_percent,y=ori_seq,by="Mutation index",all = TRUE) -> merged_seq
    merged_seq$`Query Nucleotide.x`<-merged_seq$`Query Nucleotide.y`
    merged_seq %>% replace(is.na(.),0) -> merged_seq
    merged_seq[,-dim(merged_seq)[2]] -> merged_seq
    
    merged_seq %>% arrange(as.numeric(`Mutation index`)) -> merged_seq_ordered
    
    merged_seq_ordered %>% pivot_longer(where(is.numeric), names_to = "hit", values_to = "count") -> forbar_percent_long
    
    ceiling(max(forbar_percent_long$count)) -> maxy
    
    ggplot(data=forbar_percent_long, aes(x=`Mutation index`, fill=hit, y = count)) +
      geom_bar(stat="identity") +
      # geom_text(aes(label = ifelse(count>4, count, "")), vjust =1.5) +
      scale_x_discrete(limits = forbar_percent_ordered[,1], labels = forbar_percent_ordered[,2]) +
      ggtitle(input$revmtitle) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.title = element_blank()) +
      geom_label_repel(aes(label = ifelse(count>input$revcutoff, paste(round(count,2), "%", sep=""), "")), max.overlaps = Inf, box.padding = 1.5, show.legend = FALSE) +
      scale_fill_manual(values = c("A" = input$revacol, "T" = input$revtcol, "G" = input$revgcol, "C" = input$revccol)) +
      annotate("text", x = 0, y = -3.5, label = "5'") + coord_cartesian(ylim = c(0, maxy),  clip = "off") + annotate("text", x = 30, y = -3.5, label = "3'") +
      xlab(input$revxlabel) + ylab(input$revylabel)
    
  })

}
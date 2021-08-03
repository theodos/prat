pt <- PivotTable$new()
pt$addData(fwd)
pt$addColumnDataGroups("Variant nucleotide")
#pt$addColumnDataGroups("PowerType", expandExistingTotals=TRUE) # << ** CODE CHANGE ** <<
pt$addRowDataGroups("Mutation index")
pt$addRowDataGroups("Query Nucleotide", addTotal = FALSE)
pt$defineCalculation(calculationName="TotalSNPs", summariseExpression="n()")
pt$evaluatePivot()
pt$asDataFrame(rowGroupsAsColumns = TRUE) -> pivotframe

#pt$renderPivot()

pivotframe %>% replace(is.na(.), 0) -> forbar
forbar[dim(forbar)[1],dim(forbar)[2]] -> total
forbar[-nrow(forbar),-ncol(forbar)] -> forbar

forbar %>% mutate(across(where(is.numeric), ~ .x / {{total}}))
forbar %>% mutate(across(where(~ is.numeric(.x) && !is.integer(.x)), ~ round(.x / {{total}} * 100,5))) -> forbar_percent
forbar %>% mutate(`A percent` = round(A / {{total}} * 100, 3 ))

forbar[1:21,c(-7)] %>% pivot_longer(cols=c('A','C','G','T'), names_to = "hit", values_to = "count") -> forbar_long
forbar_percent %>% pivot_longer(where(is.numeric), names_to = "hit", values_to = "count") -> forbar_percent_long
forbar_percent_ordered %>% pivot_longer(where(is.numeric), names_to = "hit", values_to = "count") -> forbar_percent_long


ggplot(data=forbar_percent, aes(x=`Mutation index`, y=A)) +
  geom_bar(stat="identity")

ggplot(data=forbar_percent_long, aes(x=`Mutation index`, fill=hit, y = count)) +
  geom_bar(stat="identity") +
 # geom_text(aes(label = ifelse(count>4, count, "")), vjust =1.5) +
  scale_x_discrete(limits = forbar_percent_ordered[,1], labels = forbar_percent_ordered[,2]) +
  ggtitle("Forward primer mutations (%)") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), legend.title = element_blank()) +
  geom_label_repel(aes(label = ifelse(count>4, paste(round(count,2), "%", sep=""), "")), max.overlaps = Inf, box.padding = 1.5, show.legend = FALSE) +
  scale_fill_manual(values = c("A" = "lightgreen", "T" = "darkorange1", "G" = "grey", "C" = "skyblue")) +
  annotate("text", x = 0, y = -3.5, label = "5'") + coord_cartesian(ylim = c(0, 50),  clip = "off") + annotate("text", x = 30, y = -3.5, label = "3'")

  #guides(fill = guide_legend(override.aes = aes(label=""))) #remove "a" from inside keys of the legend


as.data.frame(list(strsplit(as.character(ori_seq[1,1]), fixed = 1, split=""),c(1:29)),col.names = c("Nucleotide","Mutation index"))
colnames(ori_seq_tmp) <- c("Nucleotide","Mutation index")

merge(x=forbar_percent,y=ori_seq_tmp,by="Mutation index",all = TRUE) -> merged_seq
merged_seq$`Query Nucleotide.x`<-merged_seq$`Query Nucleotide.y`
merged_seq %>% replace(is.na(.),0) -> merged_seq
merged_seq[,-dim(merged_seq)[2]] -> merged_seq

forbar_percent %>% arrange(as.numeric(`Mutation index`)) -> forbar_percent_ordered



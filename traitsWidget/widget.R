require(manipulateWidget)


function_widget=function(trait_,age_adjusted_,data_) {
 
  data_ <- data_  %>% dplyr::filter(trait == trait_) %>% dplyr::filter(age_adjusted == age_adjusted_)
  
  p_ <- ggplot(data=data_,aes(log2FoldChange,-log10(padj),color=Type)) + 
    geom_point(alpha=1,size=1.2,shape=20) +
    theme(panel.background = element_rect(fill="white",colour = "gray87")) +
    theme(panel.grid.major = element_line(colour = "gray87",size=0.1)) +
    theme(plot.margin = unit(c(0.2,0,0,0.3),"cm")) +
    coord_cartesian(xlim=c(-3,3),ylim = c(0,20)) +
    scale_color_manual(values=rna_colors) +
    geom_hline(yintercept = -log10(0.05),color="grey") +
    xlab(expression(log[2]~(fold~change)))+
    ylab(expression(-log[10]~(padj))) +
    theme(legend.position = "right") +
    theme(legend.text = element_text(size = 12),axis.title = element_text(size=14))
  
plotly::ggplotly(p_)
  
}


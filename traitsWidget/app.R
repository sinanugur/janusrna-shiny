library(shiny)
require(tidyverse)
require(DT)
require(plotly)



traits_005_age <- read_tsv("traits_005_age.tsv") %>%   mutate(trait=str_split(trait,",",simplify = T)[,1],age_adjusted="Yes")


traits_005 <- read_tsv("traits_005.tsv") %>%  mutate(age_adjusted="No")

data_table_for_widget = bind_rows(traits_005,traits_005_age)

rna_colors=c("lncRNA"='#a6cee3',"miRNA"='#ff7f00',"miscRNA"='#b2df8a',"tRNA derived"='#33a02c',
             "piRNA"='#b15928',"mRNA"='#6a3d9a',"tRNA"='#1f78b4',"snoRNA"='#cab2d6',
             "hairpin"='#fb9a99',"isomiR"='#e31a1c',"others"="grey",snRNA="grey")


server=function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    #iris[, c(input$xcol, input$ycol)]
    data_table_for_widget %>% dplyr::filter(trait == input$trait) %>% dplyr::filter(age_adjusted == input$age_adjusted)
    
  })
  
    output$plot1 <- renderPlotly({
      #plotly::ggplotly(ggplot(data=selectedData(),aes(log2FoldChange,-log10(padj),color=Type)) + 
       # geom_point(alpha=1,size=1.2,shape=20) +
        #theme(panel.background = element_rect(fill="white",colour = "gray87")) +
        #theme(panel.grid.major = element_line(colour = "gray87",size=0.1)) +
        #theme(plot.margin = unit(c(0.2,0,0,0.3),"cm")) +
        #coord_cartesian(xlim=c(-3,3),ylim = c(0,20)) +
        #scale_color_manual(values=rna_colors) +
        #geom_hline(yintercept = -log10(0.05),color="grey") +
        #xlab(expression(log[2]~(fold~change)))+
        #ylab(expression(-log[10]~(padj))) +
        #theme(legend.position = "right") +
        #theme(legend.text = element_text(size = 12),axis.title = element_text(size=14)))
      plotly::plot_ly(data=selectedData(),x=~log2FoldChange,y=~(-log10(padj)),color =~Type,text=~ID,type="scatter")
  })
  
}



ui=fluidPage(
  #headerPanel('Volcano Plots of RNAs and Traits'),
  sidebarPanel(
    selectInput('trait', 'Trait', unique(as.character(data_table_for_widget$trait)),selected = "smoking"),
    selectInput('age_adjusted', 'Age adjusted', unique(as.character(data_table_for_widget$age_adjusted)),selected = "Yes")
    
  ),
  mainPanel(
    plotlyOutput('plot1')
  )
)


shinyApp(ui=ui,server = server)

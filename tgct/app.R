library(shiny)
require(tidyverse)
require(DT)
require(shinythemes)



rna_table <- read_csv2("supp_table1_rna_timetodiagnose.csv")  %>%
  mutate_if(is.double,function(x) round(x,digits = 4))

pathways_table <- read_tsv("table2_tgct_pathways.tsv") %>%
  mutate_if(is.double,function(x) round(x,digits = 4))


server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    #data <- data_rna_table %>% arrange(Type,desc(medians))
    
    timetodiagnose_=ifelse(input$time == "All",c("[0,2]",
                                                "(2,5]","(5,8]","(8,10]"),input$time)
    if (input$supp == "Table 1") {
      data <- rna_table %>% filter(timetodiagnose %in% timetodiagnose_)
      
      if (input$type != "All") {
        
        data <- data %>% dplyr::filter(Type == input$type)
      } else {
        
        data
      }
        
    } else {
      
      data <- pathways_table %>% filter(timetodiagnose %in% timetodiagnose_)
    }
    
    
    data
    
    
  }))
  #output$desc <- renderText({"This is the dataset of identified RNAs in Janus healthy serum samples (i.e. controls), <br> which was shared with Umu <i>et al.</i> 2018."})
  #output$text <- renderText({"<b>If you use this dataset, please cite:</b> "})
  
  #output$cite <- renderText({"Sinan Uğur Umu et al. (2018) A comprehensive profile of circulating RNAs in human serum, RNA Biology, 15:2, 242-250, DOI: "})
  
}


# Define UI for application that draws
ui <- fluidPage(
  #theme =shinytheme("cerulean"),
          
          # Application title
#          titlePanel("JanusRNA Data Tables"),br(),
          #shiny::htmlOutput("desc"),br(),
          
          fluidRow(
            column(2, #size of type selection box
                   selectInput("supp",
                               "Supplementary tables:",
                               c("Table 1",
                                 "Table 2")))),
          fluidRow(
            column(2, #size of type selection box
                   selectInput("time",
                               "Time to diagnosis:",
                               c("All","[0,2]",
                                 "(2,5]","(5,8]","(8,10]")))),
          
          fluidRow(
            column(2, #size of type selection box
                   selectInput("type",
                               "RNA Type:",
                               c("All",
                                 unique(as.character(rna_table$Type)))))),
          
          DT::dataTableOutput("table"),br(),
          tags$hr()
          #mainPanel(
          #  shiny::htmlOutput("text"),
          #  shiny::textOutput("cite",inline = T),tags$a(href="https://www.doi.org/10.1080/15476286.2017.1403003","10.1080/15476286.2017.1403003"))
          
)


shinyApp(ui=ui,server = server)

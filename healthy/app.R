library(shiny)
require(tidyverse)
require(DT)
require(shinythemes)



data_rna_table <- read_tsv("all_rna_tables.tsv") %>% 
  dplyr::rename(miRBase=MiRBase.ID,miRBase_precursor=mirbase.precursor.ID) %>%
  mutate_if(is.double,function(x) round(x,digits = 4))


server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- data_rna_table %>% arrange(Type,desc(medians))
    if (input$type != "All") {
      data <- data %>% dplyr::filter(Type == input$type) %>% dplyr::select_if(~sum(!is.na(.)) > 0)
    } else {
      
      data <- data %>% dplyr::select(ID:Type)
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
                   selectInput("type",
                               "RNA Type:",
                               c("All",
                                 unique(as.character(data_rna_table$Type)))))),
          
          DT::dataTableOutput("table"),br(),
          tags$hr()
          #mainPanel(
          #  shiny::htmlOutput("text"),
          #  shiny::textOutput("cite",inline = T),tags$a(href="https://www.doi.org/10.1080/15476286.2017.1403003","10.1080/15476286.2017.1403003"))
          
)


shinyApp(ui=ui,server = server)

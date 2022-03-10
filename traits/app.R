library(shiny)
require(tidyverse)
require(DT)



traits_005_age <- read_tsv("traits_005_age.tsv") %>% 
  mutate(trait=str_split(trait,",",simplify = T)[,1],age_adjusted="Yes")


traits_005 <- read_tsv("traits_005.tsv") %>%
  mutate(age_adjusted="No")

data_rna_table = bind_rows(traits_005,traits_005_age) %>% 
  mutate_if(is.double,function(x) signif(x,digits = 4))


server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- data_rna_table %>% arrange(Type,desc(padj))
    
    if (input$trait != "All") {
      data <- data %>% dplyr::filter(trait == input$trait)
      
    }
    
    if (input$type != "All") {
      data <- data %>% dplyr::filter(Type == input$type)
    }
    
  
    data %>% dplyr::filter(padj <= input$padj,age_adjusted == input$age_adjusted)
    
    
  }))

}


# Define UI for application that draws
ui <- fluidPage(
                #theme =shinytheme("cerulean"),
                
                # Application title
                #          titlePanel("JanusRNA Data Tables"),br(),
                #shiny::htmlOutput("desc"),br(),
                
                fixedRow(column(2, #size of type selection box
                         selectInput("type",
                                     "RNA Type:",
                                     c("All",
                                       unique(as.character(data_rna_table$Type))))),
                  column(2, #size of type selection box
                         selectInput("trait",
                                     "Trait:",
                                     c("All",
                                       unique(as.character(data_rna_table$trait)))))),
                fixedRow(column(2, #size of type selection box
                       numericInput("padj",
                                   "Adjusted P value treshold:",
                                   c(0.05))),
                column(2, #size of type selection box
                       selectInput("age_adjusted",
                                    "Age adjusted:",
                                    c("No","Yes")))),
                
                DT::dataTableOutput("table"),br(),
                tags$hr()
                #mainPanel(
                #  shiny::htmlOutput("text"),
                #  shiny::textOutput("cite",inline = T),tags$a(href="https://www.doi.org/10.1080/15476286.2017.1403003","10.1080/15476286.2017.1403003"))
                
)


shinyApp(ui=ui,server = server)



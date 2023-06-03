library(shiny)

ui <- fluidPage(  
  
  fileInput("myfileinput", "Please choose a csv File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  selectInput('myselectinput','Select the first var', ""),
  
  tableOutput('mytable')
  
)

server <- function(input, output, session) {
  
  #Reactive to store loaded data
  reactives <- reactiveValues(
    
    mydata = NULL
    
  )
  
  #Observe file being selected
  observeEvent(input$myfileinput, {
    
    #Store loaded data in reactive
    reactives$mydata <- read.csv(file = input$myfileinput$datapath,
                                 sep = '\t',
                                 dec = ',',
                                 header = TRUE,
                                 skip = 1)
    
    #Update select input
    updateSelectInput(session, inputId = 'myselectinput', label = 'Select the first var', choices  = colnames(reactives$mydata))
    
  })
  
  #Data table
  output$mytable <- renderTable({ 
    
    reactives$mydata
    
  })
  
}

shinyApp(ui = ui, server = server)
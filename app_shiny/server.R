library(shiny)

#server define la funcion de la app
server <- server <- function(input, output, session) {
  # Create a reactive expression
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  output$summary <- renderPrint({
    summary(dataset())
  })
  output$table <- renderTable({
    dataset()
  })
  output$contents <- renderTable({
    req(input$selection)
    data <- read.csv(input$selection$datapath,sep='\t', dec=',', header=TRUE, skip = 1)
    colnames(data) <- c("INCLUDE","COLOR","POS","NAME","CP","CONCENTRATION","STANDARD","STATUS")
    data
  })
}

library(shiny)
library(DT)
library(data.table)

server <- function(input, output, session) {
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
  })
  
  observeEvent(input$csvs, {
  csvs <- reactive({
    # list( rbindlist(lapply(input$csvs$datapath, fread),
    #                           use.names = TRUE, fill = TRUE))
    tmp <- lapply(input$csvs$datapath, fread)
    names(tmp) <- input$csvs$name
    tmp
    
  })
  
  output$contents2 <- renderDT({
    datatable(csvs()[[2]],
              options = list(pageLength = 5,
                             columnDefs = list(list(className = 'dt-center', targets = 5)),
                             lengthMenu = c(5, 10, 15, 20),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                               "}")))
  })
  output$contents3 <- renderDT({
    datatable(csvs()[[1]],
              options = list(pageLength = 10))
  })
  output$contents <- renderDT({
    datatable(csvs()[[3]],
              options = list(pageLength = 10))
  })
  })
  
  #Reactive to store loaded data
  reactives <- reactiveValues(mydata = NULL)
  
  #Observe file being selected
  observeEvent(input$selection, {
    #Store loaded data in reactive
    reactives$mydata <-  read.csv(
      input$selection$datapath,
      sep = '\t',
      dec = ',',
      header = TRUE,
      skip = 1
    )
    
    #Update select input
    updateSelectInput(
      session,
      inputId = 'colorInput',
      label = 'Select the first var',
      choices  = c("", reactives$mydata$Color),
      selected = NULL
    )
    updateSelectizeInput(
      session,
      inputId = 'NameSelect',
      label = 'Select the name',
      choices  = reactives$mydata$Name,
      selected = NULL
    )
    updateSelectizeInput(
      session,
      inputId = 'PosSelect',
      label = 'Select the position',
      choices  = reactives$mydata$Pos,
      selected = NULL
    )
    updateSelectInput(
      session,
      inputId = 'cpSelect',
      label = 'Select the cp',
      choices  = c("", reactives$mydata$Cp),
      selected = NULL
    )
    
  })
  
  df2 <- reactive({
    filtered <- reactives$mydata
    if (input$colorInput != "") {
      filtered <- subset(reactives$mydata, Color %in% input$colorInput)
    }
    if (input$cpSelect != "") {
      filtered <- subset(reactives$mydata, Cp > input$cpSelect)
    }
    if (!is.null(input$NameSelect)) {
      filtered <- subset(reactives$mydata, Name %in% input$NameSelect)
    }
    if (!is.null(input$PosSelect)) {
      filtered <- subset(reactives$mydata, Pos %in% input$PosSelect)
    }
    filtered
  })
  
  df <- reactive({
    data <-
      read.csv(
        mycsvs()[[2]],
        sep = '\t',
        dec = ',',
        header = TRUE,
        skip = 1
      )
    colnames(data) <-
      c("INCLUDE",
        "COLOR",
        "POS",
        "NAME",
        "CP",
        "CONCENTRATION",
        "STANDARD",
        "STATUS")
    data
  })
}
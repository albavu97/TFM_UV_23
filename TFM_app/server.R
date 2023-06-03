library(shiny)
library(DT)

server <- function(input, output, session) {
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
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
      inputId = 'myselectinput',
      label = 'Select the first var',
      choices  = c("",reactives$mydata$Color),
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
      choices  = c("",reactives$mydata$Cp),
      selected = NULL
    )
    
  })
  
  df2 <- reactive({
    filtered <- reactives$mydata
    if (input$myselectinput != "") {
      filtered <- subset(reactives$mydata, Color %in% input$myselectinput)
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
    req(input$selection)
    data <-
      read.csv(
        input$selection$datapath,
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
  output$contents <- renderDT({
    datatable(df2(),
              options = list(pageLength = 10),)
  })
}
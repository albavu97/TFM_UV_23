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
    titleUpdate <- reactive({
      output$title1 <- renderText(names(csvs())[1])
      output$title2 <- renderText(names(csvs())[2])
      output$title3 <- renderText(names(csvs())[3])
    })
    
    #function to update filters of table
    update_function1 <- function(number) {
      #Update select input
      updateSelectInput(
        session,
        inputId = paste0('colorInput',number),
        label = 'Select the first var',
        choices  = c("", csvs()[[number]]$Color),
        selected = NULL
      )
      updateSelectizeInput(
        session,
        inputId = paste0('NameSelect',number),
        label = 'Select the name',
        choices  = csvs()[[number]]$Name,
        selected = NULL
      )
      updateSelectizeInput(
        session,
        inputId = paste0('PosSelect',number),
        label = 'Select the position',
        choices  = csvs()[[number]]$Pos,
        selected = NULL
      )
      updateSelectInput(
        session,
        inputId = paste0('cpSelect',number),
        label = 'Select the cp',
        choices  = c("", csvs()[[number]]$Cp),
        selected = NULL
      )}
    
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
    
    if (length(csvs()) > 3) {
      shinyalert("Warning!", "You can't upload more than 3 files.", type = "warning")
    } else if (length(csvs()) == 3){
      titleUpdate()
    output$contents2 <- renderDT({
      datatable(
        csvs()[[2]],
        options = list(
          pageLength = 10,
          columnDefs = list(list(
            className = 'dt-center', targets = 5
          )),
          lengthMenu = c(5, 10, 15, 20),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        )
      )
    })
    output$contents3 <- renderDT({
      datatable(
        csvs()[[3]],
        options = list(
          pageLength = 10,
          columnDefs = list(list(
            className = 'dt-center', targets = 5
          )),
          lengthMenu = c(5, 10, 15, 20),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        )
      )
    })
    output$contents <- renderDT({
      datatable(
        csvs()[[1]],
        options = list(
          pageLength = 10,
          columnDefs = list(list(
            className = 'dt-center', targets = 5
          )),
          lengthMenu = c(5, 10, 15, 20),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        )
      )
    })
    update_function1(1)
    update_function1(2)
    update_function1(3)
    }
    else if (length(csvs()) == 2){
      titleUpdate()
      output$contents2 <- renderDT({
        datatable(
          csvs()[[2]],
          options = list(
            pageLength = 10,
            columnDefs = list(list(
              className = 'dt-center', targets = 5
            )),
            lengthMenu = c(5, 10, 15, 20),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"
            )
          )
        )
      })
      output$contents <- renderDT({
        datatable(
          csvs()[[1]],
          options = list(
            pageLength = 10,
            columnDefs = list(list(
              className = 'dt-center', targets = 5
            )),
            lengthMenu = c(5, 10, 15, 20),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"
            )
          )
        )
      })
      update_function1(1)
      update_function1(2)
      }else if(length(csvs())==1){
        titleUpdate()
        output$contents <- renderDT({
          datatable(
            csvs()[[1]],
            options = list(
              pageLength = 10,
              columnDefs = list(list(
                className = 'dt-center', targets = 5
              )),
              lengthMenu = c(5, 10, 15, 20),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                "}"
              )
            )
          )
        })
        update_function1(1)
      }
  })
  
  observeEvent(input$reset, {
    output$contents <- NULL
    output$contents2 <- NULL
    output$contents3 <- NULL
    output$title1 <- NULL
    output$title2 <- NULL
    output$title3 <- NULL
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
  
  
}
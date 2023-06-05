library(shiny)
library(DT)
library(data.table)
require(ggforce)
require(scales)

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
    titleUpdate3 <- reactive({
      output$title1 <- renderText(names(csvs())[1])
      output$title2 <- renderText(names(csvs())[2])
      output$title3 <- renderText(names(csvs())[3])
      output$title4 <- renderText(names(csvs())[1])
      output$title5 <- renderText(names(csvs())[2])
      output$title6 <- renderText(names(csvs())[3])
    })
    titleUpdate2 <- reactive({
      output$title1 <- renderText(names(csvs())[1])
      output$title2 <- renderText(names(csvs())[2])
      output$title4 <- renderText(names(csvs())[1])
      output$title5 <- renderText(names(csvs())[2])
    })
    titleUpdate1 <- reactive({
      output$title1 <- renderText(names(csvs())[1])
      output$title4 <- renderText(names(csvs())[1])
    })
    
    #function to update filters of table
    update_function1 <- function(number) {
      #Update select input
      updateSelectInput(
        session,
        inputId = paste0('colorInput',number),
        label = 'Select the color',
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
    
    #Update first table
    datafile1 <- reactive({
      filtered <- csvs()[[1]]
      if (input$colorInput1 != "") {
         filtered <- subset(csvs()[[1]], Color %in% input$colorInput1)
       }
      if (input$cpSelect1 != "") {
         filtered <- subset(csvs()[[1]], Cp > input$cpSelect1)
       }
      if (!is.null(input$NameSelect1)) {
         filtered <- subset(csvs()[[1]], Name %in% input$NameSelect1)
       }
      if (!is.null(input$PosSelect1)) {
         filtered <- subset(csvs()[[1]], Pos %in% input$PosSelect1)
       }
      filtered
    })
    
    datafile2 <- reactive({
      filtered <- csvs()[[2]]
      if (input$colorInput2 != "") {
        filtered <- subset(csvs()[[2]], Color %in% input$colorInput2)
      }
      if (input$cpSelect2 != "") {
        filtered <- subset(csvs()[[2]], Cp > input$cpSelect2)
      }
      if (!is.null(input$NameSelect2)) {
        filtered <- subset(csvs()[[2]], Name %in% input$NameSelect2)
      }
      if (!is.null(input$PosSelect2)) {
        filtered <- subset(csvs()[[2]], Pos %in% input$PosSelect2)
      }
      filtered
    })
    
    datafile3 <- reactive({
      filtered <- csvs()[[3]]
      if (input$colorInput3 != "") {
        filtered <- subset(csvs()[[3]], Color %in% input$colorInput3)
      }
      if (input$cpSelect3 != "") {
        filtered <- subset(csvs()[[3]], Cp > input$cpSelect3)
      }
      if (!is.null(input$NameSelect3)) {
        filtered <- subset(csvs()[[3]], Name %in% input$NameSelect3)
      }
      if (!is.null(input$PosSelect3)) {
        filtered <- subset(csvs()[[3]], Pos %in% input$PosSelect3)
      }
      filtered
    })
    
    if (length(csvs()) > 3) {
      shinyalert("Warning!", "You can't upload more than 3 files.", type = "warning")
    } else if (length(csvs()) == 3){
      titleUpdate3()
    output$contents2 <- renderDT({
      datatable(
        datafile2(),
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
        datafile3(),
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
        datafile1(),
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
      titleUpdate2()
      output$contents2 <- renderDT({
        datatable(
          datafile2(),
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
          datafile1(),
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
        titleUpdate1()
        output$contents <- renderDT({
          datatable(
            datafile1(),
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
    ###############################
    #TRANSFORM INPUT FILES TO A MATRIX (for Cp and log10(Conc) values)
    #and convert that to data frames:
    v2plot <- reactive({
      input$var2plot
    })
    
    v3plot <- reactive({
      input$var3plot
    })
    v4plot <- reactive({
      input$var4plot
    })
    
    
    df1 <-  function(number){
      if (number==1){
        variable=v2plot()
      }else if(number=="2"){
        variable=v3plot()
      }else if(number==3){
        variable=v4plot()
      }
      
      if (variable=="Cp"){
        as.data.frame(t(matrix(csvs()[[number]]$Cp, nrow = 12, ncol = 8)))
      }else{
        as.data.frame(t(matrix(log10(csvs()[[number]]$Concentration), nrow = 12, ncol = 8)))
      }
    }
    
    #TRANSFORM INPUT DATA TO LONG DATA FORMAT
    #Adding row and column numbers and overall well name. 
    #Note that rows are represent by letters in microtiter plate, but lets keep them as integers. 
    #Continous scale will help later on with scaling.
    require(tidyverse)
    ldf1 <- function(number){
      df1(number) %>% 
        mutate(row = 1:8) %>% 
        pivot_longer(-row, names_to = "col", values_to = "value") %>%
        mutate(col = as.integer(str_remove(col, "V"))) %>%
        mutate(well = paste0(LETTERS[row], col))
    }
    
    output$plot1 <- renderPlot({
      # check input data and rise an error message if none
      validate(
        need(csvs()[[1]], "Please, upload a data set with data 1")
      )
      ggplot(data = ldf1(1)) + 
        geom_circle(aes(x0 = col, y0 = row, r = 0.45, fill = value)) +
        coord_equal() +
        scale_x_continuous(breaks = 1:12, expand = expansion(mult = c(0.01, 0.01))) +
        scale_y_continuous(breaks = 1:8, labels = LETTERS[1:8], expand = expansion(mult = c(0.01, 0.01)), trans = reverse_trans()) +
        scale_fill_gradient(low = input$colNum2, high = input$colNum1) + 
        labs(title = "96 Well plate of TRECs", subtitle = "Cp or log10(Conc) values", x = "Col", y = "Row") + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none")
    })
    
    output$plot2 <- renderPlot({
      # check input data and rise an error message if none
      validate(
        need(csvs()[[2]], "Please, upload a data set with data 1")
      )
      ggplot(data = ldf1(2)) + 
        geom_circle(aes(x0 = col, y0 = row, r = 0.45, fill = value)) +
        coord_equal() +
        scale_x_continuous(breaks = 1:12, expand = expansion(mult = c(0.01, 0.01))) +
        scale_y_continuous(breaks = 1:8, labels = LETTERS[1:8], expand = expansion(mult = c(0.01, 0.01)), trans = reverse_trans()) +
        scale_fill_gradient(low = input$colNum4, high = input$colNum3) + 
        labs(title = "96 Well plate of TRECs", subtitle = "Cp or log10(Conc) values", x = "Col", y = "Row") + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none")
    })
    
    output$plot3 <- renderPlot({
      # check input data and rise an error message if none
      validate(
        need(csvs()[[3]], "Please, upload a data set with data 1")
      )
      ggplot(data = ldf1(3)) + 
        geom_circle(aes(x0 = col, y0 = row, r = 0.45, fill = value)) +
        coord_equal() +
        scale_x_continuous(breaks = 1:12, expand = expansion(mult = c(0.01, 0.01))) +
        scale_y_continuous(breaks = 1:8, labels = LETTERS[1:8], expand = expansion(mult = c(0.01, 0.01)), trans = reverse_trans()) +
        scale_fill_gradient(low = input$colNum6, high = input$colNum5) + 
        labs(title = "96 Well plate of TRECs", subtitle = "Cp or log10(Conc) values", x = "Col", y = "Row") + 
        theme_bw() + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none")
    })
    
  })
  
  observeEvent(input$reset, {
    output$contents <- NULL
    output$contents2 <- NULL
    output$contents3 <- NULL
    output$title1 <- NULL
    output$title2 <- NULL
    output$title3 <- NULL
    output$title4 <- NULL
    output$title5 <- NULL
    output$title6 <- NULL
    output$plot1 <- NULL
    output$plot2 <- NULL
    output$plot3 <- NULL
  })

  
  
}
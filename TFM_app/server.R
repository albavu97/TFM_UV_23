library(shiny)
library(DT)
library(data.table)
require(ggforce)
require(scales)

server <- function(input, output, session) {
  # initialize reactiveValues object that will contain the plots/data that we will
  # pass to the R Markdown document
  my_vals <- reactiveValues()
  
  
  output$menuitem <- renderMenu({
    menuItem("Menu item", icon = icon("calendar"))
  })
  
  #Elegir el directorio donde queremos buscar
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  observeEvent(input$fileId, {
    fileId <- reactive(toString(input$fileId))
    output$idOut <- renderText({
      fileId()
    })
    global$file <- fileId()
  })
  
  global <- reactiveValues(datapath = getwd())
  
  dir <- reactive(input$dir)
  
  
  render1 <- reactive({
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
  })
  render2 <- reactive({
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
  })
  
  render3 <- reactive({
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
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$type_plot
               },
               handlerExpr = {
                 if (input$type_plot == "boxplot") {
                   plot_big()
                 } else{
                   plot_bigotes()
                 }
               })
  
  
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir()))
                   return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 global$lista <-
                   list.files(global$datapath,
                              pattern = global$file,
                              recursive = TRUE)
                 my_vals$lista <- global$lista
                 if (length(global$lista) == 1) {
                   shinyalert("All files upload!", global$lista, type = "info")
                   titleUpdate1()
                   render1()
                   update_function1(1)
                   # Values for markdown template
                   my_vals$dat_table <- csvs(1)
                   my_vals$boxplot <- ldf1(1)
                   my_vals$cp1 <- csvs(1)$Cp
                  
                 } else if (length(global$lista) == 2) {
                   shinyalert("All files upload!", global$lista, type = "info")
                   titleUpdate2()
                   render1()
                   render2()
                   update_function1(1)
                   update_function1(2)
                   # Values for markdown template
                   my_vals$dat_table <- csvs(1)
                   my_vals$boxplot <- ldf1(1)
                   my_vals$dat_table2 <- csvs(2)
                   my_vals$boxplot2 <- ldf1(2)
                   my_vals$cp1 <- csvs(1)$Cp
                   my_vals$cp2 <- csvs(2)$Cp
                
                   # to pass to markdown incase we need the full dataset
                 } else if (length(global$lista) == 3) {
                   shinyalert("All files upload!", global$lista, type = "info")
                   titleUpdate3()
                   render1()
                   render2()
                   render3()
                   update_function1(1)
                   update_function1(2)
                   update_function1(3)
                   # to pass to markdown incase we need the full dataset
                   # Values for markdown template
                   my_vals$dat_table <- csvs(1)
                   my_vals$boxplot <- ldf1(1)
                   my_vals$dat_table2 <- csvs(2)
                   my_vals$boxplot2 <- ldf1(2)
                   my_vals$dat_table3 <- csvs(3)
                   my_vals$boxplot3 <- ldf1(3)
                   #Boxplot de los 3 archivos
                   my_vals$cp1 <- csvs(1)$Cp
                   my_vals$cp2 <- csvs(2)$Cp
                   my_vals$cp3 <- csvs(3)$Cp
                  
                 }
                 else if (length(global$lista) == 0) {
                   shinyalert("Warning!", "Id doesn't exit.Check it.", type = "warning")
                 }
               })
  
  csvs <- function(number) {
    tmp <-
      read.csv2(
        paste0(global$datapath, '/', global$lista[number]),
        sep = '\t',
        dec = ',',
        header = TRUE,
        skip = 1
      )
    # fread(
    #   paste0(global$datapath, '/', global$lista[number]),
    #   header = TRUE,
    #   sep = "\t",
    #   data.table = FALSE
    # )
    #Quitamos la columna Name para que no haya problemas
    tmp2 <- tmp[,!(names(tmp) %in% "Name")]
    tmp2[, c(4, 5)] <- apply(tmp2[, c(4, 5)], 2, function(x) {
      gsub(",", ".", x)
    })
    tmp2
  }
  
  titleUpdate3 <- reactive({
    output$title1 <- renderText(global$lista[1])
    output$title2 <- renderText(global$lista[2])
    output$title3 <- renderText(global$lista[3])
    output$title4 <- renderText(global$lista[1])
    output$title5 <- renderText(global$lista[2])
    output$title6 <- renderText(global$lista[3])
  })
  titleUpdate2 <- reactive({
    output$title1 <- renderText(global$lista[1])
    output$title2 <- renderText(global$lista[2])
    output$title4 <- renderText(global$lista[1])
    output$title5 <- renderText(global$lista[2])
  })
  
  titleUpdate1 <- reactive({
    output$title1 <- renderText(global$lista[1])
    output$title4 <- renderText(global$lista[1])
  })
  
  #function to update filters of table
  update_function1 <- function(number) {
    #Update select input
    updateSelectInput(
      session,
      inputId = paste0('colorInput', number),
      label = 'Select the color',
      choices  = c("", csvs(number)$Color),
      selected = NULL
    )
    updateSelectizeInput(
      session,
      inputId = paste0('PosSelect', number),
      label = 'Select the position',
      choices  = csvs(number)$Pos,
      selected = NULL
    )
    updateSelectInput(
      session,
      inputId = paste0('cpSelect', number),
      label = 'Select the cp',
      choices  = c("", csvs(number)$Cp),
      selected = NULL
    )
    updateSelectizeInput(
      session,
      inputId = paste0('DF_select'),
      label = 'Select wich dataframe you want (1 or more):',
      choices  = global$lista,
      selected = NULL
    )
  }
  
  datafile1 <- reactive({
    filtered <- csvs(1)
    if (input$colorInput1 != "") {
      filtered <- subset(csvs(1), Color %in% input$colorInput1)
    }
    if (input$cpSelect1 != "") {
      filtered <- subset(csvs(1), Cp > input$cpSelect1)
    }
    if (!is.null(input$PosSelect1)) {
      filtered <- subset(csvs(1), Pos %in% input$PosSelect1)
    }
    filtered
  })
  
  datafile2 <- reactive({
    filtered <- csvs(2)
    if (input$colorInput2 != "") {
      filtered <- subset(csvs(2), Color %in% input$colorInput2)
    }
    if (input$cpSelect2 != "") {
      filtered <- subset(csvs(2), Cp > input$cpSelect2)
    }
    if (!is.null(input$PosSelect2)) {
      filtered <- subset(csvs(2), Pos %in% input$PosSelect2)
    }
    filtered
  })
  
  datafile3 <- reactive({
    filtered <- csvs(3)
    if (input$colorInput3 != "") {
      filtered <- subset(csvs(3), Color %in% input$colorInput3)
    }
    if (input$cpSelect3 != "") {
      filtered <- subset(csvs(3), Cp > input$cpSelect3)
    }
    if (!is.null(input$PosSelect3)) {
      filtered <- subset(csvs(3), Pos %in% input$PosSelect3)
    }
    filtered
  })
  
  observeEvent(input$reset2, {
    update_function1(1)
    datafile1()
  })
  
  observeEvent(input$reset3, {
    update_function1(2)
    datafile2()
  })
  
  observeEvent(input$reset4, {
    update_function1(3)
    datafile3()
  })
  
  
  
  #   ###############################
  #   #TRANSFORM INPUT FILES TO A MATRIX (for Cp and log10(Conc) values)
  #   #and convert that to data frames:
  df_inicial <- reactive({
    df_inicial <-
      data.frame(
        c(
          "A1",
          "A2",
          "A3",
          "A4",
          "A5",
          "A6",
          "A7",
          "A8",
          "A9",
          "A10",
          "A11",
          "A12",
          "B1",
          "B2",
          "B3",
          "B4",
          "B5",
          "B6",
          "B7",
          "B8",
          "B9",
          "B10",
          "B11",
          "B12",
          "C1",
          "C2",
          "C3",
          "C4",
          "C5",
          "C6",
          "C7",
          "C8",
          "C9",
          "C10",
          "C11",
          "C122",
          "D1",
          "D2",
          "D3",
          "D4",
          "D5",
          "D6",
          "D7",
          "D8",
          "D9",
          "D10",
          "D11",
          "D12",
          "E1",
          "E2",
          "E3",
          "E4",
          "E5",
          "E6",
          "E7",
          "E8",
          "E9",
          "E10",
          "E11",
          "E12",
          "F1",
          "F2",
          "F3",
          "F4",
          "F5",
          "F6",
          "F7",
          "F8",
          "F9",
          "F10",
          "F11",
          "F12",
          "G1",
          "G2",
          "G3",
          "G4",
          "G5",
          "G6",
          "G7",
          "G8",
          "G9",
          "G10",
          "G11",
          "G12",
          "H1",
          "H2",
          "H3",
          "H4",
          "H5",
          "H6",
          "H7",
          "H8",
          "H9",
          "H10",
          "H11",
          "H12"
        ),
        c(rep(100, 12 * 8))
      )
    names(df_inicial) <- c("col1", "col2")
    df_inicial
  })
  
  gen_dataframe <- function(number) {
    df1 <- df_inicial()
    df2 <- csvs(number)
    selected_columns <- df2[c("Pos", "Cp")]
    merged_df <-
      merge(
        df1,
        selected_columns,
        by.x = "col1",
        by.y = "Pos",
        all.x = TRUE
      )
    merged_df$Cp[is.na(merged_df$Cp)] <- 0
    merged_df <- merged_df[c("col1", "Cp")]
    specific_order = c(
      "A1",
      "A2",
      "A3",
      "A4",
      "A5",
      "A6",
      "A7",
      "A8",
      "A9",
      "A10",
      "A11",
      "A12",
      "B1",
      "B2",
      "B3",
      "B4",
      "B5",
      "B6",
      "B7",
      "B8",
      "B9",
      "B10",
      "B11",
      "B12",
      "C1",
      "C2",
      "C3",
      "C4",
      "C5",
      "C6",
      "C7",
      "C8",
      "C9",
      "C10",
      "C11",
      "C12",
      "D1",
      "D2",
      "D3",
      "D4",
      "D5",
      "D6",
      "D7",
      "D8",
      "D9",
      "D10",
      "D11",
      "D12",
      "E1",
      "E2",
      "E3",
      "E4",
      "E5",
      "E6",
      "E7",
      "E8",
      "E9",
      "E10",
      "E11",
      "E12",
      "F1",
      "F2",
      "F3",
      "F4",
      "F5",
      "F6",
      "F7",
      "F8",
      "F9",
      "F10",
      "F11",
      "F12",
      "G1",
      "G2",
      "G3",
      "G4",
      "G5",
      "G6",
      "G7",
      "G8",
      "G9",
      "G10",
      "G11",
      "G12",
      "H1",
      "H2",
      "H3",
      "H4",
      "H5",
      "H6",
      "H7",
      "H8",
      "H9",
      "H10",
      "H11",
      "H12"
    )
    
    merged_df <-
      merged_df[order(factor(merged_df$col1, levels = specific_order)),]
    #merged_df$Cp = as.numeric(levels(merged_df$Cp))[merged_df$Cp]
    merged_df
  }
  
  gen_dataframe2 <- function(number) {
    df1 <- df_inicial()
    df2 <- csvs(number)
    selected_columns <- df2[c("Pos", "Concentration")]
    merged_df <-
      merge(
        df1,
        selected_columns,
        by.x = "col1",
        by.y = "Pos",
        all.x = TRUE
      )
    merged_df$Cp[is.na(merged_df$Concentration)] <- 100
    merged_df <- merged_df[c("col1", "Concentration")]
    specific_order = c(
      "A1",
      "A2",
      "A3",
      "A4",
      "A5",
      "A6",
      "A7",
      "A8",
      "A9",
      "A10",
      "A11",
      "A12",
      "B1",
      "B2",
      "B3",
      "B4",
      "B5",
      "B6",
      "B7",
      "B8",
      "B9",
      "B10",
      "B11",
      "B12",
      "C1",
      "C2",
      "C3",
      "C4",
      "C5",
      "C6",
      "C7",
      "C8",
      "C9",
      "C10",
      "C11",
      "C122",
      "D1",
      "D2",
      "D3",
      "D4",
      "D5",
      "D6",
      "D7",
      "D8",
      "D9",
      "D10",
      "D11",
      "D12",
      "E1",
      "E2",
      "E3",
      "E4",
      "E5",
      "E6",
      "E7",
      "E8",
      "E9",
      "E10",
      "E11",
      "E12",
      "F1",
      "F2",
      "F3",
      "F4",
      "F5",
      "F6",
      "F7",
      "F8",
      "F9",
      "F10",
      "F11",
      "F12",
      "G1",
      "G2",
      "G3",
      "G4",
      "G5",
      "G6",
      "G7",
      "G8",
      "G9",
      "G10",
      "G11",
      "G12",
      "H1",
      "H2",
      "H3",
      "H4",
      "H5",
      "H6",
      "H7",
      "H8",
      "H9",
      "H10",
      "H11",
      "H12"
    )
    
    merged_df <-
      merged_df[order(factor(merged_df$col1, levels = specific_order)),]
    options(scipen = 999)
    merged_df$Concentration <- as.numeric(merged_df$Concentration)
    merged_df
  }
  
  v2plot <- reactive({
    input$var2plot
  })
  
  v3plot <- reactive({
    input$var3plot
  })
  v4plot <- reactive({
    input$var4plot
  })
  
  
  df1 <-  function(number) {
    if (number == 1) {
      variable = v2plot()
    } else if (number == "2") {
      variable = v3plot()
    } else if (number == 3) {
      variable = v4plot()
    }
    
    if (variable == "Cp") {
      as.data.frame(t(matrix(
        gen_dataframe(number)$Cp,
        nrow = 12,
        ncol = 8
      )))
    } else{
      as.data.frame(t(matrix(
        log10(gen_dataframe2(number)$Concentration),
        nrow = 12,
        ncol = 8
      )))
    }
  }
  
  #TRANSFORM INPUT DATA TO LONG DATA FORMAT
  #Adding row and column numbers and overall well name.
  #Note that rows are represent by letters in microtiter plate, but lets keep them as integers.
  #Continous scale will help later on with scaling.
  require(tidyverse)
  
  ldf1 <- function(number) {
    df1(number) %>%
      mutate(row = 1:8) %>%
      pivot_longer(-row, names_to = "col", values_to = "value") %>%
      mutate(col = as.integer(str_remove(col, "V"))) %>%
      mutate(well = paste0(LETTERS[row], col))
  }
  
  output$plot1 <- renderPlot({
    # check input data and rise an error message if none
    #validate(need(csvs(1), "Please, upload a data set with data 1"))
    ggplot(data = ldf1(1)) +
      geom_circle(aes(
        x0 = col,
        y0 = row,
        r = 0.5,
        fill = as.numeric(value)
      )) +
      coord_equal() +
      scale_x_continuous(breaks = 1:12, expand = expansion(mult = c(0.01, 0.01))) +
      scale_y_continuous(
        breaks = 1:8,
        labels = LETTERS[1:8],
        expand = expansion(mult = c(0.01, 0.01)),
        trans = reverse_trans()
      ) +
      scale_fill_gradientn(colours = terrain.colors(40)) +
      labs(
        title = "96 Well plate of TRECs",
        subtitle = "Cp or log10(Conc) values",
        x = "Col",
        y = "Row"
      ) +
      theme_bw()
  })
  
  output$plot2 <- renderPlot({
    # check input data and rise an error message if none
    validate(need(csvs(2), "Please, upload a data set with data 1"))
    ggplot(data = ldf1(2)) +
      geom_circle(aes(
        x0 = col,
        y0 = row,
        r = 0.5,
        fill = as.numeric(value)
      )) +
      coord_equal() +
      scale_x_continuous(breaks = 1:12, expand = expansion(mult = c(0.01, 0.01))) +
      scale_y_continuous(
        breaks = 1:8,
        labels = LETTERS[1:8],
        expand = expansion(mult = c(0.01, 0.01)),
        trans = reverse_trans()
      ) +
      scale_fill_gradientn(colours = heat.colors(40)) +
      labs(
        title = "96 Well plate of TRECs",
        subtitle = "Cp or log10(Conc) values",
        x = "Col",
        y = "Row"
      ) +
      theme_bw()
  })
  
  output$plot3 <- renderPlot({
    # check input data and rise an error message if none
    validate(need(csvs(3), "Please, upload a data set with data 1"))
    ggplot(data = ldf1(3)) +
      geom_circle(aes(
        x0 = col,
        y0 = row,
        r = 0.5,
        fill = as.numeric(value)
      )) +
      coord_equal() +
      scale_x_continuous(breaks = 1:12, expand = expansion(mult = c(0.01, 0.01))) +
      scale_y_continuous(
        breaks = 1:8,
        labels = LETTERS[1:8],
        expand = expansion(mult = c(0.01, 0.01)),
        trans = reverse_trans()
      ) +
      scale_fill_gradientn(colours = topo.colors(40)) +
      labs(
        title = "96 Well plate of TRECs",
        subtitle = "Cp or log10(Conc) values",
        x = "Col",
        y = "Row"
      ) +
      theme_bw()
  })
  
  plot_bigotes <- reactive({
    if (length(global$lista) == 3) {
      output$graph_bigotes <- renderPlotly({
        fig <- plot_ly(type = 'violin')
        fig <- fig %>%
          add_trace(
            y = ~ csvs(1)$Cp,
            
            legendgroup = 'M',
            scalegroup = 'M',
            name = global$lista[1],
            box = list(visible = T),
            meanline = list(visible = T),
            color = I(input$col1)
          )
        fig <- fig %>%
          add_trace(
            y = ~ csvs(2)$Cp,
            
            legendgroup = 'F',
            scalegroup = 'F',
            name = global$lista[2],
            box = list(visible = T),
            meanline = list(visible = T),
            color = I(input$col2)
          )
        
        fig <- fig %>%
          add_trace(
            y = ~ csvs(3)$Cp,
            
            legendgroup = 'G',
            scalegroup = 'G',
            name = global$lista[3],
            box = list(visible = T),
            meanline = list(visible = T),
            color = I(input$col3)
          )
        
        fig <- fig %>%
          layout(yaxis = list(zeroline = F),
                 violinmode = 'group')
        
      })
    } else if (length(global$lista) == 2) {
      output$graph_bigotes <- renderPlotly({
        fig <- plot_ly(type = 'violin')
        fig <- fig %>%
          add_trace(
            y = ~ csvs(1)$Cp,
            
            legendgroup = 'M',
            scalegroup = 'M',
            name = global$lista[1],
            box = list(visible = T),
            meanline = list(visible = T),
            color = I(input$col1)
          )
        fig <- fig %>%
          add_trace(
            y = ~ csvs(2)$Cp,
            
            legendgroup = 'F',
            scalegroup = 'F',
            name = global$lista[2],
            box = list(visible = T),
            meanline = list(visible = T),
            color = I(input$col2)
          )
        fig <- fig %>%
          layout(yaxis = list(zeroline = F),
                 violinmode = 'group')
        
      })
    } else if (length(global$lista) == 1) {
      output$graph_bigotes <- renderPlotly({
        fig <- plot_ly(type = 'violin')
        fig <- fig %>%
          add_trace(
            y = ~ csvs(1)$Cp,
            
            legendgroup = 'M',
            scalegroup = 'M',
            name = global$lista[1],
            box = list(visible = T),
            meanline = list(visible = T),
            color = I(input$col1)
          )
        
        fig <- fig %>%
          layout(yaxis = list(zeroline = F),
                 violinmode = 'group')
        
      })
    }
  })
  
  plot_big <- reactive({
    if (length(global$lista) == 3) {
      output$graph <- renderPlotly({
        fig <- plot_ly(type = "box")
        fig <-
          fig %>% add_boxplot(
            y = csvs(1)$Cp,
            jitter = 0.3,
            pointpos = -1.8,
            boxpoints = 'all',
            marker = list(color = input$line1),
            line = list(color = input$line1),
            fillcolor = input$col1,
            name = global$lista[1]
          )
        fig <-
          fig %>% add_boxplot(
            y = csvs(2)$Cp,
            name = global$lista[2],
            boxpoints = FALSE,
            marker = list(color = input$line2),
            line = list(color = input$line2),
            fillcolor = input$col2,
          )
        fig <-
          fig %>% add_boxplot(
            y = csvs(3)$Cp,
            name = global$lista[3],
            boxpoints = 'suspectedoutliers',
            marker = list(
              color = 'rgb(8,81,156)',
              outliercolor = 'rgba(219, 64, 82, 0.6)',
              line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                          outlierwidth = 2)
            ),
            line = list(color = input$line3),
            fillcolor = input$col3,
          )
        fig <- fig %>% layout(title = "Box Plot Styling Outliers")
        
      })
    } else if (length(global$lista) == 2) {
      output$graph <- renderPlotly({
        fig <- plot_ly(type = "box")
        fig <-
          fig %>% add_boxplot(
            y = csvs(1)$Cp,
            jitter = 0.3,
            pointpos = -1.8,
            boxpoints = 'all',
            marker = list(color = input$line1),
            line = list(color = input$line1),
            fillcolor = input$col1,
            name = global$lista[1]
          )
        fig <-
          fig %>% add_boxplot(
            y = csvs(2)$Cp,
            name = global$lista[2],
            boxpoints = FALSE,
            marker = list(color = input$line2),
            line = list(color = input$line2),
            fillcolor = input$col2,
          )
        fig <- fig %>% layout(title = "Box Plot Styling Outliers")
        
      })
    } else if (length(global$lista) == 1) {
      output$graph <- renderPlotly({
        fig <- plot_ly(type = "box")
        fig <-
          fig %>% add_boxplot(
            y = csvs(1)$Cp,
            jitter = 0.3,
            pointpos = -1.8,
            boxpoints = 'all',
            marker = list(color = input$line1),
            line = list(color = input$line1),
            fillcolor = input$col1,
            name = global$lista[1]
          )
        fig <- fig %>% layout(title = "Box Plot Styling Outliers")
        
      })
    }
  })
  
  ### TABLA Y GRAFICO DE TODOS LOS ARCHIVOS ###
  #Elegir el directorio donde queremos buscar
  shinyDirChoose(
    input,
    'dir2',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  
  global2 <- reactiveValues(datapath = getwd())
  
  dir2 <- reactive(input$dir2)
  
  type_file <- reactive({
    input$type_file
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir2
               },
               handlerExpr = {
                 if (!"path" %in% names(dir2()))
                   return()
                 home <- normalizePath("~")
                 global2$datapath <-
                   file.path(home, paste(unlist(dir2()$path[-1]), collapse = .Platform$file.sep))
               })
  
  render_big_plot <- reactive({
    mypath = global2$datapath
    setwd(mypath)
    global2$lista <- list.files(
      global2$datapath,
      pattern = paste0("*", type_file(), "*"),
      recursive = TRUE
    )
    files = read.csv2(
      global2$lista[1],
      sep = '\t',
      dec = ',',
      header = TRUE,
      skip = 1
    )
    files <- cbind(files, file = rep(global2$lista[1]), nrow(files))
    contador <- 2
    for (txt_file in global2$lista[2:length(global2$lista)]) {
      tmp <-
        read.csv2(
          txt_file,
          sep = '\t',
          dec = ',',
          header = TRUE,
          skip = 1
        )
      tmp <-
        cbind(tmp, file = rep(global2$lista[contador]), nrow(tmp))
      common <- intersect(colnames(files), colnames(tmp))
      files <- rbind(files[common], tmp[common])
      files["Name"] <- NULL
      contador <- contador + 1
    }
    files
  })
  
  table_summary <- reactive({
    data <- render_big_plot()
    data$Cp <- as.numeric(as.character(data$Cp))
    data_summary <- data.frame(unclass(summary(data$Cp)))
    data_summary$names <- c("Min","1st Qu","Median","Mean","3rd Qu","Max","NA's")
    colnames(data_summary) <- c("value","statistic")
    my_data2 <- data_summary[, c(2, 1)]
    my_data2
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$display
               },
               handlerExpr = {
                 output$plot_big <- renderDT({
                   datatable(
                     render_big_plot(),
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
                 output$graph2 <- renderPlotly({
                   fig <- plot_ly(type = "box")
                   fig <-
                     fig %>% add_boxplot(
                       y = render_big_plot()$Cp,
                       name = "Suspected Outlier",
                       boxpoints = 'suspectedoutliers',
                       marker = list(
                         color = 'rgb(8,81,156)',
                         outliercolor = 'rgba(219, 64, 82, 0.6)',
                         line = list(outliercolor = 'rgba(219, 64, 82, 1.0)',
                                     outlierwidth = 2)
                       ),
                       line = list(color = 'rgb(8,81,156)'),
                       fillcolor = input$colSummary
                     )
                   fig <- fig %>% layout(title = type_file())
                 })
                 output$number_txt <- renderText({
                   global2$lista
                 })
                 output$summary <- renderTable({
                   table_summary()
                 })
               })
  
  
  output$report_gen <- downloadHandler(
    filename = "sample_report.pdf",
    content = function(file) {
      my_vals$file1 <- input$file1_input
      my_vals$file2 <- input$file2_input
      my_vals$file3 <- input$file3_input
      #Values to plot 96-well
      my_vals$plot1 <- input$plot1_input
      my_vals$plot2 <- input$plot2_input
      my_vals$plot3 <- input$plot3_input
      #Get comments
      my_vals$comment <- input$comment
      my_vals$autor <- input$autor
      my_vals$title <- input$title_doc
      
      my_vals$comment_data1 <- input$comment_data1
      my_vals$comment_data2 <- input$comment_data2
      my_vals$comment_data3 <- input$comment_data3
      # copy markdown report file to a temporary directory before knitting it with the
      # selected dataset. This is useful if we don't have write permissions for the current
      # working directory
      temp_report <-
        file.path("/Users/valleja3/Desktop/TFM_UV_23/TFM_app/template.Rmd")
      message("\n... temp_report path: ", temp_report, "\n")
      
      # copy the report template into the temp directory
      file.copy(
        here::here("shiny_report_gen", "report_template.Rmd"),
        temp_report,
        overwrite = TRUE
      )
      
      # create a named list of parameters to pass to to Rmd template.
      # can also pass reactiveValues or reactive objects
      pass_params <- list(imported = my_vals)
      
      # knit the document, passing in the `pass_params` list, and evaluate it in a
      # child of the global environment (this isolates the code in the document
      # from the code in the app).
      rmarkdown::render(
        temp_report,
        output_file = file,
        params = pass_params,
        envir = new.env(parent = globalenv())
      )
      
    })
  
  observeEvent(input$reset11, {
    output$number_txt <- NULL
    output$plot_big <- NULL
    output$graph2 <- NULL
    output$summary <- NULL
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
    output$graph <- NULL
  })
  
  
  
}
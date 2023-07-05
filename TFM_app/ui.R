library(shiny)
library(devtools)
library(shinydashboard)
library(tablerDash)
library(shinythemes)
library(shinyBS)
library(htmltools)
library(DT)
library(shinyalert)
library(dipsaus)
library(shinyjs)
library(colourpicker)
library(shinyFiles)
library(plotly)

# ----------------------------------
# ui.R
# Description: File with all elements for UI
# ----------------------------------

# ----------------------------------
# dashboardPage
# Descrption: it is the main function of all application
# ----------------------------------

my_height = "100px"

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = tags$img(
    src = "roche_logo.svg",
    height = 40,
    width = 90
  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",
               tabName = "home",
               icon = icon("house")),
      menuItem("Dashboard",
               tabName = "dashboard",
               icon = icon("table")),
      menuItem("Well plate",
               tabName = "cell",
               icon = icon("flask")),
      menuItem(
        "Distribution plots",
        tabName = "plot",
        icon = icon("chart-simple")
      ),
      menuItem("Report",
               tabName = "report",
               icon = icon("download")),
      menuItem(
        "Biomarker summary",
        tabName = "plot_all",
        icon = icon("hand")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(column(
          12, div(style = "height:200px;background-color: transparent;")
        )),
        div(
          style = "margin-rigth: 30%;margin-left: 30%;width:40%;height:100px;
                background-color: transparent;text-align:center",
          box(
            img(
              src = 'neocribsmart.jpg',
              align = "center",
              style = paste0("max-width: 100%; height: ", my_height, ";")
            ),
            div(style = "height:10px;background-color: transparent;"),
            div(style = "height:10px;background-color: transparent;"),
            div(style = "height:40px;background-color: transparent;text-align:center",
                
                textInput("fileId", "Input File ID", width = "100%")),
            div(style = "height:40px;background-color: transparent;"),
            div(
              style = "height:40px;background-color: transparent;text-align:center",
              tags$style(
                ".btn-default{
  background-color: #2CA666;
    color: white;
    border-color: black;
    font-weight: bold;
}"
              ),
              shinyDirButton(
                "dir",
                "Input directory",
                "Upload",
                icon = icon("folder"),
                viewtype = "detail"
              )
            ),
            div(style = "height:20px;background-color: transparent;"),
            div(
              style = "height:40px;background-color: transparent;text-align:center",
              actionButtonStyled(
                "reset",
                "Reset input",
                icon = icon("house"),
                width = "110px",
                type = "info"
              )
            ),
            div(style = "height:10px;background-color: transparent;"),
            width = 12,
            solidHeader = TRUE
          )
          
        )
      )
      
      ,
      tabItem(tabName = "dashboard",
              fluidRow(
                tags$style(
                  "
             .btn-file {
             background-color:black;
             border-color: white;
             color:white;
             display: inline-block;
             font-size: 16px;
             font-weight: bold;
             vertical-align: middle;
             }

             .progress-bar {
             visibility: hidden;
             background-color:white;
             width: 100px;
             color:black;
             }

             "
                ),
                tabBox(
                  width = 12,
                  tabPanel(
                    title = textOutput("title1"),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "blue",
                      status = "primary",
                      selectInput('cpSelect1', '', "")
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "orange",
                      status = "warning",
                      selectInput('colorInput1', '', "")
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "green",
                      status = "info",
                      selectizeInput("PosSelect1",
                                     "",
                                     "",
                                     selected = NULL,
                                     multiple = TRUE)
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      div(
                        style = "height:40px;background-color: transparent;text-align:center",
                        actionButtonStyled(
                          "reset2",
                          "Reset input",
                          icon = icon("house"),
                          width = "110px",
                          type = "info"
                        )
                      ),
                    ),
                    style = 'width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;',
                    DTOutput('contents', width = "98%", height = "98%"),
                    div(style = "height:20px;background-color: transparent;"),
                    textAreaInput(
                      "comment_data1",
                      "Comments to include in report",
                      "Comments",
                      width = "1000px",
                      height = "100px"
                    )
                  ),
                  tabPanel(
                    title = textOutput("title2"),
                    style = 'width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;',
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "blue",
                      status = "primary",
                      selectInput('cpSelect2', '', "")
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "orange",
                      status = "warning",
                      selectInput('colorInput2', '', "")
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "green",
                      status = "info",
                      selectizeInput("PosSelect2",
                                     "",
                                     "",
                                     selected = NULL,
                                     multiple = TRUE)
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      div(
                        style = "height:40px;background-color: transparent;text-align:center",
                        actionButtonStyled(
                          "reset3",
                          "Reset input",
                          icon = icon("house"),
                          width = "110px",
                          type = "info"
                        )
                      ),
                    ),
                    DTOutput('contents2', width = "98%", height = "98%"),
                    div(style = "height:20px;background-color: transparent;"),
                    textAreaInput(
                      "comment_data2",
                      "Comments to include in report",
                      "Comments",
                      width = "1000px",
                      height = "100px"
                    )
                  ),
                  tabPanel(
                    title = textOutput("title3"),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "blue",
                      status = "primary",
                      selectInput('cpSelect3', '', "")
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "orange",
                      status = "warning",
                      selectInput('colorInput3', '', "")
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "green",
                      status = "info",
                      selectizeInput("PosSelect3",
                                     "",
                                     "",
                                     selected = NULL,
                                     multiple = TRUE)
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      div(
                        style = "height:40px;background-color: transparent;text-align:center",
                        actionButtonStyled(
                          "reset4",
                          "Reset input",
                          icon = icon("house"),
                          width = "110px",
                          type = "info"
                        )
                      ),
                    ),
                    style = 'width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;',
                    DTOutput('contents3', width = "98%", height = "98%"),
                    div(style = "height:20px;background-color: transparent;"),
                    textAreaInput(
                      "comment_data3",
                      "Comments to include in report",
                      "Comments",
                      width = "1000px",
                      height = "100px"
                    )
                  ),
                  tabPanel(
                    title = textOutput("title3_bis"),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "blue",
                      status = "primary",
                      selectInput('cpSelect4', '', "")
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "orange",
                      status = "warning",
                      selectInput('colorInput4', '', "")
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      background = "green",
                      status = "info",
                      selectizeInput("PosSelect4",
                                     "",
                                     "",
                                     selected = NULL,
                                     multiple = TRUE)
                    ),
                    box(
                      width = 3,
                      solidHeader = TRUE,
                      div(
                        style = "height:40px;background-color: transparent;text-align:center",
                        actionButtonStyled(
                          "reset4_bis",
                          "Reset input",
                          icon = icon("house"),
                          width = "110px",
                          type = "info"
                        )
                      ),
                    ),
                    style = 'width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;',
                    DTOutput('contents3_bis', width = "98%", height = "98%"),
                    div(style = "height:20px;background-color: transparent;"),
                    textAreaInput(
                      "comment_data4",
                      "Comments to include in report",
                      "Comments",
                      width = "1000px",
                      height = "100px"
                    )
                  ),
                )
              )),
      tabItem(
        tabName = "cell",
        tabBox(
          width = 12,
          tabPanel(title = textOutput("title4"),
                   fluidPage(fluidRow(
                     column(
                       width = 3,
                       radioButtons(
                         "var2plot",
                         label = "Select value to plot",
                         choices = c("Cp" = "Cp", "Concentration" = "Conc")
                       ),
                       sliderInput("maxCp1","Seleccionar valor máximo filtrado:",
                                   min = 0,max = 45,value = 40)
                     ),
                     
                     column(width = 8,
                            
                            plotOutput("plot1"))
                   ))),
          tabPanel(title = textOutput("title5"),
                   fluidPage(fluidRow(
                     column(
                       width = 3,
                       radioButtons(
                         "var3plot",
                         label = "Select value to plot",
                         choices = c("Cp" = "Cp", "Concentration" = "Conc")
                       ),
                       sliderInput("maxCp2","Seleccionar valor máximo filtrado:",
                                   min = 0,max = 45,value = 40)
                     ),
                     #End column inputs
                     column(width = 8,
                            #img(src = "images/96-well plot 1.png", width = "99%"),
                            #img(src = "images/96-well plot 2.png", width = "99%")
                            plotOutput("plot2"), )
                   ))),
          tabPanel(title = textOutput("title6"),
                   fluidPage(fluidRow(
                     column(
                       width = 3,
                       radioButtons(
                         "var4plot",
                         label = "Select value to plot",
                         choices = c("Cp" = "Cp", "Concentration" = "Conc")
                       ),
                       sliderInput("maxCp3","Seleccionar valor máximo filtrado:",
                                   min = 0,max = 45,value = 40)
                     ),
                     column(width = 8,
                            plotOutput("plot3"), )
                   ))),
          tabPanel(title = textOutput("title6_bis"),
                   fluidPage(fluidRow(
                     column(
                       width = 3,
                       radioButtons(
                         "var4plot_bis",
                         label = "Select value to plot",
                         choices = c("Cp" = "Cp", "Concentration" = "Conc")
                       ),
                       sliderInput("maxCp4","Seleccionar valor máximo filtrado:",
                                   min = 0,max = 45,value = 40)
                     ),
                     #End column inputs
                     column(width = 8,
                            #img(src = "images/96-well plot 1.png", width = "99%"),
                            #img(src = "images/96-well plot 2.png", width = "99%")
                            plotOutput("plot4"), )
                   ))),
        )
      ),
      tabItem(tabName = "plot",
              fluidPage(fluidRow(
                column(
                  3,
                  selectInput(
                    "type_plot",
                    label = "Select type of plot",
                    selected = NULL,
                    choices = c(
                      " " = "NULL",
                      "Box Plot" = "boxplot",
                      "Violin" = "violin"
                    )
                  ),
                  colourInput("col1", "Fill color first file:", "yellow"),
                  colourInput("col2", "Fill color second file:", "green"),
                  colourInput("col3", "Fill color third file:", "orange"),
                  colourInput("col4", "Fill color fourth file:", "pink"),
                  colourInput("line1", "Line color first file", "black",
                              palette = "limited"),
                  colourInput("line2", "Line color second file", "black",
                              palette = "limited"),
                  colourInput("line3", "Line color third file", "black",
                              palette = "limited"),
                  colourInput("line4", "Line color fourth file", "black",
                              palette = "limited")
                ),
                column(
                  9,
                  plotlyOutput("graph"),
                  div(style = "height:10px;background-color: transparent;"),
                  plotlyOutput("graph_bigotes"),
                  div(style = "height:10px;background-color: transparent;"),
                  textAreaInput(
                    "comment_plot1",
                    "Comments to include in report",
                    "Comments",
                    width = "1000px",
                    height = "100px"
                  )
                )
              ))),
      tabItem(tabName = "plot_all",
              fluidPage(
                fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      "type_file",
                      label = "Select the biomarker to plot",
                      selected = NULL,
                      choices = c(
                        " " = "NULL",
                        "KREC" = "KREC",
                        "TREC" = "TREC",
                        "ACTB" = "ACTB",
                        "SMN1" = "SMN1"
                      )
                    )
                  ),
                  column(
                    width = 3,
                    shinyDirButton(
                      "dir2",
                      "Input directory",
                      "Upload",
                      icon = icon("folder"),
                      viewtype = "detail"
                    ),
                    div(style = "height:10px;background-color: transparent;"),
                    actionButtonStyled("display",
                                       "Display",
                                       type = "warning",
                                       width = "100%")
                  ),
                  column(
                    width = 3,
                    actionButtonStyled(
                      "reset11",
                      "Reset input",
                      icon = icon("house"),
                      width = "110px",
                      type = "info"
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 8,
                    style = 'width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;',
                    DTOutput('plot_big', width = "98%", height = "98%"),
                    div(style = "height:50px;background-color: transparent;")
                  )
                ),
                fluidRow(
                  column(
                    4,
                    div(style = "height:30px;background-color: transparent;"),
                    tableOutput('summary'),
                    colourInput("colSummary", "Color", "green",
                                palette = "limited")
                  ),
                  column(
                    width = 8,
                    div(style = "height:30px;background-color: transparent;"),
                    plotlyOutput("graph2")
                  )
                )
              )),
      tabItem(
        "report",
        fluidPage(
          fluidRow(
            column(
              3,
              radioButtons(
                "file1_input",
                label = "Do you want to include file 1:",
                choices = c("SI" = TRUE, "NO" = FALSE)
              )
            ),
            column(
              3,
              radioButtons(
                "file2_input",
                label = "Do you want to include file 2:",
                choices = c("SI" = TRUE, "NO" = FALSE)
              )
            ),
            column(
              3,
              radioButtons(
                "file3_input",
                label = "Do you want to include file 3:",
                choices = c("SI" = TRUE, "NO" = FALSE)
              )
            ),
            column(
              3,
              radioButtons(
                "file4_input",
                label = "Do you want to include file 4:",
                choices = c("SI" = TRUE, "NO" = FALSE)
              )
            )
          ),
          fluidRow(
            column(
              3,
              radioButtons(
                "plot1_input",
                label = "Do you want to include well-plot file 1:",
                choices = c("SI" = TRUE, "NO" = FALSE)
              )
            ),
            column(
              3,
              radioButtons(
                "plot2_input",
                label = "Do you want to include well-plot file 2:",
                choices = c("SI" = TRUE, "NO" = FALSE)
              )
            ),
            column(
              3,
              radioButtons(
                "plot3_input",
                label = "Do you want to include well-plot file 3:",
                choices = c("SI" = TRUE, "NO" = FALSE)
              )
            ),
            column(
              3,
              radioButtons(
                "plot4_input",
                label = "Do you want to include well-plot file 4:",
                choices = c("SI" = TRUE, "NO" = FALSE)
              )
            )
          ),
          fluidRow(column(
            width = 8,
            textAreaInput(
              "comment",
              "Comments to include in report",
              "Comments",
              width = "1000px",
              height = "100px"
            )
          )),
          fluidRow(column(
            width = 6,
            textAreaInput("autor", "Autor", width = "400px", height = "40px"),
            textAreaInput(
              "title_doc",
              "Título informe",
              width = "500px",
              height = "40px"
            )
          )),
          fluidRow(column(
            width = 3,
            downloadButton(outputId = "report_gen",
                           label = "Create my report")
          ))
        )
      )
    )
  )
)

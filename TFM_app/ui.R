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

my_height = "100px"

fileInputOnlyButton <- function(..., label = "") {
  temp <- fileInput(
    inputId = "csvs",
    label = "change",
    multiple = T,
    buttonLabel = "Upload files",
    accept = c(
      ".xls",
      "text/csv",
      "text/comma-separated-values, text/plain",
      ".csv",
      ".18"
    )
  )
  temp$children[[1]] <- NULL
  temp$children[[1]]$children[[2]] <- NULL
  temp
}

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
      menuItem("Cell",
               tabName = "cell",
               icon = icon("flask")),
      menuItem(
        "Box Plot",
        tabName = "plot",
        icon = icon("chart-simple")
      ),
      menuItem("Plot",
               tabName = "plot_all",
               icon = icon("hand")),
      menuItem("Report",
               tabName = "report",
               icon = icon("download"))
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
              src = 'baby_yoda.png',
              align = "center",
              style = paste0("max-width: 100%; height: ", my_height, ";")
            ),
            div(style = "height:10px;background-color: transparent;"),
            div(
              style = "height:40px;background-color: transparent;text-align:center",
              fileInputOnlyButton("csvs", label = "Upload files"),
              tags$style(".shiny-file-input-progress {display: none}")
            ),
            div(style = "height:10px;background-color: transparent;"),
            div(style = "height:40px;background-color: transparent;text-align:center",
                
                textInput("fileId", "Input File ID", width = "100%")),
            div(style = "height:40px;background-color: transparent;"),
            div(
              style = "height:40px;background-color: transparent;",
              column(
                width = 4,
                offset = 4,
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
                    DTOutput('contents', width = "98%", height = "98%")
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
                    DTOutput('contents2', width = "98%", height = "98%")
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
                    DTOutput('contents3', width = "98%", height = "98%")
                  )
                )
              )),
      tabItem(tabName = "cell",
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
                           ),
                           #End column inputs
                           column(width = 8,
                                  #img(src = "images/96-well plot 1.png", width = "99%"),
                                  #img(src = "images/96-well plot 2.png", width = "99%")
                                  plotOutput("plot1"),)
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
                           ),
                           #End column inputs
                           column(width = 8,
                                  #img(src = "images/96-well plot 1.png", width = "99%"),
                                  #img(src = "images/96-well plot 2.png", width = "99%")
                                  plotOutput("plot2"),)
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
                           ),
                           #End column inputs
                           column(width = 8,
                                  #img(src = "images/96-well plot 1.png", width = "99%"),
                                  #img(src = "images/96-well plot 2.png", width = "99%")
                                  plotOutput("plot3"),)
                         )))
              )),
      tabItem(tabName = "plot",
              tabBox(
                width = 12,
                tabPanel(title = "Box Plot",
                         plotlyOutput("graph"))
              )),
      tabItem(tabName = "plot_all",
              fluidPage(fluidRow(
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
                      "Other" = "Other"
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
                                     width = "100%")),
                  column(
                    width = 3,
                    actionButtonStyled(
                      "reset11",
                      "Reset input",
                      icon = icon("house"),
                      width = "110px",
                      type = "info"
                    )
                  ),
                  column(
                    width = 8,
                    style = 'width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;',
                    DTOutput('plot_big', width = "98%", height = "98%"),
                    div(style = "height:50px;background-color: transparent;"),
                    plotlyOutput("graph2")
                  )
                
              ))),
      tabItem("report",
              fluidPage(fluidRow(
                column(3,
                       selectizeInput("DF_select",
                                      "",
                                      "",
                                      selected = NULL,
                                      multiple = TRUE),
                       radioButtons(
                         "file1_input",
                         label = "Do you want to include file 1:",
                         choices = c("SI" = "SI", "NO" = "NO"),
                         selected = NULL
                       ),
                       verbatimTextOutput("DF_select2", placeholder = TRUE)
              )),fluidRow(
                column(
                  width = 3,
      downloadButton(
        outputId = "report_gen",
        label = "Create my report"
      )
                )))
      )
    )
  )
)
  
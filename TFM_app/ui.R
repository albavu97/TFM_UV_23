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

my_height = "auto"

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
  dashboardSidebar(sidebarMenu(
    menuItem("Home",
             tabName = "home",
             icon = icon("house")),
    menuItem("Dashboard",
             tabName = "dashboard",
             icon = icon("table")),
    menuItem("Cell",
             tabName = "cell",
             icon = icon("flask"))
  )),
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tabItems(
    tabItem(tabName = "home",
            fluidRow(column(
              12, div(style = "height:200px;background-color: transparent;")
            ), ),
            fluidRow(div(style = "margin-rigth: 20%;margin-left: 30%;width:30%;height:200px;background-color: transparent;text-align:center",
              box(
                img(
                  src = 'TS.png',
                  align = "center",
                  style = paste0("max-width: 100%; height: ", my_height, ";")
                ),
                div(style = "height:10px;background-color: transparent;"),
                div(style = "height:40px;background-color: transparent;text-align:center",
                  fileInputOnlyButton("csvs", label = "Upload files"),
                  tags$style(".shiny-file-input-progress {display: none}")
                ),
                br(),
                div(style = "height:40px;background-color: transparent;text-align:center",
                       actionButtonStyled("reset","Reset input",icon = icon("house"),width="110px",type = "info")),
                div(style = "height:10px;background-color: transparent;"),
                width = 12,
                solidHeader = TRUE
              )
              
            ))),
    tabItem(
      tabName = "dashboard",
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
          tabPanel(title= textOutput("title1"),
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
              status = "danger",
              background = "yellow",
              selectizeInput("NameSelect1",
                             "",
                             "",
                             selected = NULL,
                             multiple = TRUE),
            ),
            style = 'width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;',
            DTOutput('contents', width = "98%", height = "98%")
          ),
          tabPanel(
            title= textOutput("title2"),
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
              status = "danger",
              background = "yellow",
              selectizeInput("NameSelect2",
                             "",
                             "",
                             selected = NULL,
                             multiple = TRUE),
            ),
            DTOutput('contents2', width = "98%", height = "98%")
          ),
            tabPanel(title= textOutput("title3"),
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
                       status = "danger",
                       background = "yellow",
                       selectizeInput("NameSelect3",
                                      "",
                                      "",
                                      selected = NULL,
                                      multiple = TRUE),
                     ),
                     style = 'width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;',
                     DTOutput('contents3', width = "98%", height = "98%")
            )
        )
      )
    ),
    tabItem(
      tabName = "cell",
      box(
        status = "warning",
        width = 12
      )
    )
  )
))

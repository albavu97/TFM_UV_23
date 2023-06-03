library(shiny)
library(devtools)
library(shinydashboard)
library(tablerDash)
library(shinythemes)
library(shinyBS)
library(htmltools)
library(DT)

fileInputOnlyButton <- function(..., label = "") {
  temp <- fileInput(
    inputId = "selection",
    label = "Upload Files:",
    multiple = T,
    accept = c(
      ".xls",
      "text/csv",
      "text/comma-separated-values, text/plain",
      ".csv"
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
             background-color:#fff8d4;
             width: 100px;
             color:black;
             }

             "
        ),
        box(
          title = "Upload file",
          width = 1,
          solidHeader = TRUE,
          status = "warning",
          uiOutput("boxContentUI2"),
          fileInputOnlyButton("file", buttonLabel = "Browse")
        ),
        box(
          title = "Upload file",
          width = 1,
          solidHeader = TRUE,
          status = "warning"
        ),
        box(
          title = "Cell position",
          width = 2,
          solidHeader = TRUE,
          status = "info",
          selectizeInput("PosSelect",
                         "",
                         "",
                         selected = NULL,
                         multiple = TRUE),
        ),
        box(
          title = "Name",
          width = 2,
          solidHeader = TRUE,
          status = "danger",
          selectizeInput("NameSelect",
                         "",
                         "",
                         selected = NULL,
                         multiple = TRUE),
        ),
        box(
          title = "Cp",
          width = 2,
          solidHeader = TRUE,
          status = "primary",
          selectInput('cpSelect', '', "")
        ),
        box(
          title = "Color",
          width = 2,
          solidHeader = TRUE,
          status = "warning",
          selectInput('colorInput', '', "")
        ),
        box(
          status = "warning",
          width = 12,
          style = 'width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;',
          DTOutput('contents', width = "98%", height = "98%")
        )
      ),
    ),
    tabItem(tabName = "cell")
  ))
)

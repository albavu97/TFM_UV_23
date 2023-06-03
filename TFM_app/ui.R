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
  # Cut away the label
  temp$children[[1]] <- NULL
  # Cut away the input field (after label is cut, this is position 1 now)
  temp$children[[1]]$children[[2]] <- NULL
  # Remove input group classes (makes button flat on one side)
  # temp$children[[1]]$attribs$class <- NULL
  # temp$children[[1]]$children[[1]]$attribs$class <- NULL
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
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    )
  )),
  dashboardBody(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tabItems(
    tabItem(
      tabName = "dashboard",
      fluidRow(box(
        title = "Title 1",
        width = 3,
        solidHeader = TRUE,
        status = "primary",
        selectInput('myselectinput','Select the first var', "")
      ),
      box(
        title = "Title 2",
        width = 3,
        solidHeader = TRUE,
        status = "primary",
        selectizeInput("NameSelect", "Cut", "",selected = NULL, multiple = TRUE),
      ),
      box(
        title = "Title 3",
        width = 3,
        solidHeader = TRUE,
        status = "primary",
        selectInput('myselectinput','Select the first var', "")
      ),
        box(
          title = p(
            "Title 1",
            actionButton(
              "titleBtId",
              "",
              icon = icon("refresh"),
              class = "btn-xs",
              title = "Update"
            )
          ),
          width = 3,
          solidHeader = TRUE,
          status = "warning",
          uiOutput("boxContentUI2")
        ),
        box(
          status = "warning",
          height = 600,
          width = 12,
          style = 'width:100%;overflow-x: scroll;height:100%;overflow-y: scroll;',
          DTOutput('contents', width = "98%", height = "98%")
        )
      ),
      
      fluidRow(
        box(
          title = "Title 1",
          width = 4,
          solidHeader = TRUE,
          status = "primary",
          "Box content"
        ),
        box(
          title = "Title 2",
          width = 4,
          solidHeader = TRUE,
          span(
            `data-toggle` = "tooltip",
            `data-placement` = "right",
            title = "Please upload a file.  Supported file types are:  .txt, .csv and .xls",
            icon("info-circle")
          )
        ),
        box(
          title = "Title 1",
          width = 4,
          solidHeader = TRUE,
          status = "warning",
          fileInputOnlyButton("file", buttonLabel = "Browse", width = 30)
        )
      ),
      
      fluidRow(
        box(
          width = 4,
          background = "black",
          "A box with a solid black background"
        ),
        box(
          title = "Title 5",
          width = 4,
          background = "light-blue",
          "A box with a solid light-blue background"
        ),
        box(
          title = "Title 6",
          width = 4,
          background = "maroon",
          "A box with a solid maroon background"
        )
      )
    )
  ))
)

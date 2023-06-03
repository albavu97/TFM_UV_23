library(shiny)
library(devtools)
library(shinydashboard)
library(tablerDash)
library(shinythemes)
library(shinyBS)
library(htmltools)


fileInputOnlyButton <- function(..., label="") {
  temp <- fileInput(inputId = "selection",
                    label = "Upload Files:",
                    multiple = T,
                    accept = c(
                      ".xls",
                      "text/csv",
                      "text/comma-separated-values, text/plain",
                      ".csv"))
  # Cut away the label
  temp$children[[1]] <- NULL
  # Cut away the input field (after label is cut, this is position 1 now)
  temp$children[[1]]$children[[2]] <- NULL
  # Remove input group classes (makes button flat on one side)
  # temp$children[[1]]$attribs$class <- NULL
  # temp$children[[1]]$children[[1]]$attribs$class <- NULL
  temp
}

js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"

# Define la interfaz
# fluidPage() es una función de que crea la interfaz y una visualizacion basica
#Only one image in above side
ui <- tablerDashPage(
  navbar = tablerDashNav(mainPanel(
    img(src = "roche_logo.svg", height = 50, width = 90),
  ),
  id = "mymenu",
  navMenu = tablerNavMenu(
    tablerNavMenuItem(
      "Upload",
      tabName = "upload",
      icon = "upload"
    ),
    tablerNavMenuItem(
      "Tab1",
      tabName = "help",
      icon = "box"
    ),
    tablerNavMenuItem(
      "Tab2",
      tabName = "other",
      icon = "box"
    ),
    tablerNavMenuItem(
      "Tab3",
      tabName = "PokeInfo",
      icon = "box"
    ),
    tablerNavMenuItem(
      "Tab4",
      tabName = "PokeFight",
      icon = "box"
    ),
    tablerNavMenuItem(
      "Tab5",
      tabName = "PokeOther",
      icon = "box"
    )
  )
  ),
  footer = tablerDashFooter(
    copyrights = "Disclaimer: this app is purely intended for learning purpose. @David Granjon, 2019"
  ),
  body = tablerDashBody(
    tags$head(
              tags$script(HTML(js))),
    tablerTabItems(tablerTabItem(tabName = "upload",
                                 fluidPage(includeCSS("www/styles.css"), responsive=TRUE,
                                           fluidRow(id="row-input",
                                             #div(class="row-input",
                                             sidebarLayout(
                                               sidebarPanel(id="sidebar1",
                                                        span(
                                                          `data-toggle` = "tooltip", `data-placement` = "right",
                                                          title = "Please upload a file.  Supported file types are:  .txt, .csv and .xls",
                                                          icon("info-circle")
                                                        )),
                                                               #div(class="file-input",
                                                                mainPanel(id="sidebar",
                                                                          sidebarLayout(
                                                                            sidebarPanel(),
                                                                          mainPanel(
                                                                   fileInputOnlyButton("file", buttonLabel="Browse",width=30),
      
                                           )))),
                                                         tableOutput(outputId = "contents"))
                                 )),
                   tablerTabItem(tabName = "help",
                                 #####################################################################
                                 
                                 fluidPage(includeCSS("www/styles.css"),
                                           div(class = "split-layout",
                                               div(class = "first-space", "First space"),
                                               div(class = "second-space", "Second space"),
                                               div(class = "third-space", "Third space"),
                                               div(class = "fourth-space", "Fourth space")
                                           ),
                                           mainPanel(
                                             h1("First level title"),
                                             h2("Second level title"),
                                             h3("Third level title"),
                                             h4("Fourth level title"),
                                             h5("Fifth level title"),
                                             h6("Sixth level title")
                                           )
                                           
                                 )#endfluidpage
                   ),#endtabitem
                   tablerTabItem(tabName = "other",
                                 fluidPage(includeCSS("www/styles.css"),
                                           title = "Hello Shiny!",
                                           fluidRow(
                                             column(width = 6,
                                                    "4"
                                             ),
                                             column(width = 6,
                                                    "3 offset 2"
                                             )
                                           )
                                 )#endFluidPage
                   ),
                   tablerTabItem(tabName = "PokeOther",
                                 tags$ul(
                                   class = "animals",
                                   tags$li("Aardvark", class = "mammal", id = "aardvark"),
                                   tags$li("Bee", class = "insect", id = "bee"),
                                   tags$li("Capybara", class = "mammal", id = "capybara")
                                 )),
                   tablerTabItem(
                     tabName = "PokeInfo",
                     fluidPage()
                       
))))


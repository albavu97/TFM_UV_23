library(ggplot2)
library(shiny)
library(dplyr)
diamonds <- diamonds

#Unique cut dropdown
cut <- sort(unique(diamonds$cut))

#Unique color
color <- sort(unique(diamonds$color))

#Unique clarity
clarity <- sort(unique(diamonds$clarity))

#Shiny App

ui <- fluidPage(
  titlePanel("diamonds"),
  fluidRow(
    column(3,
           selectizeInput("CutSelect", "Cut", cut, selected = NULL, multiple = TRUE),
           br(),
           selectInput("ColorSelect", "Color", color),
           br(),
           selectInput("ClaritySelect", "Clarity", clarity)
    ),
    tableOutput("results")
  )
)
server <- function(input, output) {
  output$results <- renderTable({
    filtered <- diamonds
    if (!is.null(input$CutSelect)) {
      filtered <- filtered %>% filter(cut == input$CutSelect)
    }
    if (!is.null(input$ColorSelect)) {
      filtered <- filtered %>% filter(color == input$ColorSelect)
    }
    if (!is.null(input$ClaritySelect)) {
      filtered <- filtered %>% filter(clarity == input$ClaritySelect)
    }
    filtered
  })
}

shinyApp(ui = ui, server = server)


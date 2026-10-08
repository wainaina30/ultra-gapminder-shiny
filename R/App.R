
library(shiny)
library(bslib)
library(gapminder)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput("continent", "continent", choices = unique(gapminder$continent))
      ),
    mainPanel(
      tableOutput("data")
    )
  )
)

server <- function(input, output, session) {
  output$data <- renderTable({
    dplyr::filter(gapminder,continent == input$continent)
  })
}

shinyApp(ui, server)
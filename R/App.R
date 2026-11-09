
library(shiny)
library(bslib)
library(gapminder)
library(DT)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "continent", 
        label  = "continent",
        choices = unique(gapminder$continent),
        selected = "All"
      ),
      selectizeInput(
        inputId = "groups", 
        label= "Group by", 
        choices = names(gapminder),
        multiple = TRUE,
        options = list(
          placeholder = "Select grouping variables..."
        )
      ),
      selectizeInput(
        inputId = "variables",
        label = "variables",
        choices = names(gapminder),
        options = list(
          placeholder = "Select variable"
        )
      )
      
      ),
    mainPanel(
      
      h4("Descriptive statistics"),
      DTOutput("data_groups"),
      
      hr(),
      
      h4("Filtered dataset"),
      DTOutput("data_filtbycont")
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data<- reactive({
    req(input$continent)
    dplyr::filter(gapminder,continent == input$continent)
    })
  
  
  
  grouped_stats <- reactive({
    req(input$groups)
    #check wheather each groups has only one observation
    group_sizes <- gapminder |> 
      group_by(across(all_of(input$groups))) |> 
      summarise(
        n = n(),
        .groups = "drop"
      )
    
    #validate 
    validate(
      need(
        all(group_sizes$n > 1),
        paste(
          "Insufficient observations per group,",
          "Your selected grouping variables create groups",
          "only one observation.",
          "Please select fewer grouping or choose variables",
          "that produce larger groups to calculate",
          "meaningful summary statistics"
        )
      )
    )
    
    result<- perform_moct_allnum2(
      gapminder
      ,groups = input$groups)
    })
    

    
  output$data_filtbycont <- renderDT({
    req(input$continent)
    
    DT::datatable(
      filtered_data(),
      options = list(pageLength = 6),
      rownames = F
    )
  })
  
  output$data_groups <- renderDT({
    pivot_longer(
      grouped_stats(),
      cols= -all_of(input$groups),
      names_to = c("variable", ".value"),
      names_sep = "_",
      values_to = "value"
    )
    
    
  })
}

shinyApp(ui, server)

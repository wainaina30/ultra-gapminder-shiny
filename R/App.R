
library(shiny)
library(bslib)
library(gapminder)
library(DT)
library(tidyverse)

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
        multiple = T,
        options = list(
          placeholder = "Select variable"
        )
      )
      
      ),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          "Filter Data",
          DTOutput("data_filtbycont")
        ),
        tabPanel(
          "Summary statistics",
          DTOutput("data_groups")
        )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data<- reactive({
    req(input$continent)
    dplyr::filter(gapminder,continent == input$continent)
    })
  
  
  
  grouped_stats <- reactive({
    
    req(
      length(input$groups) > 0 | 
      length(input$variables) > 0
    )
    
    #check groups sizes only when groups are selected.
    
    if(length(input$groups) > 0){
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
    }
    
    #when variables are not selected omit vars from function call
    if(length(input$variables) == 0L){
      perform_moct(
        data = gapminder,
        groups = input$groups
      )
    }else{
     perform_moct(
      gapminder,
      vars = input$variables,
      groups = input$groups
      )
    }
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
    
    req(grouped_stats())
    grouped_stats() |> 
    pivot_longer(
      cols= -all_of(input$groups),
      names_to = c("variable", ".value"),
      names_sep = "_",
      values_to = "value"
    ) |> 
      DT::datatable(
        options = list(pageLength = 6),
        rownames = FALSE
      )
    
    
  })
}

shinyApp(ui, server)

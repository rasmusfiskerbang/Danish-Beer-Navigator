## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

beers <- read_csv("beers.csv")
unique_breweries <- unique(beers$brewery_name)


# Sidebar -----------------------------------------------------------------
sidebar <- dashboardSidebar(
  sliderInput("alcohol_percent", "Alcohol percent", min = 0, max = 40,
              value = c(0,40)),
  sliderInput("rating", "Rating", min = 0, max = 5,
              value = c(0,5), step = 0.1),
  numericInput("min_num_rating", "Minimum number of ratings", min = 0, max = 2500,
               value = 0),
  selectInput("brewery_name", "Name of brewery",
              c("All",unique(beers$brewery_name)), selected = "All", 
              multiple = TRUE),
  selectInput("brewery_type", "Type of brewery",
              c("All",unique(beers$brewery_type)), selected = "All", 
              multiple = TRUE),
  selectInput("brewery_region", "Region of brewery",
              c("All",unique(beers$region)), selected = "All", 
              multiple = TRUE),
  sliderInput("est", "Year of establishment of brewery", min = 1859, max = 2019,
              value = c(1859,2019)),
  selectInput("beer_type", "Type of beer",
              c("All",unique(beers$type)), selected = "All", 
              multiple = TRUE),
  selectInput("super_type", "Group of beer",
              c("All",unique(beers$supertype)), selected = "All", 
              multiple = TRUE)
)


# Body --------------------------------------------------------------------

body <- dashboardBody(
  DT::dataTableOutput("table")
)


# Combining UI ------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Danish Beer Navigator"),
  sidebar,
  body
)



# Server ------------------------------------------------------------------

server <- function(input, output) { 
  
    # Brewery name
    ind_brewery_name <- reactive({
      if (any(input$brewery_name == "All")) {
        ind_brewery_name <- rep(TRUE, nrow(beers))
      } else {
        ind_brewery_name <- beers$brewery_name %in% input$brewery_name
      }
    })
    
    # Brewery type
    ind_brewery_type <- reactive({
      if (any(input$brewery_type == "All")) {
        ind_brewery_type <- rep(TRUE, nrow(beers))
      } else {
        ind_brewery_type <- beers$brewery_type %in% input$brewery_type
      }
    })
    
    # Brewery region
    ind_brewery_region <- reactive({
      if (any(input$brewery_region == "All")) {
        ind_brewery_region <- rep(TRUE, nrow(beers))
      } else {
        ind_brewery_region <- beers$region %in% input$brewery_region
      }
    })
    
    # Beer type
    ind_beer_type <- reactive({
      if (any(input$beer_type == "All")) {
        ind_beer_type <- rep(TRUE, nrow(beers))
      } else {
        ind_beer_type <- beers$type %in% input$beer_type
      }
    })
    
    # Beer super type
    ind_beer_super_type <- reactive({
      if (any(input$beer_type == "All")) {
        ind_beer_super_type <- rep(TRUE, nrow(beers))
      } else {
        ind_beer_super_type <- beers$supertype %in% input$super_type
      }
    })
  
    output$table <- DT::renderDT({
      beers %>% # Rating  
        filter(between(rating, input$rating[1], input$rating[2]),
               # Alcohol percent
               between(alcohol_percent, input$alcohol_percent[1], input$alcohol_percent[2]),
               # Number of ratings
               number_of_ratings >= input$min_num_rating,
               ind_brewery_name(),
               ind_brewery_type(),
               ind_brewery_region(),
               # Establishment
               between(est, input$est[1], input$est[1]),
               ind_beer_type(),
               ind_beer_super_type())
    }, options = list(scrollX = TRUE))
    
  }

shinyApp(ui, server)
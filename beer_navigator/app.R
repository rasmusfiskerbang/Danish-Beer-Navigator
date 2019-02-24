## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(leaflet)
library(scatterD3)

beers <- read_csv("beers.csv")
unique_breweries <- unique(beers$brewery_name)

# Defining colors
colors <- data.frame(stringsAsFactors=FALSE,
                     brewery_type = c("Brewpub", "Brewpub/Brewery", "Client Brewer",
                                      "Commercial Brewery", "Commissioner", "Contract Brewer",
                                      "Microbrewery", "Missing"),
                     color = c("e17ac1", "7f7f7f", "fd7f28", "339f34", "d42a2f", "936abb",
                               "936abb", "8b564c")) %>% 
  mutate(color = str_c("#",color))
pal <- colorFactor(colors$color, domain = colors$brewery_type)

# Defining function to add labels
add_labels <- function(brewery_name, brewery_type, address){
  str_c("<strong>Brewery name:</strong> ",brewery_name,"<br />",
        "<strong>Brewery type:</strong> ",brewery_type,"<br />",
        "<strong>Address:</strong> ",address,"<br />")
}


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
  fluidRow(
    box(
      leafletOutput("beer_map"), height = 450
    ),
    box(
      scatterD3Output("scatter_plot", height = 400), height = 450
    )
  ),
  fluidRow(
    DT::dataTableOutput("table_long")      
  )
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
    if (any(input$super_type == "All")) {
      ind_beer_super_type <- rep(TRUE, nrow(beers))
    } else {
      ind_beer_super_type <- beers$supertype %in% input$super_type
    }
  })
  
  # Creating reactive data frame
  reactive_data <- reactive({
    beers %>% 
      filter(between(rating, input$rating[1], input$rating[2]),
             # Alcohol percent
             between(alcohol_percent, input$alcohol_percent[1], input$alcohol_percent[2]),
             # Number of ratings
             number_of_ratings >= input$min_num_rating,
             # Establishment
             between(est, input$est[1], input$est[2]),
             ind_brewery_name(),
             ind_brewery_type(),
             ind_brewery_region(),
             ind_beer_type(),
             ind_beer_super_type())  
  })
  
  # Creating short data frame
  beers_short <- reactive({
    reactive_data() %>% 
      mutate(brewery_type = coalesce(brewery_type, "Missing")) %>% 
      group_by(brewery_name) %>% 
      add_count() %>% 
      ungroup() %>% 
      select(brewery_name, brewery_type, n_beers = n, est, region, address, lon, lat) %>% 
      distinct()  
  })
  
  # Printing table
  output$table_long <- DT::renderDT({
    reactive_data()  
  }, 
  options = list(scrollX = TRUE))
  
  # Printing map
  output$beer_map <- renderLeaflet({
    beers_short() %>% 
      leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~1.2*log(n_beers)+3,
                       stroke = FALSE, fillOpacity = 0.8,
                       color = ~pal(brewery_type),
                       label = ~lapply(add_labels(brewery_name, brewery_type, address), HTML))
  })
  
  # Printing scatter plot
  output$scatter_plot <- renderScatterD3({
    
    tooltip_custom <- str_c("<strong>Brewery name:</strong> ",reactive_data()$brewery_name,"<br />",
                            "<strong>Brewery type:</strong> ",reactive_data()$brewery_type,"<br />",
                            "<strong>Beer name:</strong> ",reactive_data()$beer_name,"<br />",
                            "<strong>Beer type:</strong> ",reactive_data()$type,"<br />",
                            "<strong>Number of ratings:</strong> ",reactive_data()$number_of_ratings,"<br />")
    
    reactive_data() %>% 
      mutate(number_of_ratings_transformed = sqrt(number_of_ratings)) %>% 
      as.data.frame() %>% 
      scatterD3(x = alcohol_percent, y = rating,
                col_var = brewery_type, size_var = number_of_ratings_transformed,
                point_opacity = 0.5, hover_opacity = 1,
                tooltip_text = tooltip_custom, url_var = link,
                col_lab = "Type of brewery:",
                size_lab = "Sqrt of number of ratings:",
                xlab = "Alcohol percent", ylab = "Rating")
  })
  
}

shinyApp(ui, server)
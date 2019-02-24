# Clearing data and console
cat("\014")
rm(list = ls())

# Loading packages
library(tidyverse)
library(rstudioapi)
library(scatterD3)
library(leaflet)
library(htmltools)

# Setiing directory
file_temp <- getActiveDocumentContext()
file_temp <- unlist(str_split(file_temp[[2]],"/"))
file_temp <- str_c(head(file_temp,length(file_temp)-1),collapse = "/")
setwd(file_temp)

# Setting ggplot theme
theme_set(theme_bw())

# Loading data
beers <- read_csv("beers.csv")


# Scatter plot ------------------------------------------------------------


tooltip_custom <- str_c("<strong>Brewery name:</strong> ",beers$brewery_name,"<br />",
                        "<strong>Brewery type:</strong> ",beers$brewery_type,"<br />",
                        "<strong>Beer name:</strong> ",beers$beer_name,"<br />",
                        "<strong>Beer type:</strong> ",beers$type,"<br />",
                        "<strong>Number of ratings:</strong> ",beers$number_of_ratings,"<br />")

beers %>% 
  mutate(number_of_ratings_transformed = sqrt(number_of_ratings)) %>% 
  as.data.frame() %>% 
  scatterD3(x = alcohol_percent, y = rating,
            col_var = brewery_type, size_var = number_of_ratings_transformed,
            point_opacity = 0.5, hover_opacity = 1,
            tooltip_text = tooltip_custom, url_var = link,
            col_lab = "Type of brewery:",
            size_lab = "Sqrt of number of ratings:",
            xlab = "Alcohol percent", ylab = "Rating")


# Map plot ----------------------------------------------------------------

# Colors
colors <- data.frame(stringsAsFactors=FALSE,
     brewery_type = c("Brewpub", "Brewpub/Brewery", "Client Brewer",
                      "Commercial Brewery", "Commissioner", "Contract Brewer",
                      "Microbrewery", "Missing"),
            color = c("e17ac1", "7f7f7f", "fd7f28", "339f34", "d42a2f", "936abb",
                      "936abb", "8b564c")) %>% 
  mutate(color = str_c("#",color))

pal <- colorFactor(colors$color, domain = colors$brewery_type)

# Popups
beers_short <- beers %>% 
  mutate(brewery_type = coalesce(brewery_type, "Missing")) %>% 
  group_by(brewery_name) %>% 
  add_count() %>% 
  ungroup() %>% 
  select(brewery_name, brewery_type, n_beers = n, est, region, address, lon, lat) %>% 
  distinct()

add_labels <- function(brewery_name, brewery_type, address){
str_c("<strong>Brewery name:</strong> ",brewery_name,"<br />",
      "<strong>Brewery type:</strong> ",brewery_type,"<br />",
      "<strong>Address:</strong> ",address,"<br />")
}

# Plotting
beers_short %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~2*log(n_beers)+3,
                   stroke = FALSE, fillOpacity = 0.8,
                   color = ~pal(brewery_type),
                   label = ~lapply(add_labels(brewery_name, brewery_type, address), HTML))
  








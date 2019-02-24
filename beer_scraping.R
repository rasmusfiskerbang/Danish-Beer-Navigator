# Clearing data and console
cat("\014")
rm(list = ls())

# Loading packages
library(tidyverse)
library(rstudioapi)
library(rvest)
library(janitor)
library(fuzzyjoin)
library(ggmap)

# Setiing directory
file_temp <- getActiveDocumentContext()
file_temp <- unlist(str_split(file_temp[[2]],"/"))
file_temp <- str_c(head(file_temp,length(file_temp)-1),collapse = "/")
setwd(file_temp)

# Setting ggplot theme
theme_set(theme_bw())

# Scraping main page ------------------------------------------------------

url <- "https://www.ratebeer.com/breweries/denmark/0/58/"

# Downloading page
webpage <- read_html(url)

# Getting table
breweries <- webpage %>% html_table() %>% .[[1]] %>% as_tibble() %>% 
  clean_names() %>% select(-my_count)

# Removing city names
cities <- webpage %>% html_nodes(".filter") %>% html_text()
replacement_data_frame <- data.frame(x = breweries$name, 
                                     pattern = str_c(cities,"$"), 
                                     replacement = "", stringsAsFactors = FALSE)
breweries <- breweries %>% 
  mutate(name = pmap_chr(replacement_data_frame, gsub))

# Getting links
links <- webpage %>% html_nodes("a") %>% html_attr("href") %>% str_subset("/brewers/") %>% 
  data.frame(link = .) %>% 
  mutate(name = str_split(link, "/")) %>% 
  mutate(name = map_chr(name, ~ .[3]))

# Merging links
breweries <- breweries %>% 
  stringdist_left_join(links, max_dist = 50, 
                       distance_col = "distance", ignore_case = TRUE) %>% 
  group_by(name.x) %>% filter(distance == min(distance)) %>% 
  mutate(link = str_c("https://www.ratebeer.com", link))

# Scraping breweries ------------------------------------------------------

getting_beers <- function(url){
  # Downloading page
  webpage <- try(read_html(url))
  
  # Halting execution if download fails
  if(any(class(webpage) == "xml_document")){
    Sys.sleep(10)
    webpage <- try(read_html(url))
  }
  
  # Getting table with beers
  beers <- webpage %>% html_table(fill = TRUE) %>% .[[1]]
  
  if(nrow(beers) == 0){
    return("Error")
  }
  
  beers <- beers %>% 
    clean_names() %>% as_tibble() %>% 
    select(-added, -x, -style_percent, beer_name = name, number_of_ratings = number,
           alcohol_percent = abv, rating = score)
  
  # Getting brewery name
  brewery_name <- webpage %>% html_nodes("h1") %>% html_text()
  
  # Getting address
  beers$address <- webpage %>% html_nodes(xpath = '//*[@itemprop="address"]') %>%
    html_text() %>% str_remove_all("\r|\n|\t")
  
  # Getting beer name
  beers$beer_name <- webpage %>% html_nodes("strong") %>% 
    html_text() %>% .[-1]
  
  # Getting beer type
  ind_beerstyles <- webpage %>% html_nodes("a") %>% html_attr("href") %>% 
    str_detect("beerstyles") %>% coalesce(., FALSE)
  beers$type <- webpage %>% html_nodes("a") %>% .[ind_beerstyles] %>% 
    html_text() %>% .[-c(1,2)] %>% 
    .[seq(1, length(.)-1, 2)]
  return(beers)
}

beers <- list()
for (i in 1:length(breweries$link)) {
  beers[[i]] <- getting_beers(breweries$link[i])
  print(i/length(breweries$link)*100)
}

# Scraping the rest -------------------------------------------------------

ind_error <- which(beers == "Error")

urls_pre <- breweries$link[ind_error]
urls_post <- breweries$link[ind_error] %>% str_split("/") %>% map_chr(~ .[6]) %>% 
  str_c("https://www.ratebeer.com/Ratings/Beer/ShowBrewerBeers.asp?BrewerID=",.)

getting_beers_post <- function(urls_pre, urls_post){
  # Downloading pre page
  webpage_pre <- read_html(urls_pre)
  webpage_post <- read_html(urls_post)
  
  # Getting brewery name
  brewery_name <- webpage_pre %>% html_nodes("h1") %>% html_text()
  
  # Getting address
  address <- webpage_pre %>% html_nodes(xpath = '//*[@itemprop="address"]') %>%
    html_text() %>% str_remove_all("\r|\n|\t")
  
  # Getting table post
  beers_temp <- webpage_post %>% html_table(fill = TRUE) %>% .[[1]] %>% 
    clean_names() %>% as_tibble() %>% 
    select(-added, -x, -style_percent, beer_name = name, number_of_ratings = number,
           alcohol_percent = abv, rating = score, -na)
  
  # Getting beer name
  beers_temp$beer_name <- webpage_post %>% html_nodes("strong") %>% 
    html_text()
  
  # Combining address and brewery name
  beers_temp$brewery_name <- brewery_name
  beers_temp$address <- address
  
  # Getting beer style
  ind_beerstyles <- webpage_post %>% html_nodes("a") %>% html_attr("href") %>% 
    str_detect("beerstyles") %>% coalesce(., FALSE)
  beers_temp$type <- webpage_post %>% html_nodes("a") %>% .[ind_beerstyles] %>% 
    html_text() %>% .[-c(1,2)] %>% 
    .[seq(1, length(.)-1, 2)]
  
  return(beers_temp)  
}

beers_post <- list()
for (i in 1:length(urls_pre)) {
  beers_post[[i]] <- getting_beers_post(urls_pre[i], urls_post[i])
  print(i)
}


# Combining data ----------------------------------------------------------

beers <- keep(beers, is.tibble)
beers <- modify(beers, function(x) x %>% mutate_all(as.character))
beers <- bind_rows(beers)
beers_post <- modify(beers_post, function(x) x %>% mutate_all(as.character))
beers_post <- bind_rows(beers_post)
beers <- bind_rows(beers, beers_post)

# Cleaning ----------------------------------------------------------------

beers <- beers %>% select(-na) %>% 
  mutate_at(vars(alcohol_percent:number_of_ratings), as.numeric)

# Getting latitude and longitudes
beers_long_lat <- beers %>% select(brewery_name, address) %>% distinct() %>%
  mutate(longlat = map(address, ~ geocode(., source = "dsk"))) %>% 
  unnest()
beers_long_lat[beers_long_lat$brewery_name == "Randers Bryghus",]$lon <- 10.048490
beers_long_lat[beers_long_lat$brewery_name == "Randers Bryghus",]$lat <- 56.423460
beers <- beers %>% left_join(beers_long_lat)

beers %>% 
  ggplot(aes(rating, alcohol_percent)) + geom_point()
  
  
# Joining with breweries
beers <- beers %>% left_join(breweries, by = c("brewery_name" = "name.x")) %>% 
  select(beer_name, type = type.x, alcohol_percent:address, lon, lat,
         brewery_type = type.y, est, link)

# Generating super type 
supertypes <- c("Abbey","Amber", "Bock","IPA","Lambic","Stout","Weissbier")
type_join <- data.frame(supertype = supertypes,
                        regex_name = str_c("^", supertypes))

beers <- beers %>% regex_left_join(type_join, by = c(type = "regex_name")) %>% 
  mutate(supertype = as.character(supertype)) %>% 
  mutate(supertype = coalesce(supertype, "Other"))

# Generating regions
region <- read_html("https://da.wikipedia.org/wiki/Postnumre_i_Danmark") %>% 
  html_nodes("#toc") %>% html_text() %>% str_split(., "\n") %>% 
  unlist() %>% str_subset(., "") %>% str_subset(., "^[0-9]\\.[0-9]") %>% 
  str_remove(., "^[0-9]\\.[0-9] ") %>% data.frame(region = .) %>% 
  separate(., region, c("region","postnummer"), "\\(") %>% 
  mutate(postnummer = str_remove(postnummer, "\\)")) %>% 
  separate(postnummer, c("start","end"), "-") %>% 
  mutate_at(vars(start,end), as.numeric) %>% 
  add_row(region = "Missing", start = 0, end = 999)

beers <- beers %>% 
  mutate(start = as.numeric(str_extract(address, "[0-9]{4}$")),
         end = start + 1) %>% 
  mutate(start = coalesce(start, 500), end = coalesce(start, 501)) %>% 
  interval_left_join(region) %>% 
  mutate(region = na_if(region, "Missing"))

# Selecting relevant columns
beers <- beers %>% 
  select(beer_name:type, supertype, alcohol_percent:address, region, lon:link)

# Writing csv
write_csv(beers, "beer_navigator/beers.csv")




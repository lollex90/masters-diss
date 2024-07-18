library(tidyverse)
library(geojsonio)
library(ggplot2)
library(sf)

# read gdp prediction
gdp_2022 <- read.csv("data/gdp_predictions_ukraine.csv")

# read gdp in 2021
gdp_2021 <- read.csv("data/gdp_ukraine_clean.csv") %>% 
  filter(year == 2021)

# merge by region, calculate difference
gdp <- gdp_2022 %>% 
  left_join(gdp_2021, by = "region") %>% 
  mutate(diff = 100*(gdp_prediction - real_gdp)/real_gdp) %>% 
  select(region, real_gdp, gdp_prediction, diff)

# read the geojson file
ukraine_map <- geojson_read("geoBoundaries-UKR-ADM1.geojson", what = "sp")
  
# clean the region names: replace spaces with "_"
ukraine_map@data$shapeName <- gsub(" ", "_", ukraine_map@data$shapeName)

# merge the map with the gdp data
ukraine_map@data <- left_join(ukraine_map@data, gdp, by = c("shapeName" = "region"))

ukraine_map_sf <- st_as_sf(ukraine_map)

# plot the map
map <- ggplot(data = ukraine_map_sf) +
  geom_sf(aes(fill = diff), color = "black") +
  scale_fill_gradient2(low = "red", high = "white", midpoint = 0, na.value = "grey") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "GDP Growth Prediction in Ukraine by Region in 2022",
       fill = "GDP Growth (%)")
map

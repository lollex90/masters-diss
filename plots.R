library(tidyverse)
library(geojsonio)
library(ggplot2)
library(sf)

# Model performance comparison --------------------------------------------

# Compare results of the four models
fe_results <- read.csv("fe_results.csv") %>% 
  mutate(fe_mae = av_mae) %>% 
  select(year, fe_mae)
xgb_results <- read.csv("xgb_results.csv") %>% 
  mutate(xgb_mae = mae) %>% 
  select(year, xgb_mae)
rf_results <- read.csv("rf_results.csv") %>% 
  mutate(rf_mae = mae) %>% 
  select(year, rf_mae)
cnn_results <- read.csv("cnn_results.csv") %>% 
  mutate(cnn_mae = mae) %>% 
  select(year, cnn_mae)

# Merge results by year
results <- fe_results %>% 
  left_join(xgb_results, by = "year") %>% 
  left_join(rf_results, by = "year") %>% 
  left_join(cnn_results, by = "year") 

# Plot results
plot_1 <- results %>% 
  gather(model, mae, -year) %>% 
  mutate(model = factor(model, levels = c("fe_mae", "xgb_mae", "rf_mae", "cnn_mae"))) %>% 
  ggplot(aes(x = year, y = mae, color = model)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2013:2021) +
  scale_color_manual(values = c(fe_mae = "#d62728", xgb_mae = "#ff7f0e", 
                                rf_mae = "#2ca02c", cnn_mae = "#1f77b4"), 
                     labels = c(fe_mae = "FE", xgb_mae = "XGB", 
                                rf_mae = "RF", cnn_mae = "CNN")) +
  labs(x = "Year",
       y = "Mean Absolute Error", 
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_1
# Ukraine predicted gdp map -----------------------------------------------

# read gdp prediction
gdp_2022 <- read.csv("gdp_predictions_ukraine.csv")

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

# Calculate centroids of each region
centroids <- st_centroid(ukraine_map_sf)

# Extract the coordinates of the centroids
centroids_coords <- st_coordinates(centroids)

# Create a data frame with the centroids and the values of 'diff'
centroids_df <- data.frame(
  x = centroids_coords[, 1],
  y = centroids_coords[, 2],
  diff = ukraine_map_sf$diff
)

# Plot the map with text annotations
map_1 <- ggplot(data = ukraine_map_sf) +
  geom_sf(aes(fill = diff), color = "black") +
  geom_text(data = centroids_df, aes(x = x, y = y, label = round(diff, 1)), size = 3, color = "black") +
  scale_fill_gradient2(low = "red", high = "white", midpoint = 0, na.value = "grey") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(title = "Predicted change in real GDP in Ukrainian regions in 2022",
       fill = "GDP Growth (%)") 

map_1


# Poland check ------------------------------------------------------------

# read gdp prediction
poland_gdp <- read.csv("poland_predictions.csv")

# scatterplot real_gdp against gdp_pred
plot_2 <- ggplot(data = poland_gdp, aes(x = real_gdp, y = gdp_pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Real GDP (PLN)", y = "Predicted GDP (PLN)") +
  theme_minimal()

plot_2

poland_data <- read.csv("data/gdp_poland_clean.csv")
gdp_2021 <- sum(poland_data$real_gdp[poland_data$year == 2021])
  
# compute total and predicted gdp change
total <- sum(poland_gdp$real_gdp)/gdp_2021
total_pred <- sum(poland_gdp$gdp_pred)/gdp_2021



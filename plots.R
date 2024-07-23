library(tidyverse)
library(geojsonio)
library(ggplot2)
library(ggrepel)
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

# read the data
gdp_2022 <- read.csv("gdp_predictions_ukraine.csv")
gdp_2021 <- read.csv("data/gdp_ukraine_clean.csv") %>% 
  filter(year == 2021)

# merge by region, calculate difference
gdp <- gdp_2022 %>% 
  left_join(gdp_2021, by = "region") %>% 
  mutate(diff = 100*(gdp_prediction - real_gdp)/real_gdp) %>% 
  select(region, real_gdp, gdp_prediction, diff)

# read the geojson file
ukraine_map <- geojson_read("geoBoundaries-UKR-ADM1.geojson", what = "sp")
ukraine_map@data$shapeName <- gsub(" ", "_", ukraine_map@data$shapeName)

# merge the map with the gdp data
ukraine_map@data <- left_join(ukraine_map@data, gdp, by = c("shapeName" = "region"))
ukraine_map_sf <- st_as_sf(ukraine_map)

# Calculate centroids of each region
centroids <- st_centroid(ukraine_map_sf)
centroids_coords <- st_coordinates(centroids)
centroids_df <- data.frame(
  x = centroids_coords[, 1],
  y = centroids_coords[, 2],
  diff = ukraine_map_sf$diff
)

centroids_df[5, 2] <- centroids_df[5, 2] - 0.3
centroids_df[17, 1] <- centroids_df[17, 1] + 0.6
centroids_df[3, 1] <- centroids_df[3, 1] + 0.3
centroids_df[26, 2] <- centroids_df[26, 2] + 0.25
centroids_df[26, 1] <- centroids_df[26, 1] - 0.2

# Plot 
map_1 <- ggplot(data = ukraine_map_sf) +
  geom_sf(aes(fill = diff), color = "black") +
  geom_text(data = centroids_df, aes(x = x, y = y, label = round(diff, 1)), size = 3, color = "black") +
  scale_fill_gradient2(low = "#d62728", high = "white", midpoint = 0, na.value = "grey") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(fill = "GDP change (%)") 

map_1

# Poland check ------------------------------------------------------------

# read gdp prediction, calculate % difference for each region
poland_gdp <- read.csv("poland_predictions.csv") %>% 
  select(-year)
poland_data <- read.csv("data/gdp_poland_clean.csv") 
poland_gdp <- poland_gdp %>% 
  left_join(poland_data[poland_data$year == 2021, ], by = "region") %>% 
  select(-year) %>% 
  mutate(real_gdp_diff = 100*real_gdp.x/real_gdp.y, 
         pred_gdp_diff = 100*gdp_pred/real_gdp.y, 
         real_gdp = real_gdp.x) %>% 
  select(region, real_gdp_diff, pred_gdp_diff, real_gdp, gdp_pred)


# scatterplot real_gdp against gdp_pred
plot_2 <- ggplot(data = poland_gdp, aes(x = real_gdp_diff, y = pred_gdp_diff, label = region)) +
  geom_point(color = "#2ca02c") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted") +
  geom_text_repel() +
  annotate("text", x = max(poland_gdp$real_gdp_diff), y = max(poland_gdp$real_gdp_diff), label = "x = y", hjust = 1.1, vjust = -0.5) +
  labs(x = "Actual GDP change (%)", y = "Predicted GDP change (%)") +
  theme_minimal()

plot_2

# compute total and predicted gdp change
gdp_2021 <- sum(poland_data$real_gdp[poland_data$year == 2021])
total <- 100*sum(poland_gdp$real_gdp)/gdp_2021
total_pred <- 100*sum(poland_gdp$gdp_pred)/gdp_2021

# compute total and predicted gdp change excluding mazowieckie
gdp_2021_no_maz <- sum(poland_data$real_gdp[poland_data$year == 2021 & poland_data$region != "Mazowieckie"])
total_no_maz <- 100*sum(poland_gdp$real_gdp[poland_gdp$region != "Mazowieckie"])/gdp_2021_no_maz
total_pred_no_maz <- 100*sum(poland_gdp$gdp_pred[poland_gdp$region != "Mazowieckie"])/gdp_2021_no_maz


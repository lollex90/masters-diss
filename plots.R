library(tidyverse)
library(geojsonio)
library(ggplot2)
library(ggrepel)
library(sf)

setwd("~/LSE/Dissertation/masters-diss")

# Model performance comparison --------------------------------------------

# Compare results of the four models
fe_results <- read.csv("fe_results.csv") %>% 
  mutate(fe_mae_log = av_mae_log, 
         fe_mae_diff = av_mae_diff) %>% 
  select(year, fe_mae_log, fe_mae_diff)
xgb_results <- read.csv("xgb_results.csv") %>% 
  mutate(xgb_mae = mae) %>% 
  select(year, xgb_mae)
rf_results <- read.csv("rf_results.csv") %>% 
  mutate(rf_mae = mae) %>% 
  select(year, rf_mae)
cnn_results <- read.csv("cnn_results_allangle_2.csv") %>% 
  mutate(cnn_mae = mae) %>% 
  select(year, cnn_mae)
nn_results <- read.csv("nn_results.csv") %>% 
  mutate(nn_mae = mae) %>% 
  select(year, nn_mae)

# Merge results by year
results <- fe_results %>% 
  left_join(xgb_results, by = "year") %>% 
  left_join(rf_results, by = "year") %>% 
  left_join(cnn_results, by = "year") %>% 
  left_join(nn_results, by = "year")

# Plot results
plot_1 <- results %>% 
  gather(model, mae, -year) %>% 
  mutate(model = factor(model, levels = c("fe_mae_log", "fe_mae_diff", "xgb_mae", "rf_mae", "cnn_mae", "nn_mae"))) %>% 
  ggplot(aes(x = year, y = mae, color = model)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2013:2021) +
  scale_color_manual(values = c(fe_mae_log = "#d62728", fe_mae_diff = "black",
                                xgb_mae = "#ff7f0e", rf_mae = "#2ca02c", cnn_mae = "#1f77b4", nn_mae = "#9467bd"), 
                     labels = c(fe_mae_log = "Linear Log", fe_mae_diff = "First Differences",
                                xgb_mae = "XGBoost", rf_mae = "Random Forest", cnn_mae = "CNN", nn_mae = "NN")) +
  labs(x = "Year",
       y = "Mean Absolute Error", 
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_1

# compute averages for all models
mean_linlog <- mean(results$fe_mae_log)
mean_fd <- mean(results$fe_mae_diff)
mean_xgb <- mean(results$xgb_mae)
mean_rf <- mean(results$rf_mae)
mean_cnn <- mean(results$cnn_mae)
mean_nn <- mean(results$nn_mae)

# Ukraine predicted gdp map -----------------------------------------------

# read the data
gdp_2022 <- read.csv("gdp_predictions_ukraine_nn.csv")
gdp_2022 <- read.csv("gdp_predictions_ukraine_aa_sf_hq.csv") %>% 
  rename(gdp_prediction = gdp_pred)
gdp_2021 <- read.csv("data/gdp_ukraine_clean.csv") %>% 
  filter(year == 2021)

# merge by region, calculate difference
gdp <- gdp_2022 %>% 
  left_join(gdp_2021, by = "region") %>% 
  mutate(diff = 100*(gdp_prediction - real_gdp)/real_gdp) %>% 
  select(region, real_gdp, gdp_prediction, diff)

# calculate total gdp change
total_gdp_change <- (sum(gdp$gdp_prediction) - sum(gdp$real_gdp))/sum(gdp$real_gdp)*100

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
poland_gdp <- read.csv("poland_predictions_new_2.csv") %>% 
  select(-year)
poland_gdp <- read.csv("gdp_predictions_poland_nn.csv")
poland_data <- read.csv("data/gdp_poland_clean.csv") 
poland_gdp <- poland_gdp %>% 
  left_join(poland_data[poland_data$year == 2021, ], by = "region") %>% 
  select(-year) %>% 
  mutate(real_gdp_diff = 100*real_gdp.x/real_gdp.y, 
         pred_gdp_diff = 100*gdp_pred/real_gdp.y, 
         real_gdp = real_gdp.x, 
         real_gdp_2021 = real_gdp.y) %>% 
  select(region, real_gdp_diff, pred_gdp_diff, real_gdp, gdp_pred, real_gdp_2021)

# scatterplot real_gdp against gdp_pred
plot_2 <- ggplot(data = poland_gdp, aes(x = real_gdp, y = gdp_pred, label = region)) +
  geom_point(color = "#2ca02c") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dotted") +
  geom_text_repel() +
  annotate("text", x = max(poland_gdp$real_gdp_diff), y = max(poland_gdp$real_gdp_diff), label = "x = y", hjust = 1.1, vjust = -0.5) +
  labs(x = "Actual GDP change (PLN)", y = "Predicted GDP change (PLN)") +
  theme_minimal() 

plot_2

# compute average difference
mean_diff <- mean(abs(poland_gdp$real_gdp - poland_gdp$gdp_pred))
mean(poland_gdp$real_gdp_2021)

# compute total and predicted gdp change
gdp_2021 <- sum(poland_data$real_gdp[poland_data$year == 2021])
total <- 100*sum(poland_gdp$real_gdp)/gdp_2021
total_pred <- 100*sum(poland_gdp$gdp_pred)/gdp_2021

# compute average % deviation 
poland_gdp <- poland_gdp %>% 
  mutate(percentage_diff = abs(real_gdp - gdp_pred)/(real_gdp))

mean(poland_gdp$percentage_diff)*100

# sort by real gdp
poland_gdp <- poland_gdp %>% 
  arrange(real_gdp_diff)

print(poland_gdp$region)

poland_gdp <- poland_gdp %>% 
  arrange(pred_gdp_diff)

poland_gdp <- poland_gdp %>% 
  mutate(prec_diff = abs(real_gdp_diff-pred_gdp_diff))

# compute total and predicted gdp change excluding mazowieckie
gdp_2021_no_maz <- sum(poland_data$real_gdp[poland_data$year == 2021 & poland_data$region != "Mazowieckie"])
total_no_maz <- 100*sum(poland_gdp$real_gdp[poland_gdp$region != "Mazowieckie"])/gdp_2021_no_maz
total_pred_no_maz <- 100*sum(poland_gdp$gdp_pred[poland_gdp$region != "Mazowieckie"])/gdp_2021_no_maz


# Other composites --------------------------------------------------------

# read the data
allangle_hq_snow_free <- read.csv("nn_results.csv") %>% 
  mutate(aa_hq_sf = mae) %>%
  select(year, aa_hq_sf)
nearnadir_hq_snow_free <- read.csv("nn_results_nn_sf_hq.csv") %>% 
  mutate(nn_hq_sf = mae) %>% 
  select(year, nn_hq_sf)
offnadir_hq_snow_free <- read.csv("nn_results_on_sf_hq.csv") %>% 
  mutate(on_hq_sf = mae) %>% 
  select(year, on_hq_sf)
allangle_snow_free <- read.csv("nn_results_aa_sf_lq.csv") %>% 
  mutate(aa_lq_sf = mae) %>% 
  select(year, aa_lq_sf)
nearnadir_snow_free <- read.csv("nn_results_nn_sf_lq.csv") %>% 
  mutate(nn_lq_sf = mae) %>% 
  select(year, nn_lq_sf)
offnadir_snow_free <- read.csv("nn_results_on_sf_lq.csv") %>%
  mutate(on_lq_sf = mae) %>% 
  select(year, on_lq_sf)
allangle_snow_cov <- read.csv("nn_results_aa_sc_lq.csv") %>% 
  mutate(aa_lq_sc = mae) %>% 
  select(year, aa_lq_sc)
nearnadir_snow_cov <- read.csv("nn_results_nn_sf_lq.csv") %>%
  mutate(nn_lq_sc = mae) %>% 
  select(year, nn_lq_sc)
offnadir_snow_cov <- read.csv("nn_results_on_sc_lq.csv") %>%
  mutate(on_lq_sc = mae) %>% 
  select(year, on_lq_sc)

results <- allangle_hq_snow_free %>% 
  left_join(nearnadir_hq_snow_free, by = "year") %>% 
  left_join(offnadir_hq_snow_free, by = "year") %>% 
  left_join(allangle_hq_snow_cov, by = "year") %>%
  left_join(nearnadir_hq_snow_cov, by = "year") %>%
  left_join(offnadir_hq_snow_cov, by = "year") %>%
  left_join(allangle_snow_free, by = "year") %>%
  left_join(nearnadir_snow_free, by = "year") %>%
  left_join(offnadir_snow_free, by = "year") %>%
  left_join(allangle_snow_cov, by = "year") %>%
  left_join(nearnadir_snow_cov, by = "year") %>%
  left_join(offnadir_snow_cov, by = "year")

# rows as columns

transposed_df <- df %>%
  rownames_to_column(var = "row") %>%
  pivot_longer(-row, names_to = "column", values_to = "value") %>%
  pivot_wider(names_from = row, values_from = value)

results <- results %>% 
  rownames_to_column(var = "row") %>%
  pivot_longer(-row, names_to = "Composite", values_to = "value") %>%
  pivot_wider(names_from = row, values_from = value) %>% 
  na.omit()

results <- results[-1,]
names(results) <- c("Composite", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

results <- results %>% 
  mutate(average = rowMeans(select(., -Composite)))

library(tidyverse)

setwd("~/LSE/Dissertation/masters-diss")

sum_columns <- c("nearnad_snow_cov_sum", "nearnad_snow_free_sum", "offnad_snow_cov_sum",
                        "offnad_snow_free_sum", "allangle_snow_cov_sum", "allangle_snow_free_sum", 
                        "nearnad_snow_free_hq_sum", "offnad_snow_free_hq_sum", "allangle_snow_free_hq_sum")

sum_columns <- c("allangle_snow_free_hq_sum")

# read in the data
ukraine_data <- read.csv("data/tabular_data_ukraine.csv") %>% 
  filter(region != "Kyiv_Oblast_City", 
         year < 2022) %>% 
  select(region, year, real_gdp, allangle_snow_free_hq_sum) %>% 
  mutate(gdp_diff = real_gdp - lag(real_gdp, 1), 
         light_diff = allangle_snow_free_hq_sum - lag(allangle_snow_free_hq_sum, 1))

# initialise a df to store error results
error_df <- data.frame()

for (i in 2013:2021) {
  # train test split
  train_data <- ukraine_data %>% 
    filter(year != i)
  test_data <- ukraine_data %>% 
    filter(year == i)
  
  # train the model
  model_reg <- lm(gdp_diff ~ light_diff + region, data = train_data)
  model_log <- lm(log(real_gdp) ~ log(allangle_snow_free_hq_sum) + region, data = train_data)

  # predict on the test set
  test_data$gdp_pred_reg <- predict(model_reg, test_data)
  test_data$gdp_pred_log <- exp(predict(model_log, test_data))

  # calculate the mean absolute error
  av_mae <- mean(abs(test_data$gdp_diff - test_data$gdp_pred_reg), na.rm = TRUE)
  av_mae_log <- mean(abs(test_data$real_gdp - test_data$gdp_pred_log), na.rm = TRUE)

  # store the results
  error_df <- rbind(error_df, data.frame(year = i, av_mae = av_mae, av_mae_log = av_mae_log))
}

# save the results
write.csv(error_df, "fe_results.csv")


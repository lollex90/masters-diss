library(tidyverse)

setwd("~/LSE/Dissertation/masters-diss")

sum_columns <- c("nearnad_snow_cov_sum", "nearnad_snow_free_sum", "offnad_snow_cov_sum",
                        "offnad_snow_free_sum", "allangle_snow_cov_sum", "allangle_snow_free_sum", 
                        "nearnad_snow_free_hq_sum", "offnad_snow_free_hq_sum", "allangle_snow_free_hq_sum")

# read in the data
ukraine_data <- read.csv("data/tabular_data_ukraine.csv")

# train-test split, predict for 2022 and 2023
train_data <- ukraine_data %>% 
  filter(year < 2021)

test_data <- ukraine_data %>%
  filter(year == 2021)

full_data <- ukraine_data %>%
  filter(year < 2022)

prediction_data <- ukraine_data %>%
  filter(year > 2021)

for (indep_var in sum_columns) {
  
  formula <- as.formula(paste("log(real_gdp) ~ log(", indep_var, ") + region", sep = ""))
  model <- lm(formula, data = train_data)
  
  # use the model to predict for the test data
  test_data$real_gdp_pred <- exp(predict(model, test_data))
  
  # calculate the absolute % error
  test_data$error <- abs(((test_data$real_gdp - test_data$real_gdp_pred) / test_data$real_gdp) * 100)
  
  # print the mean % error
  print(paste("Mean % error for", indep_var, "is", mean(test_data$error, na.rm = TRUE)))
  
  # estimate the same model on the entire dataset
  model <- lm(formula, data = full_data)
  
  # predict on the prediction data
  prediction_data$real_gdp_pred <- exp(predict(model, prediction_data))
  
  # calculate the gdp change from 2021 to 2022
  prediction_data <- prediction_data %>% filter(year == 2022) 
  gdp_change <- ((sum(prediction_data$real_gdp_pred) - sum(test_data$real_gdp))/sum(test_data$real_gdp)) * 100
  
  print(paste("GDP change for", indep_var, "is", gdp_change))

}




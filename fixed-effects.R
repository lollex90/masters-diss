library(tidyverse)

setwd("~/LSE/Dissertation/masters-diss")

sum_columns <- c("nearnad_snow_cov_sum", "nearnad_snow_free_sum", "offnad_snow_cov_sum",
                        "offnad_snow_free_sum", "allangle_snow_cov_sum", "allangle_snow_free_sum", 
                        "nearnad_snow_free_hq_sum", "offnad_snow_free_hq_sum", "allangle_snow_free_hq_sum")

sum_columns <- c("allangle_snow_free_hq_sum")

# read in the data
ukraine_data <- read.csv("data/tabular_data_ukraine.csv")

# train-test split, predict for 2022 and 2023
train_data <- ukraine_data %>% 
  filter(year < 2021)

test_data <- ukraine_data %>%
  filter(year == 2021)

test_data$year <- 2020  # Temporarily use the last year of the training data

full_data <- ukraine_data %>%
  filter(year < 2022)

prediction_data <- ukraine_data %>%
  filter(year == 2022)

prediction_data$year <- 2021  # Temporarily use the last year of the training data

for (indep_var in sum_columns) {
  
  # train region-only model
  formula_reg <- as.formula(paste("log(real_gdp) ~ log(", indep_var, ") + region", sep = ""))
  model_reg<- lm(formula_reg, data = train_data)
  
  # train region and year model
  formula_year <- as.formula(paste("log(real_gdp) ~ log(", indep_var, ") + region + factor(year)", sep = ""))
  model_year <- lm(formula_year, data = train_data)
  
  # get the coefficient for factor(year)2020
  coef_2020 <- coef(model_year)[names(coef(model_year)) == "factor(year)2020"]
  
  # use the models
  test_data$real_gdp_pred_reg <- exp(predict(model_reg, test_data))
  test_data$real_gdp_pred_year <- exp(predict(model_year, test_data) - coef_2020)
  
  # calculate the absolute % error
  test_data$error_reg <- abs(((test_data$real_gdp - test_data$real_gdp_pred_reg) / test_data$real_gdp) * 100)
  test_data$error_year <- abs(((test_data$real_gdp - test_data$real_gdp_pred_year) / test_data$real_gdp) * 100)
  
  # print the mean % error
  print(paste("Mean % error for REG and ", indep_var, "is", mean(test_data$error_reg, na.rm = TRUE)))
  print(paste("Mean % error for YEAR and ", indep_var, "is", mean(test_data$error_year, na.rm = TRUE)))
  
  # estimate the same models on the entire dataset
  model_reg <- lm(formula_reg, data = full_data)
  model_year <- lm(formula_year, data = full_data)
  
  # get the coefficient for factor(year)2021
  coef_2021 <- coef(model_year)[names(coef(model_year)) == "factor(year)2021"]

  # predict on the prediction data
  prediction_data$real_gdp_pred_reg <- exp(predict(model_reg, prediction_data))
  prediction_data$real_gdp_pred_year <- exp(predict(model_year, prediction_data) - coef_2021)

  # calculate the gdp change from 2021 to 2022
  gdp_change_reg <- ((sum(prediction_data$real_gdp_pred_reg) - sum(test_data$real_gdp))/sum(test_data$real_gdp)) * 100
  gdp_change_year <- ((sum(prediction_data$real_gdp_pred_year) - sum(test_data$real_gdp))/sum(test_data$real_gdp)) * 100

  print(paste("GDP REG change for", indep_var, "is", gdp_change_reg))
  print(paste("GDP YEAR change for", indep_var, "is", gdp_change_year))

}




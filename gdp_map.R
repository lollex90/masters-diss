library(tidyverse)

# read gdp prediction
gdp_2022 <- read.csv("data/gdp_predictions_ukraine.csv")

# read gdp in 2021
gdp_2021 <- read.csv("data/gdp_ukraine_clean.csv") %>% 
  filter(year == 2021)

# merge by region, calculate difference
gdp <- gdp_2022 %>% 
  left_join(gdp_2021, by = "region") %>% 
  mutate(diff = 100*(gdp_prediction - real_gdp)/real_gdp)
  select(region, real_gdp, gdp_prediction, diff)

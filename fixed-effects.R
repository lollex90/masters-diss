library(tidyverse)

# read data
ukraine_data <- read.csv("data/tabular_data_ukraine.csv")

# keep only off and on nadir snow free obsaervations
ukraine_data <- ukraine_data %>%
  # select(-contains("cov"), -contains("allangle")) %>% 
  select(-year) %>% 
  na.omit()

# run a fixed effects model
fixed_effects <- lm(gdp ~ ., data = ukraine_data)

summary(fixed_effects)

# predict for the year 2022 using the fixed effects model
test_data <- read.csv("data/tabular_data_ukraine.csv") %>% 
  filter(year == 2022) %>% 
  #select(-contains("cov"), -contains("allangle")) %>% 
  select(-year, -gdp)

test_data$real_gdp <- predict(fixed_effects, test_data)

results <- test_data %>% 
  select(region, real_gdp) %>% 
  mutate(year = 2022) 

# get gdp data
gdp_data <- read.csv("data/clean_ukr_gdp.csv") %>% 
  na.omit()

# append results to the gdp data
gdp_data <- rbind(gdp_data, results)

# plot the results
ggplot(gdp_data, aes(x = year, y = real_gdp, color = region)) +
  geom_line() +
  labs(title = "Ukraine GDP by Region",
       x = "Year",
       y = "GDP") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1")

# calculate average % change between 2021 and 2022
summary <- gdp_data %>%
  filter(year %in% c(2021, 2022)) %>% 
  pivot_wider(names_from = year, values_from = real_gdp) %>% 
  mutate(change = ((`2022` - `2021`) / `2021`) * 100)

# turn to wide format with years as columns
summary <- summary %>% 
  pivot_wider(names_from = year, values_from = real_gdp) %>% 
  mutate(change = ((`2022` - `2021`) / `2021`) * 100)

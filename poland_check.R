library(tidyverse)

# read polish tabular data
poland <- read.csv("data/tabular_data_poland.csv") %>% 
  # select(year, region, real_gdp, contains("allangle_snow_free_hq")) %>% 
  select(year, region, real_gdp, allangle_snow_free_hq_sum)

# calculate the difference in gdp: for year 2022 it is the difference between 2022 and 2021
poland <- poland %>% 
  mutate(gdp_diff = real_gdp - lag(real_gdp), 
         light_diff = allangle_snow_free_hq_sum - lag(allangle_snow_free_hq_sum))

poland_train <- poland %>% 
  filter(year != "2022") %>% 
  filter(year != "2023")

poland_test <- poland %>% 
  filter(year == "2022")

model <- lm(gdp_diff ~ light_diff+ region, data = poland_train)

mae <- mean(abs(poland_test$gdp_diff - predict(model, newdata = poland_test)))

# predict for 2022
poland_test$gdp_growth <- predict(model, newdata = poland_test)

poland_train <- poland %>% 
  filter(year != "2022") %>% 
  filter(year != "2023")

model <- lm(log(real_gdp) ~ log(allangle_snow_free_hq_sum) + region, data = poland)
summary(model)

# predict for 2022
poland_2022 <- poland %>% 
  filter(year == "2022") 

poland_2021 <- poland %>% 
  filter(year == "2021")

poland_2022$gdp_pred <- exp(predict(model, newdata = poland_2022))
poland_2021$gdp_pred <- exp(predict(model, newdata = poland_2021))


poland_2022 <- poland_2022 %>% 
  select(year, region, real_gdp, gdp_pred, allangle_snow_free_hq_sum)

# plot lights and gdp
poland_2022 %>% 
  ggplot(aes(x = allangle_snow_free_hq_sum, y = real_gdp)) +
  geom_point() +
  geom_point(aes(x = allangle_snow_free_hq_sum, y = gdp_pred), color = "red") +
  labs(title = "Poland 2022 GDP Prediction",
       x = "Lights",
       y = "GDP") +
  theme_minimal()



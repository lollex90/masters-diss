library(tidyverse)
library(readxl)

# read gdp prediction
gdp_2022 <- read.csv("gdp_predictions_ukraine_aa_sf_hq.csv") %>% 
  rename(gdp_prediction = gdp_pred)

# read gdp in 2021
gdp_2021 <- read.csv("data/gdp_ukraine_clean.csv") %>% 
  filter(year == 2021)

# names mappings
ukraine_names <- c(
  'Kherson' = 'Kherson_Oblast',
  'Volyn' = 'Volyn_Oblast',
  'Rivne' = 'Rivne_Oblast',
  'Zhytomyr' = 'Zhytomyr_Oblast',
  'Kyiv' = 'Kyiv_Oblast',
  'Chernihiv' = 'Chernihiv_Oblast',
  'Sumy' = 'Sumy_Oblast',
  'Kharkiv' = 'Kharkiv_Oblast',
  'Luhansk' = 'Luhansk_Oblast',
  'Donetsk' = 'Donetsk_Oblast',
  'Zaporizhzhya' = 'Zaporizhia_Oblast',
  'Lviv' = 'Lviv_Oblast',
  'Ivano-Frankivsk' = 'Ivano-Frankivsk_Oblast',
  'Zakarpattya' = 'Zakarpattia_Oblast',
  'Ternopyl' = 'Ternopil_Oblast',
  'Chernivtsi' = 'Chernivtsi_Oblast',
  'Odesa' = 'Odessa_Oblast',
  'Mykolayiv' = 'Mykolaiv_Oblast',
  'Vinnytsya' = 'Vinnytsia_Oblast',
  'Khmelnytskiy' = 'Khmelnytskyi_Oblast',
  'Cherkasy' = 'Cherkasy_Oblast',
  'Poltava' = 'Poltava_Oblast',
  'Dnipropetrovsk' = 'Dnipropetrovsk_Oblast',
  'Kirovohrad' = 'Kirovohrad_Oblast',
  'Kyiv city' = 'Kyiv',
  'Sevastopol city' = 'Sevastopol'
)

# merge by region, calculate difference
gdp <- gdp_2022 %>% 
  left_join(gdp_2021, by = "region") %>% 
  mutate(diff = 100*(gdp_prediction - real_gdp)/real_gdp) %>% 
  select(region, real_gdp, gdp_prediction, diff)

# war data
war_data <- read.csv("Ukraine_Black_Sea_2020_2024_May24.csv") %>%
  select(event_id_cnty, year, disorder_type, sub_event_type, event_type, country, admin1, fatalities) %>%
  filter(year == 2022 & country == "Ukraine") %>%
  group_by(admin1) %>%
  summarise(no_explosions = sum(event_type == "Explosions/Remote violence"),
            fatalities_explosions = sum(fatalities[event_type == "Explosions/Remote violence"]),
            no_battles = sum(event_type == "Battles"),
            fatalities_battles = sum(fatalities[event_type == "Battles"]),
            no_violence = sum(event_type == "Violence against civilians"),
            fatalities_violence = sum(fatalities[event_type == "Violence against civilians"]), 
            no_drones = sum(sub_event_type == "Air/drone strike"),
            no_armed_clash = sum(sub_event_type == "Armed clash"),
            no_artillery = sum(sub_event_type == "Shelling/artillery/missile attack"), 
            no_rape = sum(sub_event_type == "Sexual violence"), 
            no_arrest = sum(sub_event_type == "Arrests"), 
            no_peaceful = sum(sub_event_type == "Peaceful protest")) %>%
  mutate(admin1 = paste0(admin1, "_Oblast"),
         admin1 = case_when(
           admin1 == "Kyiv City_Oblast" ~ "Kyiv",
           admin1 == "Odesa_Oblast" ~ "Odessa_Oblast",
           admin1 == "Kirovograd_Oblast" ~ "Kirovohrad_Oblast",
           TRUE ~ admin1
         )) %>% 
  filter(admin1 != "_Oblast")

# merge war data with prediction
war_gdp <- gdp %>% 
  left_join(war_data, by = c("region" = "admin1"))

# build a model
model <- lm(gdp_prediction ~ fatalities_battles + fatalities_violence + no_peaceful + real_gdp, data = war_gdp)
summary(model)

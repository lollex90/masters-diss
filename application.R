library(tidyverse)
library(geojsonio)
library(ggplot2)
library(sf)
library(readxl)

# read gdp prediction
gdp_2022 <- read.csv("data/gdp_predictions_ukraine.csv")

# read gdp in 2021
gdp_2021 <- read.csv("data/gdp_ukraine_clean.csv") %>% 
  filter(year == 2021)

# get population data, clean
population <- read_excel("data/pop_ukraine.xls", skip = 3)
population <- population[, c(2, 6)]
names(population) <- c("pop", "region")

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

population$region <- ukraine_names[population$region]


# merge by region, calculate difference
gdp <- gdp_2022 %>% 
  left_join(gdp_2021, by = "region") %>% 
  left_join(population, by = "region") %>%
  mutate(diff = 100*(gdp_prediction - real_gdp)/real_gdp) %>% 
  select(region, real_gdp, gdp_prediction, diff, pop)

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
  left_join(war_data, by = c("region" = "admin1")) %>% 
  mutate(fatalities_battles_rat = fatalities_battles/pop,
         fatalities_violence_rat = fatalities_violence/pop,
         fatalities_explosions_rat = fatalities_explosions/pop)

model_explosions <- lm(diff ~ no_explosions, data = war_gdp)
model_battles <- lm(diff ~ no_battles, data = war_gdp)
model_violence <- lm(diff ~ no_violence, data = war_gdp)
model_full <- lm(gdp_prediction ~ fatalities_battles + fatalities_violence + no_peaceful + real_gdp, data = war_gdp)

model_explosions_fat <- lm(diff ~ fatalities_explosions, data = war_gdp)
model_battles_fat <- lm(diff ~ fatalities_battles, data = war_gdp)
model_violence_fat <- lm(diff ~ fatalities_violence, data = war_gdp)
model_full_fat <- lm(diff ~ fatalities_explosions + fatalities_battles + fatalities_violence, data = war_gdp)

summary(model_explosions)
summary(model_battles)
summary(model_violence)
summary(model_full)

summary(model_explosions_fat)
summary(model_battles_fat)
summary(model_violence_fat)
summary(model_full_fat)

# check the correlation between the variables
cor(war_gdp %>% select(fatalities_explosions, fatalities_violence, no_peaceful))

# plot battles against gdp change
ggplot(war_gdp, aes(x = fatalities_explosions, y = diff)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Battles vs GDP change",
       x = "Number of battles",
       y = "GDP change (%)")

    
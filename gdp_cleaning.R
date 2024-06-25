library(tidyverse)
library(readxl)
library(stringr)

setwd("~/LSE/Dissertation/masters-diss")

# read gdp data, select the relevant table
gdp_poland <- read_excel("data/gdp_poland.xlsx", sheet = 2)
gdp_poland_nominal <- read_excel("data/gdp_poland_nominal.xlsx", sheet = 2)
gdp_ukraine <- read_excel("data/gdp_ukraine.xls", sheet = 1)

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

poland_names <- c(
  'Dolnośląskie' = 'Dolnoslaskie',
  'Kujawsko-Pomorskie' = 'Kujawsko-Pomorskie',
  'Lubelskie' = 'Lubelskie',
  'Lubuskie' = 'Lubuskie',
  'Łódzkie' = 'Lodzkie',
  'Małopolskie' = 'Malopolskie',
  'Mazowieckie' = 'Mazowieckie',
  'Opolskie' = 'Opolskie',
  'Podkarpackie' = 'Podkarpackie',
  'Podlaskie' = 'Podlaskie',
  'Pomorskie' = 'Pomorskie',
  'Śląskie' = 'Slaskie',
  'Świętokrzyskie' = 'Swietokrzyskie',
  'Warmińsko-Mazurskie' = 'Warminsko-Mazurskie',
  'Wielkopolskie' = 'Wielkopolskie',
  'Zachodniopomorskie' = 'Zachodniopomorskie'
)

# take relevant columns and rows, rename
gdp_ukraine <- gdp_ukraine[c(4:32), c(28:37, 93, 10)]
names(gdp_ukraine) <- c("y2012", "y2013", "y2014", "y2015", "y2016", "y2017", "y2018", "y2019", "y2020", "y2021", "region", "nominal_2012")
gdp_ukraine$region <- ukraine_names[gdp_ukraine$region]
gdp_ukraine <- gdp_ukraine[!is.na(gdp_ukraine$region),]

gdp_poland <- gdp_poland[-c(1:2), -1]
names(gdp_poland) <- c("region", "y2012", "y2013", "y2014", "y2015", "y2016", "y2017", "y2018", "y2019", "y2020", "y2021", "y2022")
gdp_poland$region <- str_to_title(tolower(gdp_poland$region))
gdp_poland$region <- poland_names[gdp_poland$region]

# attach nominal gdp for Poland
gdp_poland_nominal <- gdp_poland_nominal[-c(1:2), c(2, 3)]
names(gdp_poland_nominal) <- c("region", "nominal_2012")
gdp_poland_nominal$region <- str_to_title(tolower(gdp_poland_nominal$region))
gdp_poland_nominal$region <- poland_names[gdp_poland_nominal$region]
gdp_poland <- merge(gdp_poland, gdp_poland_nominal, by = "region")


# function to calculate real gdp in each year, in 2012 prices and local currency
calculate_gdp <- function(data, country) {
  
  data <- data %>% 
    select(region, nominal_2012, everything())
  
  # Convert all columns except for region to numeric
  data[, -1] <- lapply(data[, -1], as.numeric)
  
  data$y2012 <- 1
  
  # Divide each column from 100
  years <- paste0("y", 2013:2021)
  
  if (country == "pol") {
    years <- c(years, "y2022")
  }
  
  data[years] <- data[years] / 100
  
  # calculate real gdp in 2012 prices
  for (i in 2:length(years)) {
    data[[years[i]]] <- data[[years[i]]] * data[[years[i-1]]] 
  }
  
  years <- c("y2012", years)
  
  # multiply by nominal gdp in 2012
  data[years] <- data[years] * data$nominal_2012
  
  # add years 2022 and 2023 for ukraine, 2023 for poland
  if (country == "ukr") {
    data$y2022 <- NA
    data$y2023 <- NA
  } else if (country == "pol") {
    data$y2023 <- NA
  }
  
  # convert to long format
  data <- data %>% select(-nominal_2012)
  
  data <- data %>% 
    pivot_longer(cols = -region, names_to = "year", values_to = "real_gdp")
  
  # delete y from the year column
  data$year <- str_remove(data$year, "y")
  
  
  return(data)
}

gdp_poland_clean <- calculate_gdp(gdp_poland, "pol") 
gdp_ukraine_clean <- calculate_gdp(gdp_ukraine, "ukr") 

# save data
write_csv(gdp_poland_clean, "data/gdp_poland_clean.csv")
write_csv(gdp_ukraine_clean, "data/gdp_ukraine_clean.csv")



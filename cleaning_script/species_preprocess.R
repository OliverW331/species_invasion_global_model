setwd("D:/Environment Honor Thesis/global_model/raw_data")
library(readxl) 
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
country_codes_V202301 <- read_csv("D:/Environment Honor Thesis/global_model/raw_data/BACI_trade/country_codes_V202301.csv")
country_codes <- data.frame(
  Country = c("Aland Islands", "Amsterdam Island", "Andaman and Nicobar Islands", "Anticosti Island", "Ascension",
              "Azores", "Balearic Islands", "Biak", "Bolivia", "Bonaire", "Bosnia and Herzegovina", "Canary Islands",
              "Cape Verde", "Chagos Archipelago", "Channel Islands", "Christmas Island", "Clipperton Island",
              "Cocos (Keeling) Islands", "Congo, Democratic Republic of the", "Corse", "Cote D'Ivoire", "Crete",
              "Crozet Islands Group", "Curacao", "Czech Republic", "Falkland Islands", "Faroe Islands",
              "Fernando De Noronha", "France", "French Guiana", "Galapagos", "Guadeloupe", "Hawaiian Islands",
              "Hong Kong", "Izu Islands", "Kerguelen Islands", "Kermadec Islands", "Laos", "Liechtenstein",
              "Lord Howe Island", "Macao", "Macedonia", "Madeira", "Martinique", "Micronesia, Federated States of",
              "Moldova", "Monaco", "Norfolk Island", "North Korea", "Norway", "Ogasawara Islands",
              "Palestinian Territory, Occupied", "Pitcairn Islands", "Puerto Rico", "Reunion", "Rodriguez Island",
              "Russia", "Ryukyu Islands", "Saint Barthelemy", "Saint Martin", "Saint Paul (France)", "Sardinia",
              "Scattered Islands", "Sea of Cortez Islands", "Shetland Islands", "Sicily", "Sint Maarten",
              "Socotra Island", "South Georgia and the South Sandwich Islands", "South Korea", "Sulawesi",
              "Svalbard and Jan Mayen", "Switzerland", "Taiwan", "Tanzania", "Tasmania", "Timor Leste",
              "Tristan da Cunha", "Turks and Caicos", "United States of America", "US Minor Outlying Islands",
              "Vancouver Island", "Vietnam", "Virgin Islands, US", "Wallis and Futuna", "Western Sahara",
              "Zanzibar Island"),
  ISO3 = c("ALA", "ATF", "AND", "CAN", "ASC", "AZE", "BAL", "IDN", "BOL", "BES", "BIH", "CAN", "CPV", "CHG", "CHI",
           "CXR", "CLP", "CCK", "COD", "FRA", "CIV", "GRC", "TAF", "CUW", "CZE", "FLK", "FRO", "FEN", "FRA", "GUF",
           "GAL", "GLP", "HAW", "HKG", "JPN", "KER", "KER", "LAO", "LIE", "LHI", "MAC", "MKD", "MDE", "MTQ", "FSM",
           "MDA", "MCO", "NFK", "PRK", "NOR", "JPN", "PSE", "PCN", "PRI", "REU", "ROD", "RUS", "JPN", "BLM", "MAF",
           "SPM", "ITA", "SCA", "SEA", "SHE", "ITA", "SXM", "YEM", "SGS", "KOR", "IDN", "SJM", "CHE", "TWN", "TZA",
           "AUS", "TLS", "SHN", "TCA", "USA", "UMI", "CAN", "VNM", "VIR", "WLF", "ESH", "TZA")
)

sp <- read_excel("species_data/Hanno_database.xlsx")
sp <- sp %>%
  select(Region, Taxon, FirstRecord) %>%
  mutate(Region = ifelse(Region == "USACanada", "United States of America&Canada", Region)) %>%
  separate_rows(Region, sep = "&")
  
sp$ISO3 <- ifelse(sp$Region %in% country_codes$Country, country_codes$ISO3[match(sp$Region, country_codes$Country)], sp$Region)
sp$ISO3 <- ifelse(sp$ISO3 %in% country_codes_V202301$country_name_full, country_codes_V202301$iso_3digit_alpha[match(sp$ISO3, country_codes_V202301$country_name_full)], sp$ISO3)

n_sp <- sp %>%
  group_by(ISO3, Taxon) %>%
  summarize(FirstRecord = max(FirstRecord)) %>%
  spread(Taxon, FirstRecord)

n_sp <- as.data.frame(n_sp)  # Convert n_sp to a data frame
rownames(n_sp) <- n_sp$ISO3
saveRDS(n_sp, "formatted/species_data.rds")

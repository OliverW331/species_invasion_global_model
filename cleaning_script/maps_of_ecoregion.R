rm(list=ls());gc()
# Install and load the required packages
setwd("D:/Environment Honor Thesis/global_model")
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(countrycode)
library(dplyr)
# List of regions with ISO3 country codes
socio_ecoregions = readRDS("D:/Environment Honor Thesis/global_model/cleaned_data/socio_ecoregions.rds")


custom_match <- c(
  'USA' = 'USA', 'GBR' = 'UK',
  "AIA" = "Anguilla", "ATA" = "Antarctica", "ATF" = "French Southern and Antarctic Lands",
  "ATG" = "Antigua", "ATG" = "Barbuda", "BIH" = "Bosnia and Herzegovina",
  "BLM" = "Saint Barthelemy", "CIV" = "Ivory Coast",
  "COD" = "Democratic Republic of the Congo", "COG" = "Republic of Congo",
  "COK" = "Cook Islands", "CUW" = "Curacao", "CZE" = "Czech Republic",
  "ESP" = "Spain", "FLK" = "Falkland Islands", "REU" = "Reunion",
  "MYT" = "Mayotte", "GUF" = "French Guiana", "MTQ" = "Martinique",
  "GLP" = "Guadeloupe", "FRO" = "Faroe Islands", "FSM" = "Micronesia",
  "GGY" = "Guernsey", "HMD" = "Heard Island", "IMN" = "Isle of Man",
  "CCK" = "Cocos Islands", "CXR" = "Christmas Island", "IOT" = "Chagos Archipelago",
  "JEY" = "Jersey", "KAS" = "Siachen Glacier", "KNA" = "Nevis",
  "KNA" = "Saint Kitts", "XKX" = "Kosovo", "LCA" = "Saint Lucia",
  "LIE" = "Liechtenstein", "MAF" = "Saint Martin", "MCO" = "Monaco",
  "MMR" = "Myanmar", "MSR" = "Montserrat", "NFK" = "Norfolk Island",
  "NIU" = "Niue", "BES" = "Bonaire", "BES" = "Sint Eustatius",
  "BES" = "Saba", "PCN" = "Pitcairn Islands", "PNG" = "Papua New Guinea",
  "PRI" = "Puerto Rico", "PRT" = "Portugal",
  "PSE" = "Palestine", "ESH" = "Western Sahara", "SSD" = "South Sudan",
  "SGS" = "South Sandwich Islands", "SGS" = "South Georgia", "SHN" = "Saint Helena",
  "ASC" = "Ascension Island", "SPM" = "Saint Pierre and Miquelon",
  "STP" = "Sao Tome and Principe", "SWZ" = "Swaziland",
  "TCA" = "Turks and Caicos Islands", "TTO" = "Trinidad", "TTO" = "Tobago",
  "TWN" = "China", "VAT" = "Vatican", "VCT" = "Grenadines",
  "VCT" = "Saint Vincent", "VGB" = "Virgin Islands", "WLF" = "Wallis and Futuna"
)
# Function to convert ISO3 codes to country names
iso3_to_country <- function(iso3_codes) {
  country_names <- countrycode(iso3_codes, origin = "iso3c", destination = "country.name", custom_match = custom_match)
  return(country_names)
}

# Convert ISO3 codes to country names for each region
regions_with_names <- lapply(socio_ecoregions, iso3_to_country)

world_map <- map_data("world")
region_map <- map_data("world")

# Function to replace region with corresponding label
replace_with_label <- function(region) {
  for (reg_label in names(regions_with_names)) {
    if (region %in% regions_with_names[[reg_label]]) {
      return(reg_label)
    }
  }
  return(NA)
}

region_map$region <- sapply(region_map$region, replace_with_label)

eco_map = 
  ggplot(data = region_map) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) + 
  guides(fill=FALSE)

eco_map
saveRDS(regions_with_names, "cleaned_data/ecoregions_with_names.rds")
saveRDS(region_map, "cleaned_data/ecoregion_map.rds")

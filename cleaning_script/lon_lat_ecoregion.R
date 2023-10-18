rm(list=ls());gc()
graphics.off()
setwd("D:/Environment Honor Thesis/global_model")

library(ggplot2)
library(maps)
library(mapdata)
library(countrycode)
library(dplyr)
library(geosphere)
ecoregion_with_names = readRDS("cleaned_data/ecoregions_with_names.rds")
socio_ecoregions = readRDS("cleaned_data/socio_ecoregions.rds")
pop = readRDS("cleaned_data/pop.rds")

#calculate the mean latitude and longitude for each ecoregion
pop_df <- as.data.frame(pop)
pop_df$Region <- NA

# Loop through each region and assign region labels to countries
for (region_label in names(socio_ecoregions)) {
  countries <- socio_ecoregions[[region_label]]
  pop_df$Region[pop_df$Country.Code %in% countries] <- region_label
}


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

# Step 1: Obtain latitude and longitude data for each country
calculate_mean_lat_long <- function(pop_df, iso3_to_region, world_map) {
  # Merge the population data with the world map data based on ISO3 code
  merged_data <- merge(world_map, pop_df, by.x = "region", by.y = "Country.Code")
  
  # Calculate the weighted mean latitude and longitude for each region
  weighted_mean_data <- merged_data %>%
    group_by(region) %>%
    summarize(mean_lat = sum(lat * X1995 / sum(X1995)),
              mean_long = sum(long * X1995 / sum(X1995)))
  
  return(weighted_mean_data)
}

#reform world_map
world_map <- map_data("world")

mean_world_map <- world_map %>%
  group_by(region) %>%
  summarize(mean_lat = sum(lat * !is.na(long)) / sum(!is.na(long)),
            mean_long = sum(long * !is.na(lat)) / sum(!is.na(lat)))



# Step 2: Merge the 'pop_df' data frame with latitude and longitude data
pop_df<- pop_df %>%
  mutate(Country.Name = iso3_to_country(Country.Code))

pop_df$Total_Population <- rowSums(pop_df[, -c(1, ncol(pop_df)-1, ncol(pop_df))])
pop_df <- pop_df %>%
  group_by(Region) %>%
  mutate(Region_total_population = sum(Total_Population)) %>%
  ungroup()
pop_df <- pop_df %>%
  mutate(Weight = Total_Population / Region_total_population)




merged_data <- merge(pop_df, mean_world_map, by.x = "Country.Name", by.y = "region", all.x = TRUE)

# Step 3: Calculate the weighted latitude and longitude for each region
mean_lat_lon_by_region <- function(region_data) {
  weighted_lat <- sum(region_data$mean_lat * region_data$Weight, na.rm = TRUE)
  weighted_lon <- sum(region_data$mean_long * region_data$Weight, na.rm = TRUE)
  return(data.frame(Weighted_Latitude = weighted_lat, Weighted_Longitude = weighted_lon))
}

# Create an empty data frame to store the mean latitude and longitude for each region
mean_lat_lon_df <- data.frame(Region = character(), Weighted_Latitude = numeric(), Weighted_Longitude = numeric(), stringsAsFactors = FALSE)

# Loop through each region, calculate the mean latitude and longitude, and store the results in 'mean_lat_lon_df'
for (region_label in names(socio_ecoregions)) {
  countries <- socio_ecoregions[[region_label]]
  region_data <- merged_data[merged_data$Region == region_label, ]
  
  # Calculate the weighted latitude and longitude for the current region
  region_mean_lat_lon <- mean_lat_lon_by_region(region_data)
  
  # Add the region label to the data frame
  region_mean_lat_lon$Region <- region_label
  
  # Append the result to the 'mean_lat_lon_df' data frame
  mean_lat_lon_df <- rbind(mean_lat_lon_df, region_mean_lat_lon)
}

###use mean_lat_lon_df to calculate the great circle distance
calculate_distance <- function(region1, region2, data) {
  lat_lon1 <- data[data$Region == region1, c("Weighted_Longitude", "Weighted_Latitude")]
  lat_lon2 <- data[data$Region == region2, c("Weighted_Longitude", "Weighted_Latitude")]
  return(distGeo(lat_lon1, lat_lon2))
}

regions <- unique(mean_lat_lon_df$Region)
distance_matrix <- matrix(NA, nrow = length(regions), ncol = length(regions), dimnames = list(regions, regions))
for (i in 1:length(regions)) {
  for (j in 1:length(regions)) {
    distance_matrix[i, j] <- calculate_distance(regions[i], regions[j], mean_lat_lon_df)
  }
}

saveRDS(distance_matrix, "cleaned_data/biregional_dis.rds")

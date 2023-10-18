rm(list=ls());gc()

gdp = readRDS("D:/Environment Honor Thesis/global_model/cleaned_data/gdp.rds")
pop = readRDS("D:/Environment Honor Thesis/global_model/cleaned_data/pop.rds")

#classify countries into socio-ecoregions: Based on Sardain's paper
# Define the socio-ecoregions and associated countries
#Only need to change socio_ecoregions if the classification needs modification
socio_ecoregions <- list(
  "North America" = c("CAN", "USA", "GRL"),
  "Central America, Caribbean, & Northern South America" = c("VGB", "TCA", "SXM", "PAN", "KNA", "GRD", "DMA", "CYM", "CUW", "BMU", "ABW", "ATG", "BHS", "BRB", "BLZ", "BRA", "COL", "CRI", "CUB", "DOM", "SLV", "GTM", "GUY", "HTI", "HND", "JAM", "MEX", "NIC", "LCA", "VCT", "SUR", "TTO", "VEN"),
  "Southern South America" = c("PRY", "BOL", "ARG", "CHL", "ECU", "PER", "URY"),
  "Northern Europe" = c("SVK", "LUX", "IRL", "HUN", "CZE", "CHE", "BLR", "BEL", "DNK", "EST", "FIN", "FRA", "DEU", "ISL", "LVA", "LTU", "NLD", "NOR", "POL", "SWE", "GBR"),
  "Mediterranean" = c("SRB", "SMR", "MKD", "GIB", "AUT", "ALB", "AND", "BIH", "DZA", "HRV", "CYP", "GRC", "ISR", "ITA", "LBN", "LBY", "MLT", "MNE", "MAR", "PRT", "SVN", "ESP", "SYR", "TUN", "TUR"),
  "Western Africa" = c("TCD", "MLI", "COD", "CAF", "AGO", "BFA", "BEN", "CPV", "CMR", "CIV", "GNQ", "GAB", "GMB", "GHA", "GIN", "GNB", "LBR", "MRT", "NER", "NGA", "STP", "SEN", "SLE", "TGO"),
  "Southern Africa" = c("ZWE", "ZMB", "SWZ", "NAM", "ZAF", "BWA", "LSO"),
  "Eastern Africa" = c("SOM", "UGA", "SYC", "RWA", "MWI", "ETH", "BDI", "COM", "KEN", "MDG", "MUS", "MOZ", "TZA"),
  "South Asia and Northern Africa" = c("NPL", "PSE", "EGY", "BTN","AZE", "AFG", "BHR", "BGD", "DJI", "ERI", "IND", "IRN", "IRQ", "JOR", "KWT", "MDV", "MMR", "OMN", "PAK", "QAT", "SAU",  "LKA", "SDN", "ARE", "YEM"),
  "Southeast Asia" = c("GUM", "BRN","PNG", "LAO","KHM", "IDN", "MYS", "PHL", "SGP", "SLB", "THA", "TLS", "VNM"),
  "Australia and New Zealand" = c("AUS", "NZL"),
  "Northeast Asia" = c("MAC", "PRK", "MNG", "CHN", "HKG", "JPN", "KOR"),
  "Eastern Indo-Pacific" = c("ASM", "FSM", "KIR", "MHL", "MNP", "NRU", "PLW", "TUV", "FJI", "PYF", "NCL", "WSM", "TON", "VUT"),
  "Russia and Central Asia" = c("TKM", "RUS", "UZB", "TJK", "KAZ", "KGZ"),
  "Black Sea Regions" = c("MDA", "BGR", "GEO", "ROU", "UKR", "ARM")
)

# Create a new dataframe to store regional GDP
regional_gdp <- data.frame(socio_eco = character(), stringsAsFactors = FALSE)

# Create a new dataframe to store regional population
regional_pop <- data.frame(socio_eco = character(), stringsAsFactors = FALSE)

# Loop through each socio-ecoregion
for (region in names(socio_ecoregions)) {
  countries <- socio_ecoregions[[region]]
  
  # Subset the gdp dataframe based on the countries in the current region
  gdp_subset <- gdp[gdp$Country.Code %in% countries, ]
  
  # Calculate the sum of GDP for the current region (across all years)
  gdp_sum <- colSums(gdp_subset[, -1], na.rm = TRUE)
  
  # Create a new row for the current region in the regional_gdp dataframe
  row <- c(region, gdp_sum)
  regional_gdp <- rbind(regional_gdp, row)
  
  # Subset the pop dataframe based on the countries in the current region
  pop_subset <- pop[pop$Country.Code %in% countries, ]
  
  # Calculate the sum of population for the current region (across all years)
  pop_sum <- colSums(pop_subset[, -1], na.rm = TRUE)
  
  # Create a new row for the current region in the regional_pop dataframe
  row <- c(region, pop_sum)
  regional_pop <- rbind(regional_pop, row)
}

# Rename the columns in regional_gdp and regional_pop based on years
colnames(regional_gdp)[-1] <- colnames(gdp)[-1]
colnames(regional_pop)[-1] <- colnames(pop)[-1]
colnames(regional_gdp)[1] <- "socio-ecoregion"
colnames(regional_pop)[1] <- "socio-ecoregion"
# Print the resulting dataframes
print(regional_gdp)
print(regional_pop)

regional_gdp[, -1] <- lapply(regional_gdp[, -1], as.numeric)
regional_pop[, -1] <- lapply(regional_pop[, -1], as.numeric)
saveRDS(regional_gdp, "D:/Environment Honor Thesis/global_model/cleaned_data/regional_gdp.rds")
saveRDS(regional_pop, "D:/Environment Honor Thesis/global_model/cleaned_data/regional_pop.rds")
saveRDS(socio_ecoregions, "D:/Environment Honor Thesis/global_model/cleaned_data/socio_ecoregions.rds")


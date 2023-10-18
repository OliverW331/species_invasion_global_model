rm(list=ls());gc()
library(dplyr)
sp = readRDS("D:/Environment Honor Thesis/global_model/cleaned_data/sp.rds")
regions = readRDS("D:/Environment Honor Thesis/global_model/cleaned_data/socio_ecoregions.rds")

find_smallest_first_occurrence <- function(species_first_occurrence, country_to_region) {


  # Initialize an empty list to store results for each region
  result_list <- list()

  # Iterate through each region
  for (region_name in names(country_to_region)) {
    # Get the list of country names in the current region
    countries_in_region <- country_to_region[[region_name]]

    # Filter the species_first_occurrence dataframe to include only countries in the current region
    region_data <- species_first_occurrence %>%
      filter(ISO3 %in% countries_in_region)

    # Find the smallest first occurrence year for each species in the current region
    region_result <- region_data %>%
      select(-ISO3) %>%  # Exclude the "Country" column
      summarise(across(.cols = everything(), ~min(., na.rm = TRUE)))  # Calculate the minimum for all columns

    region_result <- region_result %>%
      mutate(across(.cols = everything(), ~replace(., is.infinite(.), NA)))

    # Append the result for the current region to the result_list
    result_list[[region_name]] <- region_result
  }

  # Combine all the results into a single dataframe
  result_df <- do.call(rbind, result_list)

  return(result_df)
}


# Call the function with the provided sample data
regionalized_sp <- find_smallest_first_occurrence(sp, regions)
saveRDS(regionalized_sp,"D:/Environment Honor Thesis/global_model/cleaned_data/regional_sp.rds")


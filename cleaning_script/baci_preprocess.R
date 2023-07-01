rm(list=ls());gc()
graphics.off()
library(readr)
library(dplyr)
library(abind)
setwd("D:/Environment Honor Thesis/global_model/raw_data/BACI_trade")
file_list <- list.files(pattern = "BACI")
file_list <- file_list[-((length(file_list)-4):length(file_list))]
data_list <- list()
for (file in file_list) {
  data_list[[file]] = read_csv(file)
}
names(data_list) <- as.character(seq(1995, 2016))

country_list = read.csv('country_codes_V202301.csv')


data_list <- lapply(data_list, function(obj) {
  obj %>%
    group_by(i, j) %>%
    summarise(new_col = sum(v)) %>%
    ungroup()
})


countries = unique(data_list[["2016"]]$i)
countries = country_list$iso_3digit_alpha[match(countries, country_list$country_code)]
nc = length(unique(data_list[["2016"]]$i))

result_list = list()
for (k in names(data_list)) {
  current_data <- data_list[[k]]  # Get the current data frame
  
  # Get unique values of 'i' and 'j' for the current data frame
  rows <- unique(current_data$i)
  cols <- unique(current_data$j)
  
  # Create an empty matrix with dimensions based on unique values
  result <- matrix(NA, nrow = length(rows), ncol = length(cols))
  
  # Assign values from 'v' to corresponding positions in the matrix
  for (i in seq_along(current_data$i)) {
    row_index <- match(current_data$i[i], rows)
    col_index <- match(current_data$j[i], cols)
    result[row_index, col_index] <- current_data$new_col[i]
  }
  rownames(result) <- rows
  colnames(result) <- cols
  
  result <- result[, order(as.numeric(colnames(result)))]
  result <- result[order(as.numeric(rownames(result))), ]
  
  row_names = rownames(result)
  col_names = colnames(result)
  
  row_indices <- match(row_names, country_list$country_code)
  row_names_new <- country_list$iso_3digit_alpha[row_indices]
  rownames(result) <- row_names_new
  
  col_indices <- match(col_names, country_list$country_code)
  col_names_new <- country_list$iso_3digit_alpha[col_indices]
  colnames(result) <- col_names_new
  
  #row: exporter; col: importer
  #create a output matrix
  final_m = matrix(NA, nrow = nc, ncol = nc)
  rownames(final_m) = countries
  colnames(final_m) = countries
  
  for (i in seq_along(row_names_new)) {
    for (j in seq_along(col_names_new)) {
      row_index <- match(row_names_new[i], countries)
      col_index <- match(col_names_new[j], countries)
      final_m[row_index, col_index] <- result[i, j]
    }
  }
  
  result_list[[k]] <- final_m
}
result_array <- abind(result_list, along = 3)

saveRDS(result_array, "../formatted/bilateral_trade.rds")


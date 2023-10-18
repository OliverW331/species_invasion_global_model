rm(list=ls());gc()
setwd("D:/Environment Honor Thesis/global_model/raw_data")
library(dplyr)
library(tidyr)
library(abind)
bi_dis = readRDS("bilateral_distance/Gravity_V202211.rds")

bi_dis = select(bi_dis, year, iso3_o, iso3_d, dist)
bi_dis <- subset(bi_dis, year == 2016)

# Get unique iso3_o, iso3_d, and year values
iso3_o <- unique(bi_dis$iso3_o)
iso3_d <- unique(bi_dis$iso3_d)
years <- unique(bi_dis$year)
dist_matrix <- array(NA, dim = c(length(iso3_o), length(iso3_d)),
                     dimnames = list(iso3_o, iso3_d))
dist_matrix <- abind(dist_matrix, along = 3) 
gc()
for (i in 1:nrow(bi_dis)) {
  iso_o_idx <- match(bi_dis$iso3_o[i], iso3_o)
  iso_d_idx <- match(bi_dis$iso3_d[i], iso3_d)
  year_idx <- match(bi_dis$year[i], years)
  dist <- bi_dis$dist[i]

  dist_matrix[iso_o_idx, iso_d_idx, year_idx] <- dist
}


saveRDS(dist_matrix, "formatted/bilateral_distance.rds")

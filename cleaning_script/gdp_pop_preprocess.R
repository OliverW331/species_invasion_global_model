rm(list=ls());gc()
graphics.off()
setwd("D:/Environment Honor Thesis/global_model/raw_data")

library(abind) # For abind
library(tidyr) # For spread()
library(readr)
country_codes_V202301 <- read_csv("BACI_trade/country_codes_V202301.csv")
pop = read.csv("popolation/global_population.csv", stringsAsFactor = FALSE)
pop = pop[,c("Country.Code", paste0("X",1995:2016))]
rownames(pop) = pop$Country.Code

gdp = read.csv("GDP/gdp.csv", stringsAsFactors = FALSE)
gdp = gdp[,c("Country.Code", paste0("X",1995:2016))]
rownames(gdp) = gdp$Country.Code

filtered_gdp <- gdp[gdp$Country.Code %in% country_codes_V202301$iso_3digit_alpha, ]
filtered_pop <- pop[pop$Country.Code %in% country_codes_V202301$iso_3digit_alpha, ]



saveRDS(pop, "formatted/pop.rds")
saveRDS(gdp, "formatted/gdp.rds")

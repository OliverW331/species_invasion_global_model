rm(list=ls());gc()
graphics.off()
setwd("D:/Environment Honor Thesis/global_model/raw_data/formatted")
gdp = readRDS("gdp.rds")
pop = readRDS("pop.rds")
bi_dis = readRDS("bilateral_distance.rds")
bi_tra = readRDS("bilateral_trade.rds")
sp = readRDS("species_data.rds")
country = sp$ISO3
country = country[(country %in% gdp$Country.Code)]
country = country[(country %in% rownames(bi_dis))]
country = country[(country %in% rownames(bi_tra))]

sp = sp[country, ]
gdp = gdp[country, ]
pop = pop[country, ]
bi_dis = bi_dis[country, country, ]
bi_tra = bi_tra[country, country, ]

saveRDS(sp, "D:/Environment Honor Thesis/global_model/cleaned_data/sp.rds")
saveRDS(gdp, "D:/Environment Honor Thesis/global_model/cleaned_data/gdp.rds")
saveRDS(pop, "D:/Environment Honor Thesis/global_model/cleaned_data/pop.rds")
saveRDS(bi_dis, "D:/Environment Honor Thesis/global_model/cleaned_data/bi_dis.rds")
saveRDS(bi_tra, "D:/Environment Honor Thesis/global_model/cleaned_data/bi_tra.rds")



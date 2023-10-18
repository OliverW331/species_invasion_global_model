library(ggplot2)
library(tidyr)
rm(list=ls());gc()
graphics.off()
setwd("D:/Environment Honor Thesis/global_model/model/model_data/regionalized_model_data")

fs = readRDS("C-firstSightings-matrix.rds")
year_counts <- table(as.vector(fs))


cumulative_counts <- cumsum(year_counts)
df <- data.frame(year=as.numeric(names(cumulative_counts)), cumulative_count=as.numeric(cumulative_counts))

ggplot(df, aes(x=year, y=cumulative_count)) +
  geom_point() +
  geom_line(group=1) +
  labs(title="Cumulative Number of First Records by Year", x="Year", y="Cumulative Number of Records") +
  theme_bw()
ggsave(paste0("first_sight_graph/", "first_record_acc.png"))

year_counts <- year_counts[-1]
df <- data.frame(year=as.numeric(names(year_counts)), count=as.numeric(year_counts))

ggplot(df, aes(x=year, y=count)) +
  geom_point() +
  geom_line(group=1) +
  labs(title="Number of First Record by Year", x="Year", y="Number of Records") +
  theme_bw()
  ggsave(paste0("first_sight_graph/", "first_record.png"))
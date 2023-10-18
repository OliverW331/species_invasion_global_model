library(ggplot2)
library(tidyr)
rm(list=ls());gc()
graphics.off()
setwd("D:/Environment Honor Thesis/global_model/model/model_data/regionalized_model_data")



# Convert the matrix to an array
array_data <- readRDS("A-socioEcoDat-array.rds")

# Convert the array to a data frame
df <- as.data.frame(as.table(array_data))


df_long <- df %>%
  gather(variable, value, -Var1, -Var2, -Var3)

# ggplot(df_long, aes(x=Var1, y=value, color=Var3)) + 
#   geom_line() + 
#   facet_wrap(~Var2 + variable, scales="free_y") + 
#   labs(title="Time Series Plot for Each Region", x="Time", y="Value") +
#   theme_minimal()
for(region in unique(df$Var2)){
  subset_df <- df[df$Var2 == region,]
  for (v in unique(df$Var3)){
    # Plot GDP vs Time
    subsub_df <- subset_df[subset_df$Var3 == v,]
    
    p = ggplot(subsub_df, aes(x=Var1, y=Freq)) +
      geom_point() +
      ggtitle(paste(v, " vs Time for", region)) +
      xlab("Time") +
      ylab(v) +
      theme_bw()
    ggsave(paste0("gdp_pop_graph/", region, "_", v, ".png"))
  }
}

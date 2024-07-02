##### For visualizations and transformation. Mostly temp script.
library(dplyr)
vif_selected_df <- data.frame()
### 12 is the starting number of temp_list, here are the kind of normalizations that took place in previous steps.
for (i in temp_list){

  data <-read.csv(paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/normalization/vif_filtered_data_normalized_",i,".csv",sep = ""))
  data <- select(data,-"X")
  vif_selected_df <- c(data,vif_selected_df)
  #if (i == 12){
  #  vif_selected_df <- data
  #}
  #else{
  #  vif_selected_df <- merge(data, vif_selected_df, all = T)
  #}
    
}

write.csv(vif_selected_df,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/normalization/vif_filtered_data_normalized_REVIEW.csv")


vif_selected_df <- read.csv("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/normalization/vif_filtered_data_normalized_REVIEW.csv")
vif_selected_df_present <- vif_selected_df[grep("_nm11_",vif_selected_df$predictors),2:3] %>% arrange(desc(vif_value))

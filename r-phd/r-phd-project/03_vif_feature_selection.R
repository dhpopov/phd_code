# Correlation matrix
data_normalized <- read.csv("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/data_prep_csv_imp_normalized.csv")
correlation_matrix <- as.data.frame(cor(data_normalized[,3:136]))
typeof(correlation_matrix)
library(xlsx)
write.xlsx(correlation_matrix,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/correlation_matrix.xlsx")
####
names(data_normalized)

# vif calculated with faraway library.
library(faraway)
library(dplyr)
model <-  lm(not_survive ~ .,data_normalized[3:216])

vif_model_results<- as.data.frame(vif(model))
colnames(vif_model_results)[1] <- "vif_value"
write.csv(vif_model_results,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/vif_model_results.csv")

vif_filtered_data <- vif_model_results %>%  filter(vif_value<5) %>% arrange(desc(vif_value))

vif_filtered_data <- cbind(newColName = rownames(vif_filtered_data), vif_filtered_data) #index names to column
rownames(vif_filtered_data) <- 1:nrow(vif_filtered_data)
colnames(vif_filtered_data)[1]<-"predictors"

write.csv(vif_filtered_data,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/vif_filtered_data.csv")

mylist <- list()
for (i in vif_filtered_data[1])
{
  print(i)
  mylist <- c(i, mylist)
}
selected_columns <- unlist(mylist)
rm(mylist)

data_normalized_vif_selected <- cbind(data_normalized[,selected_columns],data_normalized[216])
write.csv(data_normalized_vif_selected, "D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/data_normalized_vif_selected.csv")

############ VIF on data normalized/not normalized/logarithm of. Test if VIF gives different results with
############ different data normalization/standardization.

# vanilla data
library(dplyr) 
data_vanilla <- data_normalized %>% select(-contains("_nm")) %>% select(-contains("_ln"))

model <- lm(not_survive ~ .,data_vanilla[3:87])

vif_model_results<- as.data.frame(vif(model))
colnames(vif_model_results)[1] <- "vif_value"
write.csv(vif_model_results,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/vif_model_results_vanilla.csv")

vif_filtered_data <- vif_model_results %>%  filter(vif_value<5) %>% arrange(desc(vif_value))

vif_filtered_data <- cbind(newColName = rownames(vif_filtered_data), vif_filtered_data) #index names to column
rownames(vif_filtered_data) <- 1:nrow(vif_filtered_data)
colnames(vif_filtered_data)[1]<-"predictors"

write.csv(vif_filtered_data,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/vif_filtered_data_vanilla.csv")

mylist <- list()
for (i in vif_filtered_data[1])
{
  print(i)
  mylist <- c(i, mylist)
}
selected_columns <- unlist(mylist)
rm(mylist)

data_vanilla_vif_selected <- cbind(data_vanilla[,selected_columns],data_vanilla[87])
write.csv(data_vanilla_vif_selected, "D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/data_vanilla_vif_selected.csv")

# ln data
data_ln_vanilla <- data_normalized %>% select(contains("_ln") | contains("not_survive"))
model <- lm(not_survive ~ .,data_ln_vanilla[3:46])

vif_model_results<- as.data.frame(vif(model))
colnames(vif_model_results)[1] <- "vif_value"
write.csv(vif_model_results,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/vif_model_results_ln_vanilla.csv")

vif_filtered_data <- vif_model_results %>%  filter(vif_value<10) %>% arrange(desc(vif_value))

vif_filtered_data <- cbind(newColName = rownames(vif_filtered_data), vif_filtered_data) #index names to column
rownames(vif_filtered_data) <- 1:nrow(vif_filtered_data)
colnames(vif_filtered_data)[1]<-"predictors"

write.csv(vif_filtered_data,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/vif_filtered_data_ln_vanilla.csv")

mylist <- list()
for (i in vif_filtered_data[1])
{
  print(i)
  mylist <- c(i, mylist)
}
selected_columns <- unlist(mylist)
rm(mylist)

data_ln_vanilla_vif_selected <- cbind(data_ln_vanilla[,selected_columns],data_ln_vanilla[46])
write.csv(data_ln_vanilla_vif_selected, "D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/data_ln_vanilla_vif_selected.csv")


# nm data
data_normalized_vanilla <- data_normalized %>% select(contains("_nm") | contains("not_survive"))

model <- lm(not_survive ~ .,data_normalized_vanilla[3:85])

vif_model_results<- as.data.frame(vif(model))
colnames(vif_model_results)[1] <- "vif_value"
write.csv(vif_model_results,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/vif_model_results_normalized_vanilla.csv")

vif_filtered_data <- vif_model_results %>%  filter(vif_value<5) %>% arrange(desc(vif_value))

vif_filtered_data <- cbind(newColName = rownames(vif_filtered_data), vif_filtered_data) #index names to column
rownames(vif_filtered_data) <- 1:nrow(vif_filtered_data)
colnames(vif_filtered_data)[1]<-"predictors"

write.csv(vif_filtered_data,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/vif_filtered_data_normalized_vanilla.csv")

mylist <- list()
for (i in vif_filtered_data[1])
{
  print(i)
  mylist <- c(i, mylist)
}
selected_columns <- unlist(mylist)
rm(mylist)

data_normalized_vanilla_vif_selected <- cbind(data_normalized_vanilla[,selected_columns],data_normalized_vanilla[85])
write.csv(data_normalized_vanilla_vif_selected, "D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/data_normalized_vanilla_vif_selected.csv")

############# Data all normalized #########

data_all_normalized <-read.csv ("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/normalization/data_all_normalized.csv")
rm(temp_list)
temp_list <- list(99)
temp_list <-c(33, temp_list)
for (nb in 1:12){
  temp_list <-c(nb, temp_list)
}

for (number in temp_list){
  
  num_char = as.character(number)
  print(num_char)
  #num_char = "1"
  data <- data_all_normalized  %>% select(contains(paste("_nm",num_char,"_",sep = "")) | contains("not_survive"))
  data <-data[ , colSums(is.na(data)) == 0] #remove all na columns
  model <- lm(not_survive ~ .,data,na.action = na.exclude)
  vif_model_results<- as.data.frame(vif(model))
  colnames(vif_model_results)[1] <- "vif_value"
  write.csv(vif_model_results, paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/normalization/vif_model_results_normalized_",num_char,".csv",sep = ""))
#}
  vif_filtered_data <- vif_model_results %>%  filter(vif_value<5) %>% arrange(desc(vif_value))
  
  vif_filtered_data <- cbind(newColName = rownames(vif_filtered_data), vif_filtered_data) #index names to column
  rownames(vif_filtered_data) <- 1:nrow(vif_filtered_data)
  colnames(vif_filtered_data)[1]<-"predictors"
  write.csv(vif_filtered_data,paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/normalization/vif_filtered_data_normalized_",num_char,".csv",sep = ""))
  
  mylist <- list()
  for (i in vif_filtered_data[1])
  {
    print(i)
    mylist <- c(i, mylist)
  }
  selected_columns <- unlist(mylist)
  rm(mylist)

  ##Check the length of data_all_normalized, the number should be the last column "not_survive"
  data_normalized_vif_selected <- cbind(data[,selected_columns],data_all_normalized[1179])
  write.csv(data_normalized_vif_selected, paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/normalization/data_normalized_",num_char,".csv",sep = ""))
  print(paste("ready: ",num_char,sep=""))
}

################################## ALL DATA #####################

data <- data_all_normalized[,3:1011]
data <-data[ , colSums(is.na(data)) == 0] #remove all na columns
model <- lm(not_survive ~ .,data)
vif_model_results<- as.data.frame(vif(model))
colnames(vif_model_results)[1] <- "vif_value"
vif_filtered_data <- vif_model_results %>%  filter(vif_value<0.0012) %>% arrange(desc(vif_value))

vif_filtered_data <- cbind(newColName = rownames(vif_filtered_data), vif_filtered_data) #index names to column
rownames(vif_filtered_data) <- 1:nrow(vif_filtered_data)
colnames(vif_filtered_data)[1]<-"predictors"
write.csv(vif_filtered_data,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/normalization/vif_filtered_all_data_normalized.csv")

mylist <- list()
for (i in vif_filtered_data[1])
{
  print(i)
  mylist <- c(i, mylist)
}
selected_columns <- unlist(mylist)
rm(mylist)

all_data_normalized_vif_selected <- cbind(data[,selected_columns],data_all_normalized[1011])
write.csv(all_data_normalized_vif_selected, "D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/normalization/all_data_normalized.csv")

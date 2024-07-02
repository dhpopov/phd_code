# Do the sampling. Keep the y variable original ratio in both samples.
library(caret)

list_data <- list('data_normalized_vanilla_vif_selected','data_ln_vanilla_vif_selected','data_vanilla_vif_selected')
for (i in list_data){
  
  print(i)
  data <- read.csv(paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/",i,".csv",sep=""))
  
  set.seed(123)
  training_index <- createDataPartition(data$not_survive, p = .7, 
                                        list = FALSE, 
                                        times = 1)
  train_sample <- data[training_index,]
  test_sample <- data[-training_index,]
  
  # data y variable ratio.
  sumdata <- sum(data$not_survive)
  sumdata/ nrow(data)
  # Train sample y variable ratio.
  sumTrain <- sum(train_sample$not_survive)
  sumTrain/nrow(train_sample)
  # Test sample y variable ratio.
  sumTest <- sum(test_sample$not_survive)
  sumTest / nrow(test_sample)
  
  
  #Export Sampling
  
  write.csv(train_sample,paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/train_sample_",i,".csv",sep=""),row.names = FALSE)
  write.csv(test_sample,paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/test_sample_",i,".csv",sep=""),row.names = FALSE)
}

################### Do the same for all normalized ##########################

list_data <- list()
for (numbers in temp_list){
  num_chars = as.character(numbers)
  list_data <- c(paste("data_normalized_",num_chars,sep = ""), list_data)
}
#print(list_data)

for (i in list_data){
  
  print(i)
  
  data <- read.csv(paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/normalization/",i,".csv",sep=""))
  
  set.seed(123)
  training_index <- createDataPartition(data$not_survive, p = .7, 
                                        list = FALSE, 
                                        times = 1)
  train_sample <- data[training_index,]
  test_sample <- data[-training_index,]
  
  train_sample<- train_sample %>% select(-"X")
  test_sample<- test_sample %>% select(-"X")
  # data y variable ratio.
  sumdata <- sum(data$not_survive)
  print(sumdata/ nrow(data))
  # Train sample y variable ratio.
  sumTrain <- sum(train_sample$not_survive)
  print(sumTrain/nrow(train_sample))
  # Test sample y variable ratio.
  sumTest <- sum(test_sample$not_survive)
  print(sumTest / nrow(test_sample))
  
  
  #Export Sampling
  
  write.csv(train_sample,paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/normalization/train_sample_",i,".csv",sep=""),row.names = FALSE)
  write.csv(test_sample,paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/normalization/test_sample_",i,".csv",sep=""),row.names = FALSE)
}

nrow(data)
nrow(train_sample)
nrow(test_sample)
################################## ALL DATA #####################

data <- read.csv("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/normalization/all_data_normalized.csv")

set.seed(123)
training_index <- createDataPartition(data$not_survive, p = .7, 
                                      list = FALSE, 
                                      times = 1)
train_sample <- data[training_index,]
test_sample <- data[-training_index,]

train_sample<- train_sample %>% select(-"X")
test_sample<- test_sample %>% select(-"X")
# data y variable ratio.
sumdata <- sum(data$not_survive)
sumdata/ nrow(data)
# Train sample y variable ratio.
sumTrain <- sum(train_sample$not_survive)
sumTrain/nrow(train_sample)
# Test sample y variable ratio.
sumTest <- sum(test_sample$not_survive)
sumTest / nrow(test_sample)


#Export Sampling

write.csv(train_sample,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/normalization/train_sample_all_data_normalized.csv",row.names = FALSE)
write.csv(test_sample,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/normalization/test_sample_all_data_normalized.csv",row.names = FALSE)

###### Try simple random sample base R #####
## All normalized in separate samples ###

list_data <- list()
for (numbers in temp_list){
  num_chars = as.character(numbers)
  list_data <- c(paste("data_normalized_",num_chars,sep = ""), list_data)
}
#print(list_data)

for (i in list_data){
  
  print(i)
  
  data <- read.csv(paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/correlation/normalization/",i,".csv",sep=""))
  
  set.seed(123)
  training_index <- createDataPartition(data$not_survive, p = .7, 
                                        list = FALSE, 
                                        times = 1)
  train_sample <- data[training_index,]
  test_sample <- data[-training_index,]
  
  train_sample<- train_sample %>% select(-"X")
  test_sample<- test_sample %>% select(-"X")
  # data y variable ratio.
  sumdata <- sum(data$not_survive)
  print(sumdata/ nrow(data))
  # Train sample y variable ratio.
  sumTrain <- sum(train_sample$not_survive)
  print(sumTrain/nrow(train_sample))
  # Test sample y variable ratio.
  sumTest <- sum(test_sample$not_survive)
  print(sumTest / nrow(test_sample))
  
  
  #Export Sampling

  write.csv(train_sample,paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/normalization/train_sample_",i,".csv",sep=""),row.names = FALSE)
  write.csv(test_sample,paste("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/normalization/test_sample_",i,".csv",sep=""),row.names = FALSE)
}
# Compare the samples
nrow(data)
nrow(train_sample)
nrow(test_sample)
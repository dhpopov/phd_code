
#Normalization
library(clusterSim)

data_prep_csv_imp <- read.csv("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/data_prep_csv_imp.csv")
# n4 - unitization with zero minimum ((x-min)/range)

data_normalizedTemp1 <- data.Normalization (data_prep_csv_imp[,3:86],type="n1",normalization="column")
data_normalizedTemp2 <- data.Normalization (data_prep_csv_imp[,3:86],type="n2",normalization="column")
data_normalizedTemp3 <- data.Normalization (data_prep_csv_imp[,3:86],type="n3",normalization="column")

data_normalizedTemp33 <- data.Normalization (data_prep_csv_imp[,3:86],type="n3a",normalization="column")

data_normalizedTemp4 <- data.Normalization (data_prep_csv_imp[,3:86],type="n4",normalization="column")
data_normalizedTemp5 <- data.Normalization (data_prep_csv_imp[,3:86],type="n5",normalization="column")
data_normalizedTemp6 <- data.Normalization (data_prep_csv_imp[,3:86],type="n6",normalization="column")
data_normalizedTemp7 <- data.Normalization (data_prep_csv_imp[,3:86],type="n7",normalization="column")
data_normalizedTemp8 <- data.Normalization (data_prep_csv_imp[,3:86],type="n8",normalization="column")
data_normalizedTemp9 <- data.Normalization (data_prep_csv_imp[,3:86],type="n9",normalization="column")
data_normalizedTemp99 <- data.Normalization (data_prep_csv_imp[,3:86],type="n9a",normalization="column")
data_normalizedTemp10 <- data.Normalization (data_prep_csv_imp[,3:86],type="n10",normalization="column")
data_normalizedTemp11 <- data.Normalization (data_prep_csv_imp[,3:86],type="n11",normalization="column")
data_normalizedTemp12 <- data.Normalization (data_prep_csv_imp[,3:86],type="n12",normalization="column")

colnames(data_normalizedTemp1)<-paste(colnames(data_normalizedTemp1),"nm1_",sep="_")
colnames(data_normalizedTemp2)<-paste(colnames(data_normalizedTemp2),"nm2_",sep="_")
colnames(data_normalizedTemp3)<-paste(colnames(data_normalizedTemp3),"nm3_",sep="_")

colnames(data_normalizedTemp33)<-paste(colnames(data_normalizedTemp33),"nm33_",sep="_")

colnames(data_normalizedTemp4)<-paste(colnames(data_normalizedTemp4),"nm4_",sep="_")
colnames(data_normalizedTemp5)<-paste(colnames(data_normalizedTemp5),"nm5_",sep="_")
colnames(data_normalizedTemp6)<-paste(colnames(data_normalizedTemp6),"nm6_",sep="_")
colnames(data_normalizedTemp7)<-paste(colnames(data_normalizedTemp7),"nm7_",sep="_")
colnames(data_normalizedTemp8)<-paste(colnames(data_normalizedTemp8),"nm8_",sep="_")
colnames(data_normalizedTemp9)<-paste(colnames(data_normalizedTemp9),"nm9_",sep="_")
colnames(data_normalizedTemp99)<-paste(colnames(data_normalizedTemp99),"nm99_",sep="_")
colnames(data_normalizedTemp10)<-paste(colnames(data_normalizedTemp10),"nm10_",sep="_")
colnames(data_normalizedTemp11)<-paste(colnames(data_normalizedTemp11),"nm11_",sep="_")
colnames(data_normalizedTemp12)<-paste(colnames(data_normalizedTemp12),"nm12_",sep="_")

data_all_normalized <- cbind(data_prep_csv_imp[,1:2],
                             data_normalizedTemp1,
                             data_normalizedTemp2,
                             data_normalizedTemp3,
                             
                             data_normalizedTemp33,
                             
                             data_normalizedTemp4,
                             data_normalizedTemp5,
                             data_normalizedTemp6,
                             data_normalizedTemp7,
                             data_normalizedTemp8,
                             data_normalizedTemp9,
                             data_normalizedTemp99,
                             data_normalizedTemp10,
                             data_normalizedTemp11,
                             data_normalizedTemp12, data_prep_csv_imp[132])

write.csv(data_all_normalized,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/normalization/data_all_normalized.csv",row.names = FALSE)

################################################################## New code ####
# Test if clusterSim and formula normalization return the same results.
# Export normalization coefficients so that data can be denormalized later on.

library(clusterSim)

# Subset the data frame
data_subset <- data_prep_csv_imp[, 3:86]

# Calculate the sum of squares (SSQ) for each column in the original data frame
ssq <- apply(data_subset, 2, function(x) sum(x^2))
sqrt_ssq <- sqrt(ssq)

# Normalize the original data frame using the formula x / sqrt(SSQ)
normalized_df_manual <- as.data.frame(lapply(1:ncol(data_subset), function(i) {
  data_subset[[i]] / sqrt(sum(data_subset[[i]]^2))
}))
colnames(normalized_df_manual) <- colnames(data_subset)

# Print  normalized data
print("Normalized Data Frame (Manual Calculation):")
print(head(normalized_df_manual$X2014_CAPAS))

# Denormalize the data using the formula x * sqrt(SSQ)
denormalized_df <- as.data.frame(lapply(1:ncol(normalized_df_manual), function(i) {
  normalized_df_manual[[i]] * sqrt(sum(data_subset[[i]]^2))
}))
colnames(denormalized_df) <- colnames(data_subset)

# Print the denormalized data frame
print("Denormalized Data Frame:")
print(head(denormalized_df$X2014_CAPAS))

# Normalize the original data using n11 method
normalized_df_clusterSim <- data.Normalization(data_subset, type="n11", normalization="column")

# Denormalize ClusterSim data
denormalized_df_clusterSim <- as.data.frame(lapply(1:ncol(normalized_df_clusterSim), function(i) {
  normalized_df_clusterSim[[i]] * sqrt(sum(data_subset[[i]]^2))
}))
colnames(denormalized_df_clusterSim) <- colnames(data_subset)

# coefficients for denormalization
coefficients_df <- data.frame(
  Variable = colnames(data_subset),
  Sqrt_SSQ = sqrt_ssq
)

# Transposed df
transposed_df <- t(coefficients_df)
transposed_df <- as.data.frame(transposed_df)
colnames(transposed_df) <- transposed_df[1, ]
transposed_df <- transposed_df[-1, ]

print("Transposed Coefficients Data Frame:")
print(head(transposed_df$X2014_CAPAS))

# Convert the transposed data frame row to a numeric vector
transposed_df_vector <- as.numeric(transposed_df["Sqrt_SSQ", ])

# Denormalize ClusterSim data using coefficients
denormalized_df_clusterSim_coeffs <- as.data.frame(mapply(function(col, coef) {
  col * coef
}, normalized_df_clusterSim, transposed_df_vector))

colnames(denormalized_df_clusterSim_coeffs) <- colnames(data_subset)

# Print results
print("Denormalized Data Frame Using Coefficients (ClusterSim):")
print(head(denormalized_df_clusterSim_coeffs$X2014_CAPAS))

print("Normalized Data Frame (ClusterSim):")
print(head(normalized_df_clusterSim$X2014_CAPAS))

print("Denormalized Data Frame (Manual Calculation):")
print(head(denormalized_df$X2014_CAPAS))

print("Denormalized Data Frame Using Coefficients (ClusterSim):")
print(head(denormalized_df_clusterSim_coeffs$X2014_CAPAS))

## Export results so that they can be used when denormalizing data at a later time
write.csv(coeff_df_transposed,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/normalization/coeff_df_transposed.csv",row.names = FALSE)

# Remove temp objects

rm(data_normalizedTemp1,
   data_normalizedTemp2,
   data_normalizedTemp3,
   
   data_normalizedTemp33,
   
   data_normalizedTemp4,
   data_normalizedTemp5,
   data_normalizedTemp6,
   data_normalizedTemp7,
   data_normalizedTemp8,
   data_normalizedTemp9,
   data_normalizedTemp99,
   data_normalizedTemp10,
   data_normalizedTemp11,
   data_normalizedTemp12)

library(readr)
#X2014GJMN <- read_delim("D:/PC_Archive/PHD/data_csv/2014GJMN.csv", 
#                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
# View(X2014GJMN)
# Data prep.
library(haven)
data_prep <- read_sav("D:/PC_Archive/PHD/spss-modeller - IBM Academic Initiative/popov-phd-data/3_data_preparation/data_prep.sav")
View(data_prep)

length(data_prep)

# rename columns:
library(xlsx)
column_names_df <- read.xlsx("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/varible_names/variables.xlsx",sheetName = "Sheet")
column_names <- as.list(column_names_df)
# View(column_names)
colnames(data_prep) <- unlist(column_names)



str(data_prep)
write.csv(data_prep,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/data_prep.csv",row.names =  FALSE)
data_prep_csv <-read.csv("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/data_prep.csv")
## Explore NA values in data:
sapply(data_prep_csv, function(x) sum(is.na(x)))


## Replace NA values:

#impute the missing data using RF regressions predictions
# md.pattern(data_prep)

tempData <- mice(data_prep_csv, m=2, maxit=4, meth='rf', seed=50)

data_prep_csv_imp <- complete(tempData, 1)

# Exclude the logarithms of these variables
#X2014_NIS
#X2014_ER_ln
#X2014_DEB_ln
#X2014_NT_ln
#X2014_NIS_ln
data_prep_csv_imp <- data_prep_csv_imp %>% select( -X2014_NIS,
       -X2014_ER_ln,
       -X2014_DEB_ln,
       -X2014_NT_ln,
       -X2014_NIS_ln,
)
# Export to csv:

write.csv(data_prep_csv_imp,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/data_prep_csv_imp.csv",row.names = FALSE)

sapply(data_prep_csv_imp, function(x) sum(is.na(x)))

summary(data_prep_csv_imp[,3:131])

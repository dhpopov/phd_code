data_normalized <- read.csv("D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/data_prep_csv_imp_normalized.csv")
data_trimmed <- cbind(data_normalized[,1:30],data_normalized[216])
library(tidyverse)
library(caret)
library(leaps)

library(MASS)
# Fit the full model 
full.model <- lm(not_survive ~., data = data_trimmed)
# Stepwise regression model
#step.model <- stepAIC(full.model, direction = "both", 
#                      trace = TRUE)
#summary(step.model)

intercept_only <- lm(not_survive ~ 1, data=data_trimmed)
forward <- step(intercept_only, direction='forward', scope=formula(full.model), trace=0)

backward$coefficients
forward$coefficients

data <- as.data.frame(forward$anova)
library(xlsx)
write.xlsx(data,"D:/PC_Archive/PHD/r_scripts/r-phd/r-phd-project/samples/results_trimmed.xlsx")
typeof(forward$anova)

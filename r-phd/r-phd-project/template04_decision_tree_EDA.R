
## PRIMARY ANALYSIS (DCAST, +/- REPLACE NAs with "Neu") - Option 1
## ----------------------------------------------------

## Rating levels

ReviewsProcessed_rating_level <- ReviewsProcessed %>%
        mutate(brand = ifelse(brand == "Roomba", "iRobot", brand),
               brand = ifelse(brand == "Robovac", "Eufy", brand)) %>%
        filter(grepl("2017|2018", week_start),
               !grepl("2018-09-01", month_start),
               grepl("iRobot", brand)) %>%
        group_by(week_start, rpc) %>%
        summarise(ratings = ifelse(mean(star_rating) < 3.5, "1,2,3", "4,5"))


## Sentiment levels

ReviewsProcessed_sentiment_level <- ReviewsProcessed %>%
        mutate(brand = ifelse(brand == "Roomba", "iRobot", brand),
               brand = ifelse(brand == "Robovac", "Eufy", brand)) %>%
        filter(grepl("2017|2018", week_start),
               !grepl("2018-09-01", month_start),
               grepl("iRobot", brand)) %>%
        group_by(week_start, rpc, topic) %>%
        summarise(sentiment = ifelse(mean(sentiment) < -0.1, "Neg",
                                     ifelse(mean(sentiment) >= -0.1 & mean(sentiment) <= 0.1, "Neu", "Pos")))

ReviewsProcessed_sentiment_level_dcast <- dcast(ReviewsProcessed_sentiment_level,
                                                week_start + rpc ~ topic,
                                                value.var = "sentiment")


## Replace NA values:

#replace with "Neu" (rpart works with NAs as well)
#ReviewsProcessed_sentiment_level_dcast[is.na(ReviewsProcessed_sentiment_level_dcast)] <- "Neu"


## Merge rating levels with sentiment levels

ReviewsProcessed_merged <- inner_join(ReviewsProcessed_rating_level, ReviewsProcessed_sentiment_level_dcast,
                                      by = c("week_start" = "week_start", "rpc" = "rpc"))

ReviewsProcessed_merged <- clean_names(ReviewsProcessed_merged)


## Fit the model (create train and test datasets to calculate accuracy):

#modFit_train <- train(ratings ~ battery + carpets + cleaning_quality + customer_service + floor + hair + movement + noise + pets + price + reliability + suction_power,
#                method = "rpart", data = ReviewsProcessed_merged)

#print(modFit_train$finalModel)

set.seed(123)
inTrain <- createDataPartition(y = ReviewsProcessed_merged$ratings, p = 0.8, list = FALSE)
ReviewsProcessed_merged_train <- ReviewsProcessed_merged[inTrain, ]
ReviewsProcessed_merged_test <- ReviewsProcessed_merged[-inTrain, ]

#SMOTE for balanced "ratings" variable
prop.table(table(ReviewsProcessed_merged_train$ratings))

factor_col <- names(ReviewsProcessed_merged_train[3:15])

ReviewsProcessed_merged_train[factor_col] <- lapply(ReviewsProcessed_merged_train[factor_col], factor)

test <- ReviewsProcessed_merged_train[, -c(1,2)]

ReviewsProcessed_merged_train <- SMOTE(ratings ~ battery + carpets + cleaning_quality + customer_service + floor + hair + movement + noise + pets + price + reliability + suction_power,
                                       as.data.table(test), perc.over = 100, perc.under=200)

modFit_rpart <- rpart(ratings ~ battery + carpets + cleaning_quality + customer_service + floor + hair + movement + noise + pets + price + reliability + suction_power,
                     data = ReviewsProcessed_merged_train)

#model stats and plot:
print(modFit_rpart)
printcp(modFit_rpart)

plot(modFit_rpart, uniform = TRUE, main = "Classification Tree")
text(modFit_rpart, use.n = TRUE, all = TRUE, cex = .8)

fancyRpartPlot(modFit_rpart, cex = 0.5)

#test accuracy:

pred_rpart <- predict(modFit_rpart, ReviewsProcessed_merged_test, type = "class")

t_rpart <- table(ReviewsProcessed_merged_test$ratings, pred_rpart)

confusionMatrix(t_rpart)


## TEST WITH IMPUTING NAs (AFTER DCAST) USING RF REGRESSION PREDICTIONS - Options 2 (best accuracy and interpretability)
## --------------------------------------------------------------------

## Rating levels

ReviewsProcessed_rating_level <- ReviewsProcessed %>%
        mutate(brand = ifelse(brand == "Roomba", "iRobot", brand),
               brand = ifelse(brand == "Robovac", "Eufy", brand)) %>%
        filter(grepl("2017|2018", week_start),
               !grepl("2018-09-01", month_start),
               grepl("iRobot", brand)) %>%
        group_by(week_start, rpc) %>%
        summarise(ratings = ifelse(mean(star_rating) < 3.5, "1,2,3", "4,5"))

## Sentiment

ReviewsProcessed_sentiment <- ReviewsProcessed %>%
        mutate(brand = ifelse(brand == "Roomba", "iRobot", brand),
               brand = ifelse(brand == "Robovac", "Eufy", brand)) %>%
        filter(grepl("2017|2018", week_start),
               !grepl("2018-09-01", month_start),
               grepl("iRobot", brand)) %>%
        group_by(week_start, rpc, topic) %>%
        summarise(sentiment = mean(sentiment))

ReviewsProcessed_sentiment_dcast <- dcast(ReviewsProcessed_sentiment,
                                                week_start + rpc ~ topic,
                                                value.var = "sentiment")

View(ReviewsProcessed_sentiment_dcast)
## Merge rating levels with sentiment

ReviewsProcessed_merged <- inner_join(ReviewsProcessed_rating_level, ReviewsProcessed_sentiment_dcast,
                                      by = c("week_start" = "week_start", "rpc" = "rpc"))

ReviewsProcessed_merged <- clean_names(ReviewsProcessed_merged)


## Replace NA values:

#impute the missing data using RF regressions predictions

tempData <- mice(ReviewsProcessed_merged, m=2, maxit=4, meth='rf', seed=50)

ReviewsProcessed_merged_imp <- complete(tempData, 1)

ReviewsProcessed_merged_imp_sent_levels <- ReviewsProcessed_merged_imp

ReviewsProcessed_merged_imp_sent_levels[4:15] <- as.data.frame(lapply(ReviewsProcessed_merged_imp_sent_levels[4:15],
                                                                function(x) ifelse(x < -0.1, "Neg",
                                                                                   ifelse(x >= -0.1 & x <= 0.1, "Neu", "Pos"))))


## Fit the model:

#modFit_imp_train <- train(ratings ~ battery + carpets + cleaning_quality + customer_service + floor + hair + movement + noise + pets + price + reliability + suction_power,
#                method = "rpart", data = ReviewsProcessed_merged_imp_sent_levels)

#print(modFit_imp_train$finalModel)

set.seed(456)
inTrain <- createDataPartition(y = ReviewsProcessed_merged_imp_sent_levels$ratings, p = 0.8, list = FALSE)
ReviewsProcessed_merged_imp_sent_levels_train <- ReviewsProcessed_merged_imp_sent_levels[inTrain, ]
ReviewsProcessed_merged_imp_sent_levels_test <- ReviewsProcessed_merged_imp_sent_levels[-inTrain, ]

#SMOTE for balanced "ratings" variable
prop.table(table(ReviewsProcessed_merged_imp_sent_levels_train$ratings))

factor_col <- names(ReviewsProcessed_merged_imp_sent_levels_train[3:15])

ReviewsProcessed_merged_imp_sent_levels_train[factor_col] <- lapply(ReviewsProcessed_merged_imp_sent_levels_train[factor_col], factor)

test <- ReviewsProcessed_merged_imp_sent_levels_train[, -c(1,2)]

ReviewsProcessed_merged_imp_sent_levels_train <- SMOTE(ratings ~ battery + carpets + cleaning_quality + customer_service + floor + hair + movement + noise + pets + price + reliability + suction_power,
                                       as.data.table(test), perc.over = 100, perc.under=200)

#model fit
modFit_imp_rpart <- rpart(ratings ~ battery + carpets + cleaning_quality + customer_service + floor + hair + movement + noise + pets + price + reliability + suction_power,
                      data = ReviewsProcessed_merged_imp_sent_levels_train, cp = 0.022541)

#model stats and plot:
print(modFit_imp_rpart)
printcp(modFit_imp_rpart)

plot(modFit_imp_rpart, uniform = TRUE, main = "Classification Tree")
text(modFit_imp_rpart, use.n = TRUE, all = TRUE, cex = .8)

fancyRpartPlot(modFit_imp_rpart, cex = 0.5)

#test accuracy:

pred_rpart <- predict(modFit_imp_rpart, ReviewsProcessed_merged_imp_sent_levels_test, type = "class")

t_rpart <- table(ReviewsProcessed_merged_imp_sent_levels_test$ratings, pred_rpart)

confusionMatrix(t_rpart)

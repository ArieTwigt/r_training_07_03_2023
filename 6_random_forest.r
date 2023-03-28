library(randomForest)
library(caret)
library(dplyr)
library(Metrics)
library(superml)

# import data
data <- read.csv("~/Projecten/r_training_7_03_2023/data/german_data_clean.csv")

# modify the data types
data_fac <- data %>%
  mutate_if(is.character, as.factor) %>% # change all character columns to factors
  mutate(response = as.factor(response))
  

# split the data in a train- and test-set
set.seed(123)
trainDataIndex <- createDataPartition(data_fac$response, p=0.8, list=FALSE)
train_data <- data_fac[trainDataIndex, ] 
test_data <- data_fac[-trainDataIndex,]

# train the model
model_1 <- randomForest(response ~ .,
                        data=train_data,
                        importance=TRUE
                        )


# show the variable importances
varImpPlot(model_1)

# plot the model
plot(model_1)

# generate predictions
test_predictions <- predict(model_1, newdata=test_data)

# create a data frame to compare the predictions and actual values
df_pred <- data.frame(prediction = as.integer(test_predictions) - 1,
                      actual = as.integer(test_data$response) -1)


# label the positives
df_pred$judgement <- ifelse(df_pred$prediction == 1 & df_pred$actual == 1, "TP", 
                        ifelse(df_pred$prediction == 1 & df_pred$actual == 0, "FP",
                               ifelse(df_pred$prediction == 0 & df_pred$actual == 1, "FN",
                                      ifelse(df_pred$prediction == 0 & df_pred$actual == 0, "TN", ""))))


# create the confusion matrix
confusionMatrix(as.factor(df_pred$actual),
                as.factor(df_pred$prediction)
                )

# show metrics of the model
precision(df_pred$actual,
          df_pred$prediction)

# Manual calculation of 

# sensitivity
print(25 / (25 + 131)) # sensivity

# accuracy
print((25 + 131) / (25 + 131 + 9 + 35))



# second iteration. Finetune the RandomForest model
prediction_cols <- c("checking_account", "duration_months", 
                     "credit_history", "savings",
                     "credit_amount")

# compose the GridSearchCV
rf <- RFTrainer$new()

gs <- GridSearchCV$new(trainer=rf,
                      parameters=list(
                                      max_depth = c(2,3,4,5),
                                      n_estimators = c(100, 200, 300, 400, 500)),
                      n_folds=5,
                      scoring = "recall"
                   )

# execute the GridsearchCV
gs$fit(train_data[, c(prediction_cols, "response")], "response")

# get the best iteration of the GridsearchCV
gs$best_iteration()


# train the model based on:
# - Specified collection of columns
# - Best found hyper parameters

# train the model
model_2 <- randomForest(response ~ .,
                        data=train_data[, c(prediction_cols, "response")],
                        max_depth=2,
                        n_estimators=400,
                        importance=TRUE
)


# show the variable importances
varImpPlot(model_2)

# plot the model
plot(model_2)

# generate predictions
test_predictions <- predict(model_2, newdata=test_data)

# create a data frame to compare the predictions and actual values
df_pred_2 <- data.frame(prediction = as.integer(test_predictions) - 1,
                      actual = as.integer(test_data$response) -1)


# label the positives
df_pred_2$judgement <- ifelse(df_pred_2$prediction == 1 & df_pred_2$actual == 1, "TP", 
                            ifelse(df_pred_2$prediction == 1 & df_pred_2$actual == 0, "FP",
                                   ifelse(df_pred_2$prediction == 0 & df_pred_2$actual == 1, "FN",
                                          ifelse(df_pred_2$prediction == 0 & df_pred_2$actual == 0, "TN", ""))))
df_pred_2

# create the confusion matrix
confusionMatrix(as.factor(df_pred_2$actual),
                as.factor(df_pred_2$prediction)
)

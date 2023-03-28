library(xgboost)
library(caret)
library(useful)
#library(smotefamily) for R verison > 4
library(DMwR2)

# import data
data <- read.csv("~/Projecten/r_training_7_03_2023/data/german_data_clean.csv")

# specify the y and x variables
target <- "response"
features <- setdiff(names(data_filtered),  # alle waarden in de vector
                    "response")   # behalve deze waarde(n)

# provided the formula
formula <- as.formula(
  paste(target, paste(features, collapse = " + "), sep = " ~ "))

# balance the dataset
SMOTE(X=data[,features], y=data[,target], K=5,  perc.over = 0.4, perc.under=200)

# select columns
selected_cols <- names(data)#c("credit_amount", "duration_months", "age_years")

# keep the required columns
data_filtered <- data


# feature engineering



# split the train and test-set
set.seed(123)
train_data_index <- createDataPartition(data_filtered$response, p=0.8, list=FALSE)
train_data <- data[train_data_index, ]
test_val_data <- data[-train_data_index,]

test_data_index <- createDataPartition(test_val_data$response, p=0.5, list=FALSE)
test_data <- test_val_data[test_data_index,]
val_data <- test_val_data[-test_data_index,]

# clean up variables
remove(list = c("test_val_data", "test_data_index", "train_data_index"))

# sum, the totals should be 1000
sum(nrow(test_data), nrow(train_data), nrow(val_data))

# create the dataset for xgboost
#train
X_train <- build.x(formula, data = train_data, 
                   contrasts = FALSE,
                   sparse = TRUE)

Y_train <- build.y(formula, data = train_data)

# test
X_test <- build.x(formula, data = test_data, 
                   contrasts = FALSE,
                   sparse = TRUE)

Y_test <- build.y(formula, data = test_data)

# val
X_val <- build.x(formula, data = val_data, 
                   contrasts = FALSE,
                   sparse = TRUE)

Y_val <- build.y(formula, data = val_data)

# create the final sets for XGBoost
xgTrain <- xgb.DMatrix(data=X_train, label=Y_train)
xgTest <- xgb.DMatrix(data=X_test, label=Y_test)
xgVal <- xgb.DMatrix(data=X_val, label=Y_val)


# Train the xgboost model
model_1 <- xgb.train(
  data = xgTrain,
  objective="binary:logistic",
  nrounds = 1000,
  watchlist = list(train=xgTrain, validate=xgVal),
  eval_metric="auc", 
  early_stopping_rounds = 50,
  max_depth = 3,
  colsample_bytree=0.5
)

# get the model with the best score
model_1$best_iteration

# show the feature importances
xgb.plot.importance(
  xgb.importance(model_1, feature_names=colnames(X_train))
)


# generate the predictions
preds_probs <- predict(model_1, xgTest)
preds <- as.numeric(preds_probs > 0.5)

# create a data frame with the predictions and the actual values
df_pred = data.frame(prediction = preds,
                      actual = test_data$response)


# label the positives
df_pred$judgement <- ifelse(df_pred$prediction == 1 & df_pred$actual == 1, "TP", 
                            ifelse(df_pred$prediction == 1 & df_pred$actual == 0, "FP",
                                   ifelse(df_pred$prediction == 0 & df_pred$actual == 1, "FN",
                                          ifelse(df_pred$prediction == 0 & df_pred$actual == 0, "TN", ""))))


# create the confusion matrix
confusionMatrix(as.factor(df_preds$actual),
                as.factor(df_preds$prediction))


# manually calculate the sensitivity (recall)
table(df_pred$judgement)

# specificity (TP / (TP + FN))
TP <- table(df_pred$judgement)["TP"]
FN <- table(df_pred$judgement)["FN"]

TP / (TP + FN)



# open required packages
library(dplyr)
library(caret)
library(Metrics)
library(corrplot)
library(PRROC)

# import data
data <- read.csv("~/Projecten/r_training_7_03_2023/data/german_data_clean.csv")


# show relationships between columns
cor(data$response, data$duration_months)

# correlation plot for all (numeric) columns
data_num <- data[, c("duration_months", "credit_amount", "installment_rate", "present_residence", "age_years", "existing_credits", "people_liable_maintenance", "response")]

# create correlation matrix
cor(data_num)

# create correlation plot
corrplot(cor(data_num), method="number")

# plot all the variables against each other
plot(data_num)

# choose only required columns
data_subset <- data %>%
  select(duration_months, purpose, response)

# check if "response" is balanced
table(data_subset$response)

# split the data in a "train" and "test" set
set.seed(123)
train_data_index <- createDataPartition(data_subset$response, p=0.7, list=FALSE)
train_data <- data_subset[train_data_index, ]
test_data <- data_subset[-train_data_index,]

# check the balances of the sets
table(train_data$response)
table(test_data$response)

# train the logistic model
model_1 <- glm(response ~ duration_months + purpose, data=train_data, family = "binomial")

# generate predictions (in probabilities)
test_predictions_prob <- predict(model_1, newdata = test_data, type="response")

# show some predictions
head(test_predictions_prob)

# convert to predictions
test_predictions <- ifelse(test_predictions_prob > 0.5, 1, 0)

# show the test predictions
head(test_predictions)

# create table to compare predictions with actual results
data_comparision <- test_data %>%
  mutate(prediction = test_predictions) %>%
  mutate(prediction_prob = test_predictions_prob)

# metrics to assess the model
accuracy(data_comparision$response, data_comparision$prediction)
precision(data_comparision$response, data_comparision$prediction)
recall(data_comparision$response, data_comparision$prediction)

# visualisations to assess the model
confusionMatrix(as.factor(data_comparision$response), 
                as.factor(data_comparision$prediction))


test_roc <- roc(data_comparision$response ~ data_comparision$prediction_prob, plot=TRUE, print.auc=TRUE)


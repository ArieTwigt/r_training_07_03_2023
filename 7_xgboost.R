library(xgboost)
library(caret)

# import data
data <- read.csv("~/Projecten/r_training_7_03_2023/data/german_data_clean.csv")


# select columns
selected_cols <- c("credit_amount", "duration_months", "age_years")

# keep the required columns
data_filtered <- data[, c(selected_cols, "response")]


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

# specify the y and x variables
target <- "reponse"
features <- setdiff(names(data_filtered),  # alle waarden in de vector
                    "response")   # behalve deze waarde(n)

# provided the formula
formula <- as.formula(
  paste(target, paste(features, collapse = " + "), sep = " ~ "))



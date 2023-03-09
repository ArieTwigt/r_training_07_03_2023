# load packages
library(dplyr)

# load the data
data <- read.csv("~/Projecten/r_training_7_03_2023/data/german_data_clean.csv")

# modify the data set
selected_purposes <- c("car (new)", "radio/television", "furniture/equipment")

subset_1 <- data %>%
  filter(personal_status_sex == "male   : divorced/separated") %>%
  filter(age_years >= 30) %>%
  filter(purpose %in% selected_purposes) %>%
  select(age_years, duration_months, credit_amount, purpose, response) %>%
  arrange(desc(credit_amount))

write.csv(subset_1, "my_subset.csv", sep=";", row.names = FALSE)

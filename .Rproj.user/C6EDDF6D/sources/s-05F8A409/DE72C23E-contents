print("Hello world!")

# load the data set
data <- read_csv("data/german_data_clean.csv")

# explore the data set

# mean of duration months
mean(data$duration_months)

# provide a summary of the "credit_amount"
summary(data$credit_amount)

# show the unique purposes
unique(data$purpose)

# show the unique purposes, and how many times they occur
table(data$purpose)

# show a scatter plot
plot(data$duration_months, 
     data$credit_amount)


# show a bar plot
barplot(table(data$purpose),
        col = "red",
        angle=180)

# add calculated column
colnames(data)

# create column that indicates the payment per month
data$payment_per_month <- data$credit_amount / data$duration_months

# train a linear model
model_1 <- lm(credit_amount ~ duration_months + purpose + installment_rate, data = data)

# provide a summary of the model
summary(model_1)


# create data frame with new data
new_durations <- c(10, 20 , 30)
new_purposes <- c("education", "domestic appliances", "car (new)" )
new_installment_rates <- c(2, 3, 4)

new_customers <- data.frame(duration_months = new_durations, 
                            purpose = new_purposes,
                            installment_rate = new_installment_rates)

# create predictions
predictions <- predict(model_1, newdata=new_customers)

# add predictions to new_customers table
new_customers$prediction <- predictions

library(ggplot2)

# load the dataset
data <- read.csv("~/Projecten/r_training_7_03_2023/data/german_data_clean.csv")

# create the visualisation
plot_1 <- ggplot(data = data, aes(x=credit_amount))

# show the plot
plot_1 + geom_density(fill="gray") +
         geom_vline(xintercept = mean(data$credit_amount), 
                    color="blue", 
                    size = 3, 
                    alpha=0.5) +
         geom_vline(xintercept = mean(data$credit_amount) + sd(data$credit_amount)  * 2, 
                    color="red") +
        geom_vline(xintercept = mean(data$credit_amount) - sd(data$credit_amount)  * 2,
                    color="red")



# plot 2
plot_2 <- ggplot(data = data, aes(x=purpose, y=age_years))
plot_2 + geom_boxplot()

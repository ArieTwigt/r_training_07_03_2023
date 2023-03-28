# unlabeled dataset

# load the dataset
data(mtcars)

# specify columns to consider for clustering
mtcars <- mtcars[, c("qsec", "hp", "wt")]

# convert the DataFrame to a matrix (neede for calculation)
cars_matrix <- as.matrix(mtcars)


# find optimal number of cluster
set.seed(123)

max_k <- 10

tries <- sapply(1:max_k, 
                function(k){kmeans(cars_matrix, k, 
                                   nstart=50)$tot.withinss})

tries

plot(x=1:max_k, 
     y=tries,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# create k-means with optimal number of k's
kmeans_model <- kmeans(cars_matrix, 3)

# show cluster_centers
kmeans_model$centers

# add the clusters to the dataset
mtcars_subset$segment <- kmeans_model$cluster

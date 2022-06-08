# importing all libraries are used in project 
# Note : all libraries need install
library(dplyr)
library(amap)
library(animation)
library(ggplot2)
library(factoextra)

  #Import data:
  getwd()
  path <- 'Mall_Customers.csv'
  data <- read.csv(path)


# dropping the columns we will not use in analysis 
names(data)
df =  data[, c(3, 4, 5)]

# overview of our data
summary(df)


# Preprocessing 
Scaleddf_annual <- df %>%
  mutate(
          SAge = scale(Age),
          AnnualIncome= scale(Annual.Income..k..)
    ) %>%
  select(-c(Annual.Income..k..,Spending.Score..1.100.,Age))

Scaleddf_annual
#Training the model with default Kmeans model:
# given a random k
model = kmeans(Scaleddf_annual,2)

model$centers


#Plot the animation
set.seed(2000)
kmeans.ani(Scaleddf_annual, 2)


# finding Optimal k
kmean_withinss <- function(k) {
  cluster <- kmeans(Scaleddf_annual, k)
  return (cluster$tot.withinss)
}

# Set maximum cluster 
maxk <-25 

# Run algorithm over a range of k 
wss <- sapply(2:maxk, kmean_withinss)


# Create a data frame to plot the graph
elbow <-data.frame(2:maxk, wss)


# Plot the graph with ggloplot 
ggplot(elbow, aes(x = X2.maxk, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

# clusters and centers
optimalmodel = kmeans(Scaleddf_annual,7)
optimalmodel$cluster
optimalmodel$centers

# the model use eucildian  by default

#Examining the cluster with the optimal k (we chose 7)
kmeans.ani(Scaleddf_annual, 7)



#the clusters here show that the people almost (30 - 50) have higher
#annual than the other ages

# --------------------------- another model for age and spending time ------------------------
# --------------------------- using different approach with manhattan method -----------------

Scaleddf_spending <- df %>%
  mutate(
    spAge = scale(Age),
    spendingtime = scale(Spending.Score..1.100.)
  ) %>%
  select(-c(Annual.Income..k..,Spending.Score..1.100.,Age))

Scaleddf_spending
#Training the model with Kmeans model which uses manhattan:
# we use library amap 

# given a random k
model2 = Kmeans(Scaleddf_spending,2,method = 'manhattan')
#Plot the clusters with factoextra 
set.seed(2000)
fviz_cluster(model2, geom = "point", data = Scaleddf_spending) + ggtitle("k is 2")

# Set maximum cluster 
maxk <-25 

# Run algorithm over a range of k 
wss2 <- sapply(2:maxk, kmean_withinss)


# Create a data frame to plot the graph
elbow2 <-data.frame(2:maxk, wss2)


# Plot the graph with ggloplot 
ggplot(elbow2, aes(x = X2.maxk, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

#Examining the cluster with the optimal k (we chose 8)
model2 = Kmeans(Scaleddf_spending,8,method = 'manhattan')
model2$cluster
model2$centers
fviz_cluster(model2, geom = "point", data = Scaleddf_spending ) + ggtitle("k is 8")

#the clusters here show that the people almost (less than 20
#"teenagers") spend more time in the mall than other who are in older ages
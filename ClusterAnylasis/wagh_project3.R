# Author: Vishal Wagh
# Date:   05/05/2024


#This R script, conducts K-means clustering analysis on two datasets, 
#becuase I did not have a data set with 3000 rows. So two fulfil that 
#requirment I used two different data sets.
#It begins by installing and loading necessary packages, then defines a 
#function for plotting to determine the optimal number of clusters. After 
#processing the data, it performs K-means clustering, visualizes the 
#results, and interprets the clustering outcomes.



#installing required packages (Don't run if already installed)
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
#The packages installed are stats, dplyr, ggplot2, and ggfortify. 
#These packages are commonly used for statistical analysis, data manipulation,
#and data visualization.


#load required libraries
#After installing the packages, the script loads them into the R environment 
#using the library function. This makes the functions and utilities provided 
#by these packages available for use.
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)


#Define a function named "wssplot" that generates a plot to help determine the 
#optimal number of clusters for K-means clustering (copied from the internet)
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}


#reading the CSV file
customerData <- read.csv(file = "customer_segmentation.csv",header = TRUE, sep = ",")

#removing irrelevant columns
mydata <- select(customerData, c(2,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29))

# Data cleaning: remove rows with null data
mydata <- mydata[rowSums(is.na(mydata)) == 0,]  # Remove rows with any NA values
# Remove duplicates
mydata <- mydata[!duplicated(mydata), ]


#wss Plot to choose the maximum number of clusters
wssplot(mydata)
#By analyzing the wssplot I concluded that the maximum number of clusters for K-Mean is 2

#K-means cluster analysis
#The function kmeans() is used to perform K-means clustering on the cleaned data
myKmean <- kmeans(mydata,2)


#cluster plot
autoplot(myKmean, mydata, frame = TRUE)
#'autoplot()' is used to visualize the clustering results


###Results:
#Cluster center
myKmean$centers

#In the cluster plot, there is a slight overlap of the clusters, but after
#analyzing the results cluster center, except "Z_CostContact" all other 
#variables have different cluster centers, thus the cluster analysis was a success.



######################################################################
## Second K-Mean abylisis. 
#(repeating the same steps on a diffrent data set)


#reading the dataset
customerData <- read.csv(file = "SOCR-HeightWeight.csv",header = TRUE, sep = ",")
mydata <- customerData

# Data cleaning: remove rows with null data
mydata <- mydata[rowSums(is.na(mydata)) == 0,]  # Remove rows with any NA values
# Remove duplicates
mydata <- mydata[!duplicated(mydata), ]  # Remove duplicate rows

#wss plot to choose the maximum number of clusters
wssplot(mydata)

#K-means cluster analysis
myKmean <- kmeans(mydata,2)

#cluster plot
autoplot(myKmean, mydata, frame = TRUE)
#'autoplot()' is used to visualize the clustering results


###Results:
#Cluster center
myKmean$centers

#At first glance at the cluster diagram, we can see there is a slight overlap 
#of the clusters in the middle, but after analyzing the results of the cluster 
#center, none of the variable have the same center thus the cluster analysis was a success.


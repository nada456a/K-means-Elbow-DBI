#K means clustering- "Food Delivery Case Study"
#data from: "Kaggle (author: saumya)"
#clean data
#k-means
#elbow
#modified use DBI  (David Bouldin Index) to know what's the best cluster


library(ggplot2)
library(cluster)
library(factoextra)


#read input data
customer_order <- read.csv("C:/Users/HP/Downloads/SampleAssessment.csv", header=T, sep=",")
View(customer_order)

#rename colnames
colnames(customer_order) <- c("Customer_Id", "First_Order_DateTime","Recent_Order_DateTime", "Orders_All", "Orders_Last_7_days", "Orders_Last_4_weeks", "Amount_All", "Amount_Last_7_days", "Amount_Last_4_weeks", "Avg_DistanceFromResturant", "Avg_DeliveryTime")
#check data structure
str(customer_order)

summary(customer_order)

#evaluation data
#1. last 7 days and last 4 weeks are missing value
#2. Average distance from restaurant is negative for few cases
#3. Data set includes all users who have placed atleast 1 order

#convert date value

customer_order$First_Order_Date <- as.Date(customer_order$First_Order_DateTime, "%m/%d/%y")
customer_order$Recent_Order_Date = as.Date(customer_order$Recent_Order_DateTime, "%m/%d/%y")

#creating new columns
customer_order$Current_Date = max(customer_order$Recent_Order_Date) + 1
customer_order$Days_Since_Last_Order = as.numeric(customer_order$Current_Date - customer_order$Recent_Order_Date)
customer_order$Days_Since_First_Order = as.numeric(customer_order$Current_Date -customer_order$First_Order_Date)

#check data and new columns
head(customer_order, 5)

#filter and cleaning data
#Next filter by cases where order value is NA for last 7 days and 4 Weeks and from those users I took out minimum days since last order 

#Here we see that the minimum days is greater than 7 days and 28 Days for recent order respectively. Given this we can safely assume that the NA values are not missing but actually 0. So next I replace them with 0.


customer_order$Orders_Last_7_days = ifelse(is.na(customer_order$Orders_Last_7_days),0,customer_order$Orders_Last_7_days) 

customer_order$Orders_Last_4_weeks = ifelse(is.na(customer_order$Orders_Last_4_weeks),0,customer_order$Orders_Last_4_weeks) 


head(customer_order,8)

#Avg resturant distance is negative, for simplicity I am calling them as 0. I also created Average order value (AOV) column will use this instead of total order value. 

customer_order$Avg_DistanceFromResturant = ifelse(customer_order$Avg_DistanceFromResturant<0,0,customer_order$Avg_DistanceFromResturant)

customer_order$AOV_All = round(customer_order$Amount_All/customer_order$Orders_All,0)

customer_order$AOV_Last_7_Days = round(ifelse(customer_order$Orders_Last_7_days ==0 ,0,customer_order$Amount_Last_7_days/customer_order$Orders_Last_7_days),0)

customer_order$AOV_Last_4_Weeks = round(ifelse(customer_order$Orders_Last_4_weeks ==0 ,0,customer_order$Amount_Last_4_weeks/customer_order$Orders_Last_4_weeks),0)




##Segmenting Customers

#All these have atleast 1 order but if we look at days since last order we can see then 

q1 = 100 - round(100*sum(customer_order$Orders_Last_7_days==0)/nrow(customer_order),0)


q2 = 100 - round(100*sum(customer_order$Orders_Last_4_weeks==0)/nrow(customer_order),0)
print(q1)
print(q2)

#Only `r q1`% users transacted in last 7 days and `r q2`% Users transacted over last 4 weeks. This means we have a lot of Users who have not interacted in last month.

#I am creating a filtered data for our raw data which only considers releveant columns to build the model on. For our anlaysis we are considering Order Count, AOV, Avg distance from Restaurant, Avg Delivery Time and Days since 1st and last orders as the relevant columns. Basically we have removed few columns which are giving redundant data for eg Total order value is a function of Total orders and AOV.

#On this data I apply Pricipal component analysis to see if we can reduce variables even further.


filter_data = customer_order[ ,c(1,4:6,10,11,15:19)]

#set as the random number value
#(stata.com) Nonetheless, here are two methods that we have seen used but you should not use:
#1. The first time you set the seed, you set the number 1. The next time, you set 2, and then 3,
    #and so on. Variations on this included setting 1001, 1002, 1003, . . . , or setting 1001, 2001,
    #3001, and so on.
    #Do not follow any of these procedures. The seeds you set must not exhibit a pattern.


set.seed(1978)

#use of PCA is to represent a multivariate data table as smaller set of variables (summary indices) in order to observe trends, jumps, clusters and outliers
#PCA can be used when the dimensions of the input features are high (e.g. a lot of variables)
#PCA can be also used for denoising and data compression.
pca_data1 = prcomp(filter_data[,-1],center = T,scale. = T)


pca_data1$scale
pca_data1$center
plot(pca_data1, type = "1",
     main = "Variance of PCA")


#From the plot we can see that the variance is still high when we are looking at 5th-6th Principal component. So going ahead with all the vairable in the model.

#I am trying to build a clustering model here to see if we can divide users in different buckets. For that I use the kmeans cluster. Given the summary data has vairables at different scales I decided to look at the log and normal tranformations and use the Elbow method to calculate minimum error (within group sum of squares should be minimum) and decide how many clusters to divide the data into


#The seeds you set must not exhibit a pattern.

set.seed(0264)

#normalize data
# custom function to implement min max scaling
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


#[-1] uses the fact that a data.frame is a list, so when you do dataframe[-1] it returns another data.frame (list) without the first element (i.e. column).
#[ ,-1]uses the fact that a data.frame is a two dimensional array, so when you do dataframe[, -1] you get the sub-array that does not include the first column.

#log normal
filter_data_log = log(filter_data[,-1]+2)

#normalise data using custom function
filter_data_normal = as.data.frame(lapply(filter_data[,-1], normalize))


score_wss_log <- (nrow(filter_data_log)-1)*sum(apply(filter_data_log,2,var))
for (i in 2:15) score_wss_log[i] <- sum(kmeans(filter_data_log,
                                               centers=i)$withinss)

plot(1:15, score_wss_log[1:15], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Elbow method to look at optimal clusters for Log Data",
     pch=20, cex=2)

score_wss_normal <- (nrow(filter_data_normal)-1)*sum(apply(filter_data_normal,2,var))
for (i in 2:15) score_wss_normal[i] <- sum(kmeans(filter_data_normal,
                                                  centers=i)$withinss)

plot(1:15, score_wss_normal[1:15], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Elbow method to look at optimal clusters for Normalized Data",
     pch=20, cex=2)
#To determine the optimal number of clusters, we have to select the value of k at the “elbow” ie the point after which the distortion/inertia start decreasing in a linear fashion
# that's means number of cluster choose k to be either 3 or 4.
# In such an ambiguous case, we may use the DBI

#DBI index
DBIKMeans<-NbClust(filter_data_normal, distance = "euclidean",
                   min.nc = 2, max.nc = 10,
                   index = "db", method = "complete")

DBIKMeans$Best.nc #recomendation number of cluster 
DBIKMeans$All.index # recomendation All cluster and index 

#There is a clear peak at k = 4. Hence, it is optimal.

#From the plots above we can see that the normalized data makes more sense to go ahead with. 
#Because the within group sum of squares is at much lesser scale for it. 
#Lets built 4 cluster k means and denormalize the data to look at the centres (means) of each vaiarbles in the 4 cluster

minvec <- sapply(filter_data[,-1],min)
maxvec <- sapply(filter_data[,-1],max)
denormalize <- function(x,minval,maxval) {
  return(x*(maxval-minval) + minval)
}

set.seed(2379)
km_4_cluster_normal = kmeans(filter_data_normal,4,nstart = 100)  
head(km_4_cluster_normal)
km_4_cluster_actual = NULL
test1 = NULL

for(i in 1:10)
{
  test1 = (km_4_cluster_normal$centers[,i] * (maxvec[i] - minvec[i])) + minvec[i]
  km_4_cluster_actual = cbind(km_4_cluster_actual,test1)
}
head(test1)
colnames(km_4_cluster_actual) = colnames(filter_data[-1])
#print("Mean value of all variables in each cluster is given below")
#km_4_cluster_actual
#print("Numbers of customers in each cluster is given below")
km_4_cluster_normal$size
km_4_cluster_normal$centers

#vizualitation
fviz_cluster (km_4_cluster_normal, data= filter_data_normal)

#input new column cluster at data set
Clustering_Customer_Order<-cbind(customer_order, km_4_cluster_normal$cluster)
View(Clustering_Customer_Order)

#Customers are basically divided as
#1. Active but Low freqeuncy and Low AOV over last 7 days and 4 Weeks and high delivery time users
#2. Non Active over last 7 days and 4 Weeks
#3. Active but Low freqeuncy and Low AOV over last 7 days and 4 Weeks and low delivery time users
#4. Active and High freqeuncy and High AOV over last 7 days and 4 Weeks

#the insight is for customer clusters 1 and 2 Avg Delivery time is not such a critical factor in influencing customers to order
#While clusters 3 and 4 Avg Delivery time influence customer to order


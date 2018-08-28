rm(list = ls())
library(readxl)
raw.data<-read_excel("Online Retail.xlsx")

data<-raw.data
str(data)
summary(data)
sum(is.na(data))
sum(is.na(data$CustomerID))
sum(is.na(data$Description))
sum(is.na(data$StockCode)) 
length(unique(data$Description))
length(unique(data$CustomerID))
length(unique(data$StockCode))
sum(is.na(data$CustomerID))
data <- subset(data, !is.na(data$CustomerID))

#Limiting the Invoice date to 1 year
range(data$InvoiceDate)
data <- subset(data, InvoiceDate >= "2010-12-09")
range(data$InvoiceDate)


table(data$Country)
data <- subset(data, Country == "United Kingdom")

length(unique(data$InvoiceNo))
length(unique(data$CustomerID))

data $item.return <- grepl("C", data$InvoiceNo, fixed=TRUE)
data$purchase.invoice <- ifelse(data$item.return=="TRUE", 0, 1)

customers <- as.data.frame(unique(data$CustomerID))
Items<-as.data.frame(unique(data$StockCode))
names(customers) <- "CustomerID"
names(Items)<-"Stock Code"


#Recency
data$recency <- as.Date("2011-12-10") - as.Date(data$InvoiceDate)
temp <- subset(data, purchase.invoice == 1)

recency <- aggregate(recency ~ CustomerID, data=temp, FUN=min, na.rm=TRUE)
remove(temp)

# Add recency to customer data
customers <- merge(customers, recency, by="CustomerID", all=TRUE, sort=TRUE)

remove(recency)

customers$recency <- as.numeric(customers$recency)

#Frequency

customer.invoices <- subset(data, select = c("CustomerID","InvoiceNo", "purchase.invoice"))
customer.invoices <- customer.invoices[!duplicated(customer.invoices), ]
customer.invoices <- customer.invoices[order(customer.invoices$CustomerID),]
row.names(customer.invoices) <- NULL

# Number of invoices/year (purchases only)
annual.invoices <- aggregate(purchase.invoice ~ CustomerID, data=customer.invoices, FUN=sum, na.rm=TRUE)
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] <- "frequency"

# Add # of invoices to customers data
customers <- merge(customers, annual.invoices, by="CustomerID", all=TRUE, sort=TRUE)
remove(customer.invoices, annual.invoices)

range(customers$frequency)
table(customers$frequency)



# Remove customers who have not made any purchases in the past year
customers <- subset(customers, frequency > 0)

# Monetary Value of Customers
data$Amount <- data$Quantity * data$UnitPrice

# Aggregated total sales to customer
annual.sales <- aggregate(Amount ~ CustomerID, data=data, FUN=sum, na.rm=TRUE)
names(annual.sales)[names(annual.sales)=="Amount"] <- "monetary"

# Add monetary value to customers dataset
customers <- merge(customers, annual.sales, by="CustomerID", all.x=TRUE, sort=TRUE)
remove(annual.sales)

# Identify customers with negative monetary value numbers, as they were presumably returning purchases from the preceding year
hist(customers$monetary)
# reset negative numbers to zero
customers$monetary <- ifelse(customers$monetary < 0, 0, customers$monetary) 
hist(customers$monetary)

hist(customers$recency)
hist(customers$frequency)

customers <- customers[order(-customers$monetary),]


# Apply Pareto Principle (80/20 Rule)
pareto.cutoff <- 0.8 * sum(customers$monetary)
customers$pareto <- ifelse(cumsum(customers$monetary) <= pareto.cutoff, "Top 20%", "Bottom 80%")
customers$pareto <- factor(customers$pareto, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)
levels(customers$pareto)
round(prop.table(table(customers$pareto)), 2)
remove(pareto.cutoff)

customers <- customers[order(customers$CustomerID),]

# Log-transform positively-skewed variables
customers$recency.log <- log(customers$recency)
customers$frequency.log <- log(customers$frequency)
customers$monetary.log <- customers$monetary + 0.1 # can't take log(0), so add a small value to remove zeros
customers$monetary.log <- log(customers$monetary.log)

# Z-scores
customers$recency.z <- scale(customers$recency.log, center=TRUE, scale=TRUE)
customers$frequency.z <- scale(customers$frequency.log, center=TRUE, scale=TRUE)
customers$monetary.z <- scale(customers$monetary.log, center=TRUE, scale=TRUE)


library(ggplot2)
library(scales)


# Original scale
scatter.1 <- ggplot(customers, aes(x = frequency, y = monetary))
scatter.1 <- scatter.1 + geom_point(aes(colour = recency, shape = pareto))
scatter.1 <- scatter.1 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.1 <- scatter.1 + scale_colour_gradient(name="Recency\n(Days since Last Purchase))")
scatter.1 <- scatter.1 + scale_y_continuous(label=dollar)
scatter.1 <- scatter.1 + xlab("Frequency (Number of Purchases)")
scatter.1 <- scatter.1 + ylab("Monetary Value of Customer (Annual Sales)")
scatter.1

# Log-transformed
scatter.2 <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
scatter.2 <- scatter.2 + geom_point(aes(colour = recency.log, shape = pareto))
scatter.2 <- scatter.2 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.2 <- scatter.2 + scale_colour_gradient(name="Log-transformed Recency")
scatter.2 <- scatter.2 + xlab("Log-transformed Frequency")
scatter.2 <- scatter.2 + ylab("Log-transformed Monetary Value of Customer")
scatter.2

#Handling outliers
delete <- subset(customers, monetary.log < 0)
no.value.custs <- unique(delete$CustomerID)
delete2 <- subset(data, CustomerID %in% no.value.custs)
delete2 <- delete2[order(delete2$CustomerID, delete2$InvoiceDate),]
remove(delete, delete2, no.value.custs)

# Scaled variables
scatter.3 <- ggplot(customers, aes(x = frequency.z, y = monetary.z))
scatter.3 <- scatter.3 + geom_point(aes(colour = recency.z, shape = pareto))
scatter.3 <- scatter.3 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.3 <- scatter.3 + scale_colour_gradient(name="Z-scored Recency")
scatter.3 <- scatter.3 + xlab("Z-scored Frequency")
scatter.3 <- scatter.3 + ylab("Z-scored Monetary Value of Customer")
scatter.3

hist(customers$recency.log)
hist(customers$frequency.log)
hist(customers$monetary.log)

hist(customers$recency.z)
hist(customers$frequency.z)
hist(customers$monetary.z)


remove(scatter.1, scatter.2, scatter.3)

preprocessed <- customers[,9:11]
#Applying K-means Clustering
output<-kmeans(preprocessed,centers  =5)

#To select Optimum K value for Clustering

wss <- 0
for (i in 1:15) {
   wss[i] <- sum(kmeans(preprocessed,centers=i)$withinss)
}
plot(1:15, wss, 
     type="b", 
     xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


#By Elbow method we are considering k as 5


k=5

# Add cluster membership to customers dataset
var.name <- paste("cluster", k, sep="_")
customers[,(var.name)] <- output$cluster
customers[,(var.name)] <- factor(customers[,(var.name)], levels = c(1:k))
library(ggplot2)

# Graph clusters
cluster_graph <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
cluster_graph <- cluster_graph + geom_point(aes(colour = customers[,(var.name)]))
colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
cluster_graph <- cluster_graph + xlab("Log-transformed Frequency")
cluster_graph <- cluster_graph + ylab("Log-transformed Monetary Value of Customer")
title <- paste("k-means Solution with", k, sep=" ")
title <- paste(title, "Clusters", sep=" ")
cluster_graph <- cluster_graph + ggtitle(title)
print(cluster_graph)

#HCLust
d <- dist(preprocessed)
c <- hclust(d, method = 'ward.D2')
plot(c)


#Aggregating the RFM variables
members <- cutree(c,k = 5)

members[1:5]


table(members)

table(members)

x<-aggregate(customers[,2:4], by=list(members), mean)
x
#Visualization of Clusters Using ClusterR Package
library(ClusterR)

#Optimum Clusters
opt_gmm = Optimal_Clusters_GMM(preprocessed, max_clusters = 10, criterion = "BIC", 
                               
                               dist_mode = "maha_dist", seed_mode = "random_subset",
                               
                               km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                               
                               plot_data = T)
summary(opt_gmm)



#Reducing dimensions of Online Retail Data using PCA so that a 2-dimensional plot of the resulted clusters is possible
pca_dat = stats::princomp(preprocessed)$scores[, 1:2]

summary(pca_dat)
km = KMeans_arma(pca_dat, clusters = 5, n_iter = 10, seed_mode = "random_subset", 
                 
                 verbose = T, CENTROIDS = NULL)

summary(km)
pr = predict_KMeans(pca_dat, km)

t<-table(customers$cluster_5, pr)
t
class(km) = 'matrix'

plot_2d(data = pca_dat, clusters = as.vector(pr),
        centroids_medoids = as.matrix(km))



products <- as.data.frame(unique(data$StockCode))
names(products) <- "StockCode"

##Frequency of products by aggregating stock code

product.invoices <- subset(data,select=c("StockCode","InvoiceNo","Quantity","purchase.invoice"))
product.invoices <- product.invoices[!duplicated(product.invoices),]
product.invoices <- product.invoices[order(product.invoices$StockCode),]
row.names(product.invoices) <- NULL

# Number of invoices/year (purchases only)
product.invoices1<-aggregate(purchase.invoice~StockCode+Quantity,data=product.invoices,FUN=sum,na.rm=TRUE)
product.invoices2<-data.frame(product.invoices1$Quantity*product.invoices1$purchase.invoice)

product_annual.invoices <- aggregate(purchase.invoice~StockCode,data=product.invoices,FUN=sum,na.rm=TRUE)
names(product_annual.invoices)[names(product_annual.invoices)=="purchased_Invoice"] <- "frequency"



temp1<-subset(data,data$purchase.invoice==1)
datafreqsort<-data.frame(product_annual.invoices[order(product_annual.invoices$purchase.invoice,decreasing=T),][1:25,c(1)])

names(datafreqsort)<-"StockCode"

data25<-merge(temp1[,c(1,2)],datafreqsort,by="StockCode",all=F)
data25<-data.frame(data25$InvoiceNo,data25$StockCode)
names(data25)<-c("InvoiceNO","StockCode")
data25<-data25[order(data25$InvoiceNO),]

names(data25)<-c("InvoiceNO","StockCode")

##We have saved the data25 file to give as input to the apriori rules. 

#Apriori Rules on Transactions data for Recommendation System
library(arules)
library(arulesViz)
trans = read.transactions(file="data25.csv", rm.duplicates= FALSE,skip=1,
                          format="single",sep=",",cols =c(1,2))
str(trans)

trans
image(trans)
itemFrequency(trans)
itemFrequencyPlot(trans)
rules <- apriori(trans,parameter = list(sup = 0.009,target="rules"))
summary(rules)
inspect(rules)
library(arulesViz)
plot(rules)
plot(rules, method="graph", control=list(type="items"))

top_rules = sort(rules, by = c("confidence", "support"))
head(as(top_rules, "data.frame"), n=5)
subrules<-subset(rules, lift>5)
plot(subrules, method="matrix", engine = "3d")
plot(subrules, method="matrix", shading=c("lift", "confidence"))
library(colorspace)
plot(rules, control = list(col=sequential_hcl(100)))
plot(rules, col=sequential_hcl(100))
plot(rules, col=grey.colors(50, alpha =.8))
plot(rules, verbose = TRUE)





#Ensembles (Random Forest)
datarf<-customers[,c(1,6,7,8,12)]
summary(datarf)
library(MASS)
library(caret)

#Splitting into train and test
set.seed(123)
train_rows<-createDataPartition(y=datarf[,5],p=0.7,list=FALSE)
train<-datarf[train_rows,]
test<-datarf[-train_rows,]

library(randomForest) 
datamodel_rf<- randomForest(cluster_5 ~ ., data=train, keep.forest=TRUE, ntree=30,mtry=4,nodesize=5)
summary(datamodel_rf)

#Predicting on train data
pred1<-predict(datamodel_rf,train);pred1

#Predicting on test data
pred2<-predict(datamodel_rf,test);pred2

#Confusion Matrix
confusionMatrix(pred2,test$cluster_5)
confusionMatrix(pred1,train$cluster_5)

#####Decision Tree
dim(datarf)
library(rpart)
decision_tree_model <- rpart(cluster_5 ~ .,train, method = "class")
decision_tree_model
library(rpart.plot)
rpart.plot(decision_tree_model, type = 4, extra = 101) 

#Predictions on test data
p <- predict(decision_tree_model,test, type = "class")
tab<-table(test[,5], p)

tab



library(caret)
conf<-confusionMatrix(tab)
conf$table
conf$overall
conf$byClass
conf$dots


###Using Random Forest we are getting the best f score so using RF will gives the good results.

##############################<1 Inferential statistics>##################
set.seed(100)
x<-rnorm(100)
# simulating x
par(mfrow=c(1,3))
plot(x,main="Scatter point x")
hist(x, main="Histogram x ")
boxplot(x, main="Boxplot x")
# simulating y
y<--1+2*x+rnorm(100)
par(mfrow=c(1,3))
plot(y,main="Scatter point y")
hist(y, main="Histogram y")
boxplot(y, main="Boxplot y ")

####################<Using the Regression Line for Analysis>############################# 

#a) lm(y~x)
fit<-lm(y~x)
summary(fit)
plot(x, y)
abline(fit, col = "green")

##################<variable in deviation form Using the Regression line >########################
# deviation form
u<-x-mean(x)
v= y-mean(y)
## modellm(v~u)
fit1<-lm(v~u)
summary(fit1)
plot(u, v)
abline(fit1, col = "green")


model<-lm(v~u+0)
summary(model)

model1<-lm(u~v+0)
summary(model1)
###################<Checking the model distribution using boxplot and histogram>#############################
## v onto u
n <-length(u)
t <- sqrt(n - 1)*(u %*% v)/sqrt(sum(u^2) * sum(v^2)-(u %*% v)^2)
as.numeric(t)





##############################b) Running the Regression without  the intercept term and reverse regression >########################
## u onto v
n <-length(v)
t <- sqrt(n - 1)*(v %*% u)/sqrt(sum(v^2) * sum(u^2)-(v %*% u)^2)
as.numeric(t)



########### Question 2 Cluster Analysis problem>#########################
data<- read.delim("C:/Users/garma/OneDrive/Desktop/statistical Learning/Homework3/five-personality.txt",header = TRUE ,sep = "")
dim(data)
str(data)
str(data)
summary(data)
par(mfrow=c(1,3))
hist(data$EXT4, main="Hist-EXT4 Variable")
hist(data$EXT5, main="Histo- EXT5 Variable")
hist(data$EXT6,main="Hist- EXT6 Variable")
sum(is.na(data))
data1<-na.omit(data)
####################<correlation Analysis for the Data-set >###########################
install.packages("ggcorrplot")
library(ggcorrplot)
corr <- round(cor(data), 1)
#############< Get the lower triangle>########################
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           outline.col = "white")

data<-data[1:100000,]
test<-data[200000:230000,]
######################<Principle Component Analysis>######################### 

require(scorecard)
library(scorecard)
require(ClusterR)
library(ClusterR)
require(dplyr)
library(dplyr)
library(ggplot2)
library(cluster)

############################<Normalize the data before the PCA>##########################
norm <- sapply(na.omit(data),scale)
pca <- prcomp(norm)

############<37pcs -90.32%>########################################
summary(pca)
plot(pca,type = "l", main ="Scree plot for PCA")
k.max <-6 

#data <- norm
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=100 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v =5 , lty =2)


######################<Kmean clustering Analysis>##########################
require(psych)
library(psych)
cl1 <- kmeans(norm, 5)
cl1$cluster
dist(cl1$centers)
require(ggpfortify)
library(ggfortify)
set.seed(1)
cl<-autoplot(kmeans(norm, 5), data = data)
cl$
table(cl$cluster)
install.packages("autoplot")
library(autoplot)
install.packages("autoplotly")
library(autoplotly)
cl1<-autoplot(stats::kmeans(norm, 5), data = data,label=TRUE,frame = TRUE, frame.type = 't')
cl1
set.seed(42)
cl3 <- kmeans(norm,5)

#######################< predict on testing data-set>################################33
mod<-fviz_cluster(cl3,data=data)
mod
####################< testing on cluster algorithms>########################

require(factoextra)
library(factoextra)
require(class)
library(class)
library(scorecard)
library(factoextra)
library(class)
require(clue)
library(clue)
require(class)
library(class)
library(scorecard)
library(factoextra)
library(class)
train.kmeans <- kmeans(data, centers = 5, nstart = 50, iter.max = 50)
test_clusters<- cl_predict(train.kmeans ,newdata =test)
table(test_clusters)
require(data.table)
library(data.table)


cl3
data$cluster <- as.factor(cl3$cluster)

col<-as.factor(cl1$cluster)
plot3d(pca$x[,1:3], col=data$cluster, main="k-means clusters")
data$cluster<-NULL
###############################<characteristic plot>#######################333
plot(c(0), xaxt = 'n', ylab = "", type = "l",ylim = c(min(cl1$centers), max(cl1$centers)), xlim = c(0, 50))
####################<label x-axes>#####################################333 
axis(1, at = c(1:50), labels = names(data))
###############################< plot centroids>#########################################333
for (i in c(1:5)) lines(cl1$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5), "red","dark grey" ))
###########################< name clusters>######################################################3
text(x = 0.5, y = cl1$centers[, 1], labels = paste("Cluster", c(1:5)))

############################3< clustering members>#######################################################333
d_iris <- dist(scale(data))
hc_iris <- hclust(d_iris, method = "ward.D2")
fviz_dend(hc_iris, k = 5,cex = 0.5,
          color_labels_by_k = TRUE, ggtheme = theme_minimal())
groups <- cutree(hc_iris, k = 5)
table(groups)

####################33 choosing methods
# #########################3methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

##########################< function to compute coefficient>#####################################33
ac <- function(x) {
  agnes(b_hous.test, method = x)$ac
}

map_dbl(m, ac)

d <- dist(norm, method = "euclidean")
#########################3< Hierarchical clustering using Complete Linkage>##########################################
hc1 <- hclust(d, method = "ward.D2" )

















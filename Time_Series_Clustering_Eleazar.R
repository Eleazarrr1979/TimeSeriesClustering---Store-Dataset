#SET LIBRARIES

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(dtw)        # dynamic time warping

######################################################
#READ DATA
#Original Data leaving just sales values as DF
data <- read.csv("dataset_all combinations.csv")
data$store <- NULL
data$type <- NULL
data$assortment <- NULL

# Extra DF for testing the categorical features
TEST <- read.csv("dataset_all combinations.csv")
#selecting only store type and assortment
TEST2 <-data.frame(TEST%>% select(1:3))

######################################################
#SAMPLING
#Sample sizes according to number of stores
n <- 478
m <- 449
s <- sample(1:478,n)
t <- sample(487:935,m)
#478 A / 8 B / 449 C
#Creation of counter that assigns according to the assortment order
i <- c(s,478+1,478+2,478+3,478+4,478+5,478+6,478+7, 478+8, t)
#print(s)
#print(t)
#print(i)
#Dataframe with the order of the values
d <- data[i,]
#Creation of a vector of Assortment in order of Index
pattern <- c(rep('A',n),
             rep('B',8),
             rep('C',m))
#print(pattern)

######################################################
#DISTANCE CALCULATION DTW

distance <- dist(d,method = "dtw")
dtwDist(d)
######################################################
#HIERARCHICAL CLUSTERING

hc <- hclust(distance, method = 'average')
plot(hc)
plot(hc, labels = pattern,
     cex = 0.7,
     hang = -1,
     col = 'blue')

rect.hclust(hc, k=6)

#hrerarchical clustering with agnes
hc2 <- agnes(distance, method = "average")
hc2$ac

hc3 <- agnes(distance, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
rect.hclust(hc3, k = 4, border = 2:5)
rect.hclust(hc3, k = 4, which = 1:4, x = NULL, h = 3,
            border = 2:5, cluster = NULL)
fviz_dend(hc3,k=4, h = NULL,
          k_colors = NULL,
          palette = NULL,
          show_labels = TRUE,
          color_labels_by_k = TRUE,
          label_cols = TRUE,
          lwd = 0.7,
          type = c("rectangle", "circular", "phylogenic"),
          phylo_layout = "layout.auto",
          rect = TRUE,
          rect_border = "gray",
          rect_lty = 2,
          rect_fill = FALSE,
          horiz = FALSE,
          cex = 0.8,
          main = "Stores Cluster Dendrogram",
          xlab = "Stores",
          ylab = "Height",
          sub = NULL,
          ggtheme = theme_classic(),
)

fviz_cluster(hc3(data = distance, cluster = hc3))


sub_hc3 <- cutree(hc3, k=6)
table(sub_hc3)
print(sub_hc3)

cbind(data$store,sub_hc3)
values <-TEST2%>%
        mutate(cluster = sub_hc3)

print(values)

fviz_nbclust(distance, FUNcluster = hcut, method = "wss")

#Scatterplot
hc_a <- agnes(distance, method = "ward")
hc_a <- cutree(as.hclust(hc_a), k = 6)
fviz_cluster(hc_a)


# methods to assess cluster
Methods <- c( "average", "single", "complete", "ward")
names(Methods) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
        agnes(distance, method = x)$ac
}
map_dbl(Methods, ac)

#test for shoulder 
# function to compute total within-cluster sum of square 

wss <- function(k) {
        kmeans(hc3, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:10

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust(data, kmeans, method = "wss")
fviz_nbclust(data, kmeans, method = "silhouette")
fviz_nbclust(data, FUN = hcut, method = "wss")
fviz_nbclust(data, FUN = hcut, method = "silhouette")
fviz_gap_stat(gap_stat)
final <- kmeans(data, 4)
fviz_cluster(final, data = data)
#Another

clusGap(hc3, FUNcluster, method = c("silhouette", "wss", "gap_stat"))


#test for scoring

hc3.1 <- agnes(distance, method = "ward")
hc3.2 <- agnes(distance, method = "average")
hc3.3 <- agnes(distance, method = "single")
hc3.4 <- agnes(distance, method = "complete")

cor(distance,cophenetic(hc3.1))
cor(distance,cophenetic(hc3.2))
cor(distance,cophenetic(hc3.3))
cor(distance,cophenetic(hc3.4))
####
sub_hc <- cutree(hc, k=6)
table(sub_hc)
print(sub_hc)
plot(sub_hc, )+
text(sub_hc)
x1 = data.frame(sub_hc)

tableclust <- mutate(cluster = sub_hc)

#there are many methods that can be used for doin the hierarchical clustering. so I tried different ones and 
#then I computer that correlation below which is an indicator to understand which method is better to use
#the method with the highest correlation is the best one.

#check this link for further explanation of the methods 
#https://stats.stackexchange.com/questions/195446/choosing-the-right-linkage-method-for-hierarchical-clustering/217742#217742

cor(distance,cophenetic(h5))#to check if we're using the right method --> average has the highest correlation

h1=hclust(distance,method='average')
h2=hclust(distance,method='complete')
h3=hclust(distance,method='ward.D')
h4=hclust(distance,method='single')
h5=hclust(distance,method='median')

plot(h1, labels = pattern,
     cex = 0.7,
     hang = -1,
     col = 'blue')
plot(h2, labels = pattern,
     cex = 0.7,
     hang = -1,
     col = 'blue')
plot(h3, labels = pattern,
     cex = 0.7,
     hang = -1,
     col = 'blue')
plot(h4, labels = pattern,
     cex = 0.7,
     hang = -1,
     col = 'blue')
plot(h5, labels = pattern,
     cex = 0.7,
     hang = -1,
     col = 'blue')
library(readxl)
Data_Covid <- read_excel("C:/Downloads/SEMESTER 6/MULTIV 2/Data Covid.xlsx")
View(Data_Covid)
head(Data_Covid)


#mengecek missing value
NA_Check <- is.na(Data_Covid)
summary(NA_Check)

data_covid<-Data_Covid[-1]

# Determine number of clusters
wss <- (nrow(data_covid)-1)*sum(apply(data_covid,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data_covid,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Analisis cluster

#Determining optimal clusters
library(factoextra)
fviz_nbclust(a_covid, datkmeans, method = "wss")

#Membandingkan Plot Cluster
library(factoextra)
library(gridExtra)

k2 <- kmeans(data_covid, centers = 2, nstart = 25)
k3 <- kmeans(data_covid, centers = 3, nstart = 25)
k4 <- kmeans(data_covid, centers = 4, nstart = 25)
p1 <- fviz_cluster(k2, geom = "point", data = data_covid) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = data_covid) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = data_covid) + ggtitle("k = 4")
grid.arrange(p1, p2, p3, nrow=2)


#Kmeans clustering
klaster2 <- kmeans(data_covid, centers=2, nstart=25)
klaster2
klaster3 <- kmeans(data_covid, centers=3, nstart=25)
klaster3
klaster4 <- kmeans(data_covid, centers=4, nstart=25)
klaster4
data.frame(klaster2$cluster, klaster3$cluster, klaster4$cluster)
klaster2$centers
klaster3$centers
klaster4$centers
data.frame(klaster2$totss, klaster3$totss, klaster4$totss)
klaster2$withinss
klaster3$withinss
klaster4$withinss
data.frame(klaster2$tot.withinss, klaster3$tot.withinss,  klaster4$tot.withinss)
data.frame(klaster2$betweenss, klaster3$betweenss, klaster4$betweenss)
klaster2$size
klaster3$size
klaster4$size

kluster2<-kmeans(data_covid, centers = 2, nstart = 25) 
print(kluster2)
fviz_cluster(kluster2, data=data_covid)

kluster3<-kmeans(data_covid, centers = 3, nstart = 25) 
print(kluster3)
fviz_cluster(kluster3, data=data_covid)

kluster4<-kmeans(data_covid, centers = 4, nstart = 25) 
print(kluster4)
fviz_cluster(kluster4, data=data_covid)

# Validating
KM= kmeans(data_covid, 4)
KM$centers


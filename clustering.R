library(cluster)
library(fpc)
library(NbClust)
library(mclust)
library(factoextra)
library(ggplot2)
library(corrplot)
library(dplyr)

df <- read.csv("data/df.csv")





#Columnas a evaluar
col.features <- names(df[c(7:11)])

#Normalizar data
X <- df[col.features]
df = as.data.frame(scale(X))

#Numero de K
K <-2


#Clustering Kmeans
km <- kmeans(df,K)


fviz_cluster(km,df)


#Analisis del metodo
dd <- dist(df,method="euclidean")
km_stats <- cluster.stats(dd,km$cluster)


#Analisis silueta
sil.km <- silhouette(km$cluster,dd)
fviz_silhouette(sil.km)
paste("The Dunn Index is ",km_stats$dunn)


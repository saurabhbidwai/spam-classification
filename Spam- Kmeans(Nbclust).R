data1=data.table::fread("E:/Aegis/Machine Learning/Data/spam.csv")

data1=data1[,1:57]
summary(data1)

minmax=function(a){
  anew=(a-min(a))/(max(a)-min(a))
}

data1=apply(data1,2,minmax)

#kmeans()
wtss=c()
for(i in 2:20){
  spamcluster=kmeans(data1,i,nstart = 40)
  wtss=c(wtss,spamcluster$tot.withinss)
}
plot(2:20,wtss,type = "b")

clust=kmeans(data1,7,nstart = 40)
clust



#amap::Kmeans
wtss1=c()
for(i in 2:10){
  spam1cluster=amap::Kmeans(data1,i,iter.max = 20,nstart = 5, method = "euclidean")
  #wtss1=c(wtss1,sum(spam1cluster$withiness))
  
  for(j in 1:i){
    wt=(mean(sum((data1[spam1cluster$cluster==j,] - spam1cluster$centers[j,])^2)))
  }
  
  wtss1=c(wtss1,sum(wt))
  
}
plot(2:10,wtss1,type = "b")




#stats::dist

ndist=stats::dist(data1,method = "euclidean")
  #extra


#NbClust
library("NbClust")
res=NbClust(data=data1, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "ward.D2", index = "kl")
res$All.index
res$Best.nc
res$Best.partition


res1=NbClust(data=data1, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "kmeans", index = "all")
res1$All.index
res1$Best.nc
res1$Best.partition


res2=NbClust(data=data1, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method = "complete", index = "all")
res2$All.index
res2$Best.nc
res2$Best.partition
res2$All.CriticalValues

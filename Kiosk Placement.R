library(data.table)
library(ggplot2)
bike = copy(Ch5_bike_station_locations)
setDT(bike)
str(bike)
head(bike)
grep('NA',bike)

ggplot(bike,aes(x=longitude,y=latitude)) + geom_point()
set.seed(123)
k3=kmeans(bike[,.(latitude,longitude)],3)
k3

bike[,clus3:=k3$cluster]
ggplot(bike,aes(x=longitude,y=latitude,color=clus3)) + geom_point()
bike[,clus3:=factor(clus3)]
ggplot(bike,aes(x=longitude,y=latitude,color=clus3)) + geom_point()

k3$centers
class(k3$centers)
k3$centers[1,1]
centdt=data.table(k3$centers)
centdt

ggplot(bike,aes(x=longitude,y=latitude,color=clus3)) + geom_point() + 
  geom_point(data=centdt,aes(x=longitude, y=latitude), colour="purple", shape=11,size=2)

set.seed(100)
k2 = kmeans(bike[,.(latitude,longitude)],2)
k2
bike[,clus2:=k2$cluster]
bike[,clus2:=factor(clus2)]
k2$centers
centdt2=data.table(k2$centers)

ggplot(bike,aes(x=longitude,y=latitude,color=clus2)) + geom_point() + 
  geom_point(data=centdt2,aes(x=longitude, y=latitude), colour="black", shape=19,size=2)

ggplot(bike,aes(x=longitude,y=latitude,color=clus3)) + geom_point() + 
  geom_point(data=centdt,aes(x=longitude, y=latitude), colour="purple", shape=11,size=2)+
  geom_point(data=centdt2,aes(x=longitude, y=latitude), colour="black", shape=19,size=2) +
  facet_wrap(~clus2)

library(geosphere)
dd=distm(c(40.777250, -73.872610), c(40.6895, -74.1745), fun = distHaversine)
dd
class(dd)
distm(c(40.777250, -73.872610), c(40.6895, -74.1745), fun = distHaversine) / 1609

res_matrix=distm(bike[,.(latitude,longitude)],centdt,fun=distHaversine)/1609
head(res_matrix)

bike[,c('k31','k32','k33'):=as.data.table(res_matrix)]
bike[clus3==1,c3dist:=k31]
bike[clus3==2,c3dist:=k32]
bike[clus3==3,c3dist:=k33]

res_matrix2=distm(bike[,.(latitude,longitude)],centdt2,fun=distHaversine)/1609
head(res_matrix2)

bike[,c('k21','k22'):=as.data.table(res_matrix2)]
bike[clus2==1,c2dist:=k21]
bike[clus2==2,c2dist:=k22]

bike[,c('k21','k22'):=NULL] #delete columns (NULL)

StaDist=bike[,.(clus3,k31,k32,k33)]
StaDist[,head(.SD,5),by = clus3]

StaDist[,rowMeans(.SD),.SDcols=2:4]
StaDist[,rowMeans(.SD),.SDcols=c('k31','k32','k33')]
StaDist[,pmin(k31,k32,k33)]

StaDist[,c('noK1','noK2','noK3'):=pmin(k31,k32,k33)]
StaDist[clus3==1,noK1:=pmin(k32,k33)]
StaDist[clus3==2,noK2:=pmin(k31,k33)]
StaDist[clus3==3,noK3:=pmin(k31,k32)]
StaDist[,.(mean(noK1),mean(noK2),mean(noK3))]

stadist = c(StaDist[,noK1],StaDist[,noK2],StaDist[,noK3])
#N is basically NROW
stanm = c(rep('noK1',StaDist[,.N]),rep('noK2',StaDist[,.N]),rep('noK3',StaDist[,.N]))
stanm
StaPlot = data.table(stadist,stanm)
StaPlot
ggplot(StaPlot,aes(x=stanm,y=stadist)) + geom_boxplot()

bike[,hdist:=StaDist$noK1]
bike[,summary(.SD),.SDcols=c('c3dist','c2dist','hdist')]

clusdist = c(bike[,c3dist],bike[,c2dist],bike[,hdist])
clusnm = c(rep('3 Kiosks',bike[,.N]),rep('2 Kiosks',bike[,.N]),rep('Hybrid Kiosks',bike[,.N]))
clusPlot = data.table(clusdist,clusnm)
ggplot(clusPlot,aes(x=clusdist)) + geom_histogram(breaks = seq(0,4,.5),color='white') + 
  scale_x_continuous(breaks=seq(0,4,.5)) + facet_wrap(~clusnm) 

bike[,distI:=c3dist-hdist]
ggplot(bike,aes(x=distI)) + geom_histogram(breaks = seq(-3,2.5,.5),color='white') + scale_x_continuous(breaks=seq(-3,4,.5))  

marketing = copy(Ch5_age_income_data)
setDT(marketing)
grep('NA',marketing)
str(marketing)
marketing[,summary(.SD)]
unique(marketing$bin)

marketing[,bin:=factor(bin,ordered = T)]
str(marketing)
ggplot(marketing,aes(x=bin,y=age)) + geom_boxplot(fill='pink')

ggplot(marketing,aes(x=bin,y=income)) + geom_boxplot(fill='yellow')

mk3=kmeans(marketing[,.(age,income)],3)
mk3plot = data.table(age=marketing$age,income=marketing$income,cluster=mk3$cluster)
mk3plot[,cluster:=factor(cluster)]
ggplot(mk3plot,aes(x=age,y=income,color=cluster)) + geom_point()

#scale converts values to Z-scores
marketing[,age_s:=scale(age)]
marketing[,income_s:=scale(income)]

mk3=kmeans(marketing[,.(age_s,income_s)],3)
mk3plot = data.table(age=marketing$age_s,income=marketing$income_s,cluster=mk3$cluster)
mk3plot[,cluster:=factor(cluster)]
ggplot(mk3plot,aes(x=age,y=income,color=cluster)) + geom_point()

#hclust is Hierarchy Clustering
hc = hclust(dist(marketing[,.(income_s,age_s)]),method = 'ward.D2')
hc

x=matrix(sample(5,10,replace = T), nrow = 5)
dist(x)

plot(hc)
library(dendextend)
dend = as.dendrogram(hc)
dend_six_color = color_branches(dend,k=6)
plot(dend_six_color,leaflab = 'none')

wss=c()
for(i in 2:10)
{
  km = kmeans(marketing[,.(income_s,age_s)],i)
  wss[i-1]=km$tot.withinss
}

elbowdt = data.table(num=2:10,wss)
ggplot(elbowdt,aes(x=num,y=wss)) + geom_line()

k5=kmeans(marketing[,.(income_s,age_s)],5)
k6=kmeans(marketing[,.(income_s,age_s)],6)
marketing[,k5:=k5$cluster]
marketing[,k6:=k6$cluster]
marketing[,h5:=cutree(dend,k=5)]
marketing[,h6:=cutree(dend,k=6)]
marketing[,k5:=factor(k5)]
marketing[,k6:=factor(k6)]
marketing[,h5:=factor(h5)]
marketing[,h6:=factor(h6)]
marketing

ggplot(marketing,aes(x=age,y=income,color=k5)) + geom_point()
ggplot(marketing,aes(x=age,y=income,color=k6)) + geom_point()
ggplot(marketing,aes(x=age,y=income,color=h5)) + geom_point()
ggplot(marketing,aes(x=age,y=income,color=h6)) + geom_point()

mm=marketing[,.(mean(age),median(age),max(age),min(age),mean(income),median(income),max(income),min(income)),by=h6]
names(mm)=c('group','mean_a','med_a','max_a','min_a','mean_i','med_i','max_i','min_i')
mm

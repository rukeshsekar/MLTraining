print("Om Mahaa Ganapataye Namah")

rm(list=(ls(all=TRUE)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         Import the Data set
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bfy <- read.csv("D:/dat1/bAffinity.csv",
                sep=",")
#bfy  <- na.omit(bfy)
bfy <- na.omit(bfy) # dropna(how='any')
View(bfy)
# str(bfy)
# names(bfy)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         # normalize the data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ndf <- bfy[,-c(1)]
str(ndf)
# mdf1 <- apply(ndf,2, median)

#mdf <- apply(ndf,2, mean)
#sdf <- apply(ndf,2, sd)

# Normalize/Standardize the data 
#z <- scale(ndf,mdf,sdf)
z <- scale(ndf)
distance <- dist(z)
print(distance,digits = 1)

#attributes(z)
#z  * attr(z, 'scaled:scale') + attr(z, 'scaled:center')
bfy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# fit the hierarchical  cluster ,default one is complete linkage 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hc <- hclust(distance)
plot(hc)

plot(hc, hang=-1)
plot(hc, hang=-1, labels = bfy$Regions)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# fit the single linkage cluster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sc <- hclust(distance, method = 'single')
plot(sc)

plot(sc, hang=-1, labels = bfy$Regions)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# fit the average cluster
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ac <- hclust(distance, method = 'average')
plot(ac)

plot(ac, hang=-1)
plot(ac, hang=-1, labels = bfy$Regions)

clust.mem <- cutree(hc,3)
plot(clust.mem )
aclust.mem <- cutree(ac,3)
plot(aclust.mem )
# compare complete linkage Vs Average Link
table(clust.mem, aclust.mem)
aggregate(bfy, list(clust.mem),mean)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# fit the Wards method
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wm <- hclust(distance, method = 'ward.D2')
plot(wm)

plot(wm, hang=-1)
plot(wm, hang=-1, labels = bfy$Regions)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# fit the k means clustering
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
kfit <- kmeans(z,3)
kfit

library(fpc)
bestk <- kmeansruns(z,k=2:20, criterion = 'ch')
bestk$bestk



# withinss - Vector of within-cluster sum of squares, 
# one component per cluster.
print (kfit)
print kfit$centers
print (kfit$withinss)

# tot.withinss(distortion) - Total within-cluster sum of
# squares, i.e. sum(withinss). The sum of all withinss
print (kfit$tot.withinss)
kfit$centers

# betweenss - The between-cluster sum of squares, 
# i.e. totss-tot.withinss.
print (kfit$withinss)

# Cluster - Number of cluster to which dataset belongs
print (kfit$cluster)

# totss - The sum of total of squre(Distance)
print (kfit$totss)
attributes(kfit$centers)

# kfit$centers * attr(kfit$centers, 'scaled:scale') + attr(kfit$centers, 'scaled:center')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot the scree plot
# Determine the optimal number of clusters 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nc <- 20

screePlot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
    }

screePlot(z)

branchSeg <- cbind(bfy,kfit$cluster)
branchSeg <- as.data.frame(branchSeg)
Segoutput <- branchSeg[order(branchSeg$`kfit$cluster`),]
custSegout <- data.frame(Segoutput)
write.csv(custSegout,"D:/dat1/custSeg2.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot the cluster 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(fpc)
library(cluster)
clusplot(bfy,kfit$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)
#? clusplot
clusplot(bfy,kmean.fit$cluster,color=TRUE,
         shade=TRUE, lines = 0, span = TRUE, labels = 2)



sqrt((3-1.25)^2 + (4-1.5)^2)













library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(ndf, col=kfit$clust, pch=16)

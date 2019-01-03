nci <- read.table('nci.txt')#load the dataset
nci <- t(nci)
label <- read.table('label.txt')
K <- nrow(unique(label))
Cluster.single <- hc(nci, K, 'single')# with single linkage
Cluster.complete <- hc(nci, K, 'complete')# with complete linkage
Cluster.average <- hc(nci, K, 'average')# with average linkage
set.seed(2401)
km.out=kmeans(nci,K,nstart=20)#k-means
as.vector(km.out$cluster)#gives out vectorized value of K-means function of R










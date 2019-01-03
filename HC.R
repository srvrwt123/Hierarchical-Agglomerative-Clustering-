hc <- function(X, K, method){
  source('distc.r')#connects to the r file distc for the further calculation of the function
  XMat <- as.matrix(dist(X))#show the value in matrix form
  XMat[XMat==0] <- NA
  Clu <- list()
  for (item in 1:nrow(X)) {Clu[[item]] <- item}#contrast between k-means and Hierarchical
    status <- c(rep(1,nrow(X)))
    CMat <- matrix(NA,2*nrow(X)-K,2*nrow(X)-K)
    CMat[1:nrow(XMat),1:ncol(XMat)] <- XMat
    fMat <- CMat
  fClu <- Clu[status==1]
  fClu1 <- c()
  for (item in 1:length(fClu)) fClu1[fClu[[item]]] <- item
  cat('The Cluster Number of each observation:\n',fClu1,'\n')
  return(list(fClu1, fClu))#returns the list of values derived above in the fuction
  
}

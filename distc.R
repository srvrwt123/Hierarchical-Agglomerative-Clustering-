distc <- function(X, CMat, Clu, method){
  CNum <- length(Clu)#length of the function Clu
  newClu <- Clu[[length(Clu)]]
  if(method == 'single'){#for the single linkage
    for (i in c(1:(CNum-1))[-newClu]) {
      tmp <- c()
      tmp.c <- 1
      for (j in Clu[[i]]) {
        for (k in newClu) {
          tmp[tmp.c] <- CMat[j,k]
          tmp.c <- tmp.c+1
        }
      }
      CMat[CNum,i] <- min(tmp)
      CMat[i,CNum] <- min(tmp)
    }
    return(CMat)#returns the value of Cmat which we got in the above loop
  }
  if(method == 'complete'){#for complete linkage
    for (i in c(1:(CNum-1))[-newClu]) {
      tmp <- c()
      tmp.c <- 1
      for (j in Clu[[i]]) {
        for (k in newClu) {
          tmp[tmp.c] <- CMat[j,k]
          tmp.c <- tmp.c+1
        }
      }
      CMat[CNum,i] <- max(tmp)
      CMat[i,CNum] <- max(tmp)
    }
    return(CMat)
  }
  if(method == 'average'){#for average linkage
    for (i in c(1:(CNum-1))[-newClu]) {
      tmp <- c()
      tmp.c <- 1
      for (j in Clu[[i]]) {
        for (k in newClu) {
          tmp[tmp.c] <- CMat[j,k]
          tmp.c <- tmp.c+1
        }
      }
      CMat[CNum,i] <- sum(tmp)/(length(Clu[[i]])*length(newClu))
      CMat[i,CNum] <- sum(tmp)/(length(Clu[[i]])*length(newClu))
    }
    return(CMat)
  }
  
}


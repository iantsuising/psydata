marr.lvl <- function(A){
    res <- rep(0,length(A))
    for(i in 1:length(A)){
        if(A[i]<3){res[i] <- A[i]}
        if(A[i]>=3){res[i] <- 3}
    }
    return(factor(res))
}

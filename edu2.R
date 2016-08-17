edu2 <- function(A){

    res <- rep(1,length(A))
    for(i in 1:length(A)){
        if(A[i] >= 3){res[i] <- 2}
    }
    return(as.factor(res))
}

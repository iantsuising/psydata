lvl<-function(A,upper,lower=NULL){
	res<-rep("Mild",length(A))
	if(is.null(lower)){lower<-upper}
	res[A<=lower]<-"No"
	res[A>=upper]<-"Serious"
	return(res)

}
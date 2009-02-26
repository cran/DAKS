#####################
# pattern frequency #
#####################

############################################
#                                          #
# This function computes the absolute      # 
# frequencies of the response patterns,    # 
# and optionally, the absolute frequencies #
# of a collection of specified knowledge   #
# states in a dataset.                     #
#                                          #
############################################

pattern<-function(dataset, n = 5, P = NULL){

pattern<-sort(table(apply(dataset,1, function(x) paste(x, collapse = ""))), decreasing = TRUE)

if(n < 1) stop("Number of patterns must be greater than zero.\n")
if(n > length(pattern)) n = length(pattern)

if(is.null(P)){
return(list(response.pattern = pattern[1:n], states = P))
}else{
states<-cbind(P, 0)
states[,ncol(states)] <- sapply(apply(P, 1, function(x) pattern[names(pattern) == paste(x, collapse = "")]), function(y) max(0, y))
return(list(response.pattern = pattern[1:n],states = states))
}
}
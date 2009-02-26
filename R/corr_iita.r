################## 
# corrected IITA #
##################

##############################################
#                                            # 
# This function performs the corrected       #
# inductive item tree analysis procedure     # 
# and returns the corresponding diff values. #
#                                            # 
##############################################

corr_iita<-function(dataset, A){
b<-ob_counter(dataset)
m<-ncol(dataset)
n<-nrow(dataset)

bs_neu<-list()
for(i in 1:length(A)){
bs_neu[[i]]<-matrix(0,ncol = m, nrow = m)
}
diff_value_neu<-rep(0,length(A))

error_neu<-rep(0,length(A))

#computation of error rate
for(k in 1:length(A)){
for(i in A[[k]]){
error_neu[k]<-error_neu[k] + ((b[as.integer(i[1]), as.integer(i[2])]) / sum(dataset[,as.integer(i[2])])) 
}
if(set_is_empty(A[[k]])){error_neu[k]<-NA}
if(set_is_empty(A[[k]]) == FALSE) {error_neu[k]<-error_neu[k] / length(A[[k]])}
}

#computation of diff values
for(k in 1:length(A)){
if(set_is_empty(A[[k]])){diff_value_neu[k]<-NA}
else{
for(i in A[[length(A)]]){
if(is.element(set(i), A[[k]])) {bs_neu[[k]][as.integer(i[1]),as.integer(i[2])]<-error_neu[k] * sum(dataset[,as.integer(i[2])])}
if(is.element(set(i), A[[k]]) == FALSE && is.element(set(tuple(as.integer(i[2]),as.integer(i[1]))), A[[k]]) == FALSE){bs_neu[[k]][as.integer(i[1]),as.integer(i[2])]<-(1- sum(dataset[,as.integer(i[1])]) / n) * sum(dataset[,as.integer(i[2])])}
if(is.element(set(i), A[[k]]) == FALSE && is.element(set(tuple(as.integer(i[2]),as.integer(i[1]))), A[[k]]) == TRUE){bs_neu[[k]][as.integer(i[1]),as.integer(i[2])]<-sum(dataset[,as.integer(i[2])]) - sum(dataset[,as.integer(i[1])]) + sum(dataset[,as.integer(i[1])]) * error_neu[k]}
}
diff_value_neu[k]<-sum((b - bs_neu[[k]])^2) / (m^2 - m)
}
}
return(diff_value_neu)
}
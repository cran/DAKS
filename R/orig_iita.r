################# 
# original IITA #
#################

##############################################
#                                            # 
# This function performs the original        #
# inductive item tree analysis procedure     # 
# and returns the corresponding diff values. #
#                                            # 
##############################################

orig_iita<-function(dataset, A){
b<-ob_counter(dataset)
m<-ncol(dataset)
n<-nrow(dataset)

bs<-list()
for(i in 1:length(A)){
bs[[i]]<-matrix(0,ncol = ncol(b), nrow = nrow(b))
}
diff_value_alt<-rep(0,length(A))

error<-rep(0,length(A))

#computation of error rate
for(k in 1:length(A)){
for(i in A[[k]]){
error[k]<-error[k] + ((b[as.integer(i[1]), as.integer(i[2])]) / sum(dataset[,as.integer(i[2])])) 
}
if(set_is_empty(A[[k]])){error[k]<-NA}
if(set_is_empty(A[[k]]) == FALSE) {error[k]<-error[k] / length(A[[k]])}
}

#computation of diff values
for(k in 1:length(A)){
if(set_is_empty(A[[k]])){diff_value_alt[k]<-NA}
else{
for(i in A[[length(A)]]){
if(is.element(set(i), A[[k]])) {bs[[k]][as.integer(i[1]), as.integer(i[2])]<-error[k] * sum(dataset[,as.integer(i[2])])}
else{bs[[k]][as.integer(i[1]), as.integer(i[2])]<-(1- sum(dataset[,as.integer(i[1])]) / n) * sum(dataset[,as.integer(i[2])]) * (1-error[k])
}
}
diff_value_alt[k]<-sum((b - bs[[k]])^2) / (m^2 - m)
}
}

return(diff_value_alt)
}
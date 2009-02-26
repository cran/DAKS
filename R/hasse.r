############
# Plotting #
############

################################################################
#                                                              #
# This function plots the Hasse diagram of a surmise relation. #
#                                                              #
################################################################

hasse<-function(imp, items){
struct<-relation(domain = list(1:items,1:items),graph = imp)

#computation of parallel items
parallel<-list()
k<-1
for(i in 1:items){
for(j in i:items){
if(relation_incidence(struct)[i,j] ==1 && relation_incidence(struct)[j,i] ==1){
parallel[[k]]<-tuple(i,j)
k<-k+1
}
}
}

#collapsing of parallel items
if(length(parallel) > 0){
pardrop<-vector(length = length(parallel))
for(i in 1:length(parallel)){
pardrop[i]<-as.integer(parallel[[i]][2])
}
pardrop<-pardrop[!duplicated(pardrop)]

nparitems<-1:items
nparitems<-nparitems[-pardrop]
struct<-relation(domain = list(nparitems, nparitems), incidence = relation_incidence(struct)[-pardrop, -pardrop])
}

#plotting
plot(struct)

#returning a list of parallel items
return(parallel)
}
print.iita<-function(x, ...){
cat("\n \t Inductive Item Tree Analysis\n")
cat("\nAlgorithm:")
if(x$v == 1){cat(" minimized corrected IITA\n")}
if(x$v == 2){cat(" corrected IITA\n")}		
if(x$v == 3){cat(" original IITA\n")}		
cat("diff values:",round(x$diff, digits = 3), " ")
cat("\nquasi order:")
print(x$implications)
cat("error rate: ")
write(round(x$error.rate, digits= 3 ), "")
cat("index in the selection set: ")	
write(x$selection.set.index, "")
}

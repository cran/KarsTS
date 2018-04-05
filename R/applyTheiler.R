applyTheiler <-
function(RM,thW){
  
  YmX <- RM$ones$Y - RM$ones$X
  aa <- which(YmX > thW)
  RM$ones <- RM$ones[aa,]
  rownames(RM$ones) <- NULL
  RM
  
}

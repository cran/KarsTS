getProTaos <-
function(RecMat,xlim = NULL,
                       main = NULL,doPlot = TRUE){
  
  getPOneTao <- function(tao,difO){length(which(difO == tao))}
  
  embShortening <- (RecMat$embDim - 1) * RecMat$delay
  dimRecMat <- min(RecMat$tsLength - embShortening)
  
  taos <- 1:(dimRecMat-1) 
  difO <- RecMat$ones$Y - RecMat$ones$X
  
  res <- apply(as.matrix(taos),1,FUN = getPOneTao, difO = difO)
  res <- res/(dimRecMat-taos)
  res <- c(1,res)
  
  if(doPlot == TRUE){
    
    graphics::barplot(res,col = "deeppink3", 
                      border = "deeppink4",
                      xlim = xlim,width = 1,
                      main = main,space = 0,
                      xlab = "lag",ylab = "RP",
                      xpd = FALSE)
    
    pu <- graphics::par("usr")
    nt <- grDevices::axisTicks(pu[1:2], log = FALSE, n = 10)
    graphics::axis(side = 1, at = nt + 0.5,labels = nt)
    
  }
  
  res <- list(Tao = c(0,taos), Prob = res )
  
}

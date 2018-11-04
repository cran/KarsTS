getCRP <-
function(prob1,prob2,xLims,doPlot = FALSE,main = "plotTitle"){
  
  prob1 <- prob1[xLims[1]:xLims[2]]
  prob2 <- prob2[xLims[1]:xLims[2]]
  
  aa <- stats::complete.cases(cbind(prob1,prob2))
  prob1 <- prob1[aa]
  prob2 <- prob2[aa]
  pm1 <- prob1-mean(prob1,na.rm = TRUE)
  pm2 <- prob2-mean(prob2,na.rm = TRUE)
  s1 <- stats::sd(prob1,na.rm = TRUE)
  s2 <- stats::sd(prob2,na.rm = TRUE)
  res <- pm1*pm2
  
  if(doPlot == TRUE){
    
    graphics::barplot(res,col = "cyan3", 
                      border = "cyan4",
                      xlim = xlim,width = 1,
                      main = main,space = 0,
                      xlab = "lag",ylab = "RP",
                      xpd = FALSE)
    
    pu <- graphics::par("usr")
    nt <- grDevices::axisTicks(pu[1:2], log = FALSE, n = 10)
    graphics::axis(side = 1, at = nt + 0.5,labels = nt)
    
    
  }
  
  CPR <- sum(pm1*pm2)/(s1*s2)
  CPR/length(aa)
  
}

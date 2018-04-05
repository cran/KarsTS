genRmExample <-
function(name = "SRM", InKTSEnv = TRUE, plotRM = FALSE){
  
  # generate time series
  TS <- genTSExample(stationary = FALSE, InKTSEnv = FALSE)[1:1000,]
  TS$value <- TS$value + 500*sin(as.numeric(TS$time)/1000000)
  
  # get recurrence points
  res <- getRecurrencePoints(timSer = TS,
                             embedDim = 1, 
                             lagDelay = 0, 
                             threshold = 20)
  
  recPointsX <- res$recPointsX
  recPointsY <- res$recPointsY
  
  # pretty the result
  if (length(recPointsY) == 0) {
    
    warning("No recurrence point was found. Run the example again")
    
  } else {
    
    recPoints <- as.data.frame(cbind(recPointsX, recPointsY))
    colnames(recPoints) <- c("X", "Y")
    newSimpRM <- list(ones = recPoints, tol = 20, 
                      tsName = "TS", 
                      embDim = 1, delay = 0, 
                      dist = "inf_norm", 
                      tsLength = nrow(TS), 
                      samPerSec = diff(as.numeric(TS$time[1:2])), 
                      tsIni = as.character(TS$time[1]), 
                      type = "simple")
    
    if(InKTSEnv == TRUE){
      # assign to KTSEnv environment
      assign(name, newSimpRM, envir = KTSEnv)
      assign(paste0(name,"TS"), TS, envir = KTSEnv)
    }
    
    if( plotRM == TRUE){
      
      if(length(recPointsX) > 30000){
        rcp <- sort(sample(1:length(recPointsX), 30000))
      }else{
        rcp <- 1:length(recPointsX)
      }
      
      graphics::par(pty="s") 
      graphics::plot(TS$time[recPointsX[rcp]],TS$time[recPointsY[rcp]], 
                     main = name, cex = 0.3, col = "darkblue", 
                     xlab = "", ylab = "")
      graphics::points(TS$time[recPointsY[rcp]],TS$time[recPointsX[rcp]],
                       cex = 0.3, col = "darkblue")
      graphics::points(1:nrow(TS),1:nrow(TS), cex = 0.3, col = "darkblue")
      graphics::par(pty="m")
      
    }
    
    list(newSimpRM = newSimpRM,TS = TS)
    
  }
  
}

genGapExample <-
function(timSer, lGaps, nGaps, name = "GS", InKTSEnv = TRUE){
  
  # generate nGaps gaps of lGaps length each
  newGapsIndices <- getNewGapsInd(timSer, lGaps, nGaps)
  
  # create gap set with necessary information
  newGapSet <- list(gaps = newGapsIndices, 
                    tsIni = as.character(timSer$time[1]), 
                    tsEnd = as.character(timSer$time[nrow(timSer)]), 
                    samPerMin = (diff(as.numeric(timSer$time[1:2])))/60, 
                    tsLength = nrow(timSer), 
                    tsName = deparse(substitute(timSer)))
  
  if(InKTSEnv == TRUE){
    # assign to KTSEnv environment
    assign(name, newGapSet, envir = KTSEnv)
  }
  newGapSet
  
}

refreshDataSetsList <-
function(outp = TRUE) {
  removeIfExists("dSList", envir = KTSEnv)
  tsLoaded <- tsDetect()
  gapLoaded <- gapDetect()
  rmLoaded <- rmDetect()
  if (is.null(tsLoaded)) {
    nTS <- NULL
  } else {
    nTS <- length(tsLoaded)
  }
  
  if (is.null(gapLoaded)) {
    nGaps <- NULL
  } else {
    nGaps <- length(gapLoaded)
  }
  if (is.null(rmLoaded)) {
    nRM <- NULL
  } else {
    nRM <- length(rmLoaded)
  }
  dSList <- list(TS = tsLoaded, gaps = gapLoaded, 
                 rm = rmLoaded, nTS = nTS, nGaps = nGaps, 
                 nRM = nRM)
  
  assign("dSList", dSList, envir = KTSEnv)
  
  if (outp != FALSE) {
    writeOutpTs <- function(nameTS) {
      timSer <- get(nameTS, envir = KTSEnv)
      lTimSer <- nrow(timSer)
      sampPers <- getUniqueSampPer(timSer)
      sampPers <- sampPers[which(sampPers[, 1] > 10), 2]
      txt <- c(paste("TIME SERIES:", nameTS), 
               paste("First date:", as.character(timSer$time[1])), 
               paste("Last date:", as.character(timSer$time[lTimSer])), 
               paste("Length:", as.character(lTimSer)), 
               paste("Likely sampling periods(sec):",
                     paste(sampPers, collapse = ",")))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste(txt, collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
    }
    writeOutpGap <- function(nameGap) {
      gapSet <- get(nameGap, envir = KTSEnv)
      txt <- c(paste("SET OF GAPS:", nameGap), 
               paste("Number of NAs:", 
                     as.character(length(gapSet$gaps))), 
               paste("Original time series:", gapSet$tsName), 
               "Characteristics of the original time series:", 
               paste("initial and final dates:", 
                     gapSet$tsIni, ";", gapSet$tsEnd), 
               paste("length and sampling period:", 
                     gapSet$tsLength, ";", gapSet$samPerMin))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      paste(txt, collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
    }
    writeOutpRM <- function(nameRM) {
      recMat <- get(nameRM, envir = KTSEnv)
      if (recMat$type != "cross") {
        warningRM = paste("Number of ones (only",
                          "upper triangle,without diag.):", 
                          nrow(recMat$ones))
      } else {
        warningRM = paste("Number of ones (both triangles and diag.):", 
                          nrow(recMat$ones))
      }
      txt <- c(paste("RECURRENCE MATRIX:", nameRM), warningRM, 
               paste("Type:", recMat$type), 
               paste("Tolerance:", paste(recMat$tol, collapse = ",")), 
               paste("Distance:", paste(recMat$dist, collapse = ",")), 
               paste("Embedding dimension:", 
                     paste(recMat$embDim, collapse = ",")), 
               paste("Delay:", paste(recMat$delay, collapse = ",")), 
               paste("Original time series:", 
                     paste(recMat$tsName, collapse = ",")), 
               paste("Lengths:", paste(recMat$tsLength, collapse = ",")), 
               paste("Sampling periods:", 
                     paste(recMat$samPerSec, collapse = ",")), 
               paste("Initial dates:", paste(recMat$tsIni, collapse = ",")))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste(txt, collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
    }
    tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                    paste("LOADED OBJECTS", collapse = "\n"))
    tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
    tcltk::tkinsert(KTSEnv$txtWidget, "end", date())
    tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
    if (class(KTSEnv$dSList$TS) == "character") {
      apply(as.matrix(KTSEnv$dSList$TS), 1, FUN = writeOutpTs)
    } else {
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      paste("TIME SERIES:none", collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
    }
    if (class(KTSEnv$dSList$gaps) == "character") {
      apply(as.matrix(KTSEnv$dSList$gaps), 1, FUN = writeOutpGap)
    } else {
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      paste("SETS OF GAPS:none", collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
    }
    if (class(KTSEnv$dSList$rm) == "character") {
      apply(as.matrix(KTSEnv$dSList$rm), 1, FUN = writeOutpRM)
    } else {
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      paste("RECURRENCE MATRICES:none",collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
    }
    endingLines()
  }
  
}

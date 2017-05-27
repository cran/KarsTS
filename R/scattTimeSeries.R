scattTimeSeries <-
function() {
  plotpartzoom <- function() {
    
    verifyDimLag <- function(entry) {
      
      res <- c(NA, NA)
      numberOfCommas <- try(length(which(strsplit(entry, 
                                                  split = NULL)[[1]] == ",")), 
                            silent = TRUE)
      if (class(numberOfCommas) == "try-error") {
        res <- c(NA, NA)
      } else if (numberOfCommas != 1) {
        res <- c(NA, NA)
      } else if (strsplit(entry, split = ",")[[1]][1] == ",") {
        res <- c(NA, NA)
      } else if (strsplit(entry, split = ",")[[1]][2] == ",") {
        res <- c(NA, NA)
      } else {
        res <- separateEntry(entry, class1 = verifyIntEntry, 
                             class2 = verifyIntEntry, 
                             noValid = NA)
      }
      
      res
      
    }
    puntos <- tcltk::tclvalue(KTSEnv$ele1cbValue)
    lineas <- tcltk::tclvalue(KTSEnv$ele2cbValue)
    refreshDataSetsList(outp = FALSE)
    tssel <- tsCheckedTF()
    tsToPlotNames <- KTSEnv$dSList$TS[which(tssel == TRUE)]
    nTsToPlot <- length(tsToPlotNames)
    
    if (any(tssel == TRUE)) {
      embDimEntry <- readMultEntryvalues(KTSEnv$dSList$nTS, 
                                         prefix = "tEntscbValue", 
                                         type = "Char")
      embDimEntry <- embDimEntry[which(tssel == TRUE)]
      
      dimsAndDelays <- apply(as.matrix(embDimEntry), 1, FUN = verifyDimLag)
      
      if (is.vector(dimsAndDelays)) {
        dimsAndDelays <- as.matrix(dimsAndDelays)
      }
      
      embDims <- dimsAndDelays[1, ]
      lagDelay <- dimsAndDelays[2, ]
      totDim <- sum(embDims)
      
    } else {
      
      embDims <- NA
      lagDelay <- NA
      totDim <- NA
    }
    
    if (nTsToPlot == 0 | nTsToPlot > 3) {
      tcltk::tkmessageBox(message = "Choose 1,2, or 3 time series", 
                          icon = "warning")
    } else if (is.na(totDim) | any(is.na(embDims))) {
      tcltk::tkmessageBox(message = paste("Choose embedding dimensions",
                                          "for the selected time series.",
                                          "Make sure that the entries are",
                                          "correct (see the note below)"), 
                          icon = "warning")
    } else if (totDim < 2 | totDim > 3) {
      tcltk::tkmessageBox(message = paste("The total dimension must be 2",
                                          "or 3, that is: 1 time series",
                                          "with embedding dim 2 or 3; 2",
                                          "times series, one of them possibly",
                                          "with emb.dim 2 or, finally, one",
                                          "time series with emb. dim 2 or 3"), 
                          icon = "warning")
      
    } else if (lineas == "0" & puntos == "0") {
      tcltk::tkmessageBox(message = "Choose lines, points or both", 
                          icon = "warning")
      
    } else if (any(is.na(lagDelay))) {
      tcltk::tkmessageBox(message = paste("Choose delays for",
                                          "the selected time series"), 
                          icon = "warning")
    } else {
      
      tmComptibility <- matrix(rep(FALSE, 3 * nTsToPlot), 
                                  nTsToPlot, 3)
      firsTS <- get(tsToPlotNames[1], envir = KTSEnv)
      for (i in 1:nTsToPlot) {
        tmComptibility[i, ] <- are2TsTimeCompatible(firsTS, 
                                                       get(tsToPlotNames[i],
                                                           envir = KTSEnv))
      }
      
      if (any(tmComptibility[, 1] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The initial date of all the",
                                            "time series must be the same"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 2] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The sampling period of all",
                                            "the time series must",
                                            "be the same"), 
                            icon = "warning")
      } else {
        
        L <- NULL
        for (i in 1:nTsToPlot) {
          L <- c(L, length(get(tsToPlotNames[i], envir = KTSEnv)$value))
        }
        
        maxL <- max(L)
        
        dataMatrix <- matrix(NA, maxL, totDim)
        colnamesMatrix <- rep(NA, totDim)
        cont <- 1
        for (i in 1:nTsToPlot) {
          
          X <- get(tsToPlotNames[i], envir = KTSEnv)$value
          if (embDims[i] > 1) {
            embX <- tseriesChaos::embedd(as.matrix(X), 
                                         m = embDims[i], d = lagDelay[i])
            
            dataMatrix[1:nrow(embX), cont:(cont + embDims[i] - 1)] <- embX
            colnamesMatrix[cont:(cont + embDims[i] - 
                                   1)] <- paste0(tsToPlotNames[i], 1:embDims[i])
            cont <- cont + embDims[i]
            
          } else {
            
            dataMatrix[1:L[i], cont] <- X
            colnamesMatrix[cont] <- tsToPlotNames[i]
            cont <- cont + 1
          }
          rm(X)
        }
        
        
        
        if (totDim == 2 & lineas == "1" & puntos == "1") {
          grDevices::dev.new(noRStudioGD = TRUE)
          graphics::plot(dataMatrix[, 1], dataMatrix[, 2], 
                         xlab = colnamesMatrix[1], 
                         ylab = colnamesMatrix[2], col = "darkblue")
          graphics::lines(dataMatrix[, 1], dataMatrix[, 2], 
                          col = "darkblue")
          
        } else if (totDim == 2 & lineas == "1" & puntos == "0") {
          grDevices::dev.new(noRStudioGD = TRUE)
          graphics::plot(dataMatrix[, 1], dataMatrix[, 2], type = "l", 
                         xlab = colnamesMatrix[1], 
                         ylab = colnamesMatrix[2], col = "darkblue")
          
        } else if (totDim == 2 & lineas == "0" & puntos == "1") {
          grDevices::dev.new(noRStudioGD = TRUE)
          graphics::plot(dataMatrix[, 1], dataMatrix[, 2], 
                         xlab = colnamesMatrix[1], 
                         ylab = colnamesMatrix[2], col = "darkblue")
          
        } else if (totDim == 3 & lineas == "1" & puntos == "1") {
          
          rgl::plot3d(dataMatrix[, 1], dataMatrix[, 2], dataMatrix[, 3], 
                      xlab = colnamesMatrix[1], ylab = colnamesMatrix[2], 
                      zlab = colnamesMatrix[3], 
                      col = "darkblue", type = "p")
          rgl::plot3d(dataMatrix[, 1], dataMatrix[, 2], dataMatrix[, 3], 
                      add = TRUE, col = "darkblue", type = "l")
          
          
        } else if (totDim == 3 & lineas == "1" & puntos == "0") {
          
          rgl::plot3d(dataMatrix[, 1], dataMatrix[, 2], dataMatrix[, 3], 
                      xlab = colnamesMatrix[1], ylab = colnamesMatrix[2], 
                      zlab = colnamesMatrix[3], 
                      col = "darkblue", type = "l")
          
        } else if (totDim == 3 & lineas == "0" & puntos == "1") {
          
          rgl::plot3d(dataMatrix[, 1], dataMatrix[, 2], dataMatrix[, 3], 
                      xlab = colnamesMatrix[1], ylab = colnamesMatrix[2], 
                      zlab = colnamesMatrix[3], 
                      col = "darkblue", type = "p")
          
        }
        
      }
      
    }
  }
  showPANplotTs <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "PHASE PORTRAITS")
    createTitle(labTitle = "Time series")
    myApplyVector(FUN = createChbEntry, 1:KTSEnv$dSList$nTS, 
                  elements = KTSEnv$dSList$TS, 
                  prefix = "scbValue")
    createTitle(labTitle = "Type")
    createChb(labTitle = "Lines", variableName = "ele2cbValue", 
              defaultVal = "1")
    createChb(labTitle = "Points", variableName = "ele1cbValue", 
              defaultVal = "0")
    createOK(labTitle = "ZOOM", action = plotpartzoom)
    createNote(labTitle = "In the text entries, write the embedding dimension", 
               pady = c(10, 0))
    createNote(labTitle = "and the delay separated by a comma", pady = c(1, 0))
    createNote(labTitle = "In case there is no embedding, write: 1,0",
               pady = c(1,10))
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  if (exists("SubPanR4C3", envir = KTSEnv)) {
    tcltk::tkdestroy(KTSEnv$SubPanR4C3)
  }
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANplotTs", 
               envirName = environment(showPANplotTs))
}

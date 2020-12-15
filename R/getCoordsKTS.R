getCoordsKTS <-
function() {
    selAndPlotGetCoor <- function() {
      
      KTSEnv$selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                          noValid = NA)
      
      KTSEnv$poiSs <- verifyRealEntry(tcltk::tclvalue(KTSEnv$poiS),noValid = NA)
      
      if (is.na(KTSEnv$selTsName)) {
        tcltk::tkmessageBox(message = "Choose a time series", 
                            icon = "warning")
      } else {
        
        
        idkts.gc<- function(selTs,col, poiSs){
          
          rr <- try(graphics::identify(selTs$time,selTs$value,n=1, plot = FALSE),
                    silent=TRUE)
          if(class(rr)!="try-error"){
            
            KTSEnv$grP <- sort(c(rr,KTSEnv$grP))
            graphics::points(selTs$time[KTSEnv$grP],
                             selTs$value[KTSEnv$grP],
                             col=col, cex = poiSs, pch = 19, bg= col)
            
          }
          
          try(idkts.gc(selTs,col, poiSs), silent = TRUE)
          
          
        }
        
        col = "green"
        try(grDevices::dev.off(), silent = TRUE)
        selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
        grDevices::dev.new(noRStudioGD = TRUE)
        graphics::plot(selTs, cex = KTSEnv$poiSs,pch = 19)
        graphics::points(selTs[KTSEnv$grP,],col = col,pch = 19,bg = col, cex = KTSEnv$poiSs)
        idkts.gc(selTs,col = col, poiSs = KTSEnv$poiSs)
        
        
      }
    }
    writeCoor <- function() {
      
      
      if (length(KTSEnv$grP) == 0) {
        tcltk::tkmessageBox(message = paste("Choose at least one point"),
                            icon = "warning")
      } else {
        KTSEnv$grP <- sort(KTSEnv$grP)
        selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
        greenedTable <- data.frame(index = KTSEnv$grP,
                                   date = selTs$time[KTSEnv$grP],
                                   value = selTs$value[KTSEnv$grP])
        txt1 <- c("SELECTED POINTS", paste("Time series:", KTSEnv$selTsName))
        txt6 <- utils::capture.output(print.data.frame(greenedTable))
        tcltk::tkinsert(KTSEnv$txtWidget, "end",
                        paste(txt1, collapse = "\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end",
                        paste(txt6, collapse = "\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
        
        grDevices::dev.new(noRStudioGD = TRUE)
        graphics::plot(selTs, cex = KTSEnv$poiSs, pch = 19)
        graphics::points(selTs[KTSEnv$grP,], cex = KTSEnv$poiSs, pch = 19, 
                         col = "orange",bg = "orange")
        
      }
    }
    showPANgetCoords <- function() {
      if (exists("selTsName", envir = KTSEnv)) {
        KTSEnv$selTsName <- "0"
      }
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "GET COORDINATES")
      createTsRb()
      createEntry(labTitle = "Point size", 
                  textVariableName = "poiS", defaultVal = "1")
      createOK(labTitle = "PLOT", action = selAndPlotGetCoor)
      createOK(labTitle = "WRITE RESULTS", action = writeCoor, width = 14)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANgetCoords", 
                 envirName = environment(showPANgetCoords))
  }

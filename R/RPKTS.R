RPKTS <-
function() {
    
    RPKTSOnOk <- function() {
      
      minLag <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numlag), 
                               noValid = NA)
      
      maxLag <- verifyIntEntry(tcltk::tclvalue(KTSEnv$nuMlag), 
                               noValid = NA)
      rsel <- rmCheckedTF()
      checkedRM <- which(rsel == TRUE)
      lCheckedRM <- length(checkedRM)
 
      getOneRP <- function(selRm,selRmName,dimRecMat,minLag,maxLag){
        
        RPUniPlot <- function() {
          
          selRm <- get("selRm", envir = environment(RPUniPlot))
          minLag <- get("minLag", envir = environment(RPUniPlot))
          maxLag <- get("maxLag", envir = environment(RPUniPlot))
          selRmName <- get("selRmName", envir = environment(RPUniPlot))
          RP <-  getProTaos(RecMat = selRm,
                            xlim = c(minLag,maxLag),
                            main = paste(selRmName,"RP"), 
                            doPlot = TRUE)
          assign("RP",RP, envir = environment(RPUniPlot))
          
        }
        
        grDevices::dev.new(noRStudioGD = TRUE)
        RPUniPlot()
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        
        RP
      }
      
      if (lCheckedRM < 1 | lCheckedRM > 2) {
        
        tcltk::tkmessageBox(message = paste("Choose one or",
                                            "two time series"), 
                            icon = "warning")
        
      } else if (lCheckedRM == 1) {
        
        selRmName <- KTSEnv$dSList$rm[checkedRM]
        selRm <- get(selRmName, envir = KTSEnv)
        embShortening <- (selRm$embDim - 1) * selRm$delay
        dimRecMat <- min(selRm$tsLength - embShortening)
        if(is.na(minLag)){minLag <- 0}
        if(is.na(maxLag)){maxLag <- dimRecMat}
        
        RP <- getOneRP(selRm,selRmName,dimRecMat,minLag,maxLag)
        
        cleanEnvir()
        showPANRPKTS()
        
      } else if (lCheckedRM == 2) {
        
        selRmName <- KTSEnv$dSList$rm[checkedRM]
        selRm1 <- get(selRmName[1], envir = KTSEnv)
        selRm2 <- get(selRmName[2], envir = KTSEnv)

        if (selRm1$samPerSec != selRm2$samPerSec) {
          
          tcltk::tkmessageBox(message = paste("The recurrence matrices",
                                              "come from time series",
                                              "with different sampling periods"), 
                              icon = "warning")
        } else {
          
          if (selRm1$tsIni != selRm2$tsIni) {
           
            tcltk::tkmessageBox(message = paste("There recurrence matrices ", 
                                                "come from time series",
                                                "starting on different dates"), 
                                icon = "warning")
            
          }
          
          embShortening1 <- (selRm1$embDim - 1) * selRm1$delay
          dimRecMat1 <- min(selRm1$tsLength - embShortening1)
          embShortening2 <- (selRm2$embDim - 1) * selRm2$delay
          dimRecMat2 <- min(selRm2$tsLength - embShortening2)
          dimRecMat <- min(dimRecMat1,dimRecMat2)
          
          if(is.na(minLag)){minLag <- 0}
          
          if(is.na(maxLag)){maxLag <- dimRecMat}
          
          RP1 <- getOneRP(selRm1,selRmName[1],dimRecMat,minLag,maxLag)
          RP2 <- getOneRP(selRm2,selRmName[2],dimRecMat,minLag,maxLag)
          CRP <- getCRP(prob1 = RP1$Prob,prob2 = RP2$Prob,
                        xLims =c(minLag,maxLag),
                        doPlot = TRUE,
                        main = paste(selRmName[1],selRmName[2],"RP"))
          
          txt1 <- paste("CPR = ",round(CRP,4))
          txt2 <- paste("Recurrence matrices:",selRmName[1],",",selRmName[2])
          writeMethodTitle("CORRELATION PROBABILITY OF RECURRENCE")
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste(txt1, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste(txt2, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          endingLines()
          
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANRPKTS()
          
        }
        
      }
      
    }
    showPANRPKTS <- function() {
      
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "RECURRENCE PROBABILITY")
      createRmChb()
      createEntry(labTitle = "Minimum number of lags", 
                  textVariableName = "numlag")
      createEntry(labTitle = "Maximum nuMber of lags", 
                  textVariableName = "nuMlag")
      createOK(labTitle = "PLOT", action = RPKTSOnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyRm(action = "showPANRPKTS", 
                 envirName = environment(showPANRPKTS))
    
  }

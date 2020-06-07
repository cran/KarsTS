naSplinesKTS <-
function (){
  
  showPANnaspline <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "SPLINES INTERPOLATION")
    if (is.null(KTSEnv$dSList$gaps) == FALSE) {
      createGapRb()
    }
    createTsRb()
    createEntry(labTitle = "Period", textVariableName = "peri",
                defaultVal = "1")
    createOK(labTitle = "RUN", action = nsOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
  }
  nsOnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                 noValid = NA)
    peri <- verifyIntEntry(tcltk::tclvalue(KTSEnv$peri),
                           noValid = NA)
    
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    }else {
      selTs <- get(selTsName, envir = KTSEnv)
      gapToUse <- gapForSelMethod(selTsName, selTs)
      selGap <- gapToUse$selGap
      selGapName <- gapToUse$selGapName
      nasInSelTs <- which(is.na(selTs$value))
      tmComptibility <- areTsGapTimeCompatible(selTs, selGap)
      if (length(nasInSelTs) == 0) {
        tcltk::tkmessageBox(message = paste("The selected time", 
                                            "series contains no NAs"), 
                            icon = "warning")
      }else if (length(selGap$gaps) == 0) {
        tcltk::tkmessageBox(message = "The gap set is empty", 
                            icon = "warning")
      }else if (length(setdiff(union(selGap$gaps, nasInSelTs), 
                              nasInSelTs)) != 0) {
        tcltk::tkmessageBox(message = paste("Some NAs in the gap set ", 
                                            "do not exist in the time", 
                                            "series. Check that the", 
                                            "selected gap set comes", 
                                            "from the selected", 
                                            "time series"), 
                            icon = "warning")
      }else if (tmComptibility[1] == FALSE) {
        tcltk::tkmessageBox(message = paste("The initial date of the", 
                                            "time series and the one", 
                                            "stored in the gap", 
                                            "set do not match"), 
                            icon = "warning")
      }else if (tmComptibility[2] == FALSE) {
        tcltk::tkmessageBox(message = paste("The sampling period", 
                                            "of the time series", 
                                            "and the one stored", 
                                            "in the gap set", 
                                            "do not match"), 
                            icon = "warning")
      }else if (tmComptibility[3] == FALSE) {
        tcltk::tkmessageBox(message = paste("The time series is", 
                                            "shorter than some", 
                                            "indices stored", 
                                            "in the set of gaps"), 
                            icon = "warning")
      }else {
        
        if(is.na(peri)){peri <- 1}
        
        filledTS <- selTs
        
        for (ppp in 1:peri){
          
          posi.ppp <- seq(ppp, nrow(selTs),peri)
          
          gg <- rep(3,nrow(selTs))
          gg[selGap$gaps] <- 2
          gg <- gg[posi.ppp]
          gg <- which(gg == 2)
          
          if(length(gg) > 0){
            
            filledTS.i <- selTs[posi.ppp,]
            rownames(filledTS.i) <- NULL
            
            filledTS.i$value[gg] <- zoo::na.spline(filledTS.i$value, 
                                                   xout = gg, 
                                                   na.rm = FALSE)
            
            
            filledTS$value[posi.ppp] <- filledTS.i$value
            
            rm(filledTS.i)
            
          }
          
          rm(gg,posi.ppp)
          
        }
        
        
        assign(paste0(selTsName, "_", selGapName, "_spl"), 
               filledTS, envir = KTSEnv)
        gapsAfterFill <- getGapsAfterFill(filledTS, selGap, 
                                          envir = environment(nsOnOk))
        remainingNAsInGap <- gapsAfterFill$remainingNAsInGap
        filledNasTable <- gapsAfterFill$filledNasTable
        if(peri == 1){
          txtPeri <- "Period: none (1)"
        }else{
          txtPeri <- paste("Period:", peri) 
        }
        writeMethodTitle("SPLINES INTERPOLATION")
        tcltk::tkinsert(KTSEnv$txtWidget, "end", txtPeri)
        tcltk::tkinsert(KTSEnv$txtWidget, "end", "\\n")
        writeMethodSummary(filledNasTable, remainingNAsInGap, 
                           selTsName, selGapName, selGap)
        
        endingLines()
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANnaspline()
      }
    }
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANnaspline", 
               envirName = environment(showPANnaspline))
}

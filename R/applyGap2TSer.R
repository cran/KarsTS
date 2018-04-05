applyGap2TSer <-
function() {
  applG2TsOnOk <- function() {
    selTsName <- tcltk::tclvalue(KTSEnv$selTsP)
    selGapName <- tcltk::tclvalue(KTSEnv$selGapP)
    if (selTsName == "0") {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else if (selGapName == "0") {
      tcltk::tkmessageBox(message = "Choose a gap set", icon = "warning")
    } else {
      selTs <- get(selTsName, envir = KTSEnv)
      selGap <- get(selGapName, envir = KTSEnv)
      tmComptibility <- areTsGapTimeCompatible(selTs, selGap)
      if (tmComptibility[1] == FALSE) {
        tcltk::tkmessageBox(message = paste("The initial date of", 
                                            "the time series and", 
                                            "the one stored in", 
                                            "the gap set", 
                                            "do not match"), 
                            icon = "warning")
      } else if (tmComptibility[2] == FALSE) {
        tcltk::tkmessageBox(message = paste("The sampling period", 
                                            "of the time series and", 
                                            "the one stored in the", 
                                            "gap set do not match"), 
                            icon = "warning")
      } else if (tmComptibility[3] == FALSE) {
        tcltk::tkmessageBox(message = paste("The time series is", 
                                            "shorter than some", 
                                            "indices stored in", 
                                            "the set of gaps"), 
                            icon = "warning")
      } else {
        seriesWithNewGaps <- selTs
        seriesWithNewGaps$value[selGap$gaps] <- NA
        assign(paste0(selTsName, "_", selGapName), 
               seriesWithNewGaps, envir = KTSEnv)
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANapplG2Ts()
      }
    }
  }
  showPANapplG2Ts <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "APPLY GAPS TO SERIES")
    createGapRb()
    createTsRb()
    createOK(labTitle = "APPLY", action = applG2TsOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyGapTs(action = "showPANapplG2Ts", 
                  envirName = environment(showPANapplG2Ts))
}

naApproxKTS <-
function() {
  naapprOnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else {
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
      } else if (length(selGap$gaps) == 0) {
        tcltk::tkmessageBox(message = "The gap set is empty", icon = "warning")
      } else if (length(setdiff(union(selGap$gaps, nasInSelTs), nasInSelTs)) != 
                 0) {
        tcltk::tkmessageBox(message = paste("Some NAs in the gap set do",
                                            "not exist in the time series.",
                                            "Check that the selected gap",
                                            "set comes from the",
                                            "selected time series"), 
                            icon = "warning")
      } else if (tmComptibility[1] == FALSE) {
        tcltk::tkmessageBox(message = paste("The initial date of the time",
                                            "series and the one stored in",
                                            "the gap set do not match"), 
                            icon = "warning")
      } else if (tmComptibility[2] == FALSE) {
        tcltk::tkmessageBox(message = paste("The sampling period of the time",
                                            "series and the one stored in",
                                            "the gap set do not match"), 
                            icon = "warning")
      } else if (tmComptibility[3] == FALSE) {
        tcltk::tkmessageBox(message = paste("The time series is shorter",
                                            "than some indices stored",
                                            "in the set of gaps"), 
                            icon = "warning")
      } else {
        filledTS <- selTs
        filledTS$value[selGap$gaps] <- zoo::na.approx(selTs$value, 
                                                      xout = selGap$gaps, 
                                                      na.rm = FALSE)
        assign(paste0(selTsName, "_", selGapName, "_lin"), 
               filledTS, envir = KTSEnv)
        gapsAfterFill <- getGapsAfterFill(filledTS, selGap, 
                                          envir = environment(naapprOnOk))
        remainingNAsInGap <- gapsAfterFill$remainingNAsInGap
        filledNasTable <- gapsAfterFill$filledNasTable
        writeMethodTitle("LINEAR INTERPOLATION")
        writeMethodSummary(filledNasTable, remainingNAsInGap, selTsName, 
                           selGapName, selGap)
        endingLines()
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANnaappr()
      }
    }
  }
  showPANnaappr <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "LINEAR INTERPOLATION")
    if (is.null(KTSEnv$dSList$gaps) == FALSE) {
      createGapRb()
    }
    createTsRb()
    createOK(labTitle = "RUN", action = naapprOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANnaappr", 
               envirName = environment(showPANnaappr))
}

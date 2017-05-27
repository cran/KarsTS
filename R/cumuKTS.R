cumuKTS <-
function() {
  cumuOnOk <- function() {
    refreshDataSetsList(outp = FALSE)
    tssel <- tsCheckedTF()
    selCenterOrNot <- tcltk::tclvalue(KTSEnv$centerOrNot)
    newNames <- paste0(KTSEnv$dSList$TS, "_cum")
    if (all(tssel == FALSE)) {
      tcltk::tkmessageBox(message = "Choose a time series at least", 
                          icon = "warning")
    } else {
      getcumuTimser <- function(Ind, selIniVal, selCenterOrNot) {
        timSer <- get(KTSEnv$dSList$TS[Ind], envir = KTSEnv)
        if (all(is.finite(timSer$value) == TRUE)) {
          cumuRes <- stats::diffinv(as.numeric(timSer$value))
          if (selCenterOrNot == "Yes") {
            
            L <- length(cumuRes)
            cumuRes1 <- rep(NA, 2 * L - 1)
            cumuRes1[seq(1, length(cumuRes1), 2)] <- cumuRes
            cumuRes2 <- zoo::na.spline(cumuRes1, na.rm = FALSE)
            timSer$value <- cumuRes2[seq(2, length(cumuRes1), 2)]
          } else {
            timSer$value <- cumuRes[-1]
          }
          
          assign(newNames[Ind], timSer, envir = KTSEnv)
        } else {
          tcltk::tkmessageBox(message = paste(KTSEnv$dSList$TS[Ind], 
                                              "has NAs"), 
                              icon = "warning")
          
        }
      }
      selTimSerInd <- which(tssel == TRUE)
      for (i in selTimSerInd) {
        getcumuTimser(i, selIniVal, selCenterOrNot)
      }
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      showPANcumu()
    }
  }
  showPANcumu <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "CUMULATIVE SUM")
    if (class(KTSEnv$dSList$TS) == "character") {
      createTsChb()
    }
    
    createTitle(labTitle = "Center times series")
    assign("centerOrNot", tcltk::tclVar("No"), envir = KTSEnv)
    createEachRb(labTitle = "Yes", variable = KTSEnv$centerOrNot)
    createEachRb(labTitle = "No", variable = KTSEnv$centerOrNot)
    
    createOK(labTitle = "RUN", action = cumuOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANcumu", 
               envirName = environment(showPANcumu))
}

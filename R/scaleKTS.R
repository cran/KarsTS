scaleKTS <-
function() {
  scaleOnOk <- function() {
    refreshDataSetsList(outp = FALSE)
    tssel <- which(tsCheckedTF())
    selScaleParam <- tcltk::tclvalue(KTSEnv$scaleParam)
    if (length(tssel) == 0) {
      tcltk::tkmessageBox(message = "Select, at least, a time series", 
                          icon = "warning")
    } else {
      tsToScale.names <- KTSEnv$dSList$TS[tssel]
      
      if (selScaleParam == "Median and mad") {
        scaleType = "Robust"
      } else {
        scaleType = "NonRobust"
      }
      
      for (k in tsToScale.names) {
        tsToScale.i <- get(k, envir = KTSEnv)
        tsToScale.i$value <- myScale(tsToScale.i$value, scaleType = scaleType, 
                                     outputType = "outNo")
        assign(paste0(k, "_scld"), tsToScale.i, envir = KTSEnv)
        
      }
      
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      showPANscale()
    }
  }
  showPANscale <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "SCALE")
    createTsChb()
    createTitle(labTitle = "Scale parameters")
    assign("scaleParam", tcltk::tclVar("Median and mad"), envir = KTSEnv)
    createEachRb(labTitle = "Median and mad", variable = KTSEnv$scaleParam)
    createEachRb(labTitle = "Mean and sd", variable = KTSEnv$scaleParam)
    createOK(labTitle = "RUN", action = scaleOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANscale", envirName = environment(showPANscale))
}

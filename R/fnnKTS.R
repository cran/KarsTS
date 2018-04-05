fnnKTS <-
function() {
  fnnOnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                 noValid = NA)
    embDim <- verifyIntEntry(tcltk::tclvalue(KTSEnv$embDimension), 
                             noValid = NA)
    embDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$embDelays), 
                               noValid = NA)
    theilerWin <- verifyIntEntry(tcltk::tclvalue(KTSEnv$theilWin), 
                                 noValid = NA)
    escapeFact <- verifyRealEntry(tcltk::tclvalue(KTSEnv$escFact), 
                                  noValid = NA)
    neighDiam <- verifyRealEntry(tcltk::tclvalue(KTSEnv$nDiameter), 
                                 noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else if (is.na(embDim)) {
      tcltk::tkmessageBox(message = paste("Enter the embedding dimension"), 
                          icon = "warning")
    } else if (embDim < 2) {
      tcltk::tkmessageBox(message = paste("The maximum embedding dimension",
                                          "must be an integer greater than 1"), 
                          icon = "warning")
    } else if (is.na(embDelay)) {
      tcltk::tkmessageBox(message = "Enter the delay", icon = "warning")
    } else if (embDelay == 0) {
      tcltk::tkmessageBox(message = paste("The delay must be greater than 0"), 
                          icon = "warning")
    } else if (is.na(theilerWin)) {
      tcltk::tkmessageBox(message = paste("The Theiler window must be an",
                                          "integer greater than",
                                          "or equal to 0"), 
                          icon = "warning")
    } else {
      selTs <- get(tcltk::tclvalue(KTSEnv$selTsP), envir = KTSEnv)
      if (any(is.na(selTs$value))) {
        tcltk::tkmessageBox(message = paste("NAs are not allowed.Make a",
                                            "temporary filling or use a",
                                            "piece of time series",
                                            "without any NAs"), 
                            icon = "warning")
      } else {
        fnnPlot <- function() {
          tseriesChaos::plot.false.nearest(fnnResult)
        }
        if (is.na(escapeFact)) {
          escapeFact <- 10
        }
        if (is.na(neighDiam)) {
          neighDiam <- stats::sd(selTs$value)/10
        }
        fnnResult <- tseriesChaos::false.nearest(selTs$value, m = embDim, 
                                                 d = embDelay, t = theilerWin, 
                                                 rt = escapeFact, 
                                                 eps = neighDiam)
        
        copyPlot <- function() {
          tkrplot::tkrreplot(tsPlot)
        }
        panelName <- createRandName()
        assign(panelName, tcltk::tktoplevel(bg = "white"))
        tcltk::tkwm.title(get(panelName), "False nearest neighbours")
        tsPlot <- tkrplot::tkrplot(get(panelName), fun = fnnPlot, hscale = 3, 
                                   vscale = 1.5)
        copyButton <- tcltk::tkbutton(get(panelName), 
                                      text = "Copy to clipboard", 
                                      command = copyPlot)
        tcltk::tkpack(tsPlot, expand = TRUE, fill = "both", anchor = "center")
        tcltk::tkconfigure(tsPlot, bg = "white")
        tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        
      }
    }
  }
  showPANfnn <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "FALSE NEAREST NEIGHBORS")
    createTsRb()
    createTitle(labTitle = "Parameters")
    createEntry(labTitle = "Maximum embedding dimension", 
                textVariableName = "embDimension")
    createEntry(labTitle = "Delay (in lags)", 
                textVariableName = "embDelays")
    createEntry(labTitle = "Theiler window (in lags)", 
                textVariableName = "theilWin")
    createEntry(labTitle = "Escape factor(optional)", 
                textVariableName = "escFact")
    createEntry(labTitle = "Neighborhood diameter", 
                textVariableName = "nDiameter")
    createOK(labTitle = "RUN", action = fnnOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANfnn", envirName = environment(showPANfnn))
}

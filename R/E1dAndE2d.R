E1dAndE2d <-
function() {
    showPANed1ed2 <- function() {
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "Ed(1) AND Ed(2)")
        createTsRb()
        createEntry(labTitle = "Delay (in lags)", 
                    textVariableName = "timeDelay")
        createOK(labTitle = "RUN", action = ed1ed2OnOk)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    ed1ed2OnOk <- function() {
      
      E1dAndE2dPlot <- function() {
        LL <- length(selTs$value)
        rs <- nonlinearTseries::estimateEmbeddingDim(time.series = selTs$value, 
                                                     number.points = LL, 
                                                     time.lag = timDelay, 
                                                     max.embedding.dim = 15, 
                                                     threshold = 0.95, 
                                                     max.relative.change = 0.1, 
                                                     do.plot = TRUE, 
                                                     main = "Ed(1) & Ed(2)")
      }
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                     noValid = NA)
        timDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$timeDelay), 
                                   noValid = NA)
        if (is.na(selTsName)) {
            tcltk::tkmessageBox(message = "Choose a time series", 
                                icon = "warning")
        } else if (is.na(timDelay)) {
            tcltk::tkmessageBox(message = "Choose a delay", icon = "warning")
        } else if (timDelay == 0) {
            tcltk::tkmessageBox(message = paste("The lag delay must",
                                                "be 1 at least"), 
                                icon = "warning")
        } else {
            selTs <- get(selTsName, envir = KTSEnv)
            if (any(is.na(selTs$value))) {
                tcltk::tkmessageBox(message = paste("The time series",
                                                    "cannot have any NAs"), 
                  icon = "warning")
            } else {
              tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")

              copyPlot <- function() {
                tkrplot::tkrreplot(tsPlot)
              }
              panelName <- createRandName()
              assign(panelName, tcltk::tktoplevel(bg = "white"))
              tcltk::tkwm.title(get(panelName), "Ed(1) & Ed(2)")
              tsPlot <- tkrplot::tkrplot(get(panelName), 
                                         fun = E1dAndE2dPlot, hscale = 2, 
                                         vscale = 1)
              copyButton <- tcltk::tkbutton(get(panelName), 
                                            text = "Copy to clipboard", 
                                            command = copyPlot)
              tcltk::tkpack(tsPlot, expand = TRUE, fill = "both", 
                            anchor = "center")
              tcltk::tkconfigure(tsPlot, bg = "white")
              tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
              tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
            }
        }
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANed1ed2", 
                 envirName = environment(showPANed1ed2))
}

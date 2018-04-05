mutualKTS <-
function() {
    mutKTSOnOk <- function() {
        maxLag <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numlag), 
                                 noValid = NA)
        tssel <- tsCheckedTF()
        checkedTS <- which(tssel == TRUE)
        lCheckedTS <- length(checkedTS)
        if (lCheckedTS < 1 | lCheckedTS > 2) {
            tcltk::tkmessageBox(message = paste("Choose one or",
                                                "two time series"), 
                                icon = "warning")
        } else if (is.na(maxLag)) {
            tcltk::tkmessageBox(message = paste("Choose the maximum",
                                                "delay (lags)"), 
                                icon = "warning")
        } else if (lCheckedTS == 1) {
            selTsName <- KTSEnv$dSList$TS[checkedTS]
            selTs <- get(selTsName, envir = KTSEnv)
            if (any(is.na(selTs$value))) {
                tcltk::tkmessageBox(message = paste("The time series must",
                                                    "not contain any NAs"), 
                  icon = "warning")
            } else {
                amiUniPlot <- function() {
                  amis <- tseriesChaos::mutual(selTs$value, lag.max = maxLag, 
                                               plot = TRUE, main = selTsName)
                  
                }
                copyPlot <- function() {
                  tkrplot::tkrreplot(tsPlot)
                }
                panelName <- createRandName()
                assign(panelName, tcltk::tktoplevel(bg = "white"))
                tcltk::tkwm.title(get(panelName), "Mutual information")
                tsPlot <- tkrplot::tkrplot(get(panelName), 
                                           fun = amiUniPlot, 
                                           hscale = 3,vscale = 1.5)
                copyButton <- tcltk::tkbutton(get(panelName), 
                                              text = "Copy to clipboard",
                                              command = copyPlot)
                tcltk::tkpack(tsPlot, expand = TRUE, 
                              fill = "both", anchor = "center")
                tcltk::tkconfigure(tsPlot, bg = "white")
                tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
                tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
            }
        } else if (lCheckedTS == 2) {
            selTsName <- KTSEnv$dSList$TS[checkedTS]
            selTs1 <- get(selTsName[1], envir = KTSEnv)
            selTs2 <- get(selTsName[2], envir = KTSEnv)
            timecompatibility <- are2TsTimeCompatible(selTs1, selTs2)
            if (any(is.na(selTs1$value)) | any(is.na(selTs2$value))) {
                tcltk::tkmessageBox(message = paste("The time series must",
                                                    "not contain any NAs"), 
                  icon = "warning")
            } else if (timecompatibility[2] == FALSE) {
                tcltk::tkmessageBox(message = paste("Both time series must",
                                                    "have the same",
                                                    "sampling period"), 
                  icon = "warning")
            } else {
              if (timecompatibility[1] == FALSE) {
                lagTS <- diff(as.numeric(selTs1$time[1], selTs2$time[1]))
                tcltk::tkmessageBox(message = paste("There is a lag of ", 
                                                    lagTS, 
                                                    "seconds between the",
                                                    "time series. Take",
                                                    "it into consideration"), 
                                    icon = "warning")
              }
              amisBivPlot <- function() {
                amis <- tseriesChaos::mutual(c(selTs1$value, selTs2$value),
                                             lag.max = maxLag, plot = TRUE, 
                                             main = paste(selTsName[1], 
                                                          "and", selTsName[2]))
                
              }
                copyPlot <- function() {
                  tkrplot::tkrreplot(tsPlot)
                }
                panelName <- createRandName()
                assign(panelName, tcltk::tktoplevel(bg = "white"))
                tcltk::tkwm.title(get(panelName), "Mutual information")
                tsPlot <- tkrplot::tkrplot(get(panelName), fun = amisBivPlot, 
                                           hscale = 3, vscale = 1.5)
                copyButton <- tcltk::tkbutton(get(panelName), 
                                              text = "Copy to clipboard", 
                                              command = copyPlot)
                tcltk::tkpack(tsPlot, expand = TRUE, 
                              fill = "both", anchor = "center")
                tcltk::tkconfigure(tsPlot, bg = "white")
                tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
                tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
            }
        }
    }
    showPANmutKTS <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "MUTUAL INFORMATION")
        createTsChb()
        createEntry(labTitle = "Maximum number of lags", 
                    textVariableName = "numlag")
        createOK(labTitle = "RUN", action = mutKTSOnOk)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANmutKTS", 
                 envirName = environment(showPANmutKTS))
}

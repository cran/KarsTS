windRoseKTS <-
function() {
    showPANwRose1 <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "WIND ROSE")
        createTsRb(labTitle = "Magnitude", 
                   variableName = "magTsP")
        createOK(labTitle = "NEXT", action = plotwRose1)
        tcltk::tkpack(KTSEnv$subPanR4C1, 
                      expand = TRUE, fill = "both")
        
    }
    plotwRose1 <- function() {
        magTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$magTsP), 
                                     noValid = NA)
        if (is.na(magTsName)) {
            tcltk::tkmessageBox(message = paste("Choose a magnitude",
                                                "time series"), 
                                icon = "warning")
        } else {
          magTs <- get(magTsName, envir = KTSEnv)
          if (any(is.infinite(magTs$value))) {
            tcltk::tkmessageBox(message = paste("The magnitude cannot",
                                                "have infinite values"), 
                                icon = "warning")
          } else if (any(is.nan(magTs$value))) {
            tcltk::tkmessageBox(message = paste("The magnitude cannot",
                                                "have not a numbers",
                                                "(NaNs); however,NAs are",
                                                "allowed meaning calm"), 
                                icon = "warning")
            } else if (any(magTs$value[which(is.finite(magTs$value))] < 0)) {
                tcltk::tkmessageBox(message = paste("The magnitude cannot",
                                                    "have negative values"), 
                  icon = "warning")
            } else {
                
                assign("magTs", magTs, envir = KTSEnv)
                assign("magTsName", magTsName, envir = KTSEnv)
                showPANwRose2()
            }
        }
    }
    showPANwRose2 <- function() {
        createSubPanR4C1()
        createTITLE(labTitle = "WIND ROSE")
        createTsRb(labTitle = "Direction", variableName = "dirTsP")
        createEntry(labTitle = "Number of bins", 
                    textVariableName = "numberBins")
        createEntry(labTitle = "Number of ticks", 
                    textVariableName = "numberTicks")
        createOK(labTitle = "PLOT", action = plotwRosezoom)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    plotwRosezoom <- function() {
        dirTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$dirTsP), 
                                     noValid = NA)
        nBins <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numberBins), 
                                noValid = NA)
        nTicks <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numberTicks), 
                                 noValid = NA)
        if (is.na(dirTsName)) {
            tcltk::tkmessageBox(message = "choose the direction time series", 
                                icon = "warning")
        } else {
            if (is.na(nBins)) {
                nBins <- 12
            }
            if (is.na(nTicks)) {
                nTicks <- 12
            }
            dirTs <- get(dirTsName, envir = KTSEnv)
            finiteDirTs <- dirTs$value[which(is.finite(dirTs$value))]
            tmComptibility <- are2TsTimeCompatible(dirTs, KTSEnv$magTs)
            if (any(finiteDirTs < 0 | finiteDirTs > 360)) {
                tcltk::tkmessageBox(message = paste("The direction values",
                                                    "must lie between 0",
                                                    "and 360"), 
                  icon = "warning")
            } else if (tmComptibility[1] == FALSE) {
                tcltk::tkmessageBox(message = paste("The initial dates",
                                                    "of the time series",
                                                    "do not match"), 
                  icon = "warning")
            } else if (tmComptibility[2] == FALSE) {
                tcltk::tkmessageBox(message = paste("The sampling period of",
                                                    "the time series",
                                                    "do not match"), 
                  icon = "warning")
            } else if (tmComptibility[3] == FALSE) {
                tcltk::tkmessageBox(message = paste("The length of the",
                                                    "time series do",
                                                    "not match"), 
                  icon = "warning")
            } else {
              windRosePlot <- function() {
                
                colorsWR <- c("blue", "red", "darkgreen", 
                              "magenta", "cyan", "green", 
                              "orange", "brown", "purple", "darkcyan")
                
                mceDB <- circular::circular(dirTs$value, 
                                            units = "degrees", 
                                            template = "geographics")
                
                dataToPlot <- data.frame(dir = mceDB, 
                                         mag = KTSEnv$magTs$value)
                circular::windrose(dataToPlot, bins = nBins, 
                                   main = paste(KTSEnv$magTsName, dirTsName), 
                                   fill.col = colorsWR, 
                                   num.ticks = nTicks, calm = "NA")
              }
              copyPlot <- function() {
                  tkrplot::tkrreplot(wrPlot)
                }
              panelName <- createRandName()
              assign(panelName, tcltk::tktoplevel(bg = "white"))
              tcltk::tkwm.title(get(panelName), 
                                paste("Wind Rose:", dirTsName, 
                                      KTSEnv$magTsName))
              wrPlot <- tkrplot::tkrplot(get(panelName), fun = windRosePlot, 
                                         hscale = 1.5, vscale = 1.5)
              copyButton <- tcltk::tkbutton(get(panelName), 
                                            text = "Copy to clipboard", 
                                            command = copyPlot)
              tcltk::tkpack(wrPlot, expand = TRUE, 
                            fill = "both", anchor = "center")
              tcltk::tkconfigure(wrPlot, bg = "white")
              tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
              tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
            }
        }
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANwRose1", 
                 envirName = environment(showPANwRose1))
}

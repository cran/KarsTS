createSimpleRMPlot <-
function() {
    plotsrmpzoom <- function() {
        selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP), 
                                     noValid = NA)
        if (is.na(selRmName)) {
            tcltk::tkmessageBox(message = "Choose a recurrence matrix", 
                                icon = "warning")
        } else {
            selRm <- get(selRmName, envir = KTSEnv)
            if (selRm$type != "simple") {
                tcltk::tkmessageBox(message = paste("Choose a simple",
                                                    "recurrence matrix",
                                                    "(nor cross neither",
                                                    "joint)"), 
                  icon = "warning")
            } else {
              recurrPlot <- function() {
                par1 <- graphics::par()
                graphics::par(mar = c(5, 5, 1, 1))
                graphics::plot(tsTime, tsTime, type = "p", 
                               col = "white", xlab = selRm$tsName, 
                               ylab = selRm$tsName, cex.axis = 0.85, asp = 1)
                graphics::points(tsTime[recurrPoints$X], 
                                 tsTime[recurrPoints$Y], 
                                 type = "p", col = "darkred", cex = 0.3)
                graphics::points(tsTime[recurrPoints$Y], 
                                 tsTime[recurrPoints$X], 
                                 type = "p", col = "darkred", cex = 0.3)
                
              }
              copyPlot <- function() {
                  tkrplot::tkrreplot(tsPlot)
                }
                format <- findDateFormat(selRm$tsIni, tz = KTSEnv$timeZone)
                lengthTS <- selRm$tsLength - (selRm$embDim - 1) * selRm$delay
                tsTime <- seq(strptime(selRm$tsIni, format = format, 
                                       tz = KTSEnv$timeZone), 
                  by = selRm$samPerSec, length.out = lengthTS)
                recurrPoints <- selRm$ones
                nRecurrPoints <- nrow(recurrPoints)
                if (nRecurrPoints > 50000) {
                  tcltk::tkmessageBox(message = paste("50000 random",
                                                      "recurrence points",
                                                      "were taken to",
                                                      "draw the plot"), 
                    icon = "warning")
                  recurrPoints <- recurrPoints[sort(sample(1:nRecurrPoints, 
                                                           50000)), ]
                }
                
                panelName <- createRandName()
                assign(panelName, tcltk::tktoplevel(bg = "white"))
                tcltk::tkwm.title(get(panelName), 
                                  paste("Recurrence plot:", selRmName))
                tsPlot <- tkrplot::tkrplot(get(panelName), 
                                           fun = recurrPlot, hscale = 1.5, 
                                           vscale = 1.5)
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
    plotsrmp <- function() {
        selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP), 
                                     noValid = NA)
        if (is.na(selRmName)) {
          tcltk::tkmessageBox(message = "Choose a recurrence matrix", 
                              icon = "warning")
        } else {
          selRm <- get(selRmName, envir = KTSEnv)
          if (selRm$type != "simple") {
            tcltk::tkmessageBox(message = paste("Choose a simple",
                                                "recurrence matrix",
                                                "(nor cross",
                                                "neither joint)"), 
                                icon = "warning")
          } else {
            recurrPlot <- function() {
              par1 <- graphics::par()
              graphics::par(mar = c(5, 5, 1, 1))
              graphics::plot(tsTime, tsTime, type = "p", 
                             col = "grey", xlab = selRm$tsName, 
                             ylab = selRm$tsName, cex.axis = 0.85, 
                             asp = 1, cex = 0.01)
              graphics::points(tsTime[recurrPoints$X], tsTime[recurrPoints$Y], 
                               type = "p", col = "darkred", cex = 0.3)
              graphics::points(tsTime[recurrPoints$Y], tsTime[recurrPoints$X], 
                               type = "p", col = "darkred", cex = 0.3)
                }
                
            format <- findDateFormat(selRm$tsIni, tz = KTSEnv$timeZone)
            lengthTS <- selRm$tsLength - (selRm$embDim - 1) * selRm$delay
            tsTime <- seq(strptime(selRm$tsIni, format = format, 
                                   tz = KTSEnv$timeZone), 
                          by = selRm$samPerSec, length.out = lengthTS)
            recurrPoints <- selRm$ones
            nRecurrPoints <- nrow(recurrPoints)
            if (nRecurrPoints > 50000) {
                  tcltk::tkmessageBox(message = paste("50000 random",
                                                      "recurrence points",
                                                      "were taken to",
                                                      "draw the plot"), 
                                      icon = "warning")
              recurrPoints <- recurrPoints[sort(sample(1:nRecurrPoints, 
                                                       50000)), ]
                }
                if (exists("SubPanR4C3", envir = KTSEnv)) {
                  tcltk::tkdestroy(KTSEnv$SubPanR4C3)
                }
            SubPanR4C3 = tcltk::tkframe(KTSEnv$row4.col3, 
                                        width = KTSEnv$heigth4, 
                                        borderwidth = 2, relief = "raised")
            tcltk::tkgrid(SubPanR4C3)
            tcltk::tkgrid.configure(SubPanR4C3, sticky = "n")
            tcltk::tkgrid.columnconfigure(SubPanR4C3, 0, weight = 1)
            tcltk::tkgrid.rowconfigure(SubPanR4C3, 0, weight = 1)
            recurrPlot1 <- tkrplot::tkrplot(SubPanR4C3, fun = recurrPlot)
            tcltk::tkgrid(recurrPlot1)
            tcltk::tkpack(SubPanR4C3, expand = TRUE, fill = "both")
            assign("SubPanR4C3", SubPanR4C3, envir = KTSEnv)
            cleanEnvir()
            refreshDataSetsList(outp = FALSE)
            showPANsrmp()
            }
        }
    }
    showPANsrmp <- function() {
        createSubPanR4C1()
        createTITLE(labTitle = "RECURRENCE PLOT")
        createRmRb()
        createOK(labTitle = "PLOT", action = plotsrmp)
        createOK(labTitle = "ZOOM", action = plotsrmpzoom)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    cleanEnvir()
    if (exists("SubPanR4C3", envir = KTSEnv)) {
        tcltk::tkdestroy(KTSEnv$SubPanR4C3)
    }
    refreshDataSetsList(outp = FALSE)
    checkIfAnyRm(action = "showPANsrmp", 
                 envirName = environment(showPANsrmp))
}

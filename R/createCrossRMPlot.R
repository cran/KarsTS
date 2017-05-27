createCrossRMPlot <-
function() {
  showPANCRMP1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "CROSS RECURRENCE PLOT")
    createRmRb()
    createOK(labTitle = "PLOT", action = plotCRMP)
    createOK(labTitle = "ZOOM", action = plotCRMPzoom)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  plotCRMPzoom <- function() {
    selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP), 
                                 noValid = NA)
    if (is.na(selRmName)) {
      tcltk::tkmessageBox(message = paste("Choose a recurrence matrix"), 
                          icon = "warning")
    } else {
      selRm <- get(selRmName, envir = KTSEnv)
      initialTimes <- getDelayCharTimes(selRm$tsIni)
      minIniTime <- initialTimes[1]
      maxIniTime <- initialTimes[2]
      if (selRm$type != "cross") {
        tcltk::tkmessageBox(message = paste("Choose a cross",
                                            "recurrence matrix",
                                            "(nor joint neither",
                                            "simple)"), 
                            icon = "warning")
      } else {
        if (minIniTime != maxIniTime) {
          tcltk::tkmessageBox(message = paste("There is a lag of ", 
                                              diff(c(minIniTime, 
                                                     maxIniTime)), 
                                              "seconds between two",
                                              "of the original time series.",
                                              "Take it into consideration"), 
                              icon = "warning")
        }
        cRecurrPlot <- function() {
          graphics::par(mar = c(5, 5, 1, 1))
          graphics::plot(reconstTimeX[c(1, length(reconstTimeX))], 
                         reconstTimeY[c(1, length(reconstTimeY))], 
                         type = "p", col = "white", xlab = selRm$tsName[1], 
                         ylab = selRm$tsName[2], cex.axis = 0.85)
          graphics::points(reconstTimeX[recurrPoints$X], 
                           reconstTimeY[recurrPoints$Y], 
                           type = "p", col = "darkred", cex = 0.3)
          
        }
        copyPlot <- function() {
          tkrplot::tkrreplot(tsPlot)
        }
        lengthTS <- selRm$tsLength - (selRm$embDim - 1) * selRm$delay
        format <- isTimeAlright(selRm$tsIni[1], tz = KTSEnv$timeZone)
        reconstTimeX <- seq(strptime(selRm$tsIni[1], 
                                     format = format, tz = KTSEnv$timeZone), 
                            by = selRm$samPerSec[1], 
                            length.out = lengthTS[1])
        format <- isTimeAlright(selRm$tsIni[2], tz = KTSEnv$timeZone)
        reconstTimeY <- seq(strptime(selRm$tsIni[2], 
                                     format = format, tz = KTSEnv$timeZone), 
                            by = selRm$samPerSec[1], 
                            length.out = lengthTS[2])
        recurrPoints <- selRm$ones
        nRecurrPoints <- nrow(recurrPoints)
        if (nRecurrPoints > 50000) {
          tcltk::tkmessageBox(message = paste("50000 random recurrence",
                                              "points were taken",
                                              "to draw the plot"), 
                              icon = "warning")
          recurrPoints <- recurrPoints[sort(sample(1:nRecurrPoints, 50000)), 
                                       ]
        }
        panelName <- createRandName()
        assign(panelName, tcltk::tktoplevel(bg = "white"))
        tcltk::tkwm.title(get(panelName), paste("Cross recurrence plot:", 
                                                selRmName))
        tsPlot <- tkrplot::tkrplot(get(panelName), fun = cRecurrPlot, 
                                   hscale = 1.5, vscale = 1.5)
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
  plotCRMP <- function() {
    selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP), 
                                 noValid = NA)
    if (is.na(selRmName)) {
      tcltk::tkmessageBox(message = paste("Choose a recurrence matrix"), 
                          icon = "warning")
    } else {
      selRm <- get(selRmName, envir = KTSEnv)
      initialTimes <- getDelayCharTimes(selRm$tsIni)
      minIniTime <- initialTimes[1]
      maxIniTime <- initialTimes[2]
      if (selRm$type != "cross") {
        tcltk::tkmessageBox(message = paste("Choose a cross recurrence",
                                            "matrix (nor joint",
                                            "neither simple)"), 
                            icon = "warning")
      }else{
        if (minIniTime != maxIniTime) {
          tcltk::tkmessageBox(message = paste("There is a lag of ", 
                                              diff(c(minIniTime, maxIniTime)), 
                                              "seconds between two of the",
                                              "original time series.",
                                              "Take it into consideration"), 
                              icon = "warning")
        }
        cRecurrPlot <- function() {
          graphics::par(mar = c(5, 5, 1, 1))
          graphics::plot(reconstTimeX[c(1, length(reconstTimeX))], 
                         reconstTimeY[c(1, length(reconstTimeY))], 
                         type = "p", col = "grey", xlab = selRm$tsName[1], 
                         ylab = selRm$tsName[2], 
                         cex.axis = 0.85, cex = 0.01)
          graphics::points(reconstTimeX[recurrPoints$X], 
                           reconstTimeY[recurrPoints$Y], 
                           type = "p", col = "darkred", cex = 0.3)
        }
        copyPlot <- function() {
          tkrplot::tkrreplot(tsPlot)
        }
        lengthTS <- selRm$tsLength - (selRm$embDim - 1) * selRm$delay
        format <- isTimeAlright(selRm$tsIni[1], tz = KTSEnv$timeZone)
        reconstTimeX <- seq(strptime(selRm$tsIni[1], format = format, 
                                     tz = KTSEnv$timeZone), 
                            by = selRm$samPerSec[1], 
                            length.out = lengthTS[1])
        format <- isTimeAlright(selRm$tsIni[2], tz = KTSEnv$timeZone)
        reconstTimeY <- seq(strptime(selRm$tsIni[2], format = format, 
                                     tz = KTSEnv$timeZone), 
                            by = selRm$samPerSec[1], 
                            length.out = lengthTS[2])
        recurrPoints <- selRm$ones
        nRecurrPoints <- nrow(recurrPoints)
        if (nRecurrPoints > 50000) {
          tcltk::tkmessageBox(message = paste("50000 random recurrence",
                                              "points were taken",
                                              "to draw the plot"), 
                              icon = "warning")
          recurrPoints <- recurrPoints[sort(sample(1:nRecurrPoints, 50000)), ]
        }
        if (exists("SubPanR4C3", envir = KTSEnv)) {
          tcltk::tkdestroy(KTSEnv$SubPanR4C3)
        }
        SubPanR4C3 = tcltk::tkframe(KTSEnv$row4.col3, width = KTSEnv$heigth4, 
                                    borderwidth = 2, relief = "raised")
        tcltk::tkgrid(SubPanR4C3)
        tcltk::tkgrid.configure(SubPanR4C3, sticky = "n")
        tcltk::tkgrid.columnconfigure(SubPanR4C3, 0, weight = 1)
        tcltk::tkgrid.rowconfigure(SubPanR4C3, 0, weight = 1)
        cRecurrPlot1 <- tkrplot::tkrplot(SubPanR4C3, fun = cRecurrPlot)
        tcltk::tkgrid(cRecurrPlot1)
        tcltk::tkpack(SubPanR4C3, expand = TRUE, fill = "both")
        assign("SubPanR4C3", SubPanR4C3, envir = KTSEnv)
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANCRMP1()
      }
    }
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyRm(action = "showPANCRMP1", 
               envirName = environment(showPANCRMP1))
}

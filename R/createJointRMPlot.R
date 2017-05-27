createJointRMPlot <-
function() {
  showPANjrmp1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "JOINT RECURRENCE PLOT")
    createRmRb()
    createOK(labTitle = "NEXT", action = plotjrmp1)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  plotjrmp1 <- function() {
    selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP), noValid = NA)
    if (is.na(selRmName)) {
      tcltk::tkmessageBox(message = paste("Choose a recurrence matrix"), 
                          icon = "warning")
    } else {
      selRm <- get(selRmName, envir = KTSEnv)
      initialTimes <- getDelayCharTimes(selRm$tsIni)
      minIniTime <- initialTimes[1]
      maxIniTime <- initialTimes[2]
      if (selRm$type != "joint") {
        tcltk::tkmessageBox(message = paste("Choose a joint recurrence",
                                            "matrix (nor cross",
                                            "neither simple)"), 
                            icon = "warning")
      } else {
        if (minIniTime != maxIniTime) {
          tcltk::tkmessageBox(message = paste("There is a lag of ", 
                                              diff(c(minIniTime, maxIniTime)), 
                                              "seconds between two of the",
                                              "original time series. Take",
                                              "it into consideration"), 
                              icon = "warning")
        }
        assign("selRm", selRm, envir = KTSEnv)
        assign("selRmName", selRmName, envir = KTSEnv)
        showPANjrmp2()
      }
    }
  }
  showPANjrmp2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "JOINT RECURRENCE PLOT")
    createTitle(labTitle = "Label for X axis")
    assign("xLab", tcltk::tclVar("0"), envir = KTSEnv)
    createRb(variable = KTSEnv$xLab, dataVector = KTSEnv$selRm$tsName)
    createTitle(labTitle = "Label for X axis")
    assign("yLab", tcltk::tclVar("0"), envir = KTSEnv)
    createRb(variable = KTSEnv$yLab, dataVector = KTSEnv$selRm$tsName)
    createOK(labTitle = "PLOT", action = plotjrmp)
    createOK(labTitle = "ZOOM", action = plotjrmpzoom)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  plotjrmpzoom <- function() {
    xLab <- verifyCharEntry(tcltk::tclvalue(KTSEnv$xLab), noValid = NA)
    yLab <- verifyCharEntry(tcltk::tclvalue(KTSEnv$yLab), noValid = NA)
    if (is.na(xLab)) {
      tcltk::tkmessageBox(message = paste("choose a label for the X axis"), 
                          icon = "warning")
    } else if (is.na(yLab)) {
      tcltk::tkmessageBox(message = paste("choose a label for the Y axis"), 
                          icon = "warning")
    } else {
      xLabInd <- min(which(KTSEnv$selRm$tsName == xLab), na.rm = TRUE)
      yLabInd <- min(which(KTSEnv$selRm$tsName == yLab), na.rm = TRUE)
      
      jRecurrPlot <- function() {
        xLabInd <- min(which(KTSEnv$selRm$tsName == xLab), na.rm = TRUE)
        yLabInd <- min(which(KTSEnv$selRm$tsName == yLab), na.rm = TRUE)
        graphics::par(mar = c(5, 5, 1, 1))
        graphics::plot(reconstTimeX, reconstTimeY, type = "p", col = "white", 
                       xlab = KTSEnv$selRm$tsName[xLabInd], 
                       ylab = KTSEnv$selRm$tsName[yLabInd], 
                       cex.axis = 0.85, asp = 1)
        graphics::points(reconstTimeX[recurrPoints$X], 
                         reconstTimeY[recurrPoints$Y], 
                         type = "p", col = "darkred", cex = 0.3)
        graphics::points(reconstTimeX[recurrPoints$Y], 
                         reconstTimeY[recurrPoints$X], 
                         type = "p", col = "darkred", cex = 0.3)
        
      }
      copyPlot <- function() {
        tkrplot::tkrreplot(tsPlot)
      }
      ymba <- (KTSEnv$selRm$embDim - 1) * KTSEnv$selRm$delay
      lengthTS <- min(KTSEnv$selRm$tsLength - ymba)
      format <- isTimeAlright(KTSEnv$selRm$tsIni[xLabInd])
      reconstTimeX <- seq(strptime(KTSEnv$selRm$tsIni[xLabInd], 
                                   format = format, 
                                   tz = KTSEnv$timeZone), 
                          by = KTSEnv$selRm$samPerSec[1], 
                          length.out = lengthTS)
      format <- isTimeAlright(KTSEnv$selRm$tsIni[yLabInd], 
                              tz = KTSEnv$timeZone)
      reconstTimeY <- seq(strptime(KTSEnv$selRm$tsIni[xLabInd], 
                                   format = format, 
                                   tz = KTSEnv$timeZone), 
                          by = KTSEnv$selRm$samPerSec[1], 
                          length.out = lengthTS)
      recurrPoints <- KTSEnv$selRm$ones
      nRecurrPoints <- nrow(recurrPoints)
      if (nRecurrPoints > 50000) {
        tcltk::tkmessageBox(message = paste("50000 random recurrence points",
                                            "were taken to draw the plot"), 
                            icon = "warning")
        recurrPoints <- recurrPoints[sort(sample(1:nRecurrPoints, 50000)), 
                                     ]
      }
      panelName <- createRandName()
      assign(panelName, tcltk::tktoplevel(bg = "white"))
      tcltk::tkwm.title(get(panelName), 
                        paste("Joint recurrence plot:", KTSEnv$selRmName))
      tsPlot <- tkrplot::tkrplot(get(panelName), fun = jRecurrPlot, 
                                 hscale = 1.5, vscale = 1.5)
      copyButton <- tcltk::tkbutton(get(panelName), 
                                    text = "Copy to clipboard", 
                                    command = copyPlot)
      tcltk::tkpack(tsPlot, expand = TRUE, fill = "both", anchor = "center")
      tcltk::tkconfigure(tsPlot, bg = "white")
      tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
    }
  }
  plotjrmp <- function() {
    xLab <- verifyCharEntry(tcltk::tclvalue(KTSEnv$xLab), noValid = NA)
    yLab <- verifyCharEntry(tcltk::tclvalue(KTSEnv$yLab), noValid = NA)
    if (is.na(xLab)) {
      tcltk::tkmessageBox(message = paste("choose a label for the X axis"), 
                          icon = "warning")
    } else if (is.na(yLab)) {
      tcltk::tkmessageBox(message = paste("choose a label for the Y axis"), 
                          icon = "warning")
    } else {
      xLabInd <- min(which(KTSEnv$selRm$tsName == xLab), na.rm = TRUE)
      yLabInd <- min(which(KTSEnv$selRm$tsName == yLab), na.rm = TRUE)
      jRecurrPlot <- function() {
        xLabInd <- min(which(KTSEnv$selRm$tsName == xLab), na.rm = TRUE)
        yLabInd <- min(which(KTSEnv$selRm$tsName == yLab), na.rm = TRUE)
        graphics::par(mar = c(5, 5, 1, 1))
        graphics::plot(reconstTimeX, reconstTimeY, type = "p", col = "grey", 
                       xlab = KTSEnv$selRm$tsName[xLabInd], 
                       ylab = KTSEnv$selRm$tsName[yLabInd], 
                       cex.axis = 0.85, asp = 1, cex = 0.01)
        graphics::points(reconstTimeX[recurrPoints$X], 
                         reconstTimeY[recurrPoints$Y], 
                         type = "p", col = "darkred", cex = 0.3)
        graphics::points(reconstTimeX[recurrPoints$Y], 
                         reconstTimeY[recurrPoints$X], 
                         type = "p", col = "darkred", cex = 0.3)
      }
      dmpdm <- (KTSEnv$selRm$embDim - 1) * KTSEnv$selRm$delay
      lengthTS <- min(KTSEnv$selRm$tsLength - dmpdm)
      format <- isTimeAlright(KTSEnv$selRm$tsIni[xLabInd])
      reconstTimeX <- seq(strptime(KTSEnv$selRm$tsIni[xLabInd], 
                                   format = format, 
                                   tz = KTSEnv$timeZone), 
                          by = KTSEnv$selRm$samPerSec[1], 
                          length.out = lengthTS)
      format <- isTimeAlright(KTSEnv$selRm$tsIni[yLabInd], 
                              tz = KTSEnv$timeZone)
      reconstTimeY <- seq(strptime(KTSEnv$selRm$tsIni[xLabInd], 
                                   format = format, 
                                   tz = KTSEnv$timeZone), 
                          by = KTSEnv$selRm$samPerSec[1], 
                          length.out = lengthTS)
      recurrPoints <- KTSEnv$selRm$ones
      nRecurrPoints <- nrow(recurrPoints)
      if (nRecurrPoints > 50000) {
        tcltk::tkmessageBox(message = paste("50000 random recurrence points",
                                            "were taken to draw the plot"), 
                            icon = "warning")
        recurrPoints <- recurrPoints[sort(sample(1:nRecurrPoints, 50000)), 
                                     ]
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
      jRecurrPlot1 <- tkrplot::tkrplot(SubPanR4C3, fun = jRecurrPlot)
      tcltk::tkgrid(jRecurrPlot1)
      tcltk::tkpack(SubPanR4C3, expand = TRUE, fill = "both")
      assign("SubPanR4C3", SubPanR4C3, envir = KTSEnv)
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      showPANjrmp1()
    }
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyRm(action = "showPANjrmp1", 
               envirName = environment(showPANjrmp1))
}

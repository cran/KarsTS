getCoordsKTS <-
function() {
  getCoordsOnPlot <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                 noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    } else {
      plotAndGreen <- function() {
        graphics::plot(xCr, yCr, main = "", xlab = "", 
                       xaxt = "n", 
                       ylab = selTsName)
        ticksAt <- graphics::par("xaxp")
        myTicksAt <- seq(ticksAt[1], ticksAt[2], length.out = ticksAt[3] + 1)
        getMyTicksIndices <- function(x) {
          difxCoorx <- abs(xCr - x)
          minDifxCoorx <- min(which(difxCoorx == min(difxCoorx)))
        }
        myTicksIndices <- vapply(myTicksAt, 
                                 FUN = getMyTicksIndices, FUN.VALUE = 1)
        myTickLab <- as.character(selTs$time[myTicksIndices])
        graphics::axis(1, at = myTicksAt, labels = myTickLab, cex.axis = 0.8)
        if (length(KTSEnv$grP) > 0) {
          for (i in (1:length(KTSEnv$grP))) {
            indexClosest <- KTSEnv$grP[i]
            graphics::points(xCr[indexClosest], yCr[indexClosest], 
                             col = "limegreen")
          }
        }
        parPlotSize <- graphics::par("plt")
        assign("parPlotSize", parPlotSize, envir = KTSEnv)
        uC <- graphics::par("usr")
        assign("uC", uC, envir = KTSEnv)
      }
      findPointToGreen <- function(xC, yC, imgXcr, imgYcr) {
        squaredDistance <- (xC - imgXcr)^2 + (yC - imgYcr)^2
        indexClosest <- which.min(squaredDistance)
        if (any(KTSEnv$grP == indexClosest)) {
          KTSEnv$grP <- KTSEnv$grP[which(KTSEnv$grP != indexClosest)]
        } else {
          KTSEnv$grP <- c(KTSEnv$grP, indexClosest)
          assign("grP", KTSEnv$grP, envir = KTSEnv)
        }
        tkrplot::tkrreplot(getCoorPlot)
      }
      onLeftClick <- function(x, y) {
        xC <- x
        yC <- y
        width <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("reqwidth", 
                                                           getCoorPlot)))
        height <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("reqheight", 
                                                            getCoorPlot)))
        xMin <- KTSEnv$parPlotSize[1] * width
        xMax <- KTSEnv$parPlotSize[2] * width
        yMin <- KTSEnv$parPlotSize[3] * height
        yMax <- KTSEnv$parPlotSize[4] * height
        rangeX <- KTSEnv$uC[2] - KTSEnv$uC[1]
        rangeY <- KTSEnv$uC[4] - KTSEnv$uC[3]
        imgXcr <- (xCr - KTSEnv$uC[1]) * (xMax - xMin)/rangeX + xMin
        imgYcr <- (yCr - KTSEnv$uC[3]) * (yMax - yMin)/rangeY + yMin
        xC <- as.numeric(xC) + 0.5
        yC <- as.numeric(yC) + 0.5
        yC <- height - yC
        xPlotCoord <- KTSEnv$uC[1] + (xC - xMin) * rangeX/(xMax - xMin)
        yPlotCoord <- KTSEnv$uC[3] + (yC - yMin) * rangeY/(yMax - yMin)
        findPointToGreen(xC, yC, imgXcr, imgYcr)
      }
      writeCoor <- function() {
        if (length(KTSEnv$grP) == 0) {
          tcltk::tkmessageBox(message = paste("Choose at least one point"), 
                              icon = "warning")
        } else {
          KTSEnv$grP <- sort(KTSEnv$grP)
          selTs <- get(selTsName, envir = KTSEnv)
          greenedTable <- data.frame(index = KTSEnv$grP, 
                                     date = selTs$time[KTSEnv$grP], 
                                     value = selTs$value[KTSEnv$grP])
          txt1 <- c("SELECTED POINTS", paste("Time series:", selTsName))
          txt6 <- utils::capture.output(print.data.frame(greenedTable))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txt1, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txt6, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
        }
      }
      clearSelection <- function() {
        assign("grP", c(), envir = KTSEnv)
        tkrplot::tkrreplot(getCoorPlot)
      }
      selTs <- get(selTsName, envir = KTSEnv)
      xCr <- as.numeric(selTs$time)
      yCr <- selTs$value
      assign("grP", c(), envir = KTSEnv)
      limegreendedPoints <- list()
      gCooWin <- tcltk::tktoplevel(bg = "white")
      tcltk::tkwm.title(gCooWin, selTsName)
      gCooWinFrame <- tcltk::tkframe(gCooWin, borderwidth = 2, 
                                     relief = "raised")
      tcltk::tkgrid(gCooWinFrame)
      tcltk::tkgrid.configure(gCooWinFrame, sticky = "n")
      tcltk::tkgrid.columnconfigure(gCooWinFrame, 0, weight = 0)
      tcltk::tkgrid.rowconfigure(gCooWinFrame, 0, weight = 0)
      tcltk::tkgrid.rowconfigure(gCooWinFrame, 1, weight = 0)
      getCoorPlot <- tkrplot::tkrplot(gCooWinFrame, fun = plotAndGreen, 
                                      hscale = 3, vscale = 1.5)
      botonrp <- tcltk::tkbutton(gCooWinFrame, text = "Write results", 
                                 command = writeCoor)
      botonns <- tcltk::tkbutton(gCooWinFrame, text = "New selection", 
                                 command = clearSelection)
      tcltk::tkpack(getCoorPlot, expand = TRUE, 
                    fill = "both", anchor = "center")
      tcltk::tkpack(botonrp, expand = TRUE, fill = "both", side = "left")
      tcltk::tkpack(botonns, expand = TRUE, fill = "both", side = "left")
      tcltk::tkpack(gCooWinFrame, expand = TRUE, fill = "both")
      tcltk::tkbind(getCoorPlot, "<Button-1>", onLeftClick)
      tcltk::tkconfigure(getCoorPlot, cursor = "hand2")
    }
  }
  showPANgetCoords <- function() {
    if (exists("selTsName")) {
      selTsName <- "0"
    }
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "GET COORDINATES")
    createTsRb()
    createOK(labTitle = "PLOT", action = getCoordsOnPlot)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANgetCoords", 
               envirName = environment(showPANgetCoords))
}

plotTimeSeries <-
function() {
    plotpartzoom <- function() {
        puntos <- tcltk::tclvalue(KTSEnv$ele1cbValue)
        lineas <- tcltk::tclvalue(KTSEnv$ele2cbValue)
        refreshDataSetsList(outp = FALSE)
        tssel <- tsCheckedTF()
        tsselorden <- readMultEntryvalues(KTSEnv$dSList$nTS, 
                                          prefix = "tEntscbValue", 
                                          type = "integer")
        tsselorden <- tsselorden[which(tssel == TRUE)]
        tsToPlotNames <- KTSEnv$dSList$TS[which(tssel == TRUE)]
        nTsToPlot <- length(tsToPlotNames)
        if (nTsToPlot == 0) {
            tcltk::tkmessageBox(message = paste("Choose at least",
                                                "one time series"), 
                                icon = "warning")
        } else if (any(duplicated(tsselorden[which(is.finite(tsselorden))]))) {
            tcltk::tkmessageBox(message = paste("Two time series cannot",
                                                "have the same",
                                                "ordinal number"), 
                icon = "warning")
        } else {
            orderTsForPlotting <- function(nTsToPlot, tsselorden) {
                tsselorden <- cbind(1:nTsToPlot, tsselorden)
                tsselorden <- tsselorden[order(tsselorden[, 2], 
                                               na.last = TRUE), 
                  ]
                tsselorden <- tsselorden[, 1]
                tsToPlotNames <- tsToPlotNames[tsselorden]
                tsToPlotNames <- rev(tsToPlotNames)
            }
            getPlotLims <- function(nTsToPlot, tsToPlotNames) {
                firstTime <- Inf
                lastTime <- -Inf
                minValue <- Inf
                maxValue <- -Inf
                for (X in 1:nTsToPlot) {
                  timSer <- get(tsToPlotNames[X], envir = KTSEnv)
                  if (timSer$time[1] < firstTime) {
                    tsWithMinIniTime <- X
                    firstTime <- timSer$time[1]
                  }
                  if (timSer$time[length(timSer$time)] > lastTime) {
                    tsWithMaxLastTime <- X
                    lastTime <- timSer$time[length(timSer$time)]
                  }
                  if (min(timSer$value, na.rm = TRUE) < minValue) {
                    tsWithMinVal <- X
                    minValue <- min(timSer$value, na.rm = TRUE)
                  }
                  if (max(timSer$value, na.rm = TRUE) > maxValue) {
                    tsWithMaxVal <- X
                    maxValue <- max(timSer$value, na.rm = TRUE)
                  }
                }
                aLittleMargin <- round(0.02 * (maxValue - minValue))
                minValue <- minValue - aLittleMargin
                maxValue <- maxValue + aLittleMargin
                legendSpace <- round(0.2 * (maxValue - minValue))
                maxValue <- maxValue + legendSpace
                assign("plotLims", list(firstTime = firstTime, 
                                        lastTime = lastTime, 
                                        minValue = minValue, 
                                        maxValue = maxValue), 
                       envir = environment(getPlotLims))
            }
            getColors <- function(nTsToPlot) {
              colorsToPlot <- c("blue", "red", "darkgreen", "magenta", "cyan", 
                                "green", "orange", "brown", 
                                "purple", "darkcyan")
              if (nTsToPlot > 10) {
                colorsToPlot <- c(colorsToPlot, rep("black", nTsToPlot - 10))
              } else {
                colorsToPlot <- colorsToPlot[1:nTsToPlot]
              }
              colorsToPlot <- rev(colorsToPlot)
            }
            plotVariousTs <- function() {
                plotLims <- getPlotLims(nTsToPlot, tsToPlotNames)
                colorsToPlot <- getColors(nTsToPlot)
                firstTime <- plotLims[[1]]
                lastTime <- plotLims[[2]]
                minValue <- plotLims[[3]]
                maxValue <- plotLims[[4]]
                graphics::plot(c(firstTime, lastTime), 
                               c(minValue, maxValue), 
                               ylim = c(minValue,maxValue), 
                               col = "white", xlab = "", ylab = "")
                if (lineas == "1") {
                  for (X in 1:nTsToPlot) {
                    graphics::lines(get(tsToPlotNames[X], envir = KTSEnv), 
                                    col = colorsToPlot[X])
                  }
                  if (exists("fragToZoom", envir = KTSEnv)) {
                    graphics::lines(tsToPlot[KTSEnv$fragToZoom, ], 
                                    col = "limegreen")
                  }
                }
                if (puntos == "1") {
                  for (X in 1:nTsToPlot) {
                    graphics::points(get(tsToPlotNames[X], envir = KTSEnv), 
                                     col = colorsToPlot[X], 
                      cex = 0.25)
                  }
                  if (exists("fragToZoom", envir = KTSEnv)) {
                    graphics::points(tsToPlot[KTSEnv$fragToZoom, ], 
                                     col = "limegreen", 
                                     cex = 0.25)
                  }
                }
                graphics::legend("topleft", legend = rev(tsToPlotNames), 
                                 lty = 1,lwd = 2, bty = "n", ncol = 4, 
                                 col = rev(colorsToPlot), y.intersp = 1)
                parPlotSize <- graphics::par("plt")
                assign("parPlotSize", parPlotSize, envir = KTSEnv)
                uC <- graphics::par("usr")
                assign("uC", uC, envir = KTSEnv)
            }
            copyPlot <- function() {
                tkrplot::tkrreplot(tsPlot)
            }
            onLeftClick <- function(x, y) {
                tkrplot::tkrreplot(tsPlot)
                xC <- x
                yC <- y
                width <- tcltk::tclvalue(tcltk::tkwinfo("reqwidth", 
                                                        tsPlot))
                height <- tcltk::tclvalue(tcltk::tkwinfo("reqheight", 
                                                         tsPlot))
                width <- as.numeric(width)
                height <- as.numeric( height)
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
                squaredDistance <- (xC - imgXcr)^2 + (yC - imgYcr)^2
                indexClosest <- which.min(squaredDistance)
                assign("touchedPoints", c(KTSEnv$touchedPoints, indexClosest), 
                       envir = KTSEnv)
            }
            onrelease <- function(x, y) {
                leftLimit <- min(KTSEnv$touchedPoints)
                rightLimit <- max(KTSEnv$touchedPoints)
                if (all(is.finite(c(leftLimit, rightLimit)))) {
                  assign("fragToZoom", leftLimit:rightLimit, envir = KTSEnv)
                }
                assign("touchedPoints", NULL, envir = KTSEnv)
                tkrplot::tkrreplot(tsPlot)
            }
            createZoom <- function() {
                getZoomYLims <- function(nTsToPlot, tsToPlotNames) {
                  minValue <- Inf
                  maxValue <- -Inf
                  for (X in 1:nTsToPlot) {
                    timSer <- get(tsToPlotNames[X], envir = KTSEnv)
                    if (min(timSer$value, na.rm = TRUE) < minValue) {
                      tsWithMinVal <- X
                      minValue <- min(timSer$value[KTSEnv$fragToZoom], 
                                      na.rm = TRUE)
                    }
                    if (max(timSer$value, na.rm = TRUE) > maxValue) {
                      tsWithMaxVal <- X
                      maxValue <- max(timSer$value[KTSEnv$fragToZoom], 
                                      na.rm = TRUE)
                    }
                  }
                  aLittleMargin <- round(0.02 * (maxValue - minValue))
                  minValue <- minValue - aLittleMargin
                  maxValue <- maxValue + aLittleMargin
                  legendSpace <- round(0.2 * (maxValue - minValue))
                  maxValue <- maxValue + legendSpace
                  zoomLims <- list(minValue = minValue, maxValue = maxValue)
                }
                plotFragment <- function() {
                  zoomLims <- getZoomYLims(nTsToPlot, tsToPlotNames)
                  minValue <- zoomLims[[1]]
                  maxValue <- zoomLims[[2]]
                  colorsToPlot <- getColors(nTsToPlot)
                  graphics::plot(tsToPlot[KTSEnv$fragToZoom, ], cex = 0.01, 
                                 ylim = c(minValue,maxValue), col = "white", 
                                 xlab = "", ylab = "")
                  if (lineas == "1") {
                    for (X in 1:nTsToPlot) {
                      graphics::lines(get(tsToPlotNames[X], envir = KTSEnv),
                                      col = colorsToPlot[X])
                    }
                  }
                  if (puntos == "1") {
                    for (X in 1:nTsToPlot) {
                      graphics::points(get(tsToPlotNames[X], envir = KTSEnv), 
                                       col = colorsToPlot[X], 
                        cex = 0.25)
                    }
                  }
                  graphics::legend("topleft", legend = rev(tsToPlotNames), 
                                   lty = 1,lwd = 2, bty = "n", ncol = 4, 
                                   col = rev(colorsToPlot), y.intersp = 1)
                }
                copyZoom <- function() {
                  tkrplot::tkrreplot(tsZoom)
                }
                zoomYLims <- getZoomYLims(nTsToPlot, tsToPlotNames)
                minValue <- zoomYLims[[1]]
                maxValue <- zoomYLims[[2]]
                panelZName <- createRandName()
                assign(panelZName, tcltk::tktoplevel(bg = "white"))
                tcltk::tkwm.title(get(panelZName), tsToPlotNames)
                frameZName <- createRandName("frameZName")
                assign(frameZName, tcltk::tkframe(get(panelZName), 
                                                  borderwidth = 2, 
                                                  relief = "raised"))
                tcltk::tkgrid(get(frameZName))
                tcltk::tkgrid.configure(get(frameZName), sticky = "n")
                tcltk::tkgrid.columnconfigure(get(frameZName), 0, weight = 0)
                tcltk::tkgrid.rowconfigure(get(frameZName), 0, weight = 0)
                tcltk::tkgrid.rowconfigure(get(frameZName), 1, weight = 0)
                tsZoom <- tkrplot::tkrplot(get(frameZName), fun = plotFragment, 
                                           hscale = 3, vscale = 1.5)
                tcltk::tkconfigure(tsZoom, bg = "white")
                copyButton <- tcltk::tkbutton(get(frameZName), 
                                              text = "Copy to clipboard", 
                                              command = copyZoom)
                tcltk::tkpack(tsZoom, expand = TRUE, 
                              fill = "both", anchor = "center")
                tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
                tcltk::tkpack(get(frameZName), expand = TRUE, fill = "both")
            }
            if (all(is.na(tsselorden))) {
                tsselorden <- 1:nTsToPlot
            }
            if (nTsToPlot > 1) {
                tsToPlotNames <- orderTsForPlotting(nTsToPlot, tsselorden)
            }
            tsToPlot <- get(tsToPlotNames[length(tsToPlotNames)], 
                            envir = KTSEnv)
            assign("tsToPlot", tsToPlot, envir = KTSEnv)
            xCr <- as.numeric(tsToPlot$time)
            yCr <- tsToPlot$value
            assign("touchedPoints", NULL, envir = KTSEnv)
            panelName <- createRandName()
            assign(panelName, tcltk::tktoplevel(bg = "white"))
            tcltk::tkwm.title(get(panelName), tsToPlotNames)
            frameName <- createRandName("frameName")
            assign(frameName, tcltk::tkframe(get(panelName), borderwidth = 2, 
                                             relief = "raised"))
            tcltk::tkgrid(get(frameName))
            tcltk::tkgrid.configure(get(frameName), sticky = "n")
            tcltk::tkgrid.columnconfigure(get(frameName), 0, weight = 0)
            tcltk::tkgrid.rowconfigure(get(frameName), 0, weight = 0)
            tcltk::tkgrid.rowconfigure(get(frameName), 1, weight = 0)
            tsPlot <- tkrplot::tkrplot(get(frameName), fun = plotVariousTs, 
                                       hscale = 3, vscale = 1.5)
            copyButton <- tcltk::tkbutton(get(frameName), 
                                          text = "Copy to clipboard", 
                                          command = copyPlot)
            zoomButton <- tcltk::tkbutton(get(frameName), 
                                          text = "Zoom selected area", 
                                          command = createZoom)
            tcltk::tkpack(tsPlot, expand = TRUE, 
                          fill = "both", anchor = "center")
            tcltk::tkconfigure(tsPlot, bg = "white")
            tcltk::tkpack(copyButton, expand = TRUE, 
                          fill = "both", side = "left")
            tcltk::tkpack(zoomButton, expand = TRUE, 
                          fill = "both", side = "left")
            tcltk::tkpack(get(frameName), expand = TRUE, fill = "both")
            tcltk::tkbind(tsPlot, "<B1-Motion>", onLeftClick)
            tcltk::tkbind(tsPlot, "<ButtonRelease-1>", onrelease)
        }
    }
    plotpart <- function() {
        puntos <- tcltk::tclvalue(KTSEnv$ele1cbValue)
        lineas <- tcltk::tclvalue(KTSEnv$ele2cbValue)
        refreshDataSetsList(outp = FALSE)
        tssel <- tsCheckedTF()
        tsselorden <- readMultEntryvalues(KTSEnv$dSList$nTS, 
                                          prefix = "tEntscbValue", 
                                          type = "integer")
        tsselorden <- tsselorden[which(tssel == TRUE)]
        tsToPlotNames <- KTSEnv$dSList$TS[which(tssel == TRUE)]
        nTsToPlot <- length(tsToPlotNames)
        if (nTsToPlot == 0) {
            tcltk::tkmessageBox(message = paste("Choose at least",
                                                "one time series"), 
                                icon = "warning")
        } else if (any(duplicated(tsselorden[which(is.finite(tsselorden))]))) {
            tcltk::tkmessageBox(message = paste("Two time series",
                                                "cannot have the same",
                                                "ordinal number"), 
                icon = "warning")
        } else {
            orderTsForPlotting <- function(nTsToPlot, tsselorden) {
                tsselorden <- cbind(1:nTsToPlot, tsselorden)
                tsselorden <- tsselorden[order(tsselorden[, 2], 
                                               na.last = TRUE), ]
                tsselorden <- tsselorden[, 1]
                tsToPlotNames <- tsToPlotNames[tsselorden]
                tsToPlotNames <- rev(tsToPlotNames)
            }
            getPlotLims <- function(nTsToPlot, tsToPlotNames) {
                firstTime <- Inf
                lastTime <- -Inf
                minValue <- Inf
                maxValue <- -Inf
                for (X in 1:nTsToPlot) {
                  timSer <- get(tsToPlotNames[X], envir = KTSEnv)
                  if (timSer$time[1] < firstTime) {
                    tsWithMinIniTime <- X
                    firstTime <- timSer$time[1]
                  }
                  if (timSer$time[length(timSer$time)] > lastTime) {
                    tsWithMaxLastTime <- X
                    lastTime <- timSer$time[length(timSer$time)]
                  }
                  if (min(timSer$value, na.rm = TRUE) < minValue) {
                    tsWithMinVal <- X
                    minValue <- min(timSer$value, na.rm = TRUE)
                  }
                  if (max(timSer$value, na.rm = TRUE) > maxValue) {
                    tsWithMaxVal <- X
                    maxValue <- max(timSer$value, na.rm = TRUE)
                  }
                }
                aLittleMargin <- round(0.02 * (maxValue - minValue))
                minValue <- minValue - aLittleMargin
                maxValue <- maxValue + aLittleMargin
                legendSpace <- round(0.2 * (maxValue - minValue))
                maxValue <- maxValue + legendSpace
                assign("plotLims", list(firstTime = firstTime, 
                                        lastTime = lastTime, 
                                        minValue = minValue, 
                                        maxValue = maxValue), 
                       envir = environment(getPlotLims))
            }
            getColors <- function(nTsToPlot) {
                colorsToPlot <- c("blue", "red", "darkgreen", 
                                  "magenta", "cyan","green", 
                                  "orange", "brown", "purple", "darkcyan")
                if (nTsToPlot > 10) {
                  colorsToPlot <- c(colorsToPlot, rep("black", nTsToPlot - 10))
                } else {
                  colorsToPlot <- colorsToPlot[1:nTsToPlot]
                }
                colorsToPlot <- rev(colorsToPlot)
            }
            plotVariousTs <- function() {
                plotLims <- getPlotLims(nTsToPlot, tsToPlotNames)
                colorsToPlot <- getColors(nTsToPlot)
                firstTime <- plotLims[[1]]
                lastTime <- plotLims[[2]]
                minValue <- plotLims[[3]]
                maxValue <- plotLims[[4]]
                graphics::plot(c(firstTime, lastTime), c(minValue, maxValue), 
                               ylim = c(minValue,maxValue), col = "grey", 
                               xlab = "", ylab = "", cex = 0.01)
                if (lineas == "1") {
                  for (X in 1:nTsToPlot) {
                    graphics::lines(get(tsToPlotNames[X], envir = KTSEnv), 
                                    col = colorsToPlot[X])
                  }
                }
                if (puntos == "1") {
                  for (X in 1:nTsToPlot) {
                    graphics::points(get(tsToPlotNames[X], envir = KTSEnv), 
                                     col = colorsToPlot[X], 
                      cex = 0.25)
                  }
                }
                graphics::legend("topleft", legend = rev(tsToPlotNames), 
                                 lty = 1,lwd = 2, bty = "n", ncol = 4, 
                                 col = rev(colorsToPlot), y.intersp = 1)
            }
            copyPlot <- function() {
                tkrplot::tkrreplot(tsPlot)
            }
            if (all(is.na(tsselorden))) {
                tsselorden <- 1:nTsToPlot
            }
            if (nTsToPlot > 1) {
                tsToPlotNames <- orderTsForPlotting(nTsToPlot, tsselorden)
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
            grafico <- tkrplot::tkrplot(SubPanR4C3, fun = plotVariousTs)
            tcltk::tkgrid(grafico, row = 0)
            tcltk::tkpack(SubPanR4C3, expand = TRUE, fill = "both")
            assign("SubPanR4C3", SubPanR4C3, envir = KTSEnv)
        }
    }
    showPANplotTs <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "PLOT TIME SERIES")
        createTitle(labTitle = "Time series")
        myApplyVector(FUN = createChbEntry, 1:KTSEnv$dSList$nTS, 
                      elements = KTSEnv$dSList$TS, 
            prefix = "scbValue")
        createTitle(labTitle = "Type")
        createChb(labTitle = "Lines", variableName = "ele2cbValue", 
                  defaultVal = "1")
        createChb(labTitle = "Points", variableName = "ele1cbValue", 
                  defaultVal = "0")
        createOK(labTitle = "PLOT", action = plotpart)
        createOK(labTitle = "ZOOM", action = plotpartzoom)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    cleanEnvir()
    if (exists("SubPanR4C3", envir = KTSEnv)) {
        tcltk::tkdestroy(KTSEnv$SubPanR4C3)
    }
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANplotTs", 
                 envirName = environment(showPANplotTs))
}

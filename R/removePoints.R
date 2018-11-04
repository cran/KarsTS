removePoints <-
function() {
    rmPointsOnPlot <- function() {
        newName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                   noValid = NA)
        threshold <- verifyRealEntry(tcltk::tclvalue(KTSEnv$thresh), 
                                     noValid = NA)
        selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                     noValid = NA)
        if (is.na(selTsName)) {
            tcltk::tkmessageBox(message = "Choose a time series", 
                                icon = "warning")
        } else if (is.na(threshold)) {
            tcltk::tkmessageBox(message = paste("Enter a threshold",
                                                "(the maximum distance",
                                                "between the cursor",
                                                "and the point, at which",
                                                "the point will be",
                                                "selected).If too many",
                                                " points are selected when",
                                                "you drag on them, the",
                                                "threshold should be smaller",
                                                "and viceversa. Try until",
                                                "you get an",
                                                "appropiate value"), 
                icon = "warning")
        } else {
            turnPointsToNAs <- function() {
                if (length(KTSEnv$indicesToRedden) == 0) {
                  tcltk::tkmessageBox(message = paste("Choose at least",
                                                      "one point"),
                                      icon = "warning")
                } else {
                  KTSEnv$indicesToRedden <- sort(KTSEnv$indicesToRedden)
                  selTs1 <- get(selTsName, envir = KTSEnv)
                  selTs1$value[KTSEnv$indicesToRedden] <- NA
                  if (is.na(newName)) {
                    newName <- paste0(selTsName, "_pr")
                  } else if (newName == "overwrite series") {
                    newName <- selTsName
                  }
                  assign(newName, selTs1, envir = KTSEnv)
                  cleanEnvir()
                  if (exists("rmPWin")) {
                    tcltk::tkdestroy(rmPWin)
                  }
                  refreshDataSetsList(outp = FALSE)
                  showPANrmPoints()
                }
            }
            clearSelection <- function() {
                assign("indicesToRedden", NULL, envir = KTSEnv)
                assign("candidToRedden", NULL, envir = KTSEnv)
                tkrplot::tkrreplot(rmPointsPlot)
            }
            plotTsAndRedden <- function() {
                graphics::plot(selTs, xlab = "", ylab = "", 
                               cex = 0.5, main = selTsName,
                               pch = "+")
                if (length(KTSEnv$indicesToRedden > 0)) {
                  graphics::points(selTs[KTSEnv$indicesToRedden, ], 
                                   col = "red", cex = 0.5, pch = "+")
                }
                parPlotSize <- graphics::par("plt")
                assign("parPlotSize", parPlotSize, envir = KTSEnv)
                uC <- graphics::par("usr")
                assign("uC", uC, envir = KTSEnv)
            }
            onLeftClick <- function(x, y) {

                xC <- x
                yC <- y
                
                width <- tcltk::tclvalue(tcltk::tkwinfo("reqwidth", 
                                                        rmPointsPlot))
                height <- tcltk::tclvalue(tcltk::tkwinfo("reqheight",
                                                         rmPointsPlot))
                
                width <- as.numeric(width)
                height <- as.numeric(height)
                
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
                indexClosest <- which(squaredDistance < threshold)
                assign("candidToRedden", 
                       c(KTSEnv$candidToRedden, indexClosest), 
                       envir = KTSEnv)
            }
            onrelease <- function(x, y) {
                assign("indicesToRedden", 
                       unique(c(KTSEnv$indicesToRedden, 
                                KTSEnv$candidToRedden)), 
                  envir = KTSEnv)
                tkrplot::tkrreplot(rmPointsPlot)
            }
            
            selTs <- get(selTsName, envir = KTSEnv)
            xCr <- as.numeric(selTs$time)
            yCr <- selTs$value
            assign("indicesToRedden", NULL, envir = KTSEnv)
            assign("candidToRedden", NULL, envir = KTSEnv)
            rmPWin <- tcltk::tktoplevel(bg = "white")
            tcltk::tkwm.title(rmPWin, selTsName)
            rmPWinFrame <- tcltk::tkframe(rmPWin, borderwidth = 2, 
                                          relief = "raised")
            tcltk::tkgrid(rmPWinFrame)
            tcltk::tkgrid.configure(rmPWinFrame, sticky = "n")
            tcltk::tkgrid.columnconfigure(rmPWinFrame, 0, weight = 0)
            tcltk::tkgrid.rowconfigure(rmPWinFrame, 0, weight = 0)
            tcltk::tkgrid.rowconfigure(rmPWinFrame, 1, weight = 0)
            rmPointsPlot <- tkrplot::tkrplot(rmPWinFrame, 
                                             fun = plotTsAndRedden, 
                                             hscale = 3, vscale = 1.5)
            tcltk::tkconfigure(rmPointsPlot, bg = "white")
            botonrp <- tcltk::tkbutton(rmPWinFrame, text = "Remove points", 
                                       command = turnPointsToNAs)
            botonns <- tcltk::tkbutton(rmPWinFrame, text = "New selection", 
                                       command = clearSelection)
            tcltk::tkpack(rmPointsPlot, expand = TRUE, 
                          fill = "both", anchor = "center")
            tcltk::tkpack(botonrp, expand = TRUE, 
                          fill = "both", side = "left")
            tcltk::tkpack(botonns, expand = TRUE, 
                          fill = "both", side = "left")
            tcltk::tkpack(rmPWinFrame, expand = TRUE, fill = "both")
            tcltk::tkbind(rmPointsPlot, "<B1-Motion>", onLeftClick)
            tcltk::tkbind(rmPointsPlot, "<ButtonRelease-1>", onrelease)
        }
    }
    showPANrmPoints <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "REMOVE POINTS")
        createTsRb()
        createEntry(labTitle = "Name", textVariableName = "newName")
        createEntry(labTitle = "Threshold", 
                    textVariableName = "thresh", defaultVal = "20")
        createOK(labTitle = "PLOT", action = rmPointsOnPlot)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)

    checkIfAnyTs(action = "showPANrmPoints", 
                 envirName = environment(showPANrmPoints))
}

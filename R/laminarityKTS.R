laminarityKTS <-
function() {
  showPANlamina <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "LAMINARITY")
    createRmRb()
    createEntry(labTitle = "Lines minimum length", 
                textVariableName = "minLength")
    createOK(labTitle = "RUN", action = laminaOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  laminaOnOk <- function() {
    selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP), 
                                 noValid = NA)
    selMinLength <- verifyIntEntry(tcltk::tclvalue(KTSEnv$minLength), 
                                   noValid = NA)
    if (is.na(selRmName)) {
      tcltk::tkmessageBox(message = paste("Choose a recurrence matrix"), 
                          icon = "warning")
    } else if (is.na(selMinLength) | selMinLength == 0 | selMinLength == 1) {
      tcltk::tkmessageBox(message = paste("Enter a valid minimum length",
                                          "(an integer greater than 1)"), 
                          icon = "warning")
    } else {
      selRm <- get(selRmName, envir = KTSEnv)
      if (selRm$type == "cross") {
        tcltk::tkmessageBox(message = paste("The recurrence matrix",
                                            "must be simple or joint"), 
                            icon = "warning")
      } else {
        if (any(selRm$tsIni != selRm$tsIni[1])) {
          tcltk::tkmessageBox(message = paste("The joint recurrence matrix",
                                              "comes from lagged time series.",
                                              "Take it into consideration"))
        }
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        X1 <- selRm$ones$X
        Y1 <- selRm$ones$Y
        embShortening <- (selRm$embDim - 1) * selRm$delay
        dimRecMat <- min(selRm$tsLength - embShortening)
        N2 <- dimRecMat^2
        onesNumber <- 2 * length(X1) + dimRecMat
        sampPer <- selRm$samPerSec[1]
        RR <- onesNumber/N2
        rebuildCol <- function(indcol, X1, Y1) {
          sort(c(Y1[which(X1 == indcol)], X1[which(Y1 == indcol)], indcol))
        }
        getLinesInOneVert <- function(colIndex, X1, Y1) {
          lengthLines.i <- NULL
          colToRebuild <- rebuildCol(colIndex, X1, Y1)
          linesInfo <- groupIndices(colToRebuild)
          linesInvert.i <- which(linesInfo[, 3] >= selMinLength)
          if (length(linesInvert.i) > 0) {
            lengthLines.i <- linesInfo[linesInvert.i, 3]
          }
          lengthLines.i
        }
        getOutputs <- function(vertLines) {
          if (length(vertLines) == 0) {
            LAM <- 0
            summLengths <- summary(0)
            summLengthsSec <- summary(0)
            ratioRqa <- LAM/RR
          } else {
            uniqueLengths <- as.matrix(sort(unique(vertLines)))
            funFreq <- function(ll) {res <- length(which(vertLines == ll))}
            freqLengths <- apply(uniqueLengths, 1, FUN = funFreq)
            onesInLines <- sum(uniqueLengths * freqLengths)
            LAM <- onesInLines/onesNumber
            summLengths <- summary(vertLines)
            summLengthsSec <- sampPer * summLengths
            ratioRqa <- LAM/RR
          }
          list(LAM = LAM, ratioRqa = ratioRqa, summLengths = summLengths, 
               summLengthsSec = summLengthsSec)
        }
        getHist <- function(vertLines, selRmName, selMinLength) {
          plothist <- function() {
            
            histResult <- graphics::hist(vertLines, 
                                         breaks = seq(min(vertLines) - 0.5, 
                                                      max(vertLines) + 0.5, 
                                                      by = 1), 
                                         freq = TRUE, plot = TRUE,
                                         right = TRUE, col = "darkred", 
                                         xlab = paste("Lengths (min=", 
                                                      selMinLength, ")"), 
                                         main = paste(selRmName, 
                                                      "vertical lines length"))
            
          }
          copyPlot <- function() {
            tkrplot::tkrreplot(tsPlot)
          }
          panelName <- createRandName()
          assign(panelName, tcltk::tktoplevel(bg = "white"))
          tcltk::tkwm.title(get(panelName), "Histogram")
          tsPlot <- tkrplot::tkrplot(get(panelName), fun = plothist, 
                                     hscale = 1.5, 
                                     vscale = 1.5)
          copyButton <- tcltk::tkbutton(get(panelName), 
                                        text = "Copy to clipboard", 
                                        command = copyPlot)
          tcltk::tkpack(tsPlot, expand = TRUE, fill = "both", 
                        anchor = "center")
          tcltk::tkconfigure(tsPlot, bg = "white")
          tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
        }
        writeLamResult <- function(LAM, ratioRqa, RR, selRmName, 
                                   selMinLength, 
                                   summLengths, summLengthsSec, selRm) {
          txt <- c("LAMINARITY", date(), 
                   paste(" Recurrence matrix:", selRmName), 
                   paste(" Minimum length of the vertical lines:", 
                         selMinLength), 
                   paste(" Recurrence rate:", round(RR, 4)), 
                   paste(" Laminarity:", round(LAM, 4)), 
                   paste(" Ratio:", round(ratioRqa, 4)), 
                   " Summaries of the lengths")
          txt01 <- "  In number of measurements"
          nmqidT <- as.data.frame(t(unclass(summLengths)))
          omgpzpz <- as.data.frame(t(unclass(summLengthsSec)))
          txt1 <- utils::capture.output(print.data.frame(nmqidT))
          txt2 <- "  In seconds"
          txt3 <- utils::capture.output(print.data.frame(omgpzpz))
          txt4 <- c("  Characteristics of the recurrence matrix", 
                    paste("   Type:", selRm$type), 
                    paste("   Tolerance:", 
                          paste(selRm$tol, collapse = ",")), 
                    paste("   Distance:", 
                          paste(selRm$dist, collapse = ",")), 
                    paste("   Embedding dimension:", 
                          paste(selRm$embDim, collapse = ",")), 
                    paste("   Delay:", 
                          paste(selRm$delay, collapse = ",")), 
                    paste("   Original time series:", 
                          paste(selRm$tsName, collapse = ",")), 
                    paste("   Lengths:", 
                          paste(selRm$tsLength, collapse = ",")), 
                    paste("   Sampling periods:", 
                          paste(selRm$samPerSec, collapse = ",")), 
                    paste("   Initial dates:", 
                          paste(selRm$tsIni, collapse = ",")))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txt, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txt01, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txt1, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txt2, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txt3, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txt4, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          endingLines()
        }
        vertLines <- apply(as.matrix(1:dimRecMat), 1, 
                           FUN = getLinesInOneVert, 
                           X1 = X1, Y1 = Y1)
        vertLines <- unlist(vertLines)
        names(vertLines) <- NULL
        results <- getOutputs(vertLines)
        getHist(vertLines, selRmName, selMinLength)
        writeLamResult(results$LAM, results$ratioRqa, RR, selRmName, 
                       selMinLength, 
                       results$summLengths, results$summLengthsSec, 
                       selRm)
        cleanEnvir()
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        showPANlamina()
      }
    }
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyRm(action = "showPANlamina", 
               envirName = environment(showPANlamina))
}

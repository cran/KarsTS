determinismKTS <-
function() {
  showPANdeterm <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "DETERMINISM")
    createRmRb()
    createEntry(labTitle = "Lines minimum length", 
                textVariableName = "minLength")
    createOK(labTitle = "RUN", action = determOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  determOnOk <- function() {
    selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP), 
                                 noValid = NA)
    selMinLength <- verifyIntEntry(tcltk::tclvalue(KTSEnv$minLength), 
                                   noValid = NA)
    if (is.na(selRmName)) {
      tcltk::tkmessageBox(message = paste("Choose a recurrence matrix"), 
                          icon = "warning")
    } else if (is.na(selMinLength) | selMinLength < 2) {
      tcltk::tkmessageBox(message = paste("Enter a valid minimum length",
                                          "(an integer greater than 1)"), 
                          icon = "warning")
    } else {
      selRm <- get(selRmName, envir = KTSEnv)
      if (selRm$type == "cross") {
        tcltk::tkmessageBox(message = paste("The recurrence matrix must",
                                            "be simple or joint"), 
                            icon = "warning")
      } else {
        if (any(selRm$tsIni != selRm$tsIni[1])) {
          tcltk::tkmessageBox(message = paste("The joint recurrence",
                                              "matrix comes from",
                                              "lagged time series.",
                                              "Take it into",
                                              "consideration"))
        }
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        X <- selRm$ones$X
        Y <- selRm$ones$Y
        dXY <- Y - X
        embShortening <- (selRm$embDim - 1) * selRm$delay
        dimRecMat <- min(selRm$tsLength - embShortening)
        N2 <- dimRecMat^2
        nrowRecMat <- length(X)
        onesNumber <- 2 * nrowRecMat + dimRecMat
        sampPer <- selRm$samPerSec[1]
        RR <- onesNumber/N2
        getLinesInOneDiag <- function(diagDist.i, dXY) {
          lengthLines.i <- NULL
          rowsBelongDiag <- which(dXY == diagDist.i)
          diag.i <- Y[rowsBelongDiag]
          if (length(diag.i) >= selMinLength) {
            linesInfo <- groupIndices(diag.i)
            linesInDiag.i <- which(linesInfo[, 3] >= selMinLength)
            if (length(linesInDiag.i) > 0) {
              lengthLines.i <- linesInfo[linesInDiag.i, 3]
            }
          }
          lengthLines.i
        }
        getOutputs <- function(diagLines) {
          if (length(diagLines) == 0) {
            DET <- 0
            summLengths <- summary(0)
            summLengthsSec <- summary(0)
            ratioRqa <- DET/RR
          } else {
            uniqueLengths <- as.matrix(sort(unique(diagLines)))
            freqFun <- function(ll) {res <- length(which(diagLines == ll))}
            freqLengths <- apply(uniqueLengths, 1, freqFun)
            onesInLines <- sum(uniqueLengths * freqLengths)
            DET <- onesInLines/onesNumber
            summLengths <- summary(diagLines)
            summLengthsSec <- sampPer * summLengths
            ratioRqa <- DET/RR
          }
          list(DET = DET, ratioRqa = ratioRqa, summLengths = summLengths, 
               summLengthsSec = summLengthsSec)
        }
        getHist <- function(diagLines, selRmName, selMinLength) {
          plothist <- function() {
            histResult <- graphics::hist(diagLines, 
                                         breaks = seq(min(diagLines) - 0.5, 
                                                      max(diagLines) + 0.5, 
                                                      by = 1), 
                                         freq = TRUE, plot = TRUE, 
                                         right = FALSE, col = "darkred", 
                                         xlab = paste("Lengths (min=", 
                                                      selMinLength, ")"), 
                                         main = paste(selRmName, 
                                                      "diagonal lines length"))
            
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
          tcltk::tkpack(tsPlot, expand = TRUE, 
                        fill = "both", anchor = "center")
          tcltk::tkconfigure(tsPlot, bg = "white")
          tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          
        }
        writeDetResult <- function(DET, ratioRqa, RR, selRmName, selMinLength, 
                                   summLengths, summLengthsSec, selRm) {
          txt <- c("DETERMINISM", date(), 
                   paste(" Recurrence matrix:", selRmName), 
                   paste(" Minimum length of the diagonal lines:", 
                         selMinLength), 
                   paste(" Recurrence rate:", round(RR, 4)), 
                   paste(" Determinism:", round(DET, 4)), 
                   paste(" Ratio:", round(ratioRqa, 4)), 
                   " Summaries of the lengths")
          txt01 <- "  In number of measurements"
          
          wynm1 <- as.data.frame(t(unclass(summLengths)))
          wynm2 <- as.data.frame(t(unclass(summLengthsSec)))
          txt1 <- utils::capture.output(print.data.frame(wynm1))
          txt2 <- "  In seconds"
          txt3 <- utils::capture.output(print.data.frame(wynm2))
          txt4 <- c("  Characteristics of the recurrence matrix", 
                    paste("   Type:",selRm$type), 
                    paste("   Tolerance:", paste(selRm$tol, collapse = ",")), 
                    paste("   Distance:", paste(selRm$dist, collapse = ",")), 
                    paste("   Embedding dimension:", paste(selRm$embDim, 
                                                           collapse = ",")), 
                    paste("   Delay:", paste(selRm$delay, collapse = ",")), 
                    paste("   Original time series:", paste(selRm$tsName, 
                                                            collapse = ",")), 
                    paste("   Lengths:", paste(selRm$tsLength, 
                                               collapse = ",")), 
                    paste("   Sampling periods:", paste(selRm$samPerSec, 
                                                        collapse = ",")), 
                    paste("   Initial dates:", paste(selRm$tsIni, 
                                                     collapse = ",")))
          
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
        
        diagLines <- apply(as.matrix(sort(unique(dXY))), 1, 
                           FUN = getLinesInOneDiag, dXY = dXY)
        
        diagLines <- unlist(diagLines)
        names(diagLines) <- NULL
        results <- getOutputs(diagLines)
        getHist(diagLines, selRmName, selMinLength)
        writeDetResult(results$DET, results$ratioRqa, RR, 
                       selRmName, selMinLength, 
                       results$summLengths, results$summLengthsSec, selRm)
        cleanEnvir()
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        showPANdeterm()
      }
    }
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyRm(action = "showPANdeterm", 
               envirName = environment(showPANdeterm))
}

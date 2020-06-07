invariantsKTS <-
function() {
  invar1OnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else {
      selTs <- get(selTsName, envir = KTSEnv)
      if (any(is.na(selTs$value))) {
        tcltk::tkmessageBox(message = paste("The time series can't have",
                                            "NAs. Make a preliminary",
                                            "filling"), 
                            icon = "warning")
      } else {
        assign("selTsName", selTsName, envir = KTSEnv)
        showPANinvar2()
      }
    }
  }
  invar2OnOk <- function() {
    selDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$delay), noValid = NA)
    selTheilerWin <- verifyIntEntry(tcltk::tclvalue(KTSEnv$theilerWin), 
                                    noValid = NA)
    selcorrel <- verifyIntEntry(tcltk::tclvalue(KTSEnv$correl), noValid = NA)
    selMinEmb <- verifyIntEntry(tcltk::tclvalue(KTSEnv$minEmb), noValid = NA)
    selMaxEmb <- verifyIntEntry(tcltk::tclvalue(KTSEnv$maxEmb), noValid = NA)
    selMinRad <- verifyRealEntry(tcltk::tclvalue(KTSEnv$minRad), noValid = NA)
    selMaxRad <- verifyRealEntry(tcltk::tclvalue(KTSEnv$maxRad), noValid = NA)
    selNumRad <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numRad), noValid = NA)
    if (is.na(selDelay)) {
      tcltk::tkmessageBox(message = paste("Choose the delay",
                                          "for the embedding"), 
                          icon = "warning")
    } else if (is.na(selTheilerWin)) {
      tcltk::tkmessageBox(message = paste("Choose the Theiler window"), 
                          icon = "warning")
    } else if (is.na(selcorrel)) {
      tcltk::tkmessageBox(message = paste("Choose the correltion order"), 
                          icon = "warning")
    } else if (is.na(selMinEmb)) {
      tcltk::tkmessageBox(message = paste("Choose the minimum",
                                          "embedding dimension"), 
                          icon = "warning")
    } else if (is.na(selMaxEmb)) {
      tcltk::tkmessageBox(message = paste("Choose the maximum",
                                          "embedding dimension"), 
                          icon = "warning")
    } else if (is.na(selMinRad)) {
      tcltk::tkmessageBox(message = paste("Choose the minimum radius"), 
                          icon = "warning")
    } else if (is.na(selMaxRad)) {
      tcltk::tkmessageBox(message = paste("Choose the maximum radius"), 
                          icon = "warning")
    } else if (is.na(selNumRad)) {
      tcltk::tkmessageBox(message = paste("Choose the number of",
                                          "radii to use"), 
                          icon = "warning")
    } else {
      selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      resCorrDim <- nonlinearTseries::corrDim(time.series = selTs$value, 
                                              min.embedding.dim = selMinEmb, 
                                              max.embedding.dim = selMaxEmb, 
                                              time.lag = selDelay, 
                                              min.radius = selMinRad, 
                                              max.radius = selMaxRad, 
                                              corr.order = selcorrel, 
                                              n.points.radius = selNumRad, 
                                              theiler.window = selTheilerWin, 
                                              do.plot = FALSE, 
                                              localScalingExp = TRUE)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      if (length(resCorrDim[[1]]) == 0) {
        tcltk::tkmessageBox(message = paste("The output was empty.",
                                            "You can try using less",
                                            "restrictive conditions"), 
                            icon = "warning")
      } else {
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        
        corrMdf <- round(nonlinearTseries::corrMatrix(resCorrDim), 5)
        corrMdf <- as.data.frame(corrMdf)
        colnames(corrMdf) <- signif(nonlinearTseries::radius(resCorrDim), 5)
        if (tcltk::tclvalue(KTSEnv$doCorrSum) == "1") {
          txt1 <- "CORRELATION SUMS MATRIX"
          txt2 <- paste("Time series:", KTSEnv$selTsName)
          txt3 <- paste("Delay:", selDelay)
          txt4 <- paste("Theiler window:", selTheilerWin)
          txt5 <- paste("Correlation order(q):", selcorrel)
          txt6 <- "Each column corresponds to a radius"
          txt7 <- "Each row corresponds to an embedding dimension"
          txtCorrM <- utils::capture.output(print.data.frame(corrMdf))
          txtAll <- c(txt1, txt2, txt3, txt4, txt5, txt6, txt7)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtAll, collapse = "\\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n\\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtCorrM, collapse = "\\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n\\n"))
          corrSumDimPlot <- function() {
            graphics::plot(resCorrDim)
            
          }
          copyPlot <- function() {
            tkrplot::tkrreplot(tsPlot)
          }
          panelName <- createRandName()
          assign(panelName, tcltk::tktoplevel(bg = "white"))
          tcltk::tkwm.title(get(panelName), "Correlation sums and dimension")
          tsPlot <- tkrplot::tkrplot(get(panelName), fun = corrSumDimPlot, 
                                     hscale = 3, vscale = 1.5)
          copyButton <- tcltk::tkbutton(get(panelName), 
                                        text = "Copy to clipboard", 
                                        command = copyPlot)
          tcltk::tkpack(tsPlot, expand = TRUE, 
                        fill = "both", anchor = "center")
          tcltk::tkconfigure(tsPlot, bg = "white")
          tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
          
        }
        if (tcltk::tclvalue(KTSEnv$doSampEntr) == "1") {
          resSampEnt <- nonlinearTseries::sampleEntropy(resCorrDim, 
                                                        do.plot = FALSE)
          sampEntropy <- as.data.frame(round(resSampEnt$sample.entropy, 5))
          icbSiF <- nonlinearTseries::radius(resSampEnt)
          colnames(sampEntropy) <- signif(icbSiF, 5)
          txt1 <- "SAMPLE ENTROPY MATRIX"
          txt2 <- paste("Time series:", KTSEnv$selTsName)
          txt3 <- paste("Delay:", selDelay)
          txt4 <- paste("Theiler window:", selTheilerWin)
          txt5 <- paste("Correlation order (q):", selcorrel)
          txt6 <- "Each column corresponds to a radius"
          txt7 <- "Each row corresponds to an embedding dimension"
          txtSampE <- utils::capture.output(print.data.frame(sampEntropy))
          txtAll <- c(txt1, txt2, txt3, txt4, txt5, txt6, txt7)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtAll, collapse = "\\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n\\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtSampE, collapse = "\\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n\\n"))
          sampEntroPlot <- function() {
            graphics::plot(resSampEnt)
          }
          copyPlot1 <- function() {
            tkrplot::tkrreplot(tsPlot1)
          }
          panelName1 <- createRandName()
          assign(panelName1, tcltk::tktoplevel(bg = "white"))
          tcltk::tkwm.title(get(panelName1), "Sample entropy")
          tsPlot1 <- tkrplot::tkrplot(get(panelName1), fun = sampEntroPlot, 
                                      hscale = 3, vscale = 1.5)
          copyButton1 <- tcltk::tkbutton(get(panelName1), 
                                         text = "Copy to clipboard", 
                                         command = copyPlot1)
          tcltk::tkpack(tsPlot1, expand = TRUE, 
                        fill = "both", anchor = "center")
          tcltk::tkconfigure(tsPlot1, bg = "white")
          tcltk::tkpack(copyButton1, expand = TRUE, fill = "both")
          
        }
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        showPANinvar1()
      }
    }
  }
  showPANinvar1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "INVARIANTS")
    createTsRb()
    createOK(labTitle = "NEXT", action = invar1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  showPANinvar2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "INVARIANTS")
    createNote(labTitle = paste("Time series:", KTSEnv$selTsName), 
               pady = c(10,5))
    createTitle(labTitle = "Inputs")
    createEntry(labTitle = "Delay", textVariableName = "delay")
    createEntry(labTitle = "Theiler window", textVariableName = "theilerWin")
    createEntry(labTitle = "Correlation order (q)", 
                textVariableName = "correl", 
                defaultVal = "2")
    createEntry(labTitle = "Min. embedding dim.", textVariableName = "minEmb", 
                defaultVal = "1")
    createEntry(labTitle = "Max. embedding dim.", textVariableName = "maxEmb", 
                defaultVal = "10")
    createEntry(labTitle = "Min. radius.", textVariableName = "minRad")
    createEntry(labTitle = "Max. radius.", textVariableName = "maxRad")
    createEntry(labTitle = "Number of radii", textVariableName = "numRad")
    createTitle(labTitle = "Outputs")
    createChb(labTitle = "Correlation sum and dimension", 
              variableName = "doCorrSum", 
              defaultVal = "1")
    createChb(labTitle = "KS Entropy", variableName = "doSampEntr", 
              defaultVal = "1")
    createOK(labTitle = "RUN", action = invar2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANinvar1", 
               envirName = environment(showPANinvar1))
}

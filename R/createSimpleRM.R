createSimpleRM <-
function() {
    showPANsrm1 <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "RECURRENCE MATRIX")
      createTsRb()
      createOK(labTitle = "NEXT", action = srm1OnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    srm1OnOk <- function() {
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                   noValid = NA)
      if (is.na(selTsName)) {
        tcltk::tkmessageBox(message = "Choose a time series", 
                            icon = "warning")
      } else {
        assign("selTsName", selTsName, envir = KTSEnv)
        showPANsrm2()
      }
    }
    showPANsrm2 <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "RECURRENCE MATRIX")
      createNote(labTitle = paste("Time series:", KTSEnv$selTsName))
      createTitle(labTitle = "Parameters")
      createEntry(labTitle = "Embedding dimension", 
                  textVariableName = "embDim", defaultVal = "1")
      createEntry(labTitle = "Delay", textVariableName = "lagDel", 
                  defaultVal = "0")
      createEntry(labTitle = "Tolerance", textVariableName = "thresh")
      createEntry(labTitle = "Name", textVariableName = "newName")
      createOK(labTitle = "RUN", action = srm2OnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    srm2OnOk <- function() {
      refreshDataSetsList(outp = FALSE)
      embedDim <- verifyIntEntry(tcltk::tclvalue(KTSEnv$embDim), 
                                 noValid = NA)
      lagDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lagDel), 
                                 noValid = NA)
      threshold <- verifyRealEntry(tcltk::tclvalue(KTSEnv$thresh), 
                                   noValid = NA)
      selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                    noValid = NA)
      if (is.na(embedDim)) {
        tcltk::tkmessageBox(message = paste("Choose an ",
                                            "embedding dimension"), 
                            icon = "warning")
      } else if (embedDim == 0) {
        tcltk::tkmessageBox(message = paste("The embedding",
                                            "dimension must be",
                                            "one or greater"), 
                            icon = "warning")
      } else if (is.na(lagDelay)) {
        tcltk::tkmessageBox(message = "Choose a delay", icon = "warning")
      } else if (embedDim > 1 & lagDelay == 0) {
        tcltk::tkmessageBox(message = paste("For embedding dimesions",
                                            "greater than 1, the",
                                            "delay must be greater",
                                            "than 0"), 
                            icon = "warning")
      } else if (is.na(threshold)) {
        tcltk::tkmessageBox(message = "Choose a tolerance", 
                            icon = "warning")
      } else if (is.na(selNewName)) {
        tcltk::tkmessageBox(message = paste("Enter a name for the",
                                            "recurrence matrix "), 
                            icon = "warning")
      } else {
        
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
        
        res <- getRecurrencePoints(timSer = selTs,
                                   embedDim = embedDim, 
                                   lagDelay = lagDelay, 
                                   threshold = threshold)
        
        recPointsX <- res$recPointsX
        recPointsY <- res$recPointsY
        
        
        if (length(recPointsY) == 0) {
          tcltk::tkmessageBox(message = paste("No recurrence",
                                              "point was found"), 
                              icon = "warning")
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANsrm1()
        } else {
          recPoints <- as.data.frame(cbind(recPointsX, recPointsY))
          colnames(recPoints) <- c("X", "Y")
          recMatrix <- list(ones = recPoints, tol = threshold, 
                            tsName = KTSEnv$selTsName, 
                            embDim = embedDim, delay = lagDelay, 
                            dist = "inf_norm", 
                            tsLength = nrow(selTs), 
                            samPerSec = diff(as.numeric(selTs$time[1:2])), 
                            tsIni = as.character(selTs$time[1]), 
                            type = "simple")
          assign(selNewName, recMatrix, envir = KTSEnv)
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANsrm1()
        }
      }
    }
    tcltk::tkmessageBox(message = paste("The process can take several minutes",
                                        "if your series is huge.It can even",
                                        "collapse:save everything you need",
                                        "to save before using this function.",
                                        "Save your recurrence matrix",
                                        "to the hard disk"), 
                        icon = "warning")
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANsrm1", 
                 envirName = environment(showPANsrm1))
  }

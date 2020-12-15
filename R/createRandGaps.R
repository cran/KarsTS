createRandGaps <-
function() {
  randGaps1OnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), noValid = NA)
    selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                  noValid = NA)
    nGaps <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numberGaps), noValid = NA)
    lGaps <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lengthGap), noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else if (is.na(nGaps)) {
      tcltk::tkmessageBox(message = paste("Enter the number of",
                                          "gaps to generate"), 
                          icon = "warning")
    } else if (nGaps <= 0) {
      tcltk::tkmessageBox(message = paste("The number of gaps must",
                                          "be strictly positive"), 
                          icon = "warning")
    } else if (is.na(lGaps)) {
      tcltk::tkmessageBox(message = "Enter the gap size", 
                          icon = "warning")
    } else if (lGaps <= 0) {
      tcltk::tkmessageBox(message = paste("The gap size must be",
                                          "strictly positive"), 
                          icon = "warning")
    } else if (is.na(selNewName)) {
      tcltk::tkmessageBox(message = paste("Enter a name for the",
                                          "new set of gaps"), 
                          icon = "warning")
    } else {

      writeResultsRG <- function(selNewName, selTsName, nGaps, lGaps) {
        txt1 <- paste("NEW ARTIFICIAL SET OF RANDOM GAPS:", selNewName)
        txt2 <- paste("Set of gaps generated from the time series", selTsName)
        txt3 <- paste("The new gaps do not overlay the possibly existing ones")
        txt5 <- paste(as.character(nGaps), "random gaps of length", 
                      as.character(lGaps), "were generated")
        txt6 <- paste("along with the corresponding time series")
        txt <- c(txt1, txt2, txt3, txt5, txt6)
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste(txt, collapse = "\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
        endingLines()
      }
      selTs <- get(selTsName, envir = KTSEnv)
      lseriesel <- nrow(selTs)
      newGapsIndices <- getNewGapsInd(selTs, lGaps, nGaps)
      if (is.null(newGapsIndices)) {
        tcltk::tkmessageBox(message = paste("GENERATION OF RANDOM GAPS:",
                                            "the maximum number of",
                                            "iterations was reached.",
                                            "You may try again, please"), 
                            icon = "warning")
      } else {
        timSerGaps <- selTs
        timSerGaps$value[newGapsIndices] <- NA
        assign(paste0(selTsName, "_", selNewName), timSerGaps, envir = KTSEnv)
        newGapSet <- list(gaps = newGapsIndices, 
                          tsIni = as.character(selTs$time[1]), 
                          tsEnd = as.character(selTs$time[lseriesel]), 
                          samPerMin = (diff(as.numeric(selTs$time[1:2])))/60, 
                          tsLength = lseriesel, tsName = selTsName)
        assign(selNewName, newGapSet, envir = KTSEnv)
        writeResultsRG(selNewName, selTsName, nGaps, lGaps)
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANrandGaps1()
      }
    }
  }
  showPANrandGaps1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "ARTIFICIAL RANDOM GAPS")
    createTsRb()
    createEntry(labTitle = "Number of gaps", textVariableName = "numberGaps")
    createEntry(labTitle = "Gap size", textVariableName = "lengthGap")
    createEntry(labTitle = "Name", textVariableName = "newName")
    createOK(labTitle = "RUN", action = randGaps1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANrandGaps1", 
               envirName = environment(showPANrandGaps1))
}

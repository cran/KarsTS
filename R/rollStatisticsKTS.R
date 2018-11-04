rollStatisticsKTS <-
function() {
    showPANrollstat1 <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "ROLLING STATISTICS")
        createTsRb()
        createOK(labTitle = "NEXT", action = rollstat1OnOk)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    rollstat1OnOk <- function() {
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                   noValid = NA)
      if (is.na(selTsName)) {
        tcltk::tkmessageBox(message = "Choose a time series", 
                            icon = "warning")
      } else {
        assign("selTsName", selTsName, envir = KTSEnv)
        showPANrollstat2()
      }
    }
    showPANrollstat2 <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "ROLLING STATISTICS")
      createNote(labTitle = paste("Time series", KTSEnv$selTsName), 
                 pady = c(10,5))
      createEntry(labTitle = "Sliding window size", 
                  textVariableName = "wingap")
      createTitle(labTitle = "Statistic")
      createChb(labTitle = "Minimum", variableName = "mincbValue")
      createChb(labTitle = "First Q.", variableName = "q1cbValue")
      createChb(labTitle = "Median", variableName = "medcbValue")
      createChb(labTitle = "Mean", variableName = "mecbValue")
      createChb(labTitle = "Third Q.", variableName = "q3cbValue")
      createChb(labTitle = "Maximum", variableName = "maxcbValue")
      createChb(labTitle = "St.Dev", variableName = "sdcbValue")
      createTitle(labTitle = "Estimate tails")
      assign("tailOrNot", tcltk::tclVar("No"), envir = KTSEnv)
      createRb(variable = KTSEnv$tailOrNot, dataVector = c("Yes", "No"))
      createOK(labTitle = "RUN", action = rollstat2OnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    rollstat2OnOk <- function() {
      slidingWin <- verifyIntEntry(tcltk::tclvalue(KTSEnv$wingap), 
                                   noValid = NA)
      tailYesNo <- tcltk::tclvalue(KTSEnv$tailOrNot)
      selStatisTF <- c(tcltk::tclvalue(KTSEnv$mincbValue), 
                       tcltk::tclvalue(KTSEnv$q1cbValue), 
                       tcltk::tclvalue(KTSEnv$medcbValue), 
                       tcltk::tclvalue(KTSEnv$mecbValue), 
                       tcltk::tclvalue(KTSEnv$q3cbValue), 
                       tcltk::tclvalue(KTSEnv$maxcbValue), 
                       tcltk::tclvalue(KTSEnv$sdcbValue))
      selStatisTF <- data.frame(t(selStatisTF))
      selStatisTF[which(selStatisTF == "0")] <- FALSE
      selStatisTF[which(selStatisTF == "1")] <- TRUE
      colnames(selStatisTF) <- c("Min", "FQ", "Median", "Mean", 
                                 "TQ", "Max", "Sd")
      if (is.na(slidingWin)) {
        tcltk::tkmessageBox(message = "Enter a window", icon = "warning")
      } else if (all(selStatisTF == FALSE)) {
        tcltk::tkmessageBox(message = "Choose some output", icon = "warning")
      } else {
        selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
        if (tailYesNo == "No") {
          tailsTS <- FALSE
        } else {
          tailsTS <- TRUE
        }
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        res <- getRollStatistics(selTs, KTSEnv$selTsName, 
                                 slidingWin = slidingWin, 
                                 tailsTS = tailsTS, 
                                 selStatisTF = selStatisTF)
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANrollstat1()
      }
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANrollstat1", 
                 envirName = environment(showPANrollstat1))
}

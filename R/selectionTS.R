selectionTS <-
function() {
  selecOnOk1 <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                 noValid = NA)
    selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                  noValid = NA)
    perival <- verifyIntEntry(tcltk::tclvalue(KTSEnv$peri), noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    } else if (is.na(selNewName)) {
      tcltk::tkmessageBox(message = paste("Give a name for",
                                          "the new time series"), 
                          icon = "warning")
    } else if (is.na(perival)) {
      tcltk::tkmessageBox(message = paste("Resampling factor. Set it to",
                                          "1 if you want to keep the same",
                                          "sampling period"), 
                          icon = "warning")
    } else {
      assign("selTsName", selTsName, envir = KTSEnv)
      assign("selNewName", selNewName, envir = KTSEnv)
      assign("perival", perival, envir = KTSEnv)
      showPANselec2()
    }
  }
  selecOnOk2 <- function() {
    iniDateValue <- verifyDateEntry(KTSEnv$inisecs, KTSEnv$inimins, 
                                    KTSEnv$inihour, 
                                    KTSEnv$iniday, KTSEnv$inimonth, 
                                    KTSEnv$iniyear)
    finDateValue <- verifyDateEntry(KTSEnv$finsecs, KTSEnv$finmins,
                                    KTSEnv$finhour, 
                                    KTSEnv$finday, KTSEnv$finmonth, 
                                    KTSEnv$finyear)
    if (is.null(iniDateValue)) {
      tcltk::tkmessageBox(message = paste("The initial date is wrong",
                                          "(day,month or year)"), 
                          icon = "warning")
    } else if (is.null(finDateValue)) {
      tcltk::tkmessageBox(message = paste("The final date is wrong",
                                          "(day,month or year)"), 
                          icon = "warning")
    } else {
      selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
      if (is.na(iniDateValue)) {
        iniDateValueInd <- 1
      } else {
        iniDateValue <- as.numeric(strptime(iniDateValue, 
                                            format = "%Y-%m-%d %H:%M:%S", 
                                            tz = KTSEnv$timeZone))
        iniDateValueInd <- which(iniDateValue == as.numeric(selTs$time))
      }
      if (is.na(finDateValue)) {
        finDateValueInd <- nrow(selTs)
      } else {
        finDateValue <- as.numeric(strptime(finDateValue, 
                                            format = "%Y-%m-%d %H:%M:%S", 
                                            tz = KTSEnv$timeZone))
        finDateValueInd <- which(finDateValue == as.numeric(selTs$time))
      }
      if (length(iniDateValueInd) == 0) {
        tcltk::tkmessageBox(message = paste("The initial date does",
                                            "not belong to the",
                                            "selected time series"), 
                            icon = "warning")
      } else if (length(finDateValueInd) == 0) {
        tcltk::tkmessageBox(message = paste("The final date does not",
                                            "belong to the selected",
                                            "time series"), 
                            icon = "warning")
      } else {
        newTS <- selTs[seq(iniDateValueInd, 
                           finDateValueInd, 
                           KTSEnv$perival),]
        rownames(newTS) <- NULL
        colnames(newTS) <- c("time", "value")
        assign(paste(KTSEnv$selNewName), newTS, KTSEnv)
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANselec1()
      }
    }
  }
  showPANselec1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "CUT & RESAMPLING")
    createTsRb()
    createEntry(labTitle = "Name", textVariableName = "newName")
    createEntry(labTitle = "Resampling", textVariableName = "peri", 
                defaultVal = "1")
    createOK(labTitle = "NEXT", action = selecOnOk1)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  showPANselec2 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "CUT & RESAMPLING")
    createTitle(labTitle = "Initial date")
    createEntry(labTitle = "Year(yyyy)", textVariableName = "iniyear")
    createEntry(labTitle = "Months", textVariableName = "inimonth")
    createEntry(labTitle = "Days", textVariableName = "iniday")
    createEntry(labTitle = "Hours", textVariableName = "inihour")
    createEntry(labTitle = "Minutes", textVariableName = "inimins")
    createEntry(labTitle = "Seconds", textVariableName = "inisecs")
    createTitle(labTitle = "Final date")
    createEntry(labTitle = "Year(yyyy)", textVariableName = "finyear")
    createEntry(labTitle = "Months", textVariableName = "finmonth")
    createEntry(labTitle = "Days", textVariableName = "finday")
    createEntry(labTitle = "Hours", textVariableName = "finhour")
    createEntry(labTitle = "Minutes", textVariableName = "finmins")
    createEntry(labTitle = "Seconds", textVariableName = "finsecs")
    createOK(labTitle = "RUN", action = selecOnOk2)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANselec1", 
               envirName = environment(showPANselec1))
}

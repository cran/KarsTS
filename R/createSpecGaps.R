createSpecGaps <-
function() {
  specGaps1OnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                 noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    } else {
      assign("selTsName", selTsName, envir = KTSEnv)
      showPANspecGaps2()
    }
  }
  specGaps2OnOk <- function() {
    selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                  noValid = NA)
    iniDateValue <- verifyDateEntry(KTSEnv$inisecs, KTSEnv$inimins, 
                                    KTSEnv$inihour, 
                                    KTSEnv$iniday, KTSEnv$inimonth, 
                                    KTSEnv$iniyear)
    finDateValue <- verifyDateEntry(KTSEnv$finsecs, KTSEnv$finmins, 
                                    KTSEnv$finhour, 
                                    KTSEnv$finday, KTSEnv$finmonth, 
                                    KTSEnv$finyear)
    if (is.na(selNewName)) {
      tcltk::tkmessageBox(message = "Choose a name", icon = "warning")
    } else if (is.null(iniDateValue)) {
      tcltk::tkmessageBox(message = paste("The initial date is wrong",
                                          "(day,month or year)"), 
                          icon = "warning")
    } else if (is.null(finDateValue)) {
      tcltk::tkmessageBox(message = paste("The final date is wrong",
                                          "(day,month or year)"), 
                          icon = "warning")
    } else if (is.na(iniDateValue)) {
      tcltk::tkmessageBox(message = paste("The initial date is empty",
                                          "(day,month or year)"), 
                          icon = "warning")
    } else if (is.na(finDateValue)) {
      tcltk::tkmessageBox(message = paste("The final date is empty",
                                          "(day,month or year)"), 
                          icon = "warning")
    } else {
      writeResultsSG <- function(selNewName, selTsName, 
                                 iniDateValue, finDateValue) {
        txt1 <- paste("NEW ARTIFICIAL GAP:", selNewName)
        txt2 <- paste("Set of gaps generated from the time series", selTsName)
        txt3 <- paste("Initial date", iniDateValue)
        txt4 <- paste("Final date", finDateValue)
        txt <- c(txt1, date(), "\\n", txt2, txt3, txt4)
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste(txt, collapse = "\\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n\\n"))
        endingLines()
      }
      selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
      iniDateValueInd <- which(as.character(selTs$time) == iniDateValue)
      finDateValueInd <- which(as.character(selTs$time) == finDateValue)
      if (length(iniDateValueInd) == 0) {
        tcltk::tkmessageBox(message = paste("The initial date does not",
                                            "belong to the selected",
                                            "time series"), 
                            icon = "warning")
      } else if (length(finDateValueInd) == 0) {
        tcltk::tkmessageBox(message = paste("The final date does not",
                                            "belong to the selected",
                                            "time series"), 
                            icon = "warning")
      } else {
        newTimSer <- selTs
        newTimSer$value[iniDateValueInd:finDateValueInd] <- NA
        assign(paste0(KTSEnv$selTsName, "_", selNewName), newTimSer, KTSEnv)
        newGapSet <- list(gaps = iniDateValueInd:finDateValueInd, 
                          tsIni = as.character(selTs$time[1]), 
                          tsEnd = as.character(selTs$time[nrow(selTs)]), 
                          samPerMin = (diff(as.numeric(selTs$time[1:2])))/60, 
                          tsLength = nrow(selTs), tsName = KTSEnv$selTsName)
        assign(selNewName, newGapSet, envir = KTSEnv)
        writeResultsSG(selNewName, KTSEnv$selTsName, 
                       iniDateValue, finDateValue)
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANspecGaps1()
      }
    }
  }
  showPANspecGaps1 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "ARTIFICIAL SPECIFIC GAPS")
    createTsRb()
    createOK(labTitle = "NEXT", action = specGaps1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  showPANspecGaps2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "ARTIFICIAL SPECIFIC GAPS")
    createEntry(labTitle = "Name", textVariableName = "newName")
    createTitle(labTitle = "Inicial date")
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
    createOK(labTitle = "RUN", action = specGaps2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANspecGaps1", 
               envirName = environment(showPANspecGaps1))
}

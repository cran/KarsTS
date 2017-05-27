aggregateKTS <-
function() {
  showPANaggreg1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "AGGREGATE")
    createTsRb()
    createOK(labTitle = "NEXT", action = aggregOnOk1)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  aggregOnOk1 <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                 noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    } else {
      assign("selTsName", selTsName, envir = KTSEnv)
      showPANaggreg2()
    }
  }
  showPANaggreg2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "AGGREGATE")
    createTitle(labTitle = "Statistic")
    assign("statisType", tcltk::tclVar("median"), envir = KTSEnv)
    createRb(variable = KTSEnv$statisType, 
             dataVector = c("median", "mean", "minimum",
                            "maximum", "standard", "sum"))
    createTitle(labTitle = "Treatment of NAs")
    assign("naTreatment", tcltk::tclVar("ignore"), envir = KTSEnv)
    createRb(variable = KTSEnv$naTreatment, 
             dataVector = c("ignore", "propagate"))
    createEntry(labTitle = "Window(lags)", textVariableName = "sizeWindow")
    createEntry(labTitle = "Name", textVariableName = "newName")
    createOK(labTitle = "NEXT", action = aggregOnOk2)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  aggregOnOk2 <- function() {
    statisTypeSel <- tcltk::tclvalue(KTSEnv$statisType)
    if (tcltk::tclvalue(KTSEnv$naTreatment) == "ignore") {
      naTreatmentSel <- TRUE
    } else {
      naTreatmentSel <- FALSE
    }
    selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                  noValid = NA)
    selsizeWindow <- verifyIntEntry(tcltk::tclvalue(KTSEnv$sizeWindow), 
                                    noValid = NA)
    if (is.na(selNewName)) {
      tcltk::tkmessageBox(message = paste("Enter a name for", 
                                          "the new time series"), 
                          icon = "warning")
    } else if (is.na(selsizeWindow)) {
      tcltk::tkmessageBox(message = "Enter the window", icon = "warning")
    } else {
      assignMultiple(c("statisTypeSel", "naTreatmentSel", 
                       "selNewName", "selsizeWindow"), 
                     list(statisTypeSel, naTreatmentSel, 
                          selNewName, selsizeWindow), 
                     envir = KTSEnv)
      showPANaggreg3()
    }
  }
  showPANaggreg3 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "AGGREGATE")
    createTitle(labTitle = "First date (optional)")
    createEntry(labTitle = "Year(yyyy)", textVariableName = "cenyear")
    createEntry(labTitle = "Months", textVariableName = "cenmonth")
    createEntry(labTitle = "Days", textVariableName = "cenday")
    createEntry(labTitle = "Hours", textVariableName = "cenhour")
    createEntry(labTitle = "Minutes", textVariableName = "cenmins")
    createEntry(labTitle = "Seconds", textVariableName = "censecs")
    createOK(labTitle = "RUN", action = aggregOnOk3)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  aggregOnOk3 <- function() {
    agrupar <- function(x, type, paraagre, naTreatmentSel) {
      switch(type, 
             median = stats::aggregate(x, by = list(paraagre), 
                                       FUN = stats::median, 
                                       na.rm = naTreatmentSel), 
             mean = stats::aggregate(x, by = list(paraagre),
                                     FUN = mean, 
                                     na.rm = naTreatmentSel), 
             minimum = stats::aggregate(x, by = list(paraagre), 
                                        FUN = min, 
                                        na.rm = naTreatmentSel), 
             maximum = stats::aggregate(x, by = list(paraagre), 
                                        FUN = max, 
                                        na.rm = naTreatmentSel), 
             standard = stats::aggregate(x, by = list(paraagre), 
                                         FUN = stats::sd, 
                                         na.rm = naTreatmentSel), 
             sum = stats::aggregate(x, by = list(paraagre), 
                                    FUN = sum, 
                                    na.rm = naTreatmentSel))
    }
    createAggreTs <- function(selTs, selsizeWindow, statisTypeSel, 
                              naTreatmentSel, selNewName) {
      nseriesel <- nrow(selTs)
      numberOfWindows = ceiling(nrow(selTs)/selsizeWindow)
      paraagre <- rep(1:numberOfWindows, each = selsizeWindow)[1:nseriesel]
      newData <- agrupar(selTs$value, statisTypeSel, 
                         paraagre, naTreatmentSel)$x
      newTime <- selTs$time[seq(1, nseriesel, selsizeWindow)]
      if (length(newTime) != length(newData)) {
        newData <- newData[1:length(newTime)]
      }
      assign(selNewName, data.frame(time = newTime, value = newData), 
             envir = KTSEnv)
    }
    selDateValue <- verifyDateEntry(KTSEnv$censecs, 
                                    KTSEnv$cenmins, KTSEnv$cenhour, 
                                    KTSEnv$cenday, KTSEnv$cenmonth, 
                                    KTSEnv$cenyear)
    selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
    if (is.null(selDateValue)) {
      tcltk::tkmessageBox(message = "The introduced date is not correct", 
                          icon = "warning")
    } else if (is.na(selDateValue)) {
      createAggreTs(selTs, KTSEnv$selsizeWindow, KTSEnv$statisTypeSel, 
                    KTSEnv$naTreatmentSel, 
                    KTSEnv$selNewName)
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      showPANaggreg1()
    } else {
      iniDate <- strptime(selDateValue, format = "%Y-%m-%d %H:%M:%S", 
                          tz = KTSEnv$timeZone)
      sampPer <- diff(as.numeric(selTs$time[1:2]))
      newTime <- seq(from = iniDate, by = sampPer, length.out = nrow(selTs))
      selTs$time <- newTime
      createAggreTs(selTs, KTSEnv$selsizeWindow, 
                    KTSEnv$statisTypeSel, 
                    KTSEnv$naTreatmentSel, 
                    KTSEnv$selNewName)
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      showPANaggreg1()
    }
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANaggreg1", 
               envirName = environment(showPANaggreg1))
}

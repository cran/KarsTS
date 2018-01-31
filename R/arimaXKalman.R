arimaXKalman <-
function() {
  
  refreshArimaParam <- function() {
    KTSEnv$tsPeriod <- ""
    KTSEnv$ARorder <- ""
    KTSEnv$MAorder <- ""
    KTSEnv$seasARorder <- ""
    KTSEnv$seasMAorder <- ""
    KTSEnv$degreeDiff <- ""
    KTSEnv$sDegreeDiff <- ""
  }
  
  showPANarimaXkm1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "ARIMAX MODEL")
    if (is.null(KTSEnv$dSList$gaps) == FALSE) {
      createGapRb()
    }
    createTsRb(labTitle = "Time series to model")
    createOK(labTitle = "NEXT", action = arimaXkm1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
    
  }
  arimaXkm1OnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else {
      selTs <- get(selTsName, envir = KTSEnv)
      gapToUse <- gapForSelMethod(selTsName, selTs)
      selGap <- gapToUse$selGap
      selGapName <- gapToUse$selGapName
      nasInSelTs <- which(is.na(selTs$value))
      tmComptibility <- areTsGapTimeCompatible(selTs, selGap)
      if (length(setdiff(union(selGap$gaps, nasInSelTs), nasInSelTs)) != 0) {
        tcltk::tkmessageBox(message = paste("Some NAs in the gap set do",
                                            "not exist in the time series.",
                                            "Check that the selected gap",
                                            "set comes from the",
                                            "selected time series"), 
                            icon = "warning")
      } else if (tmComptibility[1] == FALSE) {
        tcltk::tkmessageBox(message = paste("The initial date of the time",
                                            "series and the one stored",
                                            "in the gap set do not match"), 
                            icon = "warning")
      } else if (tmComptibility[2] == FALSE) {
        tcltk::tkmessageBox(message = paste("The sampling period of",
                                            "the time series and the",
                                            "one stored in the gap",
                                            "set do not match"), 
                            icon = "warning")
      } else if (tmComptibility[3] == FALSE) {
        tcltk::tkmessageBox(message = paste("The time series is shorter",
                                            "than some indices stored",
                                            "in the set of gaps"), 
                            icon = "warning")
      } else {
        assign("selTsName", selTsName, envir = KTSEnv)
        assign("selGapName", selGapName, envir = KTSEnv)
        assign("selGap", selGap, envir = KTSEnv)
        showPANarimaXkm1B()
      }
    }
  }
  
  
  showPANarimaXkm1B <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "ARIMAX MODEL")
    createTsChb(labTitle = "Regressor time series")
    createOK(labTitle = "NEXT", action = arimaXkm1BOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
  }
  arimaXkm1BOnOk <- function() {
    tssel <- tsCheckedTF()
    if (all(tssel == FALSE)) {
      tcltk::tkmessageBox(message = paste("Choose at least a",
                                          "regressor time series"), 
                          icon = "warning")
    } else {
      
      selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
      regTsName <- KTSEnv$dSList$TS[which(tssel == TRUE)]
      nTS <- length(regTsName)
      tmComptibility <- matrix(rep(FALSE, 3 * nTS), nTS, 3)
      for (i in 1:nTS) {
        tmComptibility[i, ] <- are2TsTimeCompatible(selTs, 
                                                       get(regTsName[i], 
                                                           envir = KTSEnv))
      }
      
      if (class(tmComptibility) == "logical") {
        tmComptibility <- as.matrix(t(tmComptibility))
      }
      if (any(tmComptibility[, 1] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The initial date of all the",
                                            "time series must be the same"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 2] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The sampling period of all",
                                            "the time series",
                                            "must be the same"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 3] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The final date of all the",
                                            "time series must be the same"), 
                            icon = "warning")
        
      } else {
        
        regTS <- get(regTsName[1], envir = KTSEnv)$value
        if (nTS > 1) {
          for (i in 2:nTS) {
            regTS <- cbind(regTS, get(regTsName[i], envir = KTSEnv)$value)
          }
        }
        KTSEnv$selTs <- selTs
        KTSEnv$regTS <- regTS
        KTSEnv$regTsName <- regTsName
        showPANarimaXkm2()
      }
    }
  }
  
  showPANarimaXkm2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "ARIMAX MODEL")
    createNote(labTitle = paste("Gap set:", KTSEnv$selGapName), 
               pady = c(10,0))
    createNote(labTitle = paste("Time series:", KTSEnv$selTsName), 
               pady = c(0,10))
    createEntry(labTitle = "Period(lags)", textVariableName = "tsPeriod", 
                defaultVal = KTSEnv$tsPeriod)
    createOK(labTitle = "Suggest ARIMAX parameters", 
             action = suggestParam, width = 4 * 7)
    createEntry(labTitle = "AR order", textVariableName = "ARorder", 
                defaultVal = KTSEnv$ARorder)
    createEntry(labTitle = "Differencing degree", 
                textVariableName = "degreeDiff", 
                defaultVal = KTSEnv$degreeDiff)
    createEntry(labTitle = "MA order", textVariableName = "MAorder", 
                defaultVal = KTSEnv$MAorder)
    createEntry(labTitle = "Seasonal AR order", 
                textVariableName = "seasARorder", 
                defaultVal = KTSEnv$seasARorder)
    createEntry(labTitle = "Seasonal differencing degree", 
                textVariableName = "sDegreeDiff", 
                defaultVal = KTSEnv$sDegreeDiff)
    createEntry(labTitle = "Seasonal MA order", 
                textVariableName = "seasMAorder", 
                defaultVal = KTSEnv$seasMAorder)
    createOK(labTitle = "RUN", action = arimaXkm3OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  suggestParam <- function() {
    tsPeriod <- verifyIntEntry(tcltk::tclvalue(KTSEnv$tsPeriod), noValid = NA)
    if (is.na(tsPeriod)) {
      tcltk::tkmessageBox(message = paste("Choose the period (if",
                                          "your time series has no",
                                          "seasonality, set it to 1"), 
                          icon = "warning")
    } else {
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      resAutoArima <- try(forecast::auto.arima(stats::ts(KTSEnv$selTs$value, 
                                                         frequency = tsPeriod), 
                                               xreg = as.matrix(KTSEnv$regTS)), 
                          silent = TRUE)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      if (all(class(resAutoArima) == "try-error")) {
        assign("tsPeriod", tsPeriod, envir = KTSEnv)
        tcltk::tkmessageBox(message = paste("The ARIMAX parameters",
                                            "cannot be suggested"), 
                            icon = "warning")
        showPANarimaXkm2()
      } else {
        assign("tsPeriod", tsPeriod, envir = KTSEnv)
        assignMultiple(c("ARorder", "MAorder", "seasARorder", "seasMAorder", 
                         "degreeDiff", "sDegreeDiff"), 
                       as.list(resAutoArima$arma[c(1:4,6, 7)]), envir = KTSEnv)
        showPANarimaXkm2()
      }
    }
  }
  arimaXkm3OnOk <- function() {
    KTSEnv$tsPeriod <- verifyIntEntry(tcltk::tclvalue(KTSEnv$tsPeriod), 
                                      noValid = NA)
    KTSEnv$ARorder <- verifyIntEntry(tcltk::tclvalue(KTSEnv$ARorder), 
                                     noValid = NA)
    KTSEnv$degreeDiff <- verifyIntEntry(tcltk::tclvalue(KTSEnv$degreeDiff), 
                                        noValid = NA)
    KTSEnv$MAorder <- verifyIntEntry(tcltk::tclvalue(KTSEnv$MAorder), 
                                     noValid = NA)
    KTSEnv$seasARorder <- verifyIntEntry(tcltk::tclvalue(KTSEnv$seasARorder), 
                                         noValid = NA)
    KTSEnv$sDegreeDiff <- verifyIntEntry(tcltk::tclvalue(KTSEnv$sDegreeDiff), 
                                         noValid = NA)
    KTSEnv$seasMAorder <- verifyIntEntry(tcltk::tclvalue(KTSEnv$seasMAorder), 
                                         noValid = NA)
    if (is.na(KTSEnv$tsPeriod)) {
      tcltk::tkmessageBox(message = paste("Enter the period (if your time",
                                          "series is not seasonal,",
                                          "set it to 1"), 
                          icon = "warning")
    } else if (is.na(KTSEnv$ARorder)) {
      tcltk::tkmessageBox(message = "Enter the AR order", icon = "warning")
    } else if (is.na(KTSEnv$degreeDiff)) {
      tcltk::tkmessageBox(message = paste("Enter the degree of differencing"),
                          icon = "warning")
    } else if (is.na(KTSEnv$MAorder)) {
      tcltk::tkmessageBox(message = "Enter the MA order", icon = "warning")
    } else if (is.na(KTSEnv$seasARorder)) {
      tcltk::tkmessageBox(message = paste("Enter the seasonal AR order"), 
                          icon = "warning")
    } else if (is.na(KTSEnv$sDegreeDiff)) {
      tcltk::tkmessageBox(message = paste("Enter the seasonal",
                                          "degree of differencing"), 
                          icon = "warning")
    } else if (is.na(KTSEnv$seasMAorder)) {
      tcltk::tkmessageBox(message = "Enter the seasonal MA order", 
                          icon = "warning")
    } else {
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      timeSer <- stats::ts(KTSEnv$selTs$value, frequency = KTSEnv$tsPeriod)
      order = c(KTSEnv$ARorder, KTSEnv$degreeDiff, KTSEnv$MAorder)
      seasonal = c(KTSEnv$seasARorder, KTSEnv$sDegreeDiff, KTSEnv$seasMAorder)
      arimaXModel <- try(forecast::Arima(timeSer, order = order, 
                                         seasonal = seasonal, 
                                         include.drift = TRUE, 
                                         xreg = as.matrix(KTSEnv$regTS)), 
                         silent = TRUE)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      if (all(class(arimaXModel) == "try-error")) {
        tcltk::tkmessageBox(message = paste("The ARIMAX model",
                                            "couldn't be fitted"), 
                            icon = "warning")
      } else {
        writeAModelInfo <- function(tsPeriod, ARorder, degreeDiff, MAorder, 
                                    seasARorder, sDegreeDiff, 
                                    seasMAorder, regTsName) {
          if (tsPeriod == 1) {
            txtPeriod <- paste("Non seasonal")
          } else {
            txtPeriod <- paste("Period:", tsPeriod, "(lags)")
          }
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtPeriod, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", "\n")
          txtParam <- paste("(", ARorder, degreeDiff, MAorder, 
                            ")x(", seasARorder, 
                            sDegreeDiff, seasMAorder, ")")
          txtModel <- paste("Model:", txtParam)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", txtModel)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", "\n")
          txtReg <- paste("Regressor time series:", 
                          paste(regTsName, collapse = ","))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", txtReg)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", "\n")
        }
        if (all(is.na(KTSEnv$selTs$value) == FALSE)) {
          writeMethodTitle("ARIMAX MODEL WITH KALMAN FILTER")
          txtWarning <- "The time series had no NAs"
          tcltk::tkinsert(KTSEnv$txtWidget, "end", txtWarning)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          writeAModelInfo(KTSEnv$tsPeriod, KTSEnv$ARorder, KTSEnv$degreeDiff, 
                          KTSEnv$MAorder, KTSEnv$seasARorder, 
                          KTSEnv$sDegreeDiff, KTSEnv$seasMAorder, 
                          KTSEnv$regTsName)
          endingLines()
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          refreshArimaParam()
          showPANarimaXkm1()
        } else {
          fillGapsKal <- function(timSer, arimaXModel) {
            kr <- stats::KalmanSmooth(timSer$value, arimaXModel$model)
            tmp <- which(arimaXModel$model$Z == 1)
            id <- ifelse(length(tmp) == 1, tmp[1], tmp[2])
            id.na <- which(is.na(timSer$value))
            timSer$value[id.na] <- kr$smooth[id.na, id]
            timSer
          }
          filledTS <- fillGapsKal(KTSEnv$selTs, arimaXModel)
          gapsAfterFill <- getGapsAfterFill(filledTS, 
                                            KTSEnv$selGap, envir = KTSEnv)
          remainingNAsInGap <- gapsAfterFill$remainingNAsInGap
          filledNasTable <- gapsAfterFill$filledNasTable
          filledTS$value[setdiff(which(is.na(KTSEnv$selTs$value)), 
                                 KTSEnv$selGap$gaps)] <- NA
          assign(paste0(KTSEnv$selTsName, "_", KTSEnv$selGapName, "_ark"), 
                 filledTS, envir = KTSEnv)
          
          writeMethodTitle("ARIMAX MODEL WITH KALMAN FILTER")
          writeAModelInfo(KTSEnv$tsPeriod, KTSEnv$ARorder, KTSEnv$degreeDiff, 
                          KTSEnv$MAorder, KTSEnv$seasARorder, 
                          KTSEnv$sDegreeDiff, KTSEnv$seasMAorder, 
                          KTSEnv$regTsName)
          writeMethodSummary(filledNasTable, remainingNAsInGap, 
                             KTSEnv$selTsName, 
                             KTSEnv$selGapName, KTSEnv$selGap)
          endingLines()
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          refreshArimaParam()
          showPANarimaXkm1()
        }
      }
    }
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  refreshArimaParam()
  checkIfAnyTs(action = "showPANarimaXkm1", 
               envirName = environment(showPANarimaXkm1))
}

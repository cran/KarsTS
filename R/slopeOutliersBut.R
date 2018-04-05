slopeOutliersBut <-
function() {
  showPANrmSloOut1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "RM SLOPE OUTLIERS")
    createTitle(labTitle = "Remove outliers from filling")
    KTSEnv$rmFromFilling <- tcltk::tclVar("No")
    createEachRb(labTitle = "Yes", variable = KTSEnv$rmFromFilling)
    createEachRb(labTitle = "No", variable = KTSEnv$rmFromFilling)
    createTsRb(labTitle = "Time series", variableName = "selTsP0")
    createOK(labTitle = "NEXT", action = rmSloOut1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  rmSloOut1OnOk <- function() {
    selTSName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP0), 
                                 noValid = NA)
    rmFromFill <- tcltk::tclvalue(KTSEnv$rmFromFilling)
    if (is.na(selTSName)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    } else {
      KTSEnv$selTSName <- selTSName
      KTSEnv$rmFromFill <- rmFromFill
      showPANrmSloOut2()
    }
  }
  showPANrmSloOut2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "RM SLOPE OUTLIERS")
    createTITLE(labTitle = "Slope reference")
    createNote(labTitle = "Choose a time series or enter the maxima")
    createTsRb(labTitle = "Reference time series")
    createEntry(labTitle = "Maximum positive slope", 
                textVariableName = "MPS")
    createEntry(labTitle = "Maximum negative slope", 
                textVariableName = "MNS")
    createOK(labTitle = "RUN", action = rmSloOut2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  rmSloOut2OnOk <- function() {
    refTSName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                 noValid = NA)
    
    maxPslo <- verifyRealEntry(tcltk::tclvalue(KTSEnv$MPS), 
                               noValid = NA)
    maxNslo <- verifyRealEntry(tcltk::tclvalue(KTSEnv$MNS), 
                               noValid = NA)
    if (is.na(refTSName) & KTSEnv$rmFromFill == "Yes") {
      tcltk::tkmessageBox(message = paste("To remove slopes", 
                                          "from a filling,", 
                                          "the reference time series must", 
                                          "be the not-filled time series.", 
                                          "It is not possible", 
                                          "to enter the maxima."), 
                          icon = "warning")
      
    } else if (is.null(refTSName) & is.null(maxPslo)) {
      tcltk::tkmessageBox(message = paste("Choose a reference time",
                                          "series or enter", 
                                          "the maximum slope"), 
                          icon = "warning")
    } else if (is.na(refTSName) & is.na(maxPslo)) {
      tcltk::tkmessageBox(message = paste("Choose a reference time",
                                          "series or enter", 
                                          "the minimum slope"), 
                          icon = "warning")
    } else if (is.na(refTSName) == FALSE & is.na(maxPslo) == FALSE) {
      tcltk::tkmessageBox(message = paste("Provide either a reference", 
                                          "time series or a maximum", 
                                          "positive slope"), 
                          icon = "warning")
    } else if (is.na(refTSName) == FALSE & is.na(maxNslo) == FALSE) {
      tcltk::tkmessageBox(message = paste("Provide either a reference", 
                                          "time series or a maximum", 
                                          "negative slope"), 
                          icon = "warning")
    } else {
      
      selTS <- get(KTSEnv$selTSName, envir = KTSEnv)
      if (is.na(refTSName) == FALSE) {
        refTS <- get(refTSName, envir = KTSEnv)
      }
      if (is.na(maxPslo)) {
        maxPslo <- getMaxPosSlope(refTS$value)
      }
      if (is.na(maxNslo)) {
        maxNslo <- getMaxNegSlope(refTS$value)
      }
      if (KTSEnv$rmFromFill == "Yes") {
        refTS <- get(refTSName, envir = KTSEnv)
        filling <- setdiff(which(is.na(refTS$value)), 
                           which(is.na(selTS$value)))
        timecompatibility <- are2TsTimeCompatible(selTS, refTS)
        if (length(filling) == 0) {
          tcltk::tkmessageBox(message = "The filling is empty", 
                              icon = "warning")
        } else if (timecompatibility[1] == FALSE) {
          tcltk::tkmessageBox(message = paste("The filled and the", 
                                              "non-filled time series",
                                              "must have the same", 
                                              "initial time"), 
                              icon = "warning")
        } else if (timecompatibility[2] == FALSE) {
          tcltk::tkmessageBox(message = paste("The filled and the", 
                                              "non-filled time series", 
                                              "must have the same",
                                              "sampling period"), 
                              icon = "warning")
        } else if (timecompatibility[3] == FALSE) {
          tcltk::tkmessageBox(message = paste("The filled and the", 
                                              "non-filled time", 
                                              "series must have", 
                                              "the same final time"), 
                              icon = "warning")
        } else {
          
          newTS <- rmSlopeOutliers(tS = selTS, 
                                   origMxPosSlope = maxPslo, 
                                   origMxNegSlope = maxNslo, 
                                   filling = filling)
          changed <- newTS$value - selTS$value
          condi1 <- any(changed[which(is.finite(changed))] != 0)
          condi2 <- length(setdiff(which(is.na(changed)), 
                                   which(is.na(refTS$value)))) > 0
          
          if (condi1 == FALSE & condi2 == FALSE) {
            tcltk::tkmessageBox(message = "There were no changes", 
                                icon = "warning")
          } else {
            assign(paste0(KTSEnv$selTSName, "NSO"), 
                   newTS, envir = KTSEnv)
          }
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANrmSloOut1()
        }
      } else {
        newTS <- rmSlopeOutliers(tS = selTS, origMxPosSlope = maxPslo,
                                 origMxNegSlope = maxNslo)
        
        changed <- newTS$value - selTS$value
        condi1 <- any(changed[which(is.finite(changed))] != 0)
        if (is.na(refTSName)) {
          condi2 <- FALSE
        } else {
          condi2 <- length(setdiff(which(is.na(changed)), 
                                   which(is.na(refTS$value)))) > 0
        }
        if (condi1 == FALSE & condi2 == FALSE) {
          tcltk::tkmessageBox(message = "There were no changes", 
                              icon = "warning")
        } else {
          assign(paste0(KTSEnv$selTSName, "NSO"), 
                 newTS, envir = KTSEnv)
        }
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANrmSloOut1()
      }
    }
  }
  
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANrmSloOut1", 
               envirName = environment(showPANrmSloOut1))
}

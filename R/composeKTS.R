composeKTS <-
function() {
  showPANcompo1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "OPERATIONS")
    createTsChb()
    createOK(labTitle = "NEXT", action = compo1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  compo1OnOk <- function() {
    tssel <- tsCheckedTF()
    if (all(tssel == FALSE)) {
      tcltk::tkmessageBox(message = paste("Choose at least",
                                          "a time series"), 
                          icon = "warning")
    } else {
      assign("tsToCompose", KTSEnv$dSList$TS[which(tssel == TRUE)], 
             envir = KTSEnv)
      showPANcompo2()
    }
  }
  showPANcompo2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "OPERATIONS")
    assign("operator", tcltk::tclVar("Add"), envir = KTSEnv)
    createEachRb(labTitle = "Add", variable = KTSEnv$operator)
    createEntry(labTitle = "     Name", textVariableName = "addNewName")
    createEachRb(labTitle = "Multiply", variable = KTSEnv$operator)
    createEntry(labTitle = "     Name", textVariableName = "multNewName")
    createEachRb(labTitle = "Opposite", variable = KTSEnv$operator)
    createEachRb(labTitle = "Reciprocal", variable = KTSEnv$operator)
    createEachRb(labTitle = "Ln", variable = KTSEnv$operator)
    createOK(labTitle = "RUN", action = compo2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  compo2OnOk <- function() {
    selNewNameAdd <- verifyCharEntry(tcltk::tclvalue(KTSEnv$addNewName), 
                                     noValid = NA)
    selNewNameMul <- verifyCharEntry(tcltk::tclvalue(KTSEnv$multNewName), 
                                     noValid = NA)
    selOper <- tcltk::tclvalue(KTSEnv$operator)
    if (selOper == "Add" & is.na(selNewNameAdd)) {
      tcltk::tkmessageBox(message = paste("Enter a name for",
                                          "the composed time series"),
                          icon = "warning")
    } else if (selOper == "Multiply" & is.na(selNewNameMul)) {
      tcltk::tkmessageBox(message = paste("Enter a name for",
                                          "the composed time series"),
                          icon = "warning")
    } else {
      ntsToCompose <- length(KTSEnv$tsToCompose)
      if (selOper == "Add" | selOper == "Multiply") {
        areAllTsCompatible <- function(ntsToCompose, tsToCompose) {
          if (ntsToCompose == 1) {
            tmComptibility <- matrix(rep(TRUE, 3), 1, 3)
          } else if (ntsToCompose == 2) {
            tmComptibility <- are2TsTimeCompatible(get(tsToCompose[1], 
                                                          envir = KTSEnv), 
                                                      get(tsToCompose[2], 
                                                          envir = KTSEnv))
            tmComptibility <- t(as.matrix(tmComptibility))
          } else {
            firstTS <- get(tsToCompose[1], envir = KTSEnv)
            tmComptibility <- matrix(rep(FALSE, 3 * ntsToCompose), 
                                        ntsToCompose, 3)
            for (i in 2:ntsToCompose) {
              tmComptibility[i, ] <- are2TsTimeCompatible(firstTS, 
                                                          get(tsToCompose[i], 
                                                              envir = KTSEnv))
            }
            tmComptibility <- tmComptibility[-1, ]
            if (is.vector(tmComptibility)) {
              tmComptibility <- as.matrix(t(tmComptibility))
            }
          }
          tmComptibility
        }
        if (selOper == "Add") {
          selNewName <- selNewNameAdd
        } else if (selOper == "Multiply") {
          selNewName <- selNewNameMul
        }
        tmComptibility <- areAllTsCompatible(ntsToCompose, 
                                                KTSEnv$tsToCompose)
        if (any(tmComptibility[, 1] == FALSE)) {
          tcltk::tkmessageBox(message = paste("The initial date of",
                                              "all the time series",
                                              "must be the same"),
                              icon = "warning")
        } else if (any(tmComptibility[, 2] == FALSE)) {
          tcltk::tkmessageBox(message = paste("The sampling period of",
                                              "all the time series",
                                              "must be the same"),
                              icon = "warning")
        } else if (any(tmComptibility[, 3] == FALSE)) {
          tcltk::tkmessageBox(message = paste("All time series must",
                                              "have the same length"),
                              icon = "warning")
        } else if (ntsToCompose < 2) {
          tcltk::tkmessageBox(message = paste("In order to add or",
                                              "multiply, two time",
                                              "series are need",
                                              "at least"),
                              icon = "warning")
        } else {
          buildTsMatrix <- function(tsToCompose) {
            matrixTs <- NULL
            for (i in tsToCompose) {
              matrixTs <- cbind(matrixTs, get(i, envir = KTSEnv)$value)
            }
            matrixTs
          }
          addTs <- function(matrixTs) {
            composedData <- rowSums(matrixTs)
          }
          multTs <- function(matrixTs) {
            composedData <- apply(matrixTs, 1, prod)
          }
          matrixTs <- buildTsMatrix(KTSEnv$tsToCompose)
          if (selOper == "Add") {
            composedData <- addTs(matrixTs)
          } else {
            composedData <- multTs(matrixTs)
          }
          if (any(is.finite(composedData))) {
            assign(selNewName, 
                   data.frame(time = get(KTSEnv$tsToCompose[1], 
                                         envir = KTSEnv)$time, 
                              value = composedData), 
                   envir = KTSEnv)
          }
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANcompo1()
        }
      } else {
        oppoTs <- function(tsToCompose) {
          apply(as.matrix(tsToCompose), 1, function(X) {
            assign(paste0(X, "Opp"), 
                   data.frame(time = get(X, envir = KTSEnv)$time, 
                              value = -(get(X, envir = KTSEnv)$value)), 
                   envir = KTSEnv)
          })
        }
        recipTs <- function(tsToCompose) {
          apply(as.matrix(tsToCompose), 1, function(X) {
            assign(paste0(X, "Recip"), 
                   data.frame(time = get(X, envir = KTSEnv)$time,
                              value = 1/(get(X, envir = KTSEnv)$value)), 
                   envir = KTSEnv)
          })
        }
        lnTs <- function(tsToCompose) {
          apply(as.matrix(tsToCompose), 1, function(X) {
            dataTS <- get(X, envir = KTSEnv)$value
            if (any(dataTS[union(which(is.na(dataTS) == FALSE), 
                                 which(is.na(dataTS) == FALSE))] <= 0)) {
              tcltk::tkmessageBox(message = paste(X, "has negative or zero",
                                                  "values, so the logarithms",
                                                  "couldn't be taken"), 
                                  icon = "warning")
            } else {
              assign(paste0(X, "Ln"), 
                     data.frame(time = get(X, envir = KTSEnv)$time, 
                                value = log(get(X, envir = KTSEnv)$value)), 
                     envir = KTSEnv)
            }
          })
        }
        if (selOper == "Opposite") {
          oppoTs(KTSEnv$tsToCompose)
        } else if (selOper == "Reciprocal") {
          recipTs(KTSEnv$tsToCompose)
        } else {
          lnTs(KTSEnv$tsToCompose)
        }
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANcompo1()
      }
    }
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANcompo1", 
               envirName = environment(showPANcompo1))
}

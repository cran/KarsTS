createCrossRM <-
function() {
  showPANCRM1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "CROSS RECURRENCE MATRIX")
    createTitle(labTitle = "First time series")
    assign("selTsP1", tcltk::tclVar("0"), envir = KTSEnv)
    createRb(variable = KTSEnv$selTsP1, dataVector = KTSEnv$dSList$TS)
    createOK(labTitle = "NEXT", action = CRM1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  CRM1OnOk <- function() {
    selTs1Name <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP1), 
                                  noValid = NA)
    if (is.na(selTs1Name)) {
      tcltk::tkmessageBox(message = "Choose the first time series", 
                          icon = "warning")
    } else {
      assign("selTs1Name", selTs1Name, envir = KTSEnv)
      showPANCRM1b()
    }
  }
  showPANCRM1b <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "CROSS RECURRENCE MATRIX")
    createTitle(labTitle = "Second time series")
    selTsP2 <- tcltk::tclVar("0")
    assign("selTsP2", selTsP2, envir = KTSEnv)
    createRb(variable = KTSEnv$selTsP2, dataVector = KTSEnv$dSList$TS)
    createOK(labTitle = "NEXT", action = CRM1bOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  CRM1bOnOk <- function() {
    selTs2Name <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP2), 
                                  noValid = NA)
    if (is.na(selTs2Name)) {
      tcltk::tkmessageBox(message = "Choose the second time series", 
                          icon = "warning")
    } else {
      assign("selTs2Name", selTs2Name, envir = KTSEnv)
      showPANCRM2()
    }
  }
  showPANCRM2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "CROSS RECURRENCE MATRIX")
    createNote(labTitle = paste("Time series 1:", KTSEnv$selTs1Name))
    createNote(labTitle = paste("Time series 2:", KTSEnv$selTs2Name))
    createTitle(labTitle = "Parameters")
    createEntry(labTitle = "Embedding dimension", 
                textVariableName = "embDim", 
                defaultVal = "1")
    createEntry(labTitle = "Delay", textVariableName = "lagDel", 
                defaultVal = "0")
    createEntry(labTitle = "Tolerance", textVariableName = "thresh")
    createEntry(labTitle = "Name", textVariableName = "newName")
    createOK(labTitle = "RUN", action = CRM2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  CRM2OnOk <- function() {
    embedDim <- verifyIntEntry(tcltk::tclvalue(KTSEnv$embDim), noValid = NA)
    lagDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lagDel), noValid = NA)
    threshold <- verifyRealEntry(tcltk::tclvalue(KTSEnv$thresh), 
                                 noValid = NA)
    selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                  noValid = NA)
    if (is.na(embedDim)) {
      tcltk::tkmessageBox(message = "Choose an embedding dimension", 
                          icon = "warning")
    } else if (is.na(lagDelay)) {
      tcltk::tkmessageBox(message = "Choose a delay", icon = "warning")
    } else if (embedDim > 1 & lagDelay == 0) {
      tcltk::tkmessageBox(message = paste("For embedding dimesions",
                                          "greater than 1, the delay",
                                          "must be greater than 0"), 
                          icon = "warning")
    }else if (is.na(threshold)){
      tcltk::tkmessageBox(message = "Choose a tolerance", icon = "warning")
    }else if (is.na(selNewName)){
      tcltk::tkmessageBox(message = paste("Enter a name for",
                                          "the recurrence matrix "), 
                                          icon = "warning")
    }else{
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      selTs1 <- get(KTSEnv$selTs1Name, envir = KTSEnv)
      selTs2 <- get(KTSEnv$selTs2Name, envir = KTSEnv)
      timecompatibility <- are2TsTimeCompatible(selTs1, selTs2)
      if (timecompatibility[2] == FALSE) {
        tcltk::tkmessageBox(message = paste("Both time series must",
                                            "have the same sampling period"), 
                            icon = "warning")
      }else{
        if (timecompatibility[1] == FALSE) {
          lagTS <- diff(as.numeric(selTs1$time[1], selTs2$time[1]))
          tcltk::tkmessageBox(message = paste("There is a lag of ", lagTS, 
                                              "seconds between the time",
                                              "series. Take it into",
                                              "consideration"), 
                              icon = "warning")
        }
        if (embedDim == 1) {
          dataTS1 <- matrix(selTs1$value)
          dataTS2 <- matrix(selTs2$value)
          lagDelay <- 0
          infNorm <- function(v1, v2) {
            rfimp <- abs(v2 - v1)
          }
        } else {
          dataTS1 <- tseriesChaos::embedd(selTs1$value, 
                                          m = embedDim, d = lagDelay)
          dataTS2 <- tseriesChaos::embedd(selTs2$value, 
                                          m = embedDim, d = lagDelay)
          infNorm <- function(v1, v2) {
            lv2 <- NROW(v2)
            rfimp <- apply(abs(v2 - matrix(rep(v1, each = lv2),
                                           lv2, embedDim)), 
                           1, FUN = max, na.rm = TRUE)
          }
        }
        ldataTS1 <- nrow(dataTS1)
        ldataTS2 <- nrow(dataTS2)
        cont <- 0
        coincidX <- rep(0, 1e+06)
        coincidY <- rep(0, 1e+06)
        for (i in 1:ldataTS1) {
          distanceP <- infNorm(dataTS1[i, ], dataTS2)
          distanceP[which(is.infinite(distanceP))] <- Inf
          recPointsInd <- which(distanceP < threshold)
          nRecPoints = length(recPointsInd)
          if (nRecPoints >= 1) {
            coincidX[(cont + 1):(cont + nRecPoints)] <- i
            coincidY[(cont + 1):(cont + nRecPoints)] <- recPointsInd
            cont = cont + length(recPointsInd)
          }
          rm(distanceP, recPointsInd, nRecPoints)
        }
        remnantRows <- which(coincidX == 0)
        lRemnantRows <- length(remnantRows)
        if (lRemnantRows == length(coincidX)) {
          tcltk::tkmessageBox(message = paste("No recurrence point",
                                              "was found"), 
                              icon = "warning")
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANCRM1()
        } else {
          if (length(remnantRows) >= 1) {
            usefulRows <- remnantRows[1] - 1
            coincidX = coincidX[1:usefulRows]
            coincidY = coincidY[1:usefulRows]
          }
          recurrPoints <- as.data.frame(cbind(coincidX, coincidY))
          colnames(recurrPoints) <- c("X", "Y")
          lengthSeries <- c(nrow(selTs1), nrow(selTs2))
          samperSeries <- c(diff(as.numeric(selTs1$time[1:2])), 
                            diff(as.numeric(selTs2$time[1:2])))
          iniSeries <- c(as.character(selTs1$time[1]), 
                         as.character(selTs2$time[1]))
          recMatrix <- list(ones = recurrPoints, tol = rep(threshold, 2), 
                            tsName = c(KTSEnv$selTs1Name, KTSEnv$selTs2Name), 
                            embDim = rep(embedDim, 2), 
                            delay = rep(lagDelay, 2), 
                            dist = rep("inf_norm", 2), 
                            tsLength = lengthSeries, 
                            samPerSec = samperSeries, 
                            tsIni = iniSeries, type = "cross")
          assign(selNewName, recMatrix, envir = KTSEnv)
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANCRM1()
        }
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
  checkIfAnyTs(action = "showPANCRM1", envirName = environment(showPANCRM1))
}

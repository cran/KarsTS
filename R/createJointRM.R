createJointRM <-
function() {
  jrm1OnOk <- function() {
    selRm1Name <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP1), 
                                  noValid = NA)
    if (is.na(selRm1Name)) {
      tcltk::tkmessageBox(message = paste("Choose the first",
                                          "recurrence matrix"), 
                          icon = "warning")
    } else {
      assign("selRm1Name", selRm1Name, envir = KTSEnv)
      showPANjrm2()
    }
  }
  jrm2OnOk <- function() {
    selRm2Name <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP2), 
                                  noValid = NA)
    selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                  noValid = NA)
    if (is.na(selRm2Name)) {
      tcltk::tkmessageBox(message = paste("Choose the second",
                                          "recurrence matrix"), 
                          icon = "warning")
    } else if (is.na(selNewName)) {
      tcltk::tkmessageBox(message = paste("Enter a name for the",
                                          "recurrence matrix "), 
                          icon = "warning")
    } else {
      selRm1 <- get(KTSEnv$selRm1Name, envir = KTSEnv)
      selRm2 <- get(selRm2Name, envir = KTSEnv)
      if (selRm1$samPerSec[1] != selRm2$samPerSec[1]) {
        tcltk::tkmessageBox(message = paste("The time series from",
                                            "wich the recurrence",
                                            "matrices come must have",
                                            "the same sampling period"), 
                            icon = "warning")
      } else if (selRm1$type == "cross") {
        tcltk::tkmessageBox(message = paste("The first recurrence matrix",
                                            "must be simple or joint"), 
                            icon = "warning")
      } else if (selRm2$type == "cross") {
        tcltk::tkmessageBox(message = paste("The second recurrence matrix",
                                            "must be simple or joint"), 
                            icon = "warning")
      } else {
        rmIndFromRM <- function(indices, recMat, axis = "xAxis") {
          if (length(indices) > 0) {
            indices <- sort(indices)
            if (axis == "xAxis") {
              for (X in indices) {
                aa <- which(recMat$ones$X == X)
                recMat$ones <- recMat$ones[-aa, ]
                rownames(recMat$ones) <- NULL
                rm(aa)
              }
            } else if (axis == "yAxis") {
              for (X in indices) {
                aa <- which(recMat$ones$Y == X)
                recMat$ones <- recMat$ones[-aa, ]
                rownames(recMat$ones) <- NULL
                rm(aa)
              }
            }
          }
          recMat
        }
        initialTimes <- c(selRm1$tsIni, selRm2$tsIni)
        initialTimes <- getDelayCharTimes(initialTimes)
        minIniTime <- initialTimes[1]
        maxIniTime <- initialTimes[2]
        if (minIniTime != maxIniTime) {
          tcltk::tkmessageBox(message = paste("There is a lag of ", 
                                              diff(c(minIniTime,maxIniTime)), 
                                              "seconds between two of",
                                              "the original time series.",
                                              "Take it into consideration"), 
                              icon = "warning")
        }
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        commonX <- intersect(unique(selRm1$ones$X), unique(selRm2$ones$X))
        commonY <- intersect(unique(selRm1$ones$Y), unique(selRm2$ones$Y))
        notCommonMR1X <- setdiff(unique(selRm1$ones$X), commonX)
        selRm1 <- rmIndFromRM(indices = notCommonMR1X, 
                              recMat = selRm1, axis = "xAxis")
        notCommonMR1Y <- setdiff(unique(selRm1$ones$Y), commonY)
        selRm1 <- rmIndFromRM(indices = notCommonMR1Y, 
                              recMat = selRm1, axis = "yAxis")
        notCommonMR2X <- setdiff(unique(selRm2$ones$X), commonX)
        selRm2 <- rmIndFromRM(indices = notCommonMR2X, 
                              recMat = selRm2, axis = "xAxis")
        notCommonMR2Y <- setdiff(unique(selRm2$ones$Y), commonY)
        selRm2 <- rmIndFromRM(indices = notCommonMR2Y, 
                              recMat = selRm2, axis = "yAxis")
        if (nrow(selRm1$ones) == 0 | nrow(selRm2$ones) == 0) {
          tcltk::tkmessageBox(message = paste("The joint recurrence",
                                              "matrix is empty"), 
                              icon = "warning")
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          showPANjrm1()
        } else {
          indToRevise <- unique(selRm1$ones$X)
          cont <- 0
          coindicX <- rep(0, 1e+06)
          coindicY <- rep(0, 1e+06)
          for (i in indToRevise) {
            jointRecY <- intersect(selRm1$ones$Y[which(selRm1$ones$X == i)], 
                                   selRm2$ones$Y[which(selRm2$ones$X == i)])
            nJointRecPoints <- length(jointRecY)
            if (nJointRecPoints >= 1) {
              coindicX[(cont + 1):(cont + nJointRecPoints)] <- i
              coindicY[(cont + 1):(cont + nJointRecPoints)] <- jointRecY
              cont = cont + nJointRecPoints
            }
            rm(jointRecY, nJointRecPoints)
          }
          remnantRows <- which(coindicX == 0)
          if (length(remnantRows) == length(coindicX)) {
            tcltk::tkmessageBox(message = paste("The joint recurrence",
                                                "matrix is empty"), 
                                icon = "warning")
            cleanEnvir()
            refreshDataSetsList(outp = FALSE)
            tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
            showPANjrm1()
          } else {
            if (length(remnantRows) > 0) {
              usefulRows <- remnantRows[1] - 1
              coindicX = coindicX[1:usefulRows]
              coindicY = coindicY[1:usefulRows]
            }
            jointRecPoints <- as.data.frame(cbind(coindicX, coindicY))
            colnames(jointRecPoints) <- c("X", "Y")
            rownames(jointRecPoints) <- NULL
            jRecMat <- list(ones = jointRecPoints, 
                            tol = c(selRm1$tol, selRm2$tol), 
                            tsName = c(selRm1$tsName, selRm2$tsName), 
                            embDim = c(selRm1$embDim, selRm2$embDim), 
                            delay = c(selRm1$delay, selRm2$delay), 
                            dist = c(selRm1$dist, selRm2$dist), 
                            tsLength = c(selRm1$tsLength, selRm2$tsLength), 
                            samPerSec = c(selRm1$samPerSec, selRm2$samPerSec), 
                            tsIni = c(selRm1$tsIni, selRm2$tsIni), 
                            type = "joint")
            assign(selNewName, jRecMat, envir = KTSEnv)
            cleanEnvir()
            refreshDataSetsList(outp = FALSE)
            tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
            showPANjrm1()
          }
        }
      }
    }
  }
  showPANjrm1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "JOINT RECURRENCE MATRIX")
    createTitle(labTitle = "First recurrence matrix")
    selRmP1 <- tcltk::tclVar("0")
    assign("selRmP1", selRmP1, envir = KTSEnv)
    createRb(variable = KTSEnv$selRmP1, dataVector = KTSEnv$dSList$rm)
    createOK(labTitle = "NEXT", action = jrm1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  showPANjrm2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "JOINT RECURRENCE MATRIX")
    createTitle(labTitle = "Second recurrence matrix")
    selRmP2 <- tcltk::tclVar("0")
    assign("selRmP2", selRmP2, envir = KTSEnv)
    createRb(variable = KTSEnv$selRmP2, dataVector = KTSEnv$dSList$rm)
    createEntry(labTitle = "Name", textVariableName = "newName")
    createOK(labTitle = "RUN", action = jrm2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  tcltk::tkmessageBox(message = paste("The process can take some minutes",
                                      "if your series is huge.It can even",
                                      "collapse:save everything you need to",
                                      "save before using this function.",
                                      "Save your recurrence matrix",
                                      "to the hard disk"), 
                      icon = "warning")
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyRm(action = "showPANjrm1", envirName = environment(showPANjrm1))
}

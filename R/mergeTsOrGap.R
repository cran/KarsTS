mergeTsOrGap <-
function() {
  mergeOnOk <- function() {
    refreshDataSetsList(outp = FALSE)
    if (is.null(KTSEnv$dSList$nTS) == FALSE) {
      newNameTs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                   noValid = NA)
      if (is.na(newNameTs)) {
        newNameTs <- "mergedTS"
      }
      tssel <- tsCheckedTF()
      tsToMergeInd <- which(tssel == TRUE)
      if (length(tsToMergeInd) > 1) {
        tsToMerge <- KTSEnv$dSList$TS[tsToMergeInd]
        lTTM <- length(tsToMerge)
        iniTimes <- vapply(tsToMerge, function(X) {
          series <- get(X, envir = KTSEnv)
          init <- as.numeric(series$time[1])
        }, FUN.VALUE = 1)
        finalTimes <- vapply(tsToMerge, function(X) {
          series <- get(X, envir = KTSEnv)
          fint <- as.numeric(series$time[nrow(series)])
        }, FUN.VALUE = 1)
        sampPerPeriods <- vapply(tsToMerge, function(X) {
          series <- get(X, envir = KTSEnv)
          samp <- diff(as.numeric(series$time[1:2]))
        }, FUN.VALUE = 1)
        rightOrder <- sort(iniTimes, index.return = TRUE)$ix
        tsToMergeS <- tsToMerge[rightOrder]
        finalTimesS <- finalTimes[rightOrder]
        iniTimesS <- iniTimes[rightOrder]
        dooverlap <- iniTimesS[2:lTTM] - finalTimesS[1:(lTTM - 1)]
        lengthGapsBetw <- dooverlap/sampPerPeriods[1] - 1
        isMultiple <- lengthGapsBetw%%1
        if (any(sampPerPeriods != sampPerPeriods[1])) {
          tcltk::tkmessageBox(message = paste("All series must have",
                                              "the same sampling period",
                                              "to be merged"), 
                              icon = "warning")
        } else if (any(dooverlap <= 0)) {
          tcltk::tkmessageBox(message = "Some series overlap", 
                              icon = "warning")
        } else if (any(isMultiple != 0)) {
          tcltk::tkmessageBox(message = paste("The gaps between series",
                                              "are not multiples of",
                                              "the sampling period"), 
                              icon = "warning")
        } else {
          firstDate <- get(tsToMergeS[1], envir = KTSEnv)[1, 1]
          lastSeries <- get(tsToMergeS[lTTM], envir = KTSEnv)
          lastDate <- lastSeries[length(lastSeries$value), 1]
          timeMerged <- seq(firstDate, lastDate, sampPerPeriods[1])
          dataMerged <- rep(NA, length(timeMerged))
          for (i in tsToMergeS) {
            series <- get(i, envir = KTSEnv)
            lseriei <- length(series$value)
            ind1 <- which(as.numeric(timeMerged) == as.numeric(series$time[1]))
            dataMerged[ind1:(ind1 + lseriei - 1)] <- series$value
            rm(series, lseriei, ind1)
          }
          mergedTS <- data.frame(time = timeMerged, value = dataMerged)
          assign(newNameTs, mergedTS, KTSEnv)
        }
      }
    }
    if (is.null(KTSEnv$dSList$nGaps) == FALSE) {
      newNameGap <- verifyCharEntry(tcltk::tclvalue(KTSEnv$nnmgap), 
                                    noValid = NA)
      if (is.na(newNameGap)) {
        newNameGap <- "mergedGap"
      }
      gsel <- gapCheckedTF()
      gapsToMergeInd <- which(gsel == TRUE)
      if (length(gapsToMergeInd) > 0) {
        gapsToMerge <- KTSEnv$dSList$gaps[gapsToMergeInd]
        lGTM <- length(gapsToMerge)
        iniTimes <- vapply(gapsToMerge, function(X) {
          gap <- get(X, envir = KTSEnv)
          init <- strptime(gap$tsIni, format = "%Y-%m-%d %H:%M:%S", 
                           tz = KTSEnv$timeZone)
          as.numeric(init)
        }, FUN.VALUE = 1)
        sampPerPeriods <- vapply(gapsToMerge, function(X) {
          gap <- get(X, envir = KTSEnv)
          gap$samPerMin
        }, FUN.VALUE = 1)
        if (any(sampPerPeriods != sampPerPeriods[1])) {
          tcltk::tkmessageBox(message = paste("You are trying to merge",
                                              "gap sets from different",
                                              "series (different",
                                              "sampling period)"), 
                              icon = "warning")
        } else if (any(iniTimes != iniTimes[1])) {
          tcltk::tkmessageBox(message = paste("You are trying to merge gap",
                                              "sets from different",
                                              "series (different start",
                                              "date)"), 
                              icon = "warning")
        } else {
          mergedGaps <- NULL
          for (i in gapsToMerge) {
            gap <- get(i, envir = KTSEnv)
            mergedGaps <- union(mergedGaps, gap$gaps)
            rm(gap)
          }
          mergedGaps <- sort(mergedGaps)
          lengthGaps <- vapply(gapsToMerge, function(X) {
            lengthT <- get(X, envir = KTSEnv)[[5]]
          }, FUN.VALUE = 1)
          maxLength <- min(which(lengthGaps == max(lengthGaps)))
          refGap <- get(gapsToMerge[maxLength], envir = KTSEnv)
          newGap <- refGap
          newGap$gaps <- mergedGaps
          newGap$tsName <- "merged"
          assign(newNameGap, newGap, KTSEnv)
        }
      }
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    showPANmerge()
  }
  showPANmerge <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "MERGE")
    if (class(KTSEnv$dSList$TS) == "character") {
      createTsChb()
      createEntry(labTitle = "Name", textVariableName = "newName", 
                  defaultVal = "mergedTS")
    }
    if (class(KTSEnv$dSList$gaps) == "character") {
      createGapChb()
      createEntry(labTitle = "Name", textVariableName = "nnmgap", 
                  defaultVal = "mergedGap")
    }
    createOK(labTitle = "RUN", action = mergeOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyGapOrTs(action = "showPANmerge", 
                    envirName = environment(showPANmerge))
}

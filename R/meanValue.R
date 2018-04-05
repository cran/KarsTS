meanValue <-
function() {
    showPANmnvl <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "MEAN VALUE")
      if (is.null(KTSEnv$dSList$gaps) == FALSE) {
        createGapRb()
      }
      createTsRb()
      createEntry(labTitle = "Period(in lags)", textVariableName = "period")
      createEntry(labTitle = "Number of periods around", 
                  textVariableName = "perWindow")
      createEntry(labTitle = "Maximum number of iterations", 
                  textVariableName = "maxniter")
      createEntry(labTitle = "Minimum number of observations (one side)", 
                  textVariableName = "mininumNObs")
      createEntry(labTitle = "Statistic", textVariableName = "mediaona", 
                  defaultVal = "median")
      createOK(labTitle = "RUN", action = mnvlOnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    mnvlOnOk <- function() {
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                   noValid = NA)
      statistic <- verifyCharEntry(tcltk::tclvalue(KTSEnv$mediaona))
      lPeriod <- verifyIntEntry(tcltk::tclvalue(KTSEnv$period), 
                                noValid = NA)
      nPeriods <- verifyIntEntry(tcltk::tclvalue(KTSEnv$perWindow), 
                                 noValid = NA)
      maxIter <- verifyIntEntry(tcltk::tclvalue(KTSEnv$maxniter), 
                                noValid = NA)
      minPoints <- verifyIntEntry(tcltk::tclvalue(KTSEnv$mininumNObs), 
                                  noValid = NA)
      
      if (is.na(selTsName)) {
        tcltk::tkmessageBox(message = "Choose a time series", 
                            icon = "warning")
      } else if (is.na(lPeriod)) {
        tcltk::tkmessageBox(message = paste("Enter the period",
                                            "of the phenomenon in gaps"), 
                            icon = "warning")
      } else if (is.na(nPeriods)) {
        tcltk::tkmessageBox(message = paste("Enter the number of",
                                            "periods to use around the gap"), 
                            icon = "warning")
      } else if (lPeriod == 0) {
        tcltk::tkmessageBox(message = paste("The period length must be,",
                                            "at least, 1 (lag)"), 
                            icon = "warning")
      } else if (nPeriods < 2) {
        tcltk::tkmessageBox(message = paste("The number of periods must be at",
                                            "least 2 (one at each side)"), 
                            icon = "warning")
      } else if (is.na(maxIter)) {
        tcltk::tkmessageBox(message = paste("Enter the maximum number of times",
                                            "the process will be applied"), 
                            icon = "warning")
      } else if (is.na(statistic)) {
        tcltk::tkmessageBox(message = paste("Enter the statistic:",
                                            "median or mean"), 
                            icon = "warning")
      } else if (statistic != "mean" & statistic != "median") {
        tcltk::tkmessageBox(message = paste("The statistic can be",
                                            "median or mean"), 
                            icon = "warning")
      } else {
        
        if (is.na(minPoints)) {
          minPoints <- 0
        }
        if (nPeriods%%2 == 1) {
          nPeriods <- nPeriods + 1
        }
        halfNPeriods <- nPeriods/2
        selTs <- get(selTsName, envir = KTSEnv)
        gapToUse <- gapForSelMethod(selTsName, selTs)
        selGap <- gapToUse$selGap
        selGapName <- gapToUse$selGapName
        nasInSelTs <- which(is.na(selTs$value))
        tmComptibility <- areTsGapTimeCompatible(selTs, selGap)
        if (length(nasInSelTs) == 0) {
          tcltk::tkmessageBox(message = paste("The selected time",
                                              "series contains no NAs"), 
                              icon = "warning")
        } else if (length(selGap$gaps) == 0) {
          tcltk::tkmessageBox(message = "The gap set is empty", 
                              icon = "warning")
        } else if (length(setdiff(union(selGap$gaps, nasInSelTs), 
                                  nasInSelTs)) != 0) {
          tcltk::tkmessageBox(message = paste("Some NAs in the gap set do",
                                              "not exist in the time series.",
                                              "Check that the selected gap set",
                                              "comes from the selected",
                                              "time series"), 
                              icon = "warning")
        } else if (tmComptibility[1] == FALSE) {
          tcltk::tkmessageBox(message = paste("The initial date of the time",
                                              "series and the one stored",
                                              "in the gap set do not match"), 
                              icon = "warning")
        } else if (tmComptibility[2] == FALSE) {
          tcltk::tkmessageBox(message = paste("The sampling period of the",
                                              "time series and the one stored",
                                              "in the gap set do not match"), 
                              icon = "warning")
        } else if (tmComptibility[3] == FALSE) {
          tcltk::tkmessageBox(message = paste("The time series is shorter",
                                              "than some indices stored",
                                              "in the set of gaps"), 
                              icon = "warning")
        } else if (minPoints > halfNPeriods) {
          tcltk::tkmessageBox(message = paste("The minimum number of",
                                              "observations must not excede",
                                              "half the number of periods"), 
                              icon = "warning")
        } else {
          
          getCentrVal <- function(NAInd, TSV, widthAround, 
                                  lPeriod, minPoints,
                                  statistic = "mean") {
            
            lTS <- length(TSV)
            eqP <- seq(NAInd - widthAround,
                       NAInd + widthAround,
                       lPeriod)
            eqP <- eqP[eqP > 0 & eqP <= lTS]
            
            anaobi1 <- is.finite(TSV[eqP[which(eqP < NAInd)]])
            anaobi2 <- is.finite(TSV[eqP[which(eqP >NAInd)]])
            pointsBefore <- which(anaobi1)
            pointsAfter <- which(anaobi2)
            pointsBefore <- length(pointsBefore)
            pointsAfter <- length(pointsAfter)
            
            if (pointsBefore < minPoints | pointsAfter < minPoints) {
              
              res <- NA
              
            }else if (statistic == "mean") {
              
              res <- mean(TSV[eqP], na.rm = TRUE)
              
            }else{
              
              res <- stats::median(TSV[eqP], na.rm = TRUE)
              
            }
            
            res
            
          }
          
          getValsOneIte <- function(gaps,TSVI, widthAround, 
                                    lPeriod, minPoints,
                                    statistic = "mean"){
            
            replacements <- sapply(gaps, FUN = getCentrVal, 
                                   minPoints = minPoints, 
                                   TSV = TSVI, 
                                   widthAround = widthAround, 
                                   lPeriod = lPeriod,
                                   statistic = statistic)
            
            replacements[which(is.finite(replacements) == FALSE)] <- NA
            
            replacements
            
          }
          
          fillGapsMV <- function(gaps, TS, maxIter, widthAround, 
                                 lPeriod, minPoints,statistic = "mean") {
            
            cont <- 1
            remainingGaps <- length(gaps)
            
            while (cont <= maxIter & length(gaps) != 0) {
              
              replacements <- getValsOneIte(gaps = gaps,
                                            TSVI = TS$value, 
                                            widthAround = widthAround, 
                                            lPeriod = lPeriod, 
                                            minPoints = minPoints,
                                            statistic = statistic)
              
              TS$value[gaps] <- replacements
              
              gapsNow <- intersect(which(is.na(TS$value)),gaps)
              remainingGaps <- c(remainingGaps, length(gapsNow))
              gaps <- gapsNow 
              
              cont <- cont + 1
              rm(replacements)   
              
            }
            
            resultMV <- list(filled = TS, gaps = gaps, 
                             iterations = cont, 
                             advances = remainingGaps)
            
            rownames(resultMV$filled) <- NULL
            
            resultMV
            
          }
          
          widthAround <- halfNPeriods * lPeriod
          resMeVal <- fillGapsMV(gaps = selGap$gaps, 
                                 TS = selTs, 
                                 maxIter = maxIter, 
                                 widthAround = widthAround, 
                                 lPeriod = lPeriod, 
                                 minPoints = minPoints,
                                 statistic = statistic)
          filledTS <- resMeVal$filled
          assign(paste0(selTsName, "_", selGapName, "_mvl"), 
                 filledTS, envir = KTSEnv)
          gapsAfterFill <- getGapsAfterFill(filledTS, selGap, 
                                            envir = environment(mnvlOnOk))
          remainingNAsInGap <- gapsAfterFill$remainingNAsInGap
          filledNasTable <- gapsAfterFill$filledNasTable
          sampPerSec <- diff(as.numeric(selTs$time[1:2]))
          txtParam <- c(paste("Period(sec):", lPeriod * sampPerSec), 
                        paste("Periods around:",nPeriods), 
                        paste("Minimum number of observations to each side:", 
                              minPoints), 
                        paste("NAs to fill per iteration:", 
                              paste(resMeVal$advances, collapse = ",")))
          writeMethodTitle("MEAN/MEDIAN VALUE")
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtParam, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", "\n")
          writeMethodSummary(filledNasTable, remainingNAsInGap, selTsName, 
                             selGapName, selGap)
          endingLines()
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANmnvl()
        }
      }
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANmnvl", envirName = environment(showPANmnvl))
  }

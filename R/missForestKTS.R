missForestKTS <-
function() {
  showPANmissfor1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "MISSFOREST")
    mssfrst2 <- tcltk::tklabel(KTSEnv$subPanR4C1, 
                               text = "Time series (input)", 
                               font = KTSEnv$KTSFonts$T1)
    mssfrst3 <- tcltk::tklabel(KTSEnv$subPanR4C1, 
                               text = "Time series (output)", 
                               font = KTSEnv$KTSFonts$T1)
    tcltk::tkgrid(mssfrst2, mssfrst3, sticky = "nw", 
                  padx = c(10, 10), pady = c(10,5))
    myApplyVector(FUN = createChbChb, 1:KTSEnv$dSList$nTS, 
                  elements = KTSEnv$dSList$TS, 
                  prefix1 = "inpcbValue", 
                  prefix2 = "outcbValue", 
                  envir = KTSEnv)
    createTitle(labTitle = "Parameters")
    createEntry(labTitle = "Number of trees", 
                textVariableName = "numberTrees", 
                defaultVal = "100")
    createEntry(labTitle = "Maximum number of iterations", 
                textVariableName = "maxIterations", 
                defaultVal = "10")
    createOK(labTitle = "NEXT", action = missfor1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  missfor1OnOk <- function() {
    inputChecked <- tsCheckedTF(prefix = "inpcbValue")
    outputChecked <- tsCheckedTF(prefix = "outcbValue")
    nTrees <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numberTrees), 
                             noValid = NA)
    maxIter <- verifyIntEntry(tcltk::tclvalue(KTSEnv$maxIterations), 
                              noValid = NA)
    inpCheckedInd <- which(inputChecked == TRUE)
    outCheckedInd <- which(outputChecked == TRUE)
    if (length(inpCheckedInd) < 1) {
      tcltk::tkmessageBox(message = paste("Select at least a times",
                                          "series in the left column"), 
                          icon = "warning")
    } else if (length(outCheckedInd) < 1) {
      tcltk::tkmessageBox(message = paste("Select at least a time series",
                                          "in the right column"), 
                          icon = "warning")
    } else if (is.na(nTrees)) {
      tcltk::tkmessageBox(message = paste("Enter a valid number of trees"), 
                          icon = "warning")
    } else if (is.na(maxIter)) {
      tcltk::tkmessageBox(message = paste("Enter the maximum number",
                                          "of iterations"), 
                          icon = "warning")
    } else if (length(setdiff(outCheckedInd, inpCheckedInd)) > 0) {
      tcltk::tkmessageBox(message = paste("All of the output time series",
                                          "must be input time",
                                          "series as well"), 
                          icon = "warning")
    } else {
      inpTSNames <- KTSEnv$dSList$TS[inpCheckedInd]
      nInpTS <- length(inpTSNames)
      firstTs <- get(inpTSNames[1], envir = KTSEnv)
      if (nInpTS == 1) {
        tmComptibility <- matrix(TRUE, nInpTS, 3)
      } else {
        tmComptibility <- matrix(NA, nInpTS, 3)
        for (i in 2:nInpTS) {
          tmComptibility[i, ] <- are2TsTimeCompatible(firstTs, 
                                                         get(inpTSNames[i],
                                                             envir = KTSEnv))
        }
        tmComptibility <- tmComptibility[-1, ]
        if (is.vector(tmComptibility)) {
          tmComptibility <- as.matrix(t(tmComptibility))
        }
      }
      if (any(tmComptibility[, 1] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The initial date of all",
                                            "the time series",
                                            "must be the same"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 2] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The sampling period of all",
                                            "the time series",
                                            "must be the same"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 3] == FALSE)) {
        tcltk::tkmessageBox(message = paste("All time series must",
                                            "have the same length"), 
                            icon = "warning")
      } else {
        assignMultiple(c("inpCheckedInd", "outCheckedInd", 
                         "maxIter", "nTrees"), 
                       list(inpCheckedInd, outCheckedInd, 
                            maxIter, nTrees), 
                       envir = KTSEnv)
        showPANmissfor2()
      }
    }
  }
  showPANmissfor2 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "MISSFOREST")
    createTitle(labTitle = "Scale variables")
    assign("scaleYN", tcltk::tclVar("Yes"), envir = KTSEnv)
    createRb(variable = KTSEnv$scaleYN, dataVector = c("Yes", "No"))
    createTitle(labTitle = "Embedding parameters")
    for (ind in KTSEnv$inpCheckedInd) {
      createEntry(labTitle = paste("Emb.dim,delay for", 
                                   KTSEnv$dSList$TS[ind]), 
                  textVariableName = paste0("emdcbValue", ind), 
                  defaultVal = "1,0", 
                  font = KTSEnv$KTSFonts$normal)
    }
    createTitle(labTitle = "Gaps to fill within")
    for (ind in KTSEnv$inpCheckedInd) {
      createEntry(labTitle = KTSEnv$dSList$TS[ind], 
                  textVariableName = paste0("gapcbValue",ind), 
                  defaultVal = "all", 
                  font = KTSEnv$KTSFonts$normal)
    }
    createOK(labTitle = "RUN", action = missfor2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  missfor2OnOk <- function() {
    areEmbeddCompatible <- function(embParameters) {
      embDim <- embParameters[1]
      embDelay <- embParameters[2]
      if (is.na(embDim) | is.na(embDelay)) {
        result <- FALSE
      } else if (embDim == 0) {
        result <- FALSE
      } else if (embDim == 1 & embDelay > 0) {
        result <- FALSE
      } else if (embDim > 1 & embDelay == 0) {
        result <- FALSE
      } else {
        result <- TRUE
      }
      result
    }
    doesGapExist <- function(gapSetName) {
      if (is.na(gapSetName)) {
        result = FALSE
      } else if (gapSetName == "all") {
        result = TRUE
      } else {
        existsGap <- try(get(gapSetName, envir = KTSEnv), silent = TRUE)
        if (class(existsGap) != "list") {
          result = FALSE
        } else {
          if (length(names(existsGap)) != 6) {
            result <- FALSE
          } else {
            result <- TRUE
          }
        }
      }
      result
    }
    areTsGapNAsCompatible <- function(TS1, GAP1) {
      nCommonNAs <- length(intersect(which(is.na(TS1$value)), GAP1$gaps))
      if (nCommonNAs == length(GAP1$gaps)) {
        result <- TRUE
      } else {
        result <- FALSE
      }
      result
    }
    gapTSCompatible <- function(selGapName, outTSNames) {
      notALL <- which(selGapName != "all")
      if (length(notALL) == 0) {
        tmComptibility <- matrix(rep(TRUE), 1, 3)
        NAsCompatibility <- TRUE
      } else {
        dataMatrix <- cbind(outTSNames[notALL], selGapName[notALL])
        tmComptibility <- apply(dataMatrix, 1, function(x) {
          timSer <- get(x[1], envir = KTSEnv)
          gapSet <- get(x[2], envir = KTSEnv)
          areTsGapTimeCompatible(timSer, gapSet)
        })
        tmComptibility <- t(tmComptibility)
        NAsCompatibility <- apply(dataMatrix, 1, function(x) {
          timSer <- get(x[1], envir = KTSEnv)
          gapSet <- get(x[2], envir = KTSEnv)
          areTsGapNAsCompatible(timSer, gapSet)
        })
      }
      list(tmComptibility, NAsCompatibility)
    }
    rmEmbExtraCols <- function(embDims, missResult) {
      OBErrors <- signif(100 * missResult$OOBerror, 5)
      imputedData <- missResult$ximp
      indices <- NULL
      for (i in 1:length(embDims)) {
        indices <- c(indices, c(c(1, rep(0, embDims[i] - 1))))
      }
      if (any(indices == 0)) {
        OBErrors <- OBErrors[-which(indices == 0)]
        imputedData <- imputedData[, -which(indices == 0)]
      }
      
      list(imputedData = imputedData, OBErrors = OBErrors)
      
    }
    writeCommonLines <- function(nTrees, maxIter, scaleOrNot, 
                                 OBErrors, inpTSNames) {
      txtParam <- c(paste("Number of trees:", nTrees), 
                    paste("Maximum number of iterations:", maxIter), 
                    paste("The variables were scaled:", scaleOrNot))
      txtInput <- "Input time series"
      txtError <- paste0("  ", inpTSNames, 
                         " (Out of the box error:", OBErrors, ")")
      txtOutput <- "Filled time series:"
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      paste(txtParam, collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      paste(txtInput, collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      paste(txtError, collapse = "\n"), 
                      collapse = "\n")
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      paste(txtOutput, collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
    }
    giveOutputs <- function(outTSNames.i, outTSNames, inpTSNames, 
                            imputedData, 
                            selGapName, timeTS) {
      indexInOut <- which(outTSNames == outTSNames.i)
      indexInInp <- which(inpTSNames == outTSNames.i)
      imputedData.i <- imputedData[, indexInInp]
      nonImputedData.i <- get(outTSNames.i, envir = KTSEnv)$value
      if (any(is.na(nonImputedData.i))) {
        if (selGapName[indexInOut] != "all") {
          gapSet <- get(selGapName[indexInOut], envir = KTSEnv)
          NAsToRestore <- setdiff(which(is.na(nonImputedData.i)), gapSet$gaps)
          imputedData.i[NAsToRestore] <- NA
        } else {
          gapSet <- list(gaps = which(is.na(nonImputedData.i)), 
                         tsIni = as.character(timeTS[1]), 
                         tsEnd = as.character(timeTS[length(timeTS)]), 
                         samPerMin = diff(as.numeric(timeTS[1:2]))/60, 
                         tsLength = length(timeTS), tsName = outTSNames.i)
        }
        imputedTS <- data.frame(time = timeTS, value = imputedData.i)
        assign(paste0(outTSNames.i, "_msf"), imputedTS, envir = KTSEnv)
        gapsAfterFill <- getGapsAfterFill(imputedTS, gapSet, 
                                          envir = environment(missfor2OnOk))
        remainingNAsInGap <- gapsAfterFill$remainingNAsInGap
        filledNasTable <- gapsAfterFill$filledNasTable
        writeMethodSummary(filledNasTable, remainingNAsInGap, outTSNames.i, 
                           selGapName[indexInOut], gapSet)
      }
    }
    scaleOrNot <- tcltk::tclvalue(KTSEnv$scaleYN)
    valuesEntry1 <- apply(as.matrix(KTSEnv$inpCheckedInd), 1, function(ind) {
      separateEntry(tcltk::tclvalue(get(paste0("emdcbValue", ind), 
                                        envir = KTSEnv)), 
                    class1 = verifyIntEntry, 
                    class2 = verifyIntEntry, noValid = NA)
    })
    embDims <- valuesEntry1[1, ]
    delays <- valuesEntry1[2, ]
    embeddCompatibility <- apply(cbind(embDims, delays), 1, 
                                 FUN = areEmbeddCompatible)
    selGapName <- apply(as.matrix(KTSEnv$inpCheckedInd), 1, 
                        function(ind) {
      verifyCharEntry(tcltk::tclvalue(get(paste0("gapcbValue", ind), 
                                          envir = KTSEnv)), 
                      noValid = NA)
    })
    existGaps <- apply(as.matrix(selGapName), 1, FUN = doesGapExist)
    if (any(is.na(selGapName))) {
      tcltk::tkmessageBox(message = paste("Choose the gap set to fil",
                                          "within each output time series.",
                                          "If you want all the NAs to be",
                                          "filled, write the word all",
                                          "in the text entry"), 
                          icon = "warning")
    } else if (any(is.na(valuesEntry1))) {
      tcltk::tkmessageBox(message = paste("Choose the embedding parameters",
                                          "for each output time series.",
                                          "If you do not want to embedd,",
                                          "write 1,)0 in the text entry"), 
                          icon = "warning")
    } else if (any(is.na(embDims))) {
      tcltk::tkmessageBox(message = paste("The embedding dimensions must be",
                                          "integers greater than 0"), 
                          icon = "warning")
    } else if (any(is.na(delays))) {
      tcltk::tkmessageBox(message = paste("The delays must be integers ",
                                          "greater than or equal to 0"), 
                          icon = "warning")
    } else if (any(embeddCompatibility == FALSE)) {
      tcltk::tkmessageBox(message = paste("Some embedding parameters are",
                                          "incompatible.For embedding",
                                          "dimension 1, the delay must be 0;",
                                          "for dimensions greater than 1,",
                                          "the delay must be greater than 0"), 
                          icon = "warning")
    } else if (any(existGaps == FALSE)) {
      tcltk::tkmessageBox(message = paste("Some of the gaps you entered do",
                                          "not exist. Check the spelling"), 
                          icon = "warning")
    } else if (sum(embDims, na.rm = TRUE) < 2) {
      tcltk::tkmessageBox(message = paste("The filling method needs,",
                                          " at least,two different input",
                                          "time series or an input time",
                                          "series with embedding",
                                          "dimension greater than one"), 
                          icon = "warning")
    } else {
      inpTSNames <- KTSEnv$dSList$TS[KTSEnv$inpCheckedInd]
      outTSNames <- KTSEnv$dSList$TS[KTSEnv$outCheckedInd]
      timeTS <- get(inpTSNames[1], envir = KTSEnv)$time
      gapTSCompatibility <- gapTSCompatible(selGapName, outTSNames)
      tmComptibility <- gapTSCompatibility$tmComptibility
      NAsCompatibility <- gapTSCompatibility$NAsCompatibility
      if (any(tmComptibility[, 1] == FALSE)) {
        badElement <- min(which(tmComptibility[, 1] == FALSE))
        badGap <- selGapName[notAll[badElement]]
        badOutTS <- outTSNames[notAll[badElement]]
        tcltk::tkmessageBox(message = paste("The gap set", badGap, 
                                            "and the time series", 
                                            badOutTS, 
                                            "have different initial dates"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 2] == FALSE)) {
        badElement <- min(which(tmComptibility[, 2] == FALSE))
        badGap <- selGapName[notAll[badElement]]
        badOutTS <- outTSNames[notAll[badElement]]
        tcltk::tkmessageBox(message = paste("The gap set", badGap, 
                                            "and the time series", 
                                            badOutTS, 
                                            "have different frequencies"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 3] == FALSE)) {
        badElement <- min(which(tmComptibility[, 3] == FALSE))
        badGap <- selGapName[notAll[badElement]]
        badOutTS <- outTSNames[notAll[badElement]]
        tcltk::tkmessageBox(message = paste("The gap set", badGap, 
                                            "has indices bigger than",
                                            "the length of the time series", 
                                            badOutTS), icon = "warning")
      } else if (any(NAsCompatibility == FALSE)) {
        badElement <- min(which(NAsCompatibility == FALSE))
        badGap <- selGapName[notAll[badElement]]
        badOutTS <- outTSNames[notAll[badElement]]
        tcltk::tkmessageBox(message = paste("Some of the NAs stored in ", 
                                            badGap, "do not exist in the",
                                            "time series", badOutTS), 
                            icon = "warning")
      } else {
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        multivData <- NULL
        for (i in 1:length(inpTSNames)) {
          multivData <- cbind(multivData, 
                              get(inpTSNames[i], envir = KTSEnv)$value)
        }
        if (scaleOrNot == "Yes") {
          scaleResult <- myScale(multivData, outputType = "outList")
          multivData <- scaleResult$scaledData
          scaleMedians <- scaleResult$centralStat
          scaleMads <- scaleResult$disperStat
        }
        embedded <- NULL
        for (i in 1:ncol(multivData)) {
          embedded <- cbind(embedded, 
                            embedData(multivData[, i], embDims[i],delays[i]))
        }
        colnames(embedded) <- NULL
        inputForMiss <- embedded
        colnames(inputForMiss) <- NULL
        whichComplete <- which(stats::complete.cases(inputForMiss))
        dataComplCases <- inputForMiss[whichComplete, ]
        missResult <- try(missForest::missForest(inputForMiss, 
                                                 maxiter = KTSEnv$maxIter, 
                                                 ntree = KTSEnv$nTrees, 
                                                 variablewise = TRUE, 
                                                 xtrue = dataComplCases, 
                                                 verbose = FALSE), 
                          silent = TRUE)
        if (class(missResult) == "try-error") {
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          showPANmissfor1()
          tcltk::tkmessageBox(message = paste("Missforest failed",
                                              "to impute the data"), 
                              icon = "warning")
        } else {
          
          result <- rmEmbExtraCols(embDims, missResult)
          imputedData <- result$imputedData
          OBErrors <- result$OBErrors
          if (scaleOrNot == "Yes") {
            numbRows <- nrow(imputedData)
            numbCols <- ncol(imputedData)
            scaleMedians <- t(matrix(rep(scaleMedians, numbRows), 
                                     numbCols, 
                                     numbRows))
            scaleMads <- t(matrix(rep(scaleMads, numbRows), 
                                  numbCols, numbRows))
            imputedData <- imputedData * scaleMads + scaleMedians
          }
          writeMethodTitle("MISSFOREST")
          writeCommonLines(KTSEnv$nTrees, KTSEnv$maxIter, 
                           scaleOrNot, OBErrors, 
                           inpTSNames)
          apply(as.matrix(outTSNames), 1, FUN = giveOutputs, 
                outTSNames = outTSNames, 
                inpTSNames = inpTSNames, 
                imputedData = imputedData, 
                selGapName = selGapName, 
                timeTS = timeTS)
          endingLines()
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          showPANmissfor1()
        }
      }
    }
  }
  tcltk::tkmessageBox(message = paste("The process may take some",
                                      "minutes, specially if there",
                                      "are many iterations."), 
                      icon = "warning")
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANmissfor1", 
               envirName = environment(showPANmissfor1))
}

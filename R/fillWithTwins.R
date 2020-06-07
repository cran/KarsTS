fillWithTwins <-
function() {
    showPANfTwins1 <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "TWINS")
        if (is.null(KTSEnv$dSList$gaps) == FALSE) {
            createGapRb()
        }
        createTsRb()
        createOK(labTitle = "NEXT", action = fTwins1OnOk)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    fTwins1OnOk <- function() {
        selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                     noValid = NA)
        if (is.na(selTsName)) {
            tcltk::tkmessageBox(message = "Choose a time series", 
                                icon = "warning")
        } else {
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
                tcltk::tkmessageBox(message = paste("Some NAs in the gap",
                                                    "set do not exist in",
                                                    "the time series. Check",
                                                    "that the selected gap",
                                                    "set comes from the",
                                                    "selected time series"), 
                  icon = "warning")
            } else if (tmComptibility[1] == FALSE) {
                tcltk::tkmessageBox(message = paste("The initial date of",
                                                    "the time series and",
                                                    "the one stored in the",
                                                    "gap set do not match"), 
                  icon = "warning")
            } else if (tmComptibility[2] == FALSE) {
                tcltk::tkmessageBox(message = paste("The sampling period of",
                                                    "the time series and the",
                                                    "one stored in the gap",
                                                    "set do not match"), 
                  icon = "warning")
            } else if (tmComptibility[3] == FALSE) {
                tcltk::tkmessageBox(message = paste("The time series is",
                                                    "shorter than some",
                                                    "indices stored in",
                                                    "the set of gaps"), 
                  icon = "warning")
            } else {
                assign("selTsName", selTsName, envir = KTSEnv)
                assign("selTs", selTs, envir = KTSEnv)
                assign("selGapName", selGapName, envir = KTSEnv)
                assign("selGap", selGap, envir = KTSEnv)
                showPANfTwins2()
            }
        }
    }
    showPANfTwins2 <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "TWINS")
        createRmRb()
        createEntry(labTitle = "Max. distance", 
                    textVariableName = "madila", 
                    defaultVal = "0")
        createOK(labTitle = "RUN", action = fTwins2OnOk)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    fTwins2OnOk <- function() {
        selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP), 
                                     noValid = NA)
        maxDist <- verifyIntEntry(tcltk::tclvalue(KTSEnv$madila), 
                                  noValid = NA)
        if (is.na(selRmName)) {
            tcltk::tkmessageBox(message = "Choose a recurrence matrix", 
                                icon = "warning")
        } else if (is.na(maxDist)) {
            tcltk::tkmessageBox(message = "Enter the maximum distance", 
                                icon = "warning")
        } else {
            selRm <- get(selRmName, envir = KTSEnv)
            continueYN <- "yes"
            aaques <- paste("The selected time series name matches one of",
                            "the names stored in the recurrence matrix,",
                            " so probably no gaps will be found at",
                            "distance 0.\\n                       Do",
                            "you want to go ahead?")
            if (any(selRm$tsName == KTSEnv$selTsName)) {
              continueYN <- tcltk::tkmessageBox(message = aaques, 
                                                icon = "question", 
                                                type = "yesnocancel", 
                                                default = "yes")
                continueYN <- as.character(tcltk::tclvalue(continueYN))
            }
            if (continueYN == "no" | continueYN == "cancel") {
                cleanEnvir()
                refreshDataSetsList(outp = FALSE)
                showPANfTwins1()
            } else {
                tmComptibility <- areTsRmTimeCompatible(KTSEnv$selTs, selRm)
                if (selRm$type == "cross") {
                  tcltk::tkmessageBox(message = paste("The recurrence matrix",
                                                      "must be simple",
                                                      "or joint"), 
                    icon = "warning")
                } else if (tmComptibility[1] == FALSE) {
                  tcltk::tkmessageBox(message = paste("The recurrence matrix",
                                                      "and the time series",
                                                      "do not start at",
                                                      "the same time"), 
                    icon = "warning")
                } else if (tmComptibility[2] == FALSE) {
                  tcltk::tkmessageBox(message = paste("The recurrence matrix",
                                                      "and the time series",
                                                      "do not have the",
                                                      "same sampling period"), 
                    icon = "warning")
                } else {
                  if (tmComptibility[3] == FALSE) {
                    tcltk::tkmessageBox(message = paste("The time series",
                                                        "length stored in",
                                                        "the recurrence",
                                                        "matrix does not",
                                                        "match the length",
                                                        "of the selected",
                                                        "one. Take it",
                                                        "into consideration"), 
                      icon = "warning")
                  }
                  tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
                  gaps <- KTSEnv$selGap$gaps
                  gaps0 <- gaps
                  if (maxDist > 1) {
                    for (i in 1:maxDist) {
                      gaps <- sort(union(gaps, gaps0 - i))
                    }
                  }
                  gaps <- gaps[which(gaps > 0)]
                  twins <- findTwins(recMat = selRm, pointsToFind = gaps)
                  tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
                  if (all(is.na(twins))) {
                    tcltk::tkmessageBox(message = paste("No twins were found",
                                                        "in the recurrence",
                                                        "matrix"), 
                      icon = "warning")
                    cleanEnvir()
                    refreshDataSetsList(outp = FALSE)
                    showPANfTwins1()
                  } else {
                    findPreviousInd <- function(indices, distance) {
                      previousInd <- indices - distance
                      previousInd <- previousInd[which(previousInd > 0)]
                    }
                    gaps = KTSEnv$selGap$gaps
                    fillableAtDist <- vector("list", maxDist + 1)
                    fillableAtDist[[1]] <- intersect(gaps, 
                                                     which(is.finite(twins)))
                    totalFillable <- fillableAtDist[[1]]
                    if (maxDist > 0) {
                      for (dm1 in 2:(maxDist + 1)) {
                        previousInd <- findPreviousInd(indices = gaps, 
                                                       distance = dm1 - 1)
                        lopD <- which(is.finite(twins))
                        fillableAtDist[[dm1]] <- intersect(previousInd, 
                                                           lopD) + dm1 - 1
                        fillableAtDist[[dm1]] <- setdiff(fillableAtDist[[dm1]], 
                                                         totalFillable)
                        totalFillable <- union(fillableAtDist[[dm1]], 
                                               totalFillable)
                        rm(previousInd)
                      }
                    }
                    if (length(totalFillable) == 0) {
                      tcltk::tkmessageBox(message = paste("There are twins",
                                                          "in the recurrence",
                                                          "matrix, but not",
                                                          "close enough",
                                                          "to the gaps"), 
                        icon = "warning")
                      cleanEnvir()
                      refreshDataSetsList(outp = FALSE)
                      showPANfTwins1()
                    }else if (length(fillableAtDist[[1]]) == 0 & maxDist == 0){
                      tcltk::tkmessageBox(message = paste("No gaps can be",
                                                          "filled at",
                                                          "distance 0.",
                                                          "Verify that the",
                                                          "time series to",
                                                          "fill was not used",
                                                          "to create the",
                                                          "recurrence matrix"), 
                        icon = "warning")
                      cleanEnvir()
                      refreshDataSetsList(outp = FALSE)
                      showPANfTwins1()
                    } else {
                      if (length(fillableAtDist[[1]]) == 0) {
                        tcltk::tkmessageBox(message = paste("No gaps can be",
                                                            "filled, at",
                                                            "distance 0.",
                                                            "Maybe the the",
                                                            "time series",
                                                            "to fill was",
                                                            "used to create",
                                                            "the recurrence",
                                                            "matrix"), 
                          icon = "warning")
                      }
                      
                      fillOneNA <- function(gapToFill, distance, 
                                            timSer, twins) {
                        previous <- gapToFill - distance
                        itsTwinsInd <- which(twins == twins[previous])
                        possibleFill <- timSer$value[itsTwinsInd]
                        filling <- stats::median(possibleFill, na.rm = TRUE)
                      }
                      
                      filledTS <- KTSEnv$selTs
                      for (dm1 in 1:(maxDist + 1)) {
                        if (length(fillableAtDist[[dm1]]) > 0) {
                          filling <- apply(as.matrix(fillableAtDist[[dm1]]), 1, 
                                           FUN = fillOneNA, 
                                           distance = dm1 - 1, 
                                           timSer = KTSEnv$selTs, 
                                           twins = twins)
                          filledTS$value[fillableAtDist[[dm1]]] <- filling
                        }
                      }
                      assign(paste0(KTSEnv$selTsName, "_", 
                                    KTSEnv$selGapName, "_atw"), 
                             filledTS, envir = KTSEnv)
                      filledEachDist <- unlist(lapply(fillableAtDist, 
                                                      function(x) {
                                                        length(x)
                                                      }))
                      gapsAfterFill <- getGapsAfterFill(filledTS, 
                                                        KTSEnv$selGap, 
                                                        envir = KTSEnv)
                      remainingNAsInGap <- gapsAfterFill$remainingNAsInGap
                      filledNasTable <- gapsAfterFill$filledNasTable
                      writeMethodTitle("TWINS")
                      txtSteps <- paste("Filled gaps each distance:", 
                                        paste(filledEachDist,collapse = ","))
                      tcltk::tkinsert(KTSEnv$txtWidget, "end", txtSteps)
                      tcltk::tkinsert(KTSEnv$txtWidget, "end", "\\n")
                      txtDist <- paste("Maximum distance:", maxDist)
                      tcltk::tkinsert(KTSEnv$txtWidget, "end", txtDist)
                      tcltk::tkinsert(KTSEnv$txtWidget, "end", "\\n")
                      txtMatrix <- paste("Recurrence matrix:", selRmName)
                      tcltk::tkinsert(KTSEnv$txtWidget, "end", txtMatrix)
                      tcltk::tkinsert(KTSEnv$txtWidget, "end", "\\n")
                      writeMethodSummary(filledNasTable, remainingNAsInGap, 
                                         KTSEnv$selTsName, 
                                         KTSEnv$selGapName, KTSEnv$selGap)
                      endingLines()
                      cleanEnvir()
                      refreshDataSetsList(outp = FALSE)
                      showPANfTwins1()
                    }
                  }
                }
            }
        }
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyRmTs(action = "showPANfTwins1", 
                   envirName = environment(showPANfTwins1))
}

selectionGaps <-
function() {
  selGapOnOk2 <- function() {
    verifySpecGap <- function(specificGaps) {
      isSpecGapInt <- verifyIntEntry(specificGaps, noValid = NA)
      if (is.finite(isSpecGapInt)) {
        elesToKeep <- isSpecGapInt
      } else {
        numberOfCommas <- length(which(strsplit(specificGaps, 
                                                split = NULL)[[1]] == ","))
        if (numberOfCommas == 0) {
          elesToKeep <- NA
        } else if (strsplit(specificGaps, split = NULL)[[1]][1] == ",") {
          elesToKeep <- NA
        } else if (numberOfCommas == 1) {
          elesToKeep <- separateEntry(specificGaps, class1 = verifyIntEntry, 
                                      class2 = verifyIntEntry, noValid = NA)
        } else {
          elesToKeep <- NULL
          for (j in 1:(numberOfCommas - 1)) {
            splitted <- separateEntry(specificGaps, class1 = verifyIntEntry, 
                                      class2 = verifyCharEntry, noValid = NA)
            specificGaps <- splitted[2]
            elesToKeep <- c(elesToKeep, verifyIntEntry(splitted[1], 
                                                       noValid = NA))
            rm(splitted)
          }
          splitted <- separateEntry(specificGaps, class1 = verifyIntEntry, 
                                    class2 = verifyIntEntry, noValid = NA)
          elesToKeep <- c(elesToKeep, splitted)
        }
      }
      elesToKeep
    }
    selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
    criterionMin <- tcltk::tclvalue(KTSEnv$cri1cbValue)
    criterionMax <- tcltk::tclvalue(KTSEnv$cri2cbValue)
    criterionSpec <- tcltk::tclvalue(KTSEnv$cri4cbValue)
    criterionAll <- tcltk::tclvalue(KTSEnv$cri5cbValue)
    selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                  noValid = NA)
    selMiniSize <- verifyIntEntry(tcltk::tclvalue(KTSEnv$miniSize), 
                                  noValid = NA)
    selMaxiSize <- verifyIntEntry(tcltk::tclvalue(KTSEnv$maxiSize), 
                                  noValid = NA)
    specificGaps <- verifySpecGap(tcltk::tclvalue(KTSEnv$specGap))
    
    criterion <- c(criterionAll, criterionMin, criterionMax, criterionSpec)
    if (all(criterion == "0")) {
      tcltk::tkmessageBox(message = paste("Choose, at least, a criterion"), 
                          icon = "warning")
    } else if (criterionMin == "1" & is.na(selMiniSize)) {
      tcltk::tkmessageBox(message = paste("The minimum length criterion",
                                          "was chosen: introduce",
                                          "the minimum length"), 
                          icon = "warning")
    } else if (criterionMax == "1" & is.na(selMaxiSize)) {
      tcltk::tkmessageBox(message = paste("The maximum length criterion",
                                          "was chosen: introduce",
                                          "the maximum length"), 
                          icon = "warning")
    } else if (criterionSpec == "1" & any(is.na(specificGaps))) {
      tcltk::tkmessageBox(message = paste("The specific gap criterion was",
                                          "chosen, but the indices",
                                          "are empty or incorrect"), 
                          icon = "warning")
    } else if (is.na(selNewName)) {
      tcltk::tkmessageBox(message = paste("Enter a name for the",
                                          "gap set to be created"), 
                          icon = "warning")
    } else {
      if (criterionAll == "1") {
        allGaps <- which(is.na(selTs$value))
        MMN <- list(gaps = allGaps, tsIni = as.character(selTs$time[1]), 
                    tsEnd = as.character(selTs$time[nrow(selTs)]), 
                    samPerMin = diff(as.numeric(selTs$time[1:2]))/60, 
                    tsLength = nrow(selTs), tsName = KTSEnv$selTsName)
        assign(selNewName, MMN, KTSEnv)
        tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                        paste("NEW SET OF GAPS:", KTSEnv$selNewName, 
                              collapse = "\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", date())
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                        paste("Original time series", KTSEnv$selTsName, 
                              collapse = "\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                        paste("Selected:all of the gaps in the time series", 
                              collapse = "\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                        paste("The gap set contains",length(allGaps), "NAs", 
                              collapse = "\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
        endingLines()
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANselGap1()
      } else {
        gatherSpefGaps <- function(gapsIndeces, specificGaps, criterionSpec) {
          allSpecGaps <- NULL
          txtSpeGapTable <- NULL
          if (criterionSpec == "1") {
            for (k in specificGaps) {
              allSpecGaps <- union(allSpecGaps, 
                                   gapsIndeces[k, 1]:gapsIndeces[k,2])
            }
            if (is.null(allSpecGaps) == FALSE) {
              allSpecGaps <- sort(allSpecGaps)
              mghc3a <- selTs$time[gapsIndeces[specificGaps,1]]
              mghc3b1 <- gapsIndeces[specificGaps, 2]
              mghc3b2 <- gapsIndeces[specificGaps, 1]
              mghc3b <- mghc3b1 - mghc3b2 + 1
              mghc3c <- selTs$time[gapsIndeces[specificGaps, 2]]
              dfSpeGapTable <- data.frame(start.date = mghc3a, 
                                          length.gap = mghc3b, 
                                          end.date = mghc3c)
              dfSpeGapTable <- print.data.frame(dfSpeGapTable)
              txtSpeGapTable <- utils::capture.output(dfSpeGapTable)
            }
          }
          if (length(allSpecGaps) > 0) {
            allSpecGaps <- sort(allSpecGaps)
          }
          list(allSpecGaps = allSpecGaps, txtSpeGapTable = txtSpeGapTable)
        }
        gatherGapsbySize <- function(gapsIndeces, criterionMin, selMiniSize, 
                                     criterionMax, selMaxiSize, lengthGaps) {
          allSizeGaps <- NULL
          txt1 <- "none"
          if (criterionMax == "1" & criterionMin == "1") {
            aa <- which(lengthGaps >= selMiniSize & lengthGaps <= selMaxiSize)
            names(aa) <- NULL
            allSizeGaps <- NULL
            for (i in aa) {
              allSizeGaps <- union(allSizeGaps, 
                                   gapsIndeces[i, 1]:gapsIndeces[i, 2])
            }
            txt1 <- paste("greater than or equal to", selMiniSize,
                          "and smaller than or equal to", 
                          selMaxiSize)
          } else if (criterionMax == "1") {
            aa <- which(lengthGaps <= selMaxiSize)
            names(aa) <- NULL
            allSizeGaps <- NULL
            for (i in aa) {
              allSizeGaps <- union(allSizeGaps, 
                                   gapsIndeces[i, 1]:gapsIndeces[i,2])
            }
            txt1 <- paste("smaller than or equal to", selMaxiSize)
          } else if (criterionMin == "1") {
            aa <- which(lengthGaps >= selMiniSize)
            names(aa) <- NULL
            allSizeGaps <- NULL
            for (i in aa) {
              allSizeGaps <- union(allSizeGaps, 
                                   gapsIndeces[i, 1]:gapsIndeces[i,2])
            }
            txt1 <- paste("greater than or equal to", selMiniSize)
          }
          if (length(allSizeGaps) > 0) {
            allSizeGaps <- sort(allSizeGaps)
          }
          assign("txt1", txt1, envir = environment(selGapOnOk2))
          allSizeGaps
        }
        
        combineCriteria <- function(allSizeGaps, allSpecGaps) {
          nn <- union(allSizeGaps, allSpecGaps)
          if (length(nn) > 0) {
            nn <- sort(nn)
          } else {
            nn <- NULL
          }
          nn
        }
        
        if (is.finite(selMiniSize)) {
          if (selMiniSize < 1) {
            selMiniSize <- 1
          }
        }
        if (is.finite(selMaxiSize)) {
          if (selMaxiSize < 1) {
            selMaxiSize <- nrow(selTs)
          }
        }
        gapsIndeces <- getNAsGaps(selTs$value)
        lengthGaps <- gapsIndeces[, 2] - gapsIndeces[, 1] + 1
        res <- gatherSpefGaps(gapsIndeces, specificGaps, criterionSpec)
        allSpecGaps <- res$allSpecGaps
        txtSpeGapTable <- res$txtSpeGapTable
        allSizeGaps <- gatherGapsbySize(gapsIndeces, criterionMin, selMiniSize, 
                                        criterionMax, selMaxiSize, lengthGaps)
        if (length(allSizeGaps) == 0) {
          allSizeGaps = NULL
        }
        if (length(allSpecGaps) == 0) {
          allSpecGaps = NULL
        }
        totalGaps <- combineCriteria(allSizeGaps, allSpecGaps)
        
        if (length(totalGaps) > 0) {
          MMN <- list(gaps = totalGaps, tsIni = as.character(selTs$time[1]), 
                      tsEnd = as.character(selTs$time[nrow(selTs)]), 
                      samPerMin = diff(as.numeric(selTs$time[1:2]))/60, 
                      tsLength = nrow(selTs), tsName = KTSEnv$selTsName)
          assign(selNewName, MMN, KTSEnv)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste("NEW SET OF GAPS:", selNewName, 
                                collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", date())
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste("Original time series:",KTSEnv$selTsName, 
                                collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste("The gap set contains", 
                                length(totalGaps), "NAs", collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste("Selected gaps:", collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste("   By length:", txt1, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          if (is.null(txtSpeGapTable) == FALSE) {
            tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                            paste("   Specific gaps:", collapse = "\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                            paste(txtSpeGapTable, collapse = "\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          }
          endingLines()
        } else {
          tcltk::tkmessageBox(message = paste("The set of gaps was empty"), 
                              icon = "warning")
        }
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANselGap1()
      }
    }
  }
  selGapOnOk1 <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = paste("You must choose at least one",
                                          "time series and one criterion"), 
                          icon = "warning")
    } else {
      selTs <- get(selTsName, envir = KTSEnv)
      if (all(is.finite(selTs$value))) {
        tcltk::tkmessageBox(message = paste("The selected time",
                                            "series has no NAs"), 
                            icon = "warning")
        showPANselGap1()
      } else {
        tabla <- getSamPerTable.1Freq(selTs, 
                                      diff(as.numeric(selTs$time[1:2])))
        tablahuecos <- tabla[which(tabla$type == "NAs"), 1:4]
        rownames(tablahuecos) <- NULL
        txt <- utils::capture.output(print.data.frame(tablahuecos))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                        "GAP SELECTION", collapse = "\n")
        tcltk::tkinsert(KTSEnv$txtWidget, "end", date(), collapse = "\n")
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("Gaps in", selTsName, 
                                                       collapse = "\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                        paste0("Check the indices corresponding ",
                               "to the gaps you want to select"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste(txt, collapse = "\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
        endingLines()
        assign("selTsName", selTsName, envir = KTSEnv)
        showPANselGap2()
      }
    }
  }
  showPANselGap1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "GAP SELECTION")
    createTsRb()
    createOK(labTitle = "NEXT", action = selGapOnOk1)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  showPANselGap2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "GAP SELECTION")
    createNote(labTitle = paste("Time series:", KTSEnv$selTsName), 
               pady = c(5,2))
    createTitle(labTitle = "Criteria")
    createChb(labTitle = "All gaps", variableName = "cri5cbValue")
    createChb(labTitle = "Minimum length", variableName = "cri1cbValue")
    createEntry(labTitle = "", textVariableName = "miniSize", 
                font = KTSEnv$KTSFonts$normal)
    createChb(labTitle = "Maximum length", variableName = "cri2cbValue")
    createEntry(labTitle = "", textVariableName = "maxiSize", 
                font = KTSEnv$KTSFonts$normal)
    createChb(labTitle = "Specific gaps", variableName = "cri4cbValue")
    createEntry(labTitle = "     Indices", textVariableName = "specGap", 
                font = KTSEnv$KTSFonts$normal)
    createEntry(labTitle = "Name", textVariableName = "newName")
    createOK(labTitle = "RUN", action = selGapOnOk2)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANselGap1", 
               envirName = environment(showPANselGap1))
}

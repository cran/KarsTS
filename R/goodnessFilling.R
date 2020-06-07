goodnessFilling <-
function() {
  showPANgoodFill1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "CHECK FILLING")
    createTsRb(labTitle = "Time series with no artificial gaps", 
               variableName = "selTsP0")
    createOK(labTitle = "NEXT", action = goodFill1OnOk)
    createNote(labTitle = "Only the filling of artificial gaps can be checked")
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  goodFill1OnOk <- function() {
    tsWithNoGapsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP0), 
                                        noValid = NA)
    if (is.na(tsWithNoGapsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else {
      assign("tsWithNoGapsName", tsWithNoGapsName, envir = KTSEnv)
      showPANgoodFill2()
    }
  }
  showPANgoodFill2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "CHECK FILLING")
    createTsRb(labTitle = "Time series with artificial gaps")
    createOK(labTitle = "NEXT", action = goodFill2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  goodFill2OnOk <- function() {
    tsWithGapsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                      noValid = NA)
    if (is.na(tsWithGapsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else {
      assign("tsWithGapsName", tsWithGapsName, envir = KTSEnv)
      showPANgoodFill3()
    }
  }
  showPANgoodFill3 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "CHECK FILLING")
    createTsRb(labTitle = "Time series after the filling", 
               variableName = "selTsP1")
    createOK(labTitle = "RUN", action = goodFill3OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  goodFill3OnOk <- function() {
    
    tsFilledName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP1), 
                                    noValid = NA)
    if (is.na(tsFilledName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else {
      tsFilled <- get(tsFilledName, envir = KTSEnv)
      tsWithGaps <- get(KTSEnv$tsWithGapsName, envir = KTSEnv)
      tsWithNoGaps <- get(KTSEnv$tsWithNoGapsName, envir = KTSEnv)
      initialDates <- as.numeric(c(tsWithNoGaps$time[1], tsWithGaps$time[1], 
                                   tsFilled$time[1]))
      samPer <- c(diff(as.numeric(tsWithNoGaps$time[1:2])), 
                  diff(as.numeric(tsWithGaps$time[1:2])), 
                  diff(as.numeric(tsFilled$time[1:2])))
      tsWNoGNasInd <- which(is.na(tsWithNoGaps$value))
      tsWGNasInd <- which(is.na(tsWithGaps$value))
      tsFilledNasInd <- which(is.na(tsFilled$value))
      if (any(initialDates != initialDates[1])) {
        tcltk::tkmessageBox(message = paste("The selected time series have",
                                            "different initial dates"), 
                            icon = "warning")
        showPANgoodFill1()
      } else if (any(samPer != samPer[1])) {
        tcltk::tkmessageBox(message = paste("The selected time series have",
                                            "different sampling periods"), 
                            icon = "warning")
        showPANgoodFill1()
      } else if (is.null(tsWGNasInd)) {
        tcltk::tkmessageBox(message = paste(tsWithGapsName, "has no gaps.",
                                            "It should have artifical gaps"), 
                            icon = "warning")
        showPANgoodFill1()
      } else if (length(intersect(tsWGNasInd, 
                                  tsFilledNasInd)) == length(tsWGNasInd)) {
        tcltk::tkmessageBox(message = paste(tsWithGapsName, "and", 
                                            tsFilledName, 
                                            "have the same gaps."), 
                            icon = "warning")
      } else {
        artificialGaps <- setdiff(tsWGNasInd, tsWNoGNasInd)
        notFilledArtGaps <- intersect(artificialGaps, tsFilledNasInd)
        filledArtGaps <- setdiff(artificialGaps, notFilledArtGaps)
        observed <- tsWithNoGaps$value[filledArtGaps]
        predicted <- tsFilled$value[filledArtGaps]
        if (length(observed) != length(predicted)) {
          tcltk::tkmessageBox(message = paste("The observed and predicted",
                                              "values have different lengths"), 
                              icon = "warning")
        } else if (any(is.infinite(observed)) | any(is.nan(observed))) {
          tcltk::tkmessageBox(message = paste("Some observed values are",
                                              "Inf,-Inf or NaN"), 
                              icon = "warning")
        } else if (any(is.infinite(predicted)) | any(is.nan(predicted))) {
          tcltk::tkmessageBox(message = paste("Some predicted values are",
                                              "Inf,-Inf or NaN"), 
                              icon = "warning")
        } else {
          completeCasesInd <- which(stats::complete.cases(observed, predicted))
          if (length(completeCasesInd) < 4) {
            tcltk::tkmessageBox(message = paste("There are less than four",
                                                "complete cases"), 
                                icon = "warning")
          } else {
            observed <- observed[completeCasesInd]
            predicted <- predicted[completeCasesInd]
            lmObsPred <- myLinModel(observed, predicted)
            if (class(lmObsPred) == "character") {
              tcltk::tkmessageBox(message = lmObsPred, icon = "warning")
            } else {
              filledGapsTable <- groupDates(filledArtGaps, tsFilled)
              txt1 <- "CHECK FILLING"
              txt21 <- paste("Time series with no artificial gaps:", 
                             KTSEnv$tsWithNoGapsName)
              txt22 <- paste("Time series with artificial gaps:", 
                             KTSEnv$tsWithGapsName)
              txt23 <- paste("Filled time series:", tsFilledName)
              mvdmd <- as.data.frame(lmObsPred)
              txt4 <- utils::capture.output(print.data.frame(mvdmd))
              txt5 <- "Gaps filled:"
              txt6 <- utils::capture.output(print.data.frame(filledGapsTable))
              txt0 <- c(txt1, txt21, txt22, txt23)
              tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                              paste(txt0, collapse = "\\n"))
              tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n\\n"))
              tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                              paste(txt4, collapse = "\\n"))
              tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n\\n"))
              tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                              paste(txt5, collapse = "\\n"))
              tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n\\n"))
              tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                              paste(txt6, collapse = "\\n"))
              tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n\\n"))
              
            }
          }
        }
      }
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      showPANgoodFill1()
    }
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANgoodFill1", 
               envirName = environment(showPANgoodFill1))
}

littleTest <-
function() {
  rand1OnOk <- function() {
    tssel <- tsCheckedTF()
    if (length(which(tssel == TRUE)) < 2) {
      tcltk::tkmessageBox(message = "Choose at least two time series", 
                          icon = "warning")
    } else {
      tsToTestNames <- KTSEnv$dSList$TS[which(tssel == TRUE)]
      nTS <- length(tsToTestNames)
      tmComptibility <- matrix(rep(FALSE, 3 * nTS), nTS, 3)
      for (i in 2:nTS) {
        tmComptibility[i, ] <- are2TsTimeCompatible(get(tsToTestNames[1], 
                                                           envir = KTSEnv), 
                                                       get(tsToTestNames[i], 
                                                           envir = KTSEnv))
      }
      tmComptibility <- tmComptibility[-1, ]
      if (class(tmComptibility) == "logical") {
        tmComptibility <- as.matrix(t(tmComptibility))
      }
      if (any(tmComptibility[, 1] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The initial date of all",
                                            "the time series",
                                            "must be the same"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 2] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The sampling period of",
                                            "all the time series",
                                            "must be the same"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 3] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The final date of all",
                                            "the time series",
                                            "must be the same"), 
                            icon = "warning")
      } else {
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        tsToTest <- get(tsToTestNames[1], envir = KTSEnv)$value
        for (i in tsToTestNames[2:nTS]) {
          tsToTest <- cbind(tsToTest, get(i, envir = KTSEnv)$value)
        }
        colnames(tsToTest) <- tsToTestNames
        resTest <- try(BaylorEdPsych::LittleMCAR(tsToTest), silent = TRUE)
        if (class(resTest) == "try-error") {
          tcltk::tkmessageBox(message = paste("The Little's",
                                              "MCAR test failed"), 
                              icon = "warning")
        } else {
          writeMethodTitle("LITTLE'S MCAR TEST")
          txtLit1 <- c("Time series:", paste(tsToTestNames, collapse = ","), 
                       "Null hypothesis:missing completely at random", 
                       paste("P-value=",round(resTest$p.value, 2)), 
                       paste("Number of patterns=", 
                             round(resTest$missing.patterns, 0)))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtLit1, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          
          adaef <- round(as.data.frame(resTest$amount.missing),2)
          txtLit2 <- utils::capture.output(print.data.frame(adaef))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtLit2, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          
          tsTime <- get(tsToTestNames[1], envir = KTSEnv)$time
          
          tcltk::tkinsert(KTSEnv$txtWidget, "end", "Means of the patterns")
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          for (i in 1:length(resTest$data)) {
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("Pattern", i))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
            summaryN <- as.data.frame(t(colMeans(as.matrix(resTest$data[[i]]), 
                                                 na.rm = TRUE)))
            txtLit4 <- utils::capture.output(print.data.frame(summaryN))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                            paste(txtLit4, collapse = "\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
            rm(txtLit4, summaryN)
          }
          
          tcltk::tkinsert(KTSEnv$txtWidget, "end", "Dates of the patterns")
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          for (i in 1:length(resTest$data)) {
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("Pattern", i))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
            grIndices <- groupIndices(as.numeric(rownames(resTest$data[[i]])))
            tableN <- data.frame(tsTime[grIndices[, 1]], 
                                 tsTime[grIndices[,2]], grIndices[, 3])
            colnames(tableN) <- c("Initial", "Final", "Length")
            txtLit3 <- utils::capture.output(print.data.frame(tableN))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                            paste(txtLit3, collapse = "\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
            rm(txtLit3, grIndices, tableN)
          }
          
          endingLines()
        }
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        showPANrand1()
      }
    }
  }
  showPANrand1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "LITTLE'S MCAR TEST")
    createTsChb()
    createOK(labTitle = "RUN", action = rand1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANrand1", 
               envirName = environment(showPANrand1))
}

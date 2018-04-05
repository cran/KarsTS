anaSamPer <-
function() {
  showPANanaSP1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "SAMPLING PERIODS")
    createTsRb()
    createOK(labTitle = "NEXT", action = anaSPOnOk1)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  anaSPOnOk1 <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                 noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    } else {
      selTs <- get(selTsName, envir = KTSEnv)
      assign("selTsName", selTsName, envir = KTSEnv)
      assign("selTs", selTs, envir = KTSEnv)
      assign("UF", getUniqueSampPer(selTs), KTSEnv)
      showPANanaSP2()
    }
  }
  showPANanaSP2 <- function() {
    nuf <- NROW(KTSEnv$UF)
    assign("nuf", nuf, envir = KTSEnv)
    etis <- paste0(as.character(KTSEnv$UF[, 2]), 
                   rep(" (", KTSEnv$nuf), 
                   as.character(KTSEnv$UF[,1]), 
                   rep(" times)", KTSEnv$nuf))
    createSubPanR4C1()
    createTITLE(labTitle = "SAMPLING PERIODS")
    createOK(labTitle = "RUN", action = anaSPOnOk2)
    createTitle(labTitle = "Possible sampling periods (secs)")
    for (ind in 1:KTSEnv$nuf) {
      createChb(labTitle = etis[ind], 
                variableName = paste0("ccbValue", ind))
    }
    
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  anaSPOnOk2 <- function() {
    selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
    fsel <- rep(FALSE, KTSEnv$nuf)
    for (ind in 1:KTSEnv$nuf) {
      ccbValueind <- get(paste0("ccbValue", ind), envir = KTSEnv)
      if (tcltk::tclvalue(ccbValueind) == "1") {
        fsel[ind] <- TRUE
      }
      rm(ccbValueind)
    }
    if (all(fsel == FALSE)) {
      tcltk::tkmessageBox(message = paste("Choose, at least,", 
                                          "one candidate"), 
                          icon = "warning")
    } else {
      checkedSP <- KTSEnv$UF[which(fsel == TRUE), 2]
      names(checkedSP) = NULL
      if (length(checkedSP) == 1) {
        tabla <- getSamPerTable.1Freq(selTs, checkedSP)
      } else {
        tabla <- getSamPerTable(selTs, checkedSP)
      }
      txt <- utils::capture.output(print.data.frame(tabla))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", "SAMPLING PERIODS")
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", date())
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      paste("Sampling period analysis for", 
                            KTSEnv$selTsName, collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste(txt, collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
      endingLines()
      cleanEnvir()
      showPANanaSP1()
    }
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANanaSP1", 
               envirName = environment(showPANanaSP1))
}

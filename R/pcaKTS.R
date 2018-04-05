pcaKTS <-
function() {
  pcaOnOk <- function() {
    tssel <- tsCheckedTF()
    selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                  noValid = NA)
    if (length(which(tssel == TRUE)) < 2) {
      tcltk::tkmessageBox(message = paste("Choose at least two time series"),
                          icon = "warning")
    } else if (is.na(selNewName)) {
      tcltk::tkmessageBox(message = "Introduce a name", icon = "warning")
    } else {
      tsToPCA <- KTSEnv$dSList$TS[which(tssel == TRUE)]
      varNumber <- length(tsToPCA)
      firstTs <- get(tsToPCA[1], envir = KTSEnv)
      tmComptibility <- matrix(NA, varNumber, 3)
      for (i in 2:varNumber) {
        tmComptibility[i, ] <- are2TsTimeCompatible(firstTs, 
                                                       get(tsToPCA[i],
                                                           envir = KTSEnv))
      }
      tmComptibility <- tmComptibility[-1, ]
      if (is.vector(tmComptibility)) {
        tmComptibility <- as.matrix(t(tmComptibility))
      }
      if (any(tmComptibility[, 1] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The initial date of all the",
                                            "time series must be the same"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 2] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The sampling period of all the",
                                            "time series must be the same"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 3] == FALSE)) {
        tcltk::tkmessageBox(message = paste("All time series must",
                                            "have the same length"), 
                            icon = "warning")
      } else {
        matrixToPCA <- NULL
        for (i in tsToPCA) {
          matrixToPCA <- cbind(matrixToPCA, get(i, envir = KTSEnv)$value)
        }
        matrixToPCA <- myScale(matrixToPCA, outputType = "outNo")
        complCasesInd <- which(stats::complete.cases(matrixToPCA))
        matrixToPCA <- matrixToPCA[complCasesInd, ]
        PCAresult <- try(stats::prcomp(x = matrixToPCA, 
                                       retx = TRUE, center = FALSE, 
                                       scale. = FALSE), silent = TRUE)
        if (class(PCAresult) == "try-error") {
          tcltk::tkmessageBox(message = paste("It was not possible to",
                                              "perform a PCA on",
                                              "the selected data "), 
                              icon = "warning")
        } else {
          writePCAResult <- function(PCAresult, tsToPCA) {
            varResult <- as.data.frame(unclass(summary(PCAresult))$importance)
            loadResult <- as.data.frame(PCAresult$rotation)
            row.names(loadResult) <- tsToPCA
            txt2 <- utils::capture.output(print.data.frame(varResult))
            txt4 <- utils::capture.output(print.data.frame(loadResult))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                            "PRINCIPAL COMPONENT ANALYSIS")
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", date())
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                            "  Importance of the components")
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                            paste(txt2, collapse = "\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", "  Loadings")
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                            paste(txt4, collapse = "\n"))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
            endingLines()
          }
          createCompoTs <- function(PCAresult, firstTs, 
                                    complCasesInd, selNewName) {
            scoresResult <- matrix(NA, nrow(firstTs), ncol(PCAresult$x))
            scoresResult[complCasesInd, ] <- PCAresult$x
            timeTs <- firstTs$time
            for (i in 1:ncol(scoresResult)) {
              assign(paste0(selNewName, "PC", i), 
                     data.frame(time = timeTs,value = scoresResult[, i]), 
                     envir = KTSEnv)
            }
          }
          writePCAResult(PCAresult, tsToPCA)
          createCompoTs(PCAresult, firstTs, complCasesInd, selNewName)
        }
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANpca()
      }
    }
  }
  showPANpca <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "PRINCIPAL COMPONENT ANALYISIS")
    createTsChb()
    createEntry(labTitle = "Name", textVariableName = "newName")
    createOK(labTitle = "RUN", action = pcaOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANpca", envirName = environment(showPANpca))
}

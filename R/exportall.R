exportall <-
function() {
  exportOnOk <- function() {
    selCommonName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$commonName), 
                                     noValid = NA)
    tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
    filety <- tcltk::tclvalue(KTSEnv$tiparch)
    if (filety == ".csv") {
      sep <- ","
    } else if (filety == ".txt") {
      sep <- " "
    }
    if (is.null(KTSEnv$dSList$nTS) == FALSE) {
      tssel <- tsCheckedTF()
      if (any(tssel == TRUE)) {
        
        tsToExpNames <- KTSEnv$dSList$TS[which(tssel == TRUE)]
        nTS <- length(tsToExpNames)
        if (is.na(selCommonName) | nTS == 1) {
          export1TS <- function(TsName, filety, sep) {
            TsToExport <- get(TsName, envir = KTSEnv)
            TsToExport <- TsToExport[which(is.finite(TsToExport$value)), 
                                     ]
            TsToExport$time <- as.character(TsToExport$time)
            fileName <- paste0(TsName, filety)
            write(TsName, file = fileName, ncolumns = ncol(TsToExport), 
                  append = FALSE, sep = sep)
            if (filety == ".csv") {
              write(t(c("time", TsName)), file = fileName, 
                    ncolumns = ncol(TsToExport), 
                    append = TRUE, sep = sep)
            } else {
              write(t(c("date", "time", TsName)), file = fileName, 
                    ncolumns = ncol(TsToExport) + 
                      1, append = TRUE, sep = sep)
            }
            write(t(TsToExport), file = fileName, ncolumns = ncol(TsToExport), 
                  append = TRUE, sep = sep)
          }
          apply(as.matrix(tsToExpNames), 1, FUN = export1TS, filety = filety, 
                sep = sep)
        } else {
          tmComptibility <- matrix(rep(FALSE, 3 * nTS), nTS, 3)
          for (i in 2:nTS) {
            tmComptibility[i, ] <- are2TsTimeCompatible(get(tsToExpNames[1], 
                                                               envir = KTSEnv), 
                                                           get(tsToExpNames[i], 
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
            TsToExport <- get(tsToExpNames[1], envir = KTSEnv)
            for (i in tsToExpNames[2:nTS]) {
              TsToExport <- cbind(TsToExport, get(i, envir = KTSEnv)$value)
            }
            colnames(TsToExport) <- c("time", tsToExpNames)
            compCases <- stats::complete.cases(TsToExport[, -1])
            TsToExport <- TsToExport[compCases, ]
            TsToExport$time <- as.character(TsToExport$time)
            fileName <- paste0(selCommonName, filety)
            write(selCommonName, file = fileName, ncolumns = ncol(TsToExport), 
                  append = FALSE, sep = sep)
            if (filety == ".csv") {
              write(t(c("time", tsToExpNames)), file = fileName,
                    ncolumns = ncol(TsToExport), 
                    append = TRUE, sep = sep)
            } else {
              write(t(c("date", "time", tsToExpNames)), file = fileName, 
                    ncolumns = ncol(TsToExport) + 1, append = TRUE, sep = sep)
            }
            write(t(TsToExport), file = fileName, ncolumns = ncol(TsToExport), 
                  append = TRUE, sep = sep)
          }
          
          
        }
        
      }
    }
    if (is.null(KTSEnv$dSList$nGaps) == FALSE) {
      gsel <- gapCheckedTF()
      if (any(gsel == TRUE)) {
        export1GS <- function(GsName, filety, sep) {
          GsToExport <- get(GsName, envir = KTSEnv)
          fileName <- paste0(GsName, filety)
          write(GsName, file = fileName, ncolumns = 1, 
                append = FALSE, sep = sep)
          if (filety == ".csv") {
            write(t(names(GsToExport)), file = fileName, 
                  ncolumns = 6, append = TRUE, 
                  sep = sep)
          } else {
            write(t(c(names(GsToExport[1]), "ini_day", "ini_hour", "fin_day", 
                      "fin_hour", names(GsToExport[4:6]))), 
                  file = fileName, ncolumns = 8, 
                  append = TRUE, sep = sep)
          }
          write(c(GsToExport[[1]][1], GsToExport[[2]], GsToExport[[3]], 
                  GsToExport[[4]], 
                  GsToExport[[5]], GsToExport[[6]]), 
                file = fileName, ncolumns = 6, 
                append = TRUE, sep = sep)
          write(GsToExport[[1]][2:length(GsToExport[[1]])], file = fileName, 
                ncolumns = 1, append = TRUE, sep = sep)
        }
        gapsToExpNames <- KTSEnv$dSList$gaps[which(gsel == TRUE)]
        apply(as.matrix(gapsToExpNames), 1, FUN = export1GS, filety = filety, 
              sep = sep)
      }
    }
    if (is.null(KTSEnv$dSList$nRM) == FALSE) {
      rsel <- rmCheckedTF()
      if (any(rsel == TRUE)) {
        export1RM <- function(RmName, filety, sep) {
          RmToExport <- get(RmName, envir = KTSEnv)
          fileName <- paste0(RmName, filety)
          nCompRows <- length(RmToExport$tsName)
          if (filety == ".csv") {
            writeCompletecols <- function(nCompRows, RmToExport, RmName, 
                                          fileName, sep) {
              compBlock <- data.frame(matrix(NA, nCompRows, 10))
              compBlock[, 1] <- RmToExport$ones[1:nCompRows, 1]
              compBlock[, 2] <- RmToExport$ones[1:nCompRows, 2]
              for (kl in 2:9) {
                compBlock[, kl + 1] <- RmToExport[[kl]]
              }
              compBlock[1, 11] <- RmToExport$type
              colnames(compBlock) <- c("X", "Y", names(RmToExport)[2:10])
              write(paste0(RmName), ncolumns = 1, append = FALSE, sep = sep, 
                    file = fileName)
              utils::write.table(compBlock, file = fileName, na = "", 
                                 col.names = TRUE, 
                                 row.names = FALSE, append = TRUE, sep = sep)
            }
            writeIncompletecols <- function(nCompRows, RmToExport, RmName, 
                                            fileName, sep) {
              mlhs <- (nCompRows + 1):nrow(RmToExport$ones)
              incompBlock <- data.frame(RmToExport$ones[mlhs, ])
              utils::write.table(incompBlock, file = fileName, na = "", 
                                 col.names = FALSE, 
                                 row.names = FALSE, append = TRUE, 
                                 sep = sep)
            }
            writeCompletecols(nCompRows, RmToExport, RmName, fileName, sep)
            writeIncompletecols(nCompRows, RmToExport, RmName, fileName, 
                                sep)
          } else {
            writeCompletecols <- function(nCompRows, RmToExport, RmName, 
                                          fileName, sep) {
              compBlock <- data.frame(matrix(NA, nCompRows, 11))
              compBlock[, 1] <- RmToExport$ones[1:nCompRows, 1]
              compBlock[, 2] <- RmToExport$ones[1:nCompRows, 2]
              for (kl in 2:10) {
                compBlock[, kl + 1] <- RmToExport[[kl]]
              }
              compBlock[1, 11] <- RmToExport$type
              colnames(compBlock) <- c("X", "Y", names(RmToExport)[2:10])
              write(paste0(RmName), ncolumns = 1, append = FALSE, sep = sep, 
                    file = fileName)
              write(t(c(colnames(compBlock)[1:9], "ini_day", "ini_hour", 
                        colnames(compBlock)[11])), file = fileName, 
                    ncolumns = 12, 
                    append = TRUE, sep = sep)
              utils::write.table(compBlock, file = fileName, na = "", 
                                 col.names = FALSE, 
                                 quote = FALSE, row.names = FALSE, 
                                 append = TRUE, sep = sep)
            }
            writeIncompletecols <- function(nCompRows, RmToExport, RmName, 
                                            fileName, sep) {
              
              ednd <- (nCompRows + 1):nrow(RmToExport$ones)
              incompBlock <- data.frame(RmToExport$ones[ednd, ])
              utils::write.table(incompBlock, file = fileName, na = "", 
                                 col.names = FALSE, quote = FALSE, 
                                 row.names = FALSE, append = TRUE, sep = sep)
            }
            writeCompletecols(nCompRows, RmToExport, RmName, fileName, sep)
            writeIncompletecols(nCompRows, RmToExport, RmName, fileName, 
                                sep)
          }
        }
        rmsToExpNames <- KTSEnv$dSList$rm[which(rsel == TRUE)]
        apply(as.matrix(rmsToExpNames), 1, FUN = export1RM, filety = filety, 
              sep = sep)
      }
    }
    tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    showPANexport()
  }
  showPANexport <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "EXPORT")
    if (class(KTSEnv$dSList$TS) == "character") {
      createTsChb()
      createTitle(labTitle = "Export checked time series to the same file")
      createEntry(labTitle = "Name", textVariableName = "commonName")
    }
    if (class(KTSEnv$dSList$gaps) == "character") {
      createGapChb()
    }
    if (class(KTSEnv$dSList$rm) == "character") {
      createRmChb()
    }
    createTitle(labTitle = "Type of file")
    tiparch <- tcltk::tclVar(".csv")
    assign("tiparch", tiparch, envir = KTSEnv)
    createRb(variable = KTSEnv$tiparch, dataVector = c(".csv", ".txt"))
    createOK(labTitle = "RUN", action = exportOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAny(action = "showPANexport", envirName = environment(showPANexport))
}

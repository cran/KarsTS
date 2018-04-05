renameAllTypes <-
function() {
    renameOnOk <- function() {
        if (is.null(KTSEnv$dSList$nTS) == FALSE) {
            tssel <- tsCheckedTF()
            newNameTs <- readMultEntryvalues(KTSEnv$dSList$nTS, 
                                             prefix = "tEntscbValue")
            newNameTsNAInd <- which(is.na(newNameTs))
            cisc <- newNameTsNAInd
            newNameTs[cisc] <- paste0(KTSEnv$dSList$TS[cisc],"_2")
        } else {
            tssel <- NULL
            newNameTs <- NULL
        }
        if (is.null(KTSEnv$dSList$nGaps) == FALSE) {
            gsel <- gapCheckedTF()
            newNameGap <- readMultEntryvalues(KTSEnv$dSList$nGaps, 
                                              prefix = "tEntgcbValue")
            newNameGapNAInd <- which(is.na(newNameGap))
            misq <- newNameGapNAInd
            newNameGap[misq] <- paste0(KTSEnv$dSList$gaps[misq],"_2")
        } else {
            gsel <- NULL
            newNameGap <- NULL
        }
        if (is.null(KTSEnv$dSList$nRM) == FALSE) {
            rsel <- rmCheckedTF()
            newNameRm <- readMultEntryvalues(KTSEnv$dSList$nRM, 
                                             prefix = "tEntrcbValue")
            newNameRmNAInd <- which(is.na(newNameRm))
            smoak <- newNameRmNAInd
            newNameRm[smoak] <- paste0(KTSEnv$dSList$rm[newNameRmNAInd],"_2")
        } else {
            rsel <- NULL
            newNameRm <- NULL
        }
        toRenameInd <- which(c(tssel, gsel, rsel) == TRUE)
        if (length(toRenameInd) == 0) {
            tcltk::tkmessageBox(message = "Select at least a data set", 
                                icon = "warning")
        } else {
            toRename <- c(KTSEnv$dSList$TS, KTSEnv$dSList$gaps, 
                          KTSEnv$dSList$rm)[toRenameInd]
            newName <- c(newNameTs, newNameGap, newNameRm)[toRenameInd]
            for (i in 1:length(toRename)) {
                assign(newName[i], get(toRename[i], envir = KTSEnv), 
                       envir = KTSEnv)
            }
            cleanEnvir()
            refreshDataSetsList(outp = FALSE)
            showPANrename()
        }
    }
    showPANrename <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "RENAME")
        if (class(KTSEnv$dSList$TS) == "character") {
            createTitle(labTitle = "Time series")
            myApplyVector(FUN = createChbEntry, 1:KTSEnv$dSList$nTS, 
                          elements = KTSEnv$dSList$TS, 
                prefix = "scbValue")
        }
        if (class(KTSEnv$dSList$gaps) == "character") {
            createTitle(labTitle = "Gap sets")
            myApplyVector(FUN = createChbEntry, 1:KTSEnv$dSList$nGaps, 
                          elements = KTSEnv$dSList$gaps, 
                prefix = "gcbValue")
        }
        if (class(KTSEnv$dSList$rm) == "character") {
            createTitle(labTitle = "Recurrence matrices")
            myApplyVector(FUN = createChbEntry, 1:KTSEnv$dSList$nRM, 
                          elements = KTSEnv$dSList$rm, 
                prefix = "rcbValue")
        }
        createOK(labTitle = "RUN", action = renameOnOk)
        createNote(labTitle = "Each name defaults to yourdataname_2", 
                   pady = c(15,0))
        createNote(labTitle = "The existent data set will not be overwritten", 
                   pady = c(0,0))
        createNote(labTitle = "Only the checked data sets will be renamed", 
                   pady = c(0,15))
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAny(action = "showPANrename", 
               envirName = environment(showPANrename))
}

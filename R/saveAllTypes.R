saveAllTypes <-
function() {
    saveOnOk <- function() {
        placeInSvEnvir <- function(dSToPlaceNames) {
            placeOneDataSet <- function(X) {
                assign(X, get(X, envir = KTSEnv), 
                       envir = environment(placeInSvEnvir))
            }
            apply(as.matrix(dSToPlaceNames), 1, FUN = placeOneDataSet)
        }
        changeTsColnames <- function(selectedTSNames) {
            for (X in selectedTSNames) {
                tsToChange <- get(X, envir = KTSEnv)
                colnames(tsToChange)[2] <- X
                assign(X, tsToChange, envir = environment(changeTsColnames))
            }
        }
        refreshDataSetsList(outp = FALSE)
        selCommonName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$commonName), 
                                         noValid = NA)
        if (is.null(KTSEnv$dSList$nTS) == FALSE) {
            tssel <- tsCheckedTF()
            tsselType <- rep("ts", length(tssel))
        } else {
            tssel <- NULL
            tsselType <- NULL
        }
        if (is.null(KTSEnv$dSList$nGaps) == FALSE) {
            gsel <- gapCheckedTF()
            gselType <- rep("gap", length(gsel))
        } else {
            gsel <- NULL
            gselType <- NULL
        }
        if (is.null(KTSEnv$dSList$nRM) == FALSE) {
            rsel <- rmCheckedTF()
            rselType <- rep("rm", length(rsel))
        } else {
            rsel <- NULL
            rselType <- NULL
        }
        isSelected <- c(tssel, gsel, rsel)
        types <- c(tsselType, gselType, rselType)
        dsSelected <- which(isSelected == TRUE)
        if (length(dsSelected) == 0) {
            tcltk::tkmessageBox(message = "Select some data set", 
                                icon = "warning")
        } else {
            allDataSets <- c(KTSEnv$dSList$TS, KTSEnv$dSList$gaps,
                             KTSEnv$dSList$rm)
            dsToSaveName <- allDataSets[dsSelected]
            types <- types[dsSelected]
            placeInSvEnvir(dsToSaveName)
            changeTsColnames(dsToSaveName[which(types == "ts")])
            if (is.na(selCommonName)) {
                for (X in 1:length(dsToSaveName)) {
                  save(list = dsToSaveName[X], 
                       file = paste0(dsToSaveName[X], ".R"))
                }
            } else {
                save(list = dsToSaveName, file = paste0(selCommonName, ".R"))
            }
            cleanEnvir()
            tcltk::tkmessageBox(message = "Objects saved", icon = "warning")
            showPANsave()
        }
    }
    showPANsave <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "SAVE")
        if (class(KTSEnv$dSList$TS) == "character") {
            createTsChb()
        }
        if (class(KTSEnv$dSList$gaps) == "character") {
            createGapChb()
        }
        if (class(KTSEnv$dSList$rm) == "character") {
            createRmChb()
        }
        createTitle(labTitle = "Save checked data sets to the same file")
        createEntry(labTitle = "Name", textVariableName = "commonName")
        createOK(labTitle = "RUN", action = saveOnOk)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAny(action = "showPANsave", envirName = environment(showPANsave))
}

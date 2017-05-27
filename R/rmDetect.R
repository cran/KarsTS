rmDetect <-
function() {
    isTrueRm <- function(oneListName) {
        oneList <- get(oneListName, envir = KTSEnv)
        listNames <- names(oneList)
        if (is.null(listNames)) {
            result <- NA
        } else if (length(listNames) != 10) {
            result <- NA
        } else if (listNames[1] == "ones" & listNames[2] == "tol" 
                   & listNames[3] == "tsName" & listNames[4] == "embDim" 
                   & listNames[5] == "delay" & listNames[6] == "dist" 
                   & listNames[7] == "tsLength" & listNames[8] == "samPerSec" 
                   & listNames[9] == "tsIni" & listNames[10] == "type") {
          result <- oneListName
        } else {
            result <- NA
        }
        result
    }
    listsInGE <- getClassEnvir(classGet = "list", envir = KTSEnv)
    if (length(listsInGE) == 0) {
        loadedRms <- NULL
    } else {
        loadedRms <- apply(as.matrix(listsInGE), 1, FUN = isTrueRm)
        loadedRms <- loadedRms[which(is.na(loadedRms) == FALSE)]
        if (length(loadedRms) == 0) {
            loadedRms <- NULL
        }
    }
    loadedRms
}

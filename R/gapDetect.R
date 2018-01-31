gapDetect <-
function() {
  isTrueGap <- function(oneListName) {
    oneList <- get(oneListName, envir = KTSEnv)
    listNames <- names(oneList)
    if (is.null(listNames)) {
      result <- NA
    } else if (length(listNames) != 6) {
      result <- NA
    } else if (listNames[1] == "gaps" & listNames[2] == "tsIni" 
               & listNames[3] == "tsEnd" & listNames[4] == "samPerMin" 
               & listNames[5] == "tsLength" & listNames[6] == "tsName") {
      result <- oneListName
    } else {
      result <- NA
    }
    result
  }
  listsInGE <- getClassEnvir(classGet = "list", envir = KTSEnv)
  if (length(listsInGE) == 0) {
    loadedGaps <- NULL
  } else {
    loadedGaps <- apply(as.matrix(listsInGE), 1, FUN = isTrueGap)
    loadedGaps <- loadedGaps[which(is.na(loadedGaps) == FALSE)]
    if (length(loadedGaps) == 0) {
      loadedGaps <- NULL
    }
  }
  loadedGaps
}

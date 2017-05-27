gapForSelMethod <-
function(selTsName, selTs, envir = KTSEnv) {
  if (exists("selGapP", envir = KTSEnv) == FALSE) {
    selGapName <- paste0(selTsName, "All")
    selGap <- list(gaps = which(is.na(selTs$value)), 
                   tsIni = as.character(selTs$time[1]), 
                   tsEnd = selTs$time[nrow(selTs)], 
                   samPerMin = diff(as.numeric(selTs$time[1:2]))/60, 
                   tsLength = nrow(selTs), tsName = selTsName)
  } else {
    selGapName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selGapP), 
                                  noValid = NA)
    if (is.na(selGapName)) {
      selGapName <- paste0(selTsName, "All")
      selGap <- list(gaps = which(is.na(selTs$value)), 
                     tsIni = as.character(selTs$time[1]), 
                     tsEnd = selTs$time[nrow(selTs)], 
                     samPerMin = diff(as.numeric(selTs$time[1:2]))/60, 
                     tsLength = nrow(selTs), tsName = selTsName)
    } else {
      selGap <- get(selGapName, envir = envir)
    }
  }
  gapToUse <- list(selGapName = selGapName, selGap = selGap)
}

getGapsAfterFill <-
function(filledTS, selGap, envir = KTSEnv) {
    remainingNAsInGapInd <- which(is.na(filledTS$value[selGap$gaps]))
    remainingNAsInGap <- selGap$gaps[remainingNAsInGapInd]
    filledNasInGap <- setdiff(selGap$gaps, remainingNAsInGap)
    if (length(filledNasInGap) > 0) {
        filledNasTable <- groupDates(filledNasInGap, filledTS)
    } else {
        filledNasTable <- NULL
    }
    gapsAfterFill <- list(remainingNAsInGap = remainingNAsInGap, 
                          filledNasTable = filledNasTable)
}

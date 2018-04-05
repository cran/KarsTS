getMaxNegSlope <-
function(timSerVals) {
  diffTimSerVals <- diff(timSerVals)
  maxNegSlope <- NULL
  if (any(diffTimSerVals < 0)) {
    negDiffTimSerVals <- diffTimSerVals[which(diffTimSerVals < 0)]
    maxNegSlope <- min(negDiffTimSerVals, na.rm = TRUE)
  }
  maxNegSlope
}

getMaxPosSlope <-
function(timSerVals) {
  diffTimSerVals <- diff(timSerVals)
  maxPosSlope <- NULL
  if (any(diffTimSerVals > 0)) {
    posDiffTimSerVals <- diffTimSerVals[which(diffTimSerVals > 0)]
    maxPosSlope <- max(posDiffTimSerVals, na.rm = TRUE)
  }
  maxPosSlope
}

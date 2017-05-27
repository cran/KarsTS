areTsGapTimeCompatible <-
function(TS1, GAP1) {
  sameIniDate <- as.character(TS1$time[1]) == GAP1$tsIni
  sameSampPer <- diff(as.numeric(TS1$time[1:2]))/60 == GAP1$samPerMin
  if (length(GAP1$gaps) == 0) {
    sameLength <- TRUE
  } else {
    sameLength <- max(GAP1$gaps) <= nrow(TS1)
  }
  result <- c(sameIniDate, sameSampPer, sameLength)
}

areTsRmTimeCompatible <-
function(TS1, RM1) {
  sameIniDate <- all(RM1$tsIni == as.character(TS1$time[1]))
  sameSampPer <- all(RM1$samPerSec == diff(as.numeric(TS1$time[1:2])))
  sameLength <- max(RM1$ones) <= nrow(TS1)
  result <- c(sameIniDate, sameSampPer, sameLength)
}

are2TsTimeCompatible <-
function(TS1, TS2) {
  sameIniDate <- as.numeric(TS1$time[1]) == as.numeric(TS2$time[1])
  sameSampPer <- diff(as.numeric(TS1$time[1:2])) == 
    diff(as.numeric(TS2$time[1:2]))
  sameLength <- nrow(TS1) == nrow(TS2)
  result <- c(sameIniDate, sameSampPer, sameLength)
}

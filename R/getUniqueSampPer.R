getUniqueSampPer <-
function(timeSer) {
  colnames(timeSer) <- c("time", "value")
  timeSerNoNAs <- timeSer[is.finite(timeSer$value), ]
  diffTime <- diff(as.numeric(timeSerNoNAs$time))
  uniqueSampPer <- sort(unique(diffTime))
  uniqueSampPer <- cbind(as.matrix(table(diffTime)), 
                         uniqueSampPer, round(uniqueSampPer/60,3), 
                         round(uniqueSampPer/3660, 3), 
                         round(uniqueSampPer/87840, 3), 
                         round(uniqueSampPer/604800, 3))
  colnames(uniqueSampPer) <- c("reps", "secs", "mins", 
                               "hours", "days", "weeks")
  rownames(uniqueSampPer) <- NULL
  uniqueSampPer
}

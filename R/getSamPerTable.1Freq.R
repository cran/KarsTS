getSamPerTable.1Freq <-
function(timSer, sampPer) {
  sampPer <- diff(as.numeric(timSer$time[1:2]))/60
  if (any(is.na(timSer$value) == FALSE)) {
    noNaGroups <- groupDates(which(is.na(timSer$value) == FALSE), timSer)
    noNaGroups <- cbind(rep(sampPer, nrow(noNaGroups)), 
                        noNaGroups, rep("DATA",nrow(noNaGroups)))
    colnames(noNaGroups) <- c("sp.min", "ini", "fin", "N", "type")
  } else {
    noNaGroups <- NULL
  }
  if (any(is.na(timSer$value))) {
    naGroups <- groupDates(which(is.na(timSer$value)), timSer)
    naGroups <- cbind(rep(sampPer, nrow(naGroups)), naGroups, 
                      rep("NAs", nrow(naGroups)))
    colnames(naGroups) <- c("sp.min", "ini", "fin", "N", "type")
  } else {
    naGroups <- NULL
  }
  allGroups <- rbind(noNaGroups, naGroups)
  allGroups <- allGroups[order(allGroups$ini), ]
  row.names(allGroups) <- NULL
  allGroups
}

groupDates <-
function(rawIndices, TimSer) {
    groupsMatrix <- groupIndices(rawIndices)
    groupsDF <- data.frame(groupsMatrix)
    colnames(groupsDF) <- c("Initial", "Final", "Number of NAs")
    groupsDF$Initial <- TimSer$time[groupsDF$Initial]
    groupsDF$Final <- TimSer$time[groupsDF$Final]
    groupsDF
}

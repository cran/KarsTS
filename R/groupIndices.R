groupIndices <-
function(rawIndices) {
  rawIndices <- sort(rawIndices)
  indexJumps <- which(diff(rawIndices) != 1)
  groupEndings <- c(rawIndices[indexJumps], rawIndices[length(rawIndices)])
  groupStartings <- c(rawIndices[1], rawIndices[indexJumps + 1])
  groupsMatrix <- t(rbind(groupStartings, groupEndings, 
                          groupEndings - groupStartings + 1))
  colnames(groupsMatrix) <- c("Initial", "Final", "N")
  rownames(groupsMatrix) <- NULL
  groupsMatrix
}

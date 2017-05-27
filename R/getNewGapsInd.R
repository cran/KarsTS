getNewGapsInd <-
function(timSer, lGaps, nGaps) {
  lseriesel <- nrow(timSer)
  nGapsToCreate <- lGaps * nGaps
  NAsInd <- which(is.na(timSer$value))
  noNAsInd <- setdiff(2:(lseriesel - lGaps), NAsInd)
  newGaps <- NULL
  cont <- 0
  while (length(newGaps) != nGapsToCreate & cont < 5000) {
    cont <- cont + 1
    firstNewNA <- sample(noNAsInd, size = 1)
    newGap <- firstNewNA:(firstNewNA + lGaps - 1)
    if (length(intersect(newGap, NAsInd)) == 0) {
      newGaps <- union(newGaps, newGap)
      NAsInd <- union(NAsInd, newGap)
      noNAsInd <- setdiff(noNAsInd, newGap)
    }
    rm(newGap, firstNewNA)
  }
  if (length(newGaps) != nGapsToCreate) {
    newGaps <- NULL
  }
  newGaps
}

rmSlopeOutliers <-
function(tS = NULL, origMxPosSlope = NULL, 
                            origMxNegSlope = NULL,filling = NULL) {
  myWhichPosSlopes <- function(tS, origMxPosSlope) {
    pointToRemove <- NULL
    difftS <- diff(tS$value)
    noNADiffInd <- which(is.na(difftS) == FALSE & is.nan(difftS) == FALSE)
    noNADifftS <- difftS[noNADiffInd]
    pointToRemove <- which(noNADifftS > origMxPosSlope)
    if (length(pointToRemove) > 0) {
      pointToRemove <- min(pointToRemove)
      pointToRemove <- noNADiffInd[pointToRemove]
    }
    pointToRemove
  }
  myWhichNegSlopes <- function(tS, origMxNegSlope) {
    pointToRemove <- NULL
    difftS <- diff(tS$value)
    noNADiffInd <- which(is.na(difftS) == FALSE & is.nan(difftS) == FALSE)
    noNADifftS <- difftS[noNADiffInd]
    pointToRemove <- which(noNADifftS < origMxNegSlope)
    if (length(pointToRemove) > 0) {
      pointToRemove <- min(pointToRemove)
      pointToRemove <- noNADiffInd[pointToRemove]
      pointToRemove <- pointToRemove + 1
    }
    pointToRemove
  }
  fillingMaxPosSlope <- getMaxPosSlope(tS$value)
  fillingMaxNegSlope <- getMaxNegSlope(tS$value)
  diffMaxPosSlopes <- origMxPosSlope - fillingMaxPosSlope
  diffMaxNegSlopes <- fillingMaxNegSlope - origMxNegSlope
  nIter <- 0
  while ((diffMaxPosSlopes < 0 | diffMaxNegSlopes < 0) & nIter < 1e+05) {
    if (diffMaxPosSlopes < 0) {
      pointToRmInd <- myWhichPosSlopes(tS, origMxPosSlope)
      if (is.null(filling) == FALSE) {
        distanceInd <- abs(filling - pointToRmInd)
        pointToRmInd <- filling[which(distanceInd == min(distanceInd))]
      }
      
      valueBefore <- tS$value[pointToRmInd]
      tS$value[pointToRmInd] <- NA
      filledData <- try(zoo::na.approx(tS$value, 
                                       xout = pointToRmInd, 
                                       na.rm = FALSE))
      if (class(filledData) != "try-error") {
        if (filledData != valueBefore) {
          tS$value[pointToRmInd] <- filledData
        }
      }
      fillingMaxPosSlope <- getMaxPosSlope(tS$value)
      fillingMaxNegSlope <- getMaxNegSlope(tS$value)
      diffMaxPosSlopes <- origMxPosSlope - fillingMaxPosSlope
      diffMaxNegSlopes <- fillingMaxNegSlope - origMxNegSlope
    }
    if (diffMaxNegSlopes < 0) {
      pointToRmInd <- myWhichNegSlopes(tS, origMxNegSlope)
      if (is.null(filling) == FALSE) {
        distanceInd <- abs(filling - pointToRmInd)
        pointToRmInd <- filling[which(distanceInd == min(distanceInd))]
      }
      valueBefore <- tS$value[pointToRmInd]
      tS$value[pointToRmInd] <- NA
      filledData <- try(zoo::na.approx(tS$value, 
                                       xout = pointToRmInd, 
                                       na.rm = FALSE))
      if (class(filledData) != "try-error") {
        if (filledData != valueBefore) {
          tS$value[pointToRmInd] <- filledData
        }
      }
      fillingMaxPosSlope <- getMaxPosSlope(tS$value)
      fillingMaxNegSlope <- getMaxNegSlope(tS$value)
      diffMaxPosSlopes <- origMxPosSlope - fillingMaxPosSlope
      diffMaxNegSlopes <- fillingMaxNegSlope - origMxNegSlope
    }
    nIter <- nIter + 1
  }
  tS
}

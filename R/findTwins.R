findTwins <-
function(recMat, pointsToFind = NULL) {
    
    rebuildCol <- function(indcol, X1, Y1) {
      sort(c(Y1[which(X1 == indcol)], X1[which(Y1 == indcol)], indcol))
    }
    getLength <- function(indcol, X1, Y1) {
      length(rebuildCol(indcol = indcol, X1, Y1))
    }
    
    shorteningEmb <- max((recMat$embDim - 1) * recMat$delay)
    N <- min(recMat$tsLength) - shorteningEmb
    coincidX <- recMat$ones$X
    coincidY <- recMat$ones$Y
    
    colLengths <- rep(1, N)
    colsToRebuild <- as.matrix(sort(union(unique(coincidX), unique(coincidY))))
    colLengths0 <- apply(colsToRebuild, 1, FUN = getLength, 
                         X1 = coincidX, Y1 = coincidY)
    colLengths[colsToRebuild] <- colLengths0
    
    families <- rep(NA, N)
    familyNumber <- 1
    
    if (is.null(pointsToFind)) {
      
      lengthsToRevise <- unique(colLengths[which(colLengths > 1)])
      
    }else{
      
      lengthsToRevise <- colLengths[pointsToFind]
      lengthsToRevise <- lengthsToRevise[which(lengthsToRevise > 1)]
      lengthsToRevise <- unique(lengthsToRevise)
      
    }
    
    for (CL in lengthsToRevise) {
      
      possibleTwins <- which(colLengths == CL)
      possTwinsRebuilt <- apply(as.matrix(possibleTwins), 1, FUN = rebuildCol, 
                                X1 = coincidX, Y1 = coincidY)
      
      while (length(possibleTwins) > 1) {
        
        intersectionL <- rep(CL, length(possibleTwins))
        
        for (k in 2:length(possibleTwins)) {
          intersectionL[k] <- length(intersect(possTwinsRebuilt[, 1], 
                                               possTwinsRebuilt[, k]))
        }
        
        twinIndices <- which(intersectionL == CL)
        
        if(length(twinIndices)==1){
          
          possibleTwins <- possibleTwins[-twinIndices]
          possTwinsRebuilt <- possTwinsRebuilt[, -twinIndices]
          
        }else{
          
          families[possibleTwins[twinIndices]] <- familyNumber
          possibleTwins <- possibleTwins[-twinIndices]
          possTwinsRebuilt <- possTwinsRebuilt[, -twinIndices]
          familyNumber <- familyNumber + 1
          
        }
        rm(twinIndices, intersectionL)
      }
      rm(possibleTwins, possTwinsRebuilt)
    }
    
    families
    
  }

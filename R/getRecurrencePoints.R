getRecurrencePoints <-
function(timSer,embedDim, lagDelay, threshold){
  
  findRecPointY <- function(pointIndex, dataTS, lDataTS, threshold) {
    P <- dataTS[pointIndex, ]
    pointsAboveP <- dataTS[(pointIndex + 1):lDataTS, ]
    distanceP <- infNorm(P, pointsAboveP)
    distanceP[which(is.infinite(distanceP))] <- Inf
    recPointY <- which(distanceP < threshold) + pointIndex
    recPointY
  }
  findRecPointX <- function(pointIndex, recPointsY) {
    recPointY <- recPointsY[[pointIndex]]
    if (is.null(recPointY) == FALSE) {
      recPointX <- rep(pointIndex, length(recPointY))
    }
  }
  
  
  if (embedDim == 1) {
    dataTS <- matrix(timSer$value)
    infNorm <- function(v1, v2) {
      abs(v2 - v1)
    }
  } else {
    dataTS <- tseriesChaos::embedd(timSer$value, 
                                   m = embedDim, d = lagDelay)
    infNorm <- function(v1, v2) {
      lv2 <- NROW(v2)
      apply(abs(v2 - matrix(rep(v1, each = lv2), lv2, embedDim)), 
            1,FUN = max, na.rm = FALSE)
    }
  }
  indices <- as.matrix(1:(nrow(dataTS) - 1))
  recPointsY <- apply(indices, 1, FUN = findRecPointY, 
                      dataTS = dataTS,lDataTS = nrow(dataTS),
                      threshold = threshold)
  if(length(recPointsY) == 0){
    
    list(recPointsX = NULL, recPointsY = NULL)  
    
  }else{
    recPointsX <- apply(indices, 1, FUN = findRecPointX, 
                        recPointsY = recPointsY)
    recPointsX <- unlist(recPointsX)
    recPointsY <- unlist(recPointsY)
    list(recPointsX = recPointsX, recPointsY =recPointsY)
  }
  
}

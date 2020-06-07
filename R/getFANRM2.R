getFANRM2 <-
function(selTs,embedDim, lagDelay, threshold, theilerWin, fan){

    if (embedDim == 1) {
      dataTS <- matrix(selTs$value)
      infNorm <- function(v1, v2) {
        abs(v2 - v1)
      }
    } else {
      dataTS <- tseriesChaos::embedd(selTs$value, 
                                     m = embedDim, d = lagDelay)
      infNorm <- function(v1, v2) {
        if(any(class(v2)=="numeric")){v2 <- t(as.matrix(v2))}
        lv2 <- NROW(v2)
        apply(abs(v2 - matrix(rep(v1, each = lv2), lv2, embedDim)), 
              1,FUN = max, na.rm = FALSE)
      }
    }
    
    
    indices <- 1:nrow(dataTS)
    recPointsY <- vector("list",length(indices))
    recPointsX <- vector("list",length(indices))
    cont <- 1
    
    for(pointIndex in indices){
      
      P <- dataTS[pointIndex, ]
      
      # Apply Theiler window
      theithei <- (pointIndex - theilerWin):(pointIndex + theilerWin)
      pointsOtherThanP.Ind <- setdiff(indices, theithei)
      pointsOtherThanP <- dataTS[pointsOtherThanP.Ind, ]
      # Calculate distances and compare to threshold
      distanceP <- infNorm(P, pointsOtherThanP)
      distanceP[which(is.finite(distanceP)== FALSE)] <- NA
      withinThres <- which(distanceP < threshold) #NAs are not selected
      LwithinThres <- length(withinThres)
      # Find the n closest neighbors
      
      if (LwithinThres > 0 & LwithinThres <= fan){
        
        recPointsY[[cont]] <- pointsOtherThanP.Ind[withinThres]
        
        recPointsX[[cont]] <- rep(pointIndex, LwithinThres) 
        
        cont <- cont + 1
        
      }else{
        
        withinThresSorted <- sort(distanceP[withinThres], index.return = TRUE, na.last = NA)
        smallerInDistance <- sort(withinThres[withinThresSorted$ix][1:fan])
        recPointsY[[cont]] <- pointsOtherThanP.Ind[smallerInDistance]
        recPointsX[[cont]] <- rep(pointIndex, fan)
        
        rm(withinThresSorted, smallerInDistance)
        
        cont <- cont + 1
        
      }
      
      rm(theithei, pointsOtherThanP, pointsOtherThanP.Ind,
         distanceP, withinThres, LwithinThres)
      
    }
    
  
    recPointsX <- unlist(recPointsX)
    recPointsY <- unlist(recPointsY)
    
    res <- list(recPointsX = recPointsX, recPointsY =recPointsY)

    res
    
  }

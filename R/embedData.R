embedData <-
function(TSData, embDim, embDelay) {
  if (embDim == 1) {
    embeddedData <- TSData
  } else {
    embeddedData <- tseriesChaos::embedd(TSData, m = embDim, d = embDelay)
    embeddedData <- rbind(embeddedData, 
                          matrix(NA, embDelay * (embDim - 1), embDim))
  }
  embeddedData
}

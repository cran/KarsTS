myScale <-
function(inputMatrix, scaleType = "Robust", 
                    outputType = c("outDef", "outList","outNo")) {
  if (outputType != "outDef" & outputType != "outList" &
      outputType != "outNo") {
    stop("The types of output (outputType) are outDef, outList and outNo")
  } else {
    if (class(inputMatrix) == "numeric" | class(inputMatrix) == "integer") {
      inputMatrix <- as.matrix(inputMatrix)
    }
    if (scaleType == "Robust") {
      centralStat <- apply(inputMatrix, 2, 
                           FUN = stats::median, na.rm = TRUE)
      disperStat <- apply(inputMatrix, 2, 
                          FUN = stats::mad, na.rm = TRUE, constant = 1)
    } else {
      centralStat <- apply(inputMatrix, 2, FUN = mean, na.rm = TRUE)
      disperStat <- apply(inputMatrix, 2, FUN = stats::sd, na.rm = TRUE)
    }
    result <- scale(inputMatrix, center = centralStat, scale = disperStat)
    if (outputType == "outList" | outputType == "outNo") {
      attributes(result) <- NULL
      result <- matrix(result, nrow(inputMatrix), ncol(inputMatrix))
      if (outputType == "outList") {
        result <- list(scaledData = result, centralStat = centralStat, 
                       disperStat = disperStat, 
                       scaleType = scaleType)
      }
    }
    result
  }
}

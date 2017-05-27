getNAsGaps <-
function(y) {
  ly <- length(y)
  if (all(is.na(y))) {
    malos <- t(matrix(c(1, ly)))
  } else if (all(is.na(y) == FALSE)) {
    malos <- NULL
  } else {
    malos <- groupIndices(which(is.na(y)))
    malos <- malos[, 1:2]
    colnames(malos) <- NULL
  }
  malos
}

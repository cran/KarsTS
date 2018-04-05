assignMultiple <-
function(namesVector, valuesList, envir = KTSEnv) {
  if (class(valuesList) != "list") {
    stop(paste("The values must be a list because",
               "its values can have different classes"))
  } else {
    for (i in 1:length(namesVector)) {
      assign(namesVector[i], valuesList[[i]], envir = envir)
    }
  }
}

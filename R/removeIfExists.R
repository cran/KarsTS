removeIfExists <-
function(candidates, envir = KTSEnv) {
  doTheyExist <- sapply(candidates, 
                        function(x) y <- exists(x, envir = envir))
  elesToRm <- candidates[which(doTheyExist == TRUE)]
  if (length(elesToRm) > 0) {
    rm(list = elesToRm, envir = envir)
  }
}

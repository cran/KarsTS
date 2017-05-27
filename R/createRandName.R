createRandName <-
function(prefix = "panel") {
  paste0(prefix, sample(10000:99999, 1))
}

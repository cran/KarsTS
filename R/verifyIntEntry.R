verifyIntEntry <-
function(x, noValid = "isNoValid") {
  isAnInteger <- strtoi(x)
  if (x == "") {
    xx <- NA
  } else if (is.na(isAnInteger) == FALSE) {
    xx <- isAnInteger
  } else {
    xx = noValid
  }
  xx
}

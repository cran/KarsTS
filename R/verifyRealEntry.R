verifyRealEntry <-
function(x, noValid = "isNoValid") {
  isNumber <- try(eval(parse(text = paste0("is.numeric(", x, ")"))), 
                  silent = TRUE)
  if (class(isNumber) != "try-error") {
    xx <- eval(parse(text = x))
  } else if (x == "") {
    xx = NA
  } else {
    xx = noValid
  }
  xx
}

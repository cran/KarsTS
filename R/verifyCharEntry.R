verifyCharEntry <-
function(x, noValid = "isNoValid") {
  xx <- x
  isNumber <- try(eval(parse(text = paste0("is.numeric(", x, ")"))), 
                  silent = TRUE)
  if (class(isNumber) == "logical" & isNumber == TRUE) {
    xx <- noValid
  } else if (x == "") {
    xx = NA
  }
  xx
}

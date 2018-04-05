setCorrectDate <-
function(x, type) {
  setCorrectDateEle <- function(x, type) {
    inBetween0099 <- function(range1, range2, valueX) {
      inBetween0009 <- function(valueXrange, valueX) {
        if (valueX == valueXrange) {
          result = paste0("0", valueX)
        } else {
          result = NA
        }
      }
      inBetween1099 <- function(valueXrange, valueX) {
        if (valueX == valueXrange) {
          result = as.character(valueX)
        } else {
          result = NA
        }
      }
      posibleDates <- c(sapply(range1, FUN = inBetween0009, valueX = valueX), 
                        sapply(range2, FUN = inBetween1099, valueX = valueX))
      posibleDatesNonaInd <- which(is.na(posibleDates) == FALSE)
      if (length(posibleDatesNonaInd) == 1) {
        result <- posibleDates[posibleDatesNonaInd]
      } else {
        result = NA
      }
      result
    }
    switch(type, validYear = as.character(x), 
           validMonth = inBetween0099(1:9,10:12, valueX = x), 
           validDay = inBetween0099(1:9, 10:31, valueX = x), 
           validHour = inBetween0099(0:9, 10:23, valueX = x), 
           validMin = inBetween0099(0:9,10:59, valueX = x), 
           validSec = inBetween0099(0:9, 10:59, valueX = x))
  }
  result <- verifyIntEntry(x, noValid = NA)
  if (is.finite(result)) {
    result <- setCorrectDateEle(result, type = type)
  }
  result
}

verifyDateEntry <-
function(valSecs, valMins, valHour, 
                            valDay, valMonth, valYear) {
  valSecs <- setCorrectDate(tcltk::tclvalue(valSecs), type = "validSec")
  valMins <- setCorrectDate(tcltk::tclvalue(valMins), type = "validMin")
  valHour <- setCorrectDate(tcltk::tclvalue(valHour), type = "validHour")
  valDay <- setCorrectDate(tcltk::tclvalue(valDay), type = "validDay")
  valMonth <- setCorrectDate(tcltk::tclvalue(valMonth), type = "validMonth")
  valYear <- setCorrectDate(tcltk::tclvalue(valYear), type = "validYear")
  allValues <- c(valYear, valMonth, valDay, valHour, valMins, valSecs)
  if (all(is.na(allValues))) {
    result <- NA
  } else if (any(is.na(allValues[1:3]))) {
    result <- NULL
  } else if (all(is.character(allValues[1:3]))) {
    if (is.na(valSecs)) {
      valSecs = "00"
    }
    if (is.na(valMins)) {
      valMins = "00"
    }
    if (is.na(valHour)) {
      valHour = "00"
    }
    result <- paste0(valYear, "-", valMonth, "-", valDay, " ", valHour, ":", 
                     valMins, ":", valSecs)
  }
  result
}

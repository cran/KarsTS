isTimeAlright <-
function(timeCharacter, tz = KTSEnv$timeZone) {
  if (is.null(tz)) {
    tz = KTSEnv$timeZone
  }
  lTime <- length(timeCharacter)
  if (lTime < 30) {
    sizeSample <- lTime
  } else {
    sizeSample <- 30
  }
  timeSample <- sample(timeCharacter, sizeSample)
  dateFormat <- findDateFormat(timeSample, tz = tz)
  if (dateFormat == "notValidFormat") {
    tcltk::tkmessageBox(message = paste("The date format is not",
                                        "valid. Valid formats are:",
                                        " \n                ",
                                        " %m/%d/%Y %H:%M\n                 ",
                                        "%Y/%m/%d %H:%M\n                 ",
                                        "%Y-%m-%d %H:%M \n                 ",
                                        "%m-%d-%Y %H:%M.\n                 ",
                                        "Check also that all the dates",
                                        "have the same format"), 
                        icon = "warning")
  } else if (dateFormat == "variousFormats") {
    tcltk::tkmessageBox(message = paste("There is more than one date",
                                        "format in your data set"), 
                        icon = "warning")
  }
  dateFormat
}

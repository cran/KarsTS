findDateFormat <-
function(X, tz = NULL) {
  if (is.null(tz)) {
    
    tz <- KTSEnv$timeZone
  }
  findFormat <- function(X, tz = tz) {
    if (nchar(X) == 10) {
      allowedForm <- c("%m/%d/%Y", "%Y/%m/%d", "%Y-%m-%d", "%m-%d-%Y")
    } else {
      allowedForm <- c("%m/%d/%Y %H:%M", "%Y/%m/%d %H:%M", 
                       "%Y-%m-%d %H:%M","%m-%d-%Y %H:%M")
    }
    nFormats <- length(allowedForm)
    doFormatsWork <- apply(as.matrix(1:nFormats), 1, function(i) {
      as.character(strptime(X, format = allowedForm[i], tz = tz))
    })
    formatsWork <- allowedForm[which(is.na(doFormatsWork) == FALSE)]
    if (length(formatsWork) == 0) {
      formatsWork <- NA
    } else {
      formatsWork
    }
    formatsWork
  }
  datesFormat <- sapply(X, FUN = findFormat, tz = tz)
  names(datesFormat) <- NULL
  if (any(is.na(datesFormat))) {
    datesFormat <- "notValidFormat"
  } else if (any(datesFormat != datesFormat[1])) {
    datesFormat <- "variousFormats"
  } else {
    datesFormat <- datesFormat[1]
  }
  datesFormat
}

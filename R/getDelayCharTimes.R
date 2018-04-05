getDelayCharTimes <-
function(initialTimes, tz = NULL) {
    if (is.null(tz)) {
        
        tz <- KTSEnv$timeZone
    }
    dateFormat <- isTimeAlright(timeCharacter = initialTimes, tz = tz)
    initialTimes <- strptime(initialTimes, format = dateFormat, tz = tz)
    minIniTime <- as.numeric(min(initialTimes))
    maxIniTime <- as.numeric(max(initialTimes))
    c(minIniTime, maxIniTime)
}

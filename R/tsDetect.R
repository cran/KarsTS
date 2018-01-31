tsDetect <-
function() {
    isTrueTs <- function(oneDfName) {
        oneDf <- get(oneDfName, envir = KTSEnv)
        if (any(dim(oneDf) == 0)) {
            result <- NA
        } else if (ncol(oneDf) < 2) {
            result <- NA
        } else if (colnames(oneDf)[1] != "time" | 
                   colnames(oneDf)[2] != "value") {
            result <- NA
        } else if (length(class(oneDf[1, 1])) != 2) {
            result <- NA
        } else if (class(oneDf[1, 1])[1] == "POSIXct" & 
                   class(oneDf[1, 1])[2] == "POSIXt") {
            result <- oneDfName
        } else {
            result <- NA
        }
        result
    }
    dfsInGE <- getClassEnvir(classGet = "data.frame", envir = KTSEnv)
    if (length(dfsInGE) == 0) {
        loadedTss <- NULL
    } else {
        loadedTss <- apply(as.matrix(dfsInGE), 1, FUN = isTrueTs)
        loadedTss <- loadedTss[which(is.na(loadedTss) == FALSE)]
        if (length(loadedTss) == 0) {
            loadedTss <- NULL
        }
    }
    loadedTss
}

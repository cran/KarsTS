getClassEnvir <-
function(classGet = "list", envir = KTSEnv) {
    objectsInGE <- ls(envir = KTSEnv)
    classInGE <- apply(as.matrix(objectsInGE), 1, function(i) {
        if (any(class(get(i, envir = envir)) == classGet)) {
            i
        } else {
            NA
        }
    })
    classInGE <- classInGE[which(is.na(classInGE) == FALSE)]
}

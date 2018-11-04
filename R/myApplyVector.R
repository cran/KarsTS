myApplyVector <-
function(FUN = NULL, dataVector = NULL, out.ncols = 1, ...) {
    if (is.null(FUN)) {
        stop("First argument (FUN) missing")
    } else if (is.null(dataVector)) {
        stop("Second argument (dataVector) missing")
    } else if (is.vector(dataVector)) {
        FUN <- match.fun(FUN)
        lVector <- length(dataVector)
        res <- matrix(NA, lVector, out.ncols)
        for (i in 1:lVector) {
            res[i, ] <- forceAndCall(2, FUN, dataVector[i], ...)
        }
        if (out.ncols == 1) {
            res <- as.vector(res)
        }
        res
    } else {
        stop("The input must be a vector")
    }
}

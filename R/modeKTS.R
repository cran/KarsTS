modeKTS <-
function(x) {
    x <- x[which(is.finite(x))]
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

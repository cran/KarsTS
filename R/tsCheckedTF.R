tsCheckedTF <-
function(prefix = "scbValue", envir = KTSEnv) {
    tssel <- rep(FALSE, KTSEnv$dSList$nTS)
    for (ind in 1:KTSEnv$dSList$nTS) {
        scbValueind <- paste0(prefix, ind)
        if (tcltk::tclvalue(get(scbValueind, envir = envir)) == "1") {
            tssel[ind] = TRUE
        }
        rm(scbValueind)
    }
    tssel
}

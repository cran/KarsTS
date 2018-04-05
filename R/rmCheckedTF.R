rmCheckedTF <-
function(prefix = "rcbValue", envir = KTSEnv) {
    rsel <- rep(FALSE, KTSEnv$dSList$nRM)
    for (ind in 1:KTSEnv$dSList$nRM) {
        rcbValueind <- paste0(prefix, ind)
        if (tcltk::tclvalue(get(rcbValueind, envir = envir)) == "1") {
            rsel[ind] = TRUE
        }
        rm(rcbValueind)
    }
    rsel
}

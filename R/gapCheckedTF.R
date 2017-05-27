gapCheckedTF <-
function(prefix = "gcbValue", envir = KTSEnv) {
  gsel <- rep(FALSE, KTSEnv$dSList$nGaps)
  for (ind in 1:KTSEnv$dSList$nGaps) {
    gcbValueind <- paste0(prefix, ind)
    if (tcltk::tclvalue(get(gcbValueind, envir = envir)) == "1") {
      gsel[ind] = TRUE
    }
    rm(gcbValueind)
  }
  gsel
}

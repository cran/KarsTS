createTsChb <-
function(labTitle = "Time series", envir = KTSEnv) {
  createTitle(labTitle = labTitle)
  for (ind in 1:KTSEnv$dSList$nTS) {
    tsind <- paste0("ts", ind)
    scbValueind <- paste0("scbValue", ind)
    assign(tsind, tcltk::tkcheckbutton(KTSEnv$subPanR4C1, 
                                       text = KTSEnv$dSList$TS[ind]))
    assign(paste0("scbValue", ind), tcltk::tclVar("0"), envir = envir)
    tcltk::tkconfigure(get(tsind), 
                       variable = get(scbValueind, envir = envir))
    tcltk::tkgrid(get(tsind), sticky = "nw", 
                  padx = c(15, 0), pady = c(0, 0))
    rm(tsind, scbValueind)
  }
}

createRmChb <-
function(labTitle = "Recurrence matrices", envir = KTSEnv) {
  createTitle(labTitle = labTitle)
  for (ind in 1:KTSEnv$dSList$nRM) {
    rmind <- paste0("rm", ind)
    rcbValueind <- paste0("rcbValue", ind)
    assign(rmind, tcltk::tkcheckbutton(KTSEnv$subPanR4C1, 
                                       text = KTSEnv$dSList$rm[ind]))
    assign(paste0("rcbValue", ind), tcltk::tclVar("0"), envir = envir)
    tcltk::tkconfigure(get(rmind), 
                       variable = get(rcbValueind, envir = envir))
    tcltk::tkgrid(get(rmind), sticky = "nw", 
                  padx = c(10, 0), pady = c(0, 0))
    rm(rmind, rcbValueind)
  }
}

createGapChb <-
function(labTitle = "Gap sets", envir = KTSEnv) {
  createTitle(labTitle = labTitle)
  for (ind in 1:KTSEnv$dSList$nGaps) {
    gapind <- paste0("gap", ind)
    gcbValueind <- paste0("gcbValue", ind)
    assign(gapind, tcltk::tkcheckbutton(KTSEnv$subPanR4C1, 
                                        text = KTSEnv$dSList$gaps[ind]))
    assign(paste0("gcbValue", ind), tcltk::tclVar("0"), envir = envir)
    tcltk::tkconfigure(get(gapind), 
                       variable = get(gcbValueind, envir = envir))
    tcltk::tkgrid(get(gapind), sticky = "nw", padx = c(15, 0), 
                  pady = c(0, 0))
    rm(gapind, gcbValueind)
  }
}

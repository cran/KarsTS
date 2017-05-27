createGapRb <-
function(labTitle = "Gap sets", envir = KTSEnv) {
  createTitle(labTitle = labTitle)
  selGapP <- tcltk::tclVar("0")
  assign("selGapP", selGapP, envir = envir)
  for (ind in 1:KTSEnv$dSList$nGaps) {
    radbut2ind <- paste0("radbut2", ind)
    radbut2labind <- paste0("radbut2labind", ind)
    rbValueind <- paste0("rbValue", ind)
    assign(radbut2labind, tcltk2::tk2label(KTSEnv$subPanR4C1, 
                                           text = KTSEnv$dSList$gaps[ind], 
                                           justify = "left"))
    assign(radbut2ind, tcltk::tkradiobutton(KTSEnv$subPanR4C1, 
                                            variable = selGapP, 
                                            value = KTSEnv$dSList$gaps[ind]))
    tcltk::tkgrid(get(radbut2labind), get(radbut2ind), sticky = "nw", 
                  padx = c(10,0), pady = c(0, 0))
    rm(radbut2ind, rbValueind, radbut2labind)
  }
}

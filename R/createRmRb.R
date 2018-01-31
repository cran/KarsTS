createRmRb <-
function(labTitle = "Recurrence matrices", envir = KTSEnv) {
  createTitle(labTitle = labTitle)
  selRmP <- tcltk::tclVar("0")
  assign("selRmP", selRmP, envir = envir)
  for (ind in 1:KTSEnv$dSList$nRM) {
    radbut2ind <- paste0("radbut2", ind)
    radbut2labind <- paste0("radbut2labind", ind)
    rbValueind <- paste0("rbValue", ind)
    assign(radbut2labind, tcltk2::tk2label(KTSEnv$subPanR4C1, 
                                           text = KTSEnv$dSList$rm[ind], 
                                           justify = "left"))
    assign(radbut2ind, tcltk::tkradiobutton(KTSEnv$subPanR4C1, 
                                            variable = selRmP, 
                                            value = KTSEnv$dSList$rm[ind]))
    tcltk::tkgrid(get(radbut2labind), get(radbut2ind), sticky = "nw", 
                  padx = c(10,0), pady = c(0, 0))
    rm(radbut2ind, rbValueind, radbut2labind)
  }
}

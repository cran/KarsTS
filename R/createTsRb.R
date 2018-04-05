createTsRb <-
function(labTitle = "Time series", 
                       variableName = "selTsP", envir = KTSEnv) {
  createTitle(labTitle = labTitle)
  assign(variableName, tcltk::tclVar("0"), envir = envir)
  for (ind in 1:KTSEnv$dSList$nTS) {
    radbutind <- paste0("radbut", ind)
    radbutlabind <- paste0("radbutlabind", ind)
    rbValueind <- paste0("rbValue", ind)
    assign(radbutlabind, tcltk2::tk2label(KTSEnv$subPanR4C1, 
                                          text = KTSEnv$dSList$TS[ind], 
                                          justify = "left"))
    assign(radbutind, tcltk::tkradiobutton(KTSEnv$subPanR4C1, 
                                           variable = get(variableName, 
                                                          envir = envir), 
                                           value = KTSEnv$dSList$TS[ind]))
    tcltk::tkgrid(get(radbutlabind), get(radbutind), sticky = "nw", 
                  padx = c(10, 0), pady = c(0, 0))
    rm(radbutind, rbValueind, radbutlabind)
  }
}

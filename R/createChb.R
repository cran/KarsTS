createChb <-
function(labTitle = NULL, variableName = NULL, defaultVal = "0") {
  assign(variableName, tcltk::tclVar(defaultVal), envir = KTSEnv)
  tcltk::tkgrid(tcltk::tkcheckbutton(KTSEnv$subPanR4C1, text = labTitle, 
                                     variable = get(variableName, 
                                                    envir = KTSEnv)), 
                sticky = "nw", padx = c(10, 0), pady = c(0, 0))
}

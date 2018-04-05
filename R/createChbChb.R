createChbChb <-
function(ind, elements, prefix1 = NULL, 
                         prefix2 = NULL, envir = KTSEnv) {
  cbValueind <- paste0(prefix1, ind)
  assign(cbValueind, tcltk::tclVar("0"), envir = envir)
  tsChbind <- tcltk::tkcheckbutton(KTSEnv$subPanR4C1, 
                                   text = elements[ind], 
                                   variable = get(cbValueind, envir = envir))
  cbValueinddev <- paste0(prefix2, ind)
  assign(cbValueinddev, tcltk::tclVar("0"), envir = envir)
  tsChbinddev <- tcltk::tkcheckbutton(KTSEnv$subPanR4C1, text = elements[ind], 
                                      variable = get(cbValueinddev, 
                                                     envir = envir))
  tcltk::tkgrid(tsChbind, tsChbinddev, sticky = "nw")
  "Done"
}

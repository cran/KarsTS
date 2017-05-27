createChbEntry <-
function(ind, elements, 
                           prefix = "scbValue", envir = KTSEnv) {
  cbValueind <- paste0(prefix, ind)
  assign(cbValueind, tcltk::tclVar("0"), envir = envir)
  tsChbind <- tcltk::tkcheckbutton(KTSEnv$subPanR4C1, 
                                   text = elements[ind], 
                                   variable = get(cbValueind, envir = envir))
  tEntcbValueind <- paste0("tEnt", prefix, ind)
  assign(tEntcbValueind, tcltk::tclVar(""), envir = KTSEnv)
  tstEntind <- tcltk2::tk2entry(KTSEnv$subPanR4C1, 
                                textvariable = get(tEntcbValueind, 
                                                   envir = envir))
  tcltk::tkgrid(tsChbind, tstEntind, sticky = "nw")
  "Done"
}

createSubPanR4C1 <-
function () { 
  
  if (exists('subPanR4C1',envir = KTSEnv)) {
    if(is.null(KTSEnv$subPanR4C1)==FALSE){
      tcltk::tkdestroy(KTSEnv$subPanR4C1) 
    }
  }
  
  subPanR4C1 = tcltk::tkframe(KTSEnv$row4.col1, 
                              width = KTSEnv$width4.1,
                              borderwidth = 2,relief = 'raised') 
  tcltk::tkgrid(subPanR4C1)
  tcltk::tkgrid.configure(subPanR4C1, sticky = 'n')
  tcltk::tkgrid.columnconfigure(subPanR4C1, 0, weight = 0)
  tcltk::tkgrid.columnconfigure(subPanR4C1, 1, weight = 0)
  tcltk::tkgrid.columnconfigure(subPanR4C1, 2, weight = 0)
  tcltk::tkgrid.rowconfigure(subPanR4C1, 0, weight = 0) 
  assign('subPanR4C1', subPanR4C1, envir = KTSEnv)
}

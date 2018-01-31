createEntry <-
function(labTitle, textVariableName, 
                        defaultVal = "", font = KTSEnv$KTSFonts$T1) {
  envir <- KTSEnv
  assign(textVariableName, tcltk::tclVar(defaultVal), envir = envir)
  tcltk::tkgrid(tcltk2::tk2label(KTSEnv$subPanR4C1, text = labTitle, 
                                 justify = "left", 
                                 font = font), 
                tcltk2::tk2entry(KTSEnv$subPanR4C1,
                                 textvariable = get(textVariableName, 
                                                    envir = envir)), 
                sticky = "nw", padx = c(10, 0), pady = c(0, 0))
}

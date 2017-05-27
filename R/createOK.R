createOK <-
function(labTitle = "NEXT", action = NULL, 
                     width = 7, panel = KTSEnv$subPanR4C1) {
  tcltk::tkgrid(tcltk::tkbutton(panel, text = labTitle, 
                                command = action, width = width), 
                padx = c(10, 10), pady = c(20, 10), columnspan = 2)
}

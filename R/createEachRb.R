createEachRb <-
function(labTitle = NULL, variable = NULL, 
                         panel = KTSEnv$subPanR4C1) {
  tcltk::tkgrid(tcltk2::tk2label(panel, text = labTitle, justify = "left"), 
                tcltk::tkradiobutton(panel,variable = variable, 
                                     value = labTitle), 
                sticky = "nw", padx = c(10, 0), pady = c(0, 0))
  "Done"
}

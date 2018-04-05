createTITLE <-
function(labTitle = "TITLE", 
                        panel = KTSEnv$subPanR4C1) {
  
  tcltk::tkgrid(tcltk::tklabel(panel, text = labTitle, 
                               font = KTSEnv$KTSFonts$T0, 
                               width = KTSEnv$titleWidth), 
                padx = c(2, 2), pady = c(10, 5), columnspan = 2)
}

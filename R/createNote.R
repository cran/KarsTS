createNote <-
function(labTitle = NULL, pady = c(10, 10)) {
  tcltk::tkgrid(tcltk::tklabel(KTSEnv$subPanR4C1, text = labTitle, 
                               font = KTSEnv$KTSFonts$explain, 
                               foreground = "blue"), 
                padx = c(2, 2), 
                pady = pady, sticky = "nw", columnspan = 2)
}

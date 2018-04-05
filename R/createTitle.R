createTitle <-
function(labTitle = "Title") {
  tcltk::tkgrid(tcltk::tklabel(KTSEnv$subPanR4C1, 
                               text = labTitle, 
                               font = KTSEnv$KTSFonts$T1), 
                sticky = "nw", padx = c(10, 10), pady = c(10, 5), 
                columnspan = 2)
}

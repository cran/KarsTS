welcomeScreen <-
function() {
  KTSEnv$mainPanel.0 <- tcltk::tktoplevel(bg = "white")
  tcltk::tkwm.title(KTSEnv$mainPanel.0, 
                    "KarsTS: AN INTERFACE FOR KARSTIC TIME SERIES")
  row1.0 <- tcltk::tkframe(KTSEnv$mainPanel.0, bg = "blue")
  row2.0 <- tcltk::tkframe(KTSEnv$mainPanel.0, bg = "white", 
                           borderwidth = 2, relief = "raised", 
                           height = 50, width = 50)
  
  pathToImage <- paste0(system.file(package = "KarsTS"), "/extdata")
  prev <- getwd()
  setwd(pathToImage)
  tcltk::tcl("image", "create", "photo", "imageID", 
             file = "wombeyan_caves_formations.gif")
  setwd(prev)
  l <- tcltk::ttklabel(row1.0, image = "imageID", compound = "image")
  tcltk::tkconfigure(l, background = "darkred")
  tcltk::tkgrid.rowconfigure(l, 0, weight = 0)
  tcltk::tkgrid.columnconfigure(l, 0, weight = 0)
  tcltk::tkgrid.configure(l, sticky = "nsew")
  tcltk::tkgrid(l)
  
  fontTITULO <- tcltk::tkfont.create(family = "Arial", 
                                     size = 24, weight = "bold")
  welcomeTxt1 <- tcltk::tklabel(row2.0, text = "KarsTS", 
                                width = 150, height = 5, 
                                font = fontTITULO, background = "white", 
                                foreground = "darkblue")
  welcomeTxt2 <- tcltk::tklabel(row2.0, 
                                text = "AN INTERFACE FOR KARSTIC TIME SERIES", 
                                height = 5, font = KTSEnv$KTSFonts$mainBt, 
                                background = "white", foreground = "darkblue")
  welcomeTxt3 <- tcltk::tklabel(row2.0, text = "Marina Saez Andreu", 
                                height = 5, 
                                font = KTSEnv$KTSFonts$subBt, 
                                background = "white", foreground = "darkblue")
  startButton <- tcltk::tkbutton(row2.0, text = "START", width = 50, 
                                 command = destroyWelcome, 
                                 background = "azure2", 
                                 foreground = "chocolate4", 
                                 font = fontTITULO)
  
  
  tcltk::tkpack(welcomeTxt1, expand = TRUE, fill = "both", 
                pady = c(15, 15), padx = c(15,15))
  tcltk::tkpack(welcomeTxt2, expand = FALSE, fill = "both", 
                pady = c(15, 15), padx = c(15,15))
  tcltk::tkpack(welcomeTxt3, expand = FALSE, fill = "both", 
                pady = c(15, 15), padx = c(15,15))
  tcltk::tkpack(startButton, expand = FALSE, fill = "both", 
                pady = c(15, 15), padx = c(15,15))
  
  
  tcltk::tkpack(row1.0, expand = FALSE, fill = "both", side = "left")
  tcltk::tkpack(row2.0, expand = TRUE, fill = "both", side = "left")
  
  tcltk::tkfocus(KTSEnv$mainPanel.0)
  tcltk::tkraise(KTSEnv$mainPanel.0)
}

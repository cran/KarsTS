mainScreen <-
function() {
    
    KTSEnv$mainPanel <- tcltk::tktoplevel(bg = "white")
    tcltk::tkwm.title(KTSEnv$mainPanel, 
                      "KarsTS: An interface for microclimate time series")
    KTSEnv$frameForButtons0 <- tcltk::tkframe(KTSEnv$mainPanel, 
                                              bg = "blue", height = 70)
    KTSEnv$row1 <- tcltk::tkframe(KTSEnv$frameForButtons0, height = 70, 
                                  bg = "aliceblue", 
                                  borderwidth = 0, relief = "raised")
    KTSEnv$row11 <- tcltk::tkframe(KTSEnv$row1, height = 50, 
                                   bg = "aliceblue", borderwidth = 0, 
                                   relief = "raised")
    KTSEnv$widthRow1 <- 5
    KTSEnv$menu1Button <- tcltk::tkbutton(parent = KTSEnv$row11, 
                                          text = "Time series", 
                                          width = KTSEnv$widthRow1, 
                                          command = buttons1, 
                                          background = "palevioletred4", 
                                          foreground = "white", 
                                          font = KTSEnv$KTSFonts$mainBt)
    KTSEnv$menu2Button <- tcltk::tkbutton(parent = KTSEnv$row11, 
                                          text = "Sets of gaps", 
                                          width = KTSEnv$widthRow1, 
                                          command = buttons2, 
                                          background = "darkolivegreen4", 
                                          foreground = "white", 
                                          font = KTSEnv$KTSFonts$mainBt)
    KTSEnv$menu3Button <- tcltk::tkbutton(parent = KTSEnv$row11, 
                                          text = "Analysis", 
                                          width = KTSEnv$widthRow1, 
                                          command = buttons3, 
                                          background = "indianred4", 
                                          foreground = "white", 
                                          font = KTSEnv$KTSFonts$mainBt)
    KTSEnv$menu4Button <- tcltk::tkbutton(parent = KTSEnv$row11, 
                                          text = "Plots", 
                                          width = KTSEnv$widthRow1, 
                                          command = buttons4, 
                                          background = "goldenrod4", 
                                          foreground = "white", 
                                          font = KTSEnv$KTSFonts$mainBt)
    KTSEnv$menu5Button <- tcltk::tkbutton(parent = KTSEnv$row11, 
                                          text = "Filling", 
                                          width = KTSEnv$widthRow1, 
                                          command = buttons5, 
                                          background = "royalblue4", 
                                          foreground = "white", 
                                          font = KTSEnv$KTSFonts$mainBt)
    KTSEnv$rows2and3 <- tcltk::tkframe(KTSEnv$frameForButtons0, 
                                       height = 70, bg = "pink", 
                                       borderwidth = 0, relief = "raised")
    
    KTSEnv$row5 <- tcltk::tkframe(KTSEnv$mainPanel, 
                                  borderwidth = 2, 
                                  relief = "raised")
    KTSEnv$spaceForTxtW5 <- tcltk::tkframe(KTSEnv$row5, 
                                           borderwidth = 2, 
                                           relief = "raised")
    KTSEnv$setwdBut <- tcltk::tkbutton(parent = KTSEnv$spaceForTxtW5, 
                                       text = "Set working dir.", 
                                       command = setwdKTS, 
                                       foreground = "grey35", 
                                       background = "grey95", 
                                       font = KTSEnv$KTSFonts$mainBt)
    
    KTSEnv$saveReportBut <- tcltk::tkbutton(parent = KTSEnv$spaceForTxtW5, 
                                            text = "Save report to txt", 
                                            command = saveReport, 
                                            foreground = "grey35", 
                                            background = "grey95", 
                                            font = KTSEnv$KTSFonts$mainBt)
    KTSEnv$aboutKTSButton <- tcltk::tkbutton(parent = KTSEnv$spaceForTxtW5, 
                                             text = "About KarsTS", 
                                             command = aboutKTS, 
                                             foreground = "grey35", 
                                             background = "grey95", 
                                             font = KTSEnv$KTSFonts$mainBt)
    KTSEnv$helpButton <- tcltk::tkbutton(parent = KTSEnv$spaceForTxtW5, 
                                         text = "Help", 
                                         command = showHelp, 
                                         foreground = "grey35", background = "grey95", 
                                         font = KTSEnv$KTSFonts$mainBt)
    
    KTSEnv$row4 <- tcltk::tkframe(KTSEnv$mainPanel, bg = "aliceblue", 
                                  borderwidth = 2, 
                                  relief = "raised")
    KTSEnv$row4.col1 <- tcltk::tkframe(KTSEnv$row4, width = KTSEnv$width4.1, 
                                       height = KTSEnv$heigth4, 
                                       borderwidth = 2, 
                                       relief = "raised")
    KTSEnv$row4.col2 <- tcltk::tkframe(KTSEnv$row4, width = KTSEnv$width4.2, 
                                       height = KTSEnv$heigth4, 
                                       bg = "white", borderwidth = 2, 
                                       relief = "raised")
    KTSEnv$row4.col3 <- tcltk::tkframe(KTSEnv$row4, width = 1, 
                                       height = KTSEnv$heigth4, 
                                       bg = "white", borderwidth = 2, 
                                       relief = "raised")
    
    KTSEnv$spaceForTxtW <- tcltk::tkframe(KTSEnv$row4.col2, borderwidth = 2, 
                                          relief = "raised")
    
    auxScroll1 <- function(...) {tcltk::tkxview(KTSEnv$txtWidget, ...)}
    auxScroll2 <- function(...) {tcltk::tkyview(KTSEnv$txtWidget, ...)}
    auxScroll3 <- function(...) {tcltk::tkset(KTSEnv$xScr, ...)}
    auxScroll4 <- function(...) {tcltk::tkset(KTSEnv$yScr, ...)}
    
    KTSEnv$xScr <- tcltk::tkscrollbar(KTSEnv$spaceForTxtW, 
                                      orient = "horizontal", 
                                      command = auxScroll1)
    KTSEnv$yScr <- tcltk::tkscrollbar(KTSEnv$spaceForTxtW, 
                                      orient = "vertical", 
                                      command = auxScroll2)
    KTSEnv$txtWidget <- tcltk::tktext(KTSEnv$spaceForTxtW, 
                                      bg = "white", font = "courier", 
                                      wrap = "none", 
                                      width = KTSEnv$width4.2, 
                                      xscrollcommand = auxScroll3, 
                                      yscrollcommand = auxScroll4)
    tcltk::tkgrid.columnconfigure(KTSEnv$spaceForTxtW, 0, weight = 1)
    tcltk::tkgrid.rowconfigure(KTSEnv$spaceForTxtW, 0, weight = 1)
    tcltk::tkgrid.rowconfigure(KTSEnv$txtWidget, 0, weight = 1)
    tcltk::tkgrid.columnconfigure(KTSEnv$txtWidget, 0, weight = 1)
    tcltk::tkgrid.configure(KTSEnv$txtWidget, sticky = "nswe")
    
    KTSEnv$activMenu <- "filling"
    buttons1()
    loadAllTypes()

    
    tcltk::tkpack(KTSEnv$row1, expand = FALSE, fill = "x")
    tcltk::tkpack(KTSEnv$menu1Button, side = "left", 
                  expand = TRUE, fill = "both")
    tcltk::tkpack(KTSEnv$menu2Button, side = "left", 
                  expand = TRUE, fill = "both")
    tcltk::tkpack(KTSEnv$menu3Button, side = "left", 
                  expand = TRUE, fill = "both")
    tcltk::tkpack(KTSEnv$menu4Button, side = "left", 
                  expand = TRUE, fill = "both")
    tcltk::tkpack(KTSEnv$menu5Button, side = "left", 
                  expand = TRUE, fill = "both")
    tcltk::tkpack(KTSEnv$row11, anchor = "nw", fill = "both")
    tcltk::tkpack(KTSEnv$rows2and3, expand = FALSE, fill = "x")
    tcltk::tkpack(KTSEnv$frameForButtons0, side = "top", 
                  expand = FALSE, fill = "x")
    
    tcltk::tkpack(KTSEnv$setwdBut, expand = TRUE, 
                  fill = "both", side = "left")
    tcltk::tkpack(KTSEnv$saveReportBut, expand = TRUE, 
                  fill = "both", side = "left")
    tcltk::tkpack(KTSEnv$aboutKTSButton, expand = TRUE, 
                  fill = "both", side = "left")
    tcltk::tkpack(KTSEnv$helpButton, expand = TRUE, 
                  fill = "both", side = "left")
    tcltk::tkpack(KTSEnv$spaceForTxtW5, expand = TRUE, fill = "x")
    tcltk::tkpack(KTSEnv$row5, side = "bottom", 
                  expand = FALSE, fill = "x")
    
    
    tcltk::tkpack(KTSEnv$row4.col1, side = "left", 
                  expand = FALSE, fill = "both")
    tcltk::tkpack(KTSEnv$row4.col2, side = "left", 
                  expand = TRUE, fill = "both")
    tcltk::tkpack(KTSEnv$row4.col3, side = "left", 
                  expand = FALSE, fill = "both")
    tcltk::tkgrid(KTSEnv$yScr, sticky = "ns", row = 0, column = 1)
    tcltk::tkgrid(KTSEnv$xScr, sticky = "ew", row = 1, columnspan = 2)
    tcltk::tkgrid.configure(KTSEnv$txtWidget, sticky = "nswe")
    tcltk::tkgrid.configure(KTSEnv$txtWidget, sticky = "nswe")
    tcltk::tkpack(KTSEnv$spaceForTxtW, expand = TRUE, fill = "both")
    
    tcltk::tkpack(KTSEnv$row4, side = "left", expand = TRUE, fill = "both")
    tcltk::tkfocus(KTSEnv$mainPanel)
    tcltk::tkraise(KTSEnv$mainPanel)
  }

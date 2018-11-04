buttons1 <-
function() {
  # if (KTSEnv$activMenu != "tsermenu") {
    subMenu1But <- function(parent = NULL, text = "Load", 
                            command = loadAllTypes) {
      
      # if(text == KTSEnv$activButton){
      #   buttonSM1 <- tcltk::tkbutton(parent = parent,text = text, width = 5, 
      #                                command = command, 
      #                                background = "palevioletred3", 
      #                                foreground = "white", 
      #                                font = KTSEnv$KTSFonts$subBt1)
      # }else{
        buttonSM1 <- tcltk::tkbutton(parent = parent,text = text, width = 5, 
                                     command = command, 
                                     background = "palevioletred3", 
                                     foreground = "white", 
                                     font = KTSEnv$KTSFonts$subBt)
      # }

      tcltk::tkpack(buttonSM1, side = "left", expand = TRUE, fill = "both")
    }
    try(tcltk::tkdestroy(KTSEnv$row231), silent = TRUE)
    try(tcltk::tkdestroy(KTSEnv$row232), silent = TRUE)
    row231 <- tcltk::ttkframe(KTSEnv$rows2and3, 
                              borderwidth = 0, relief = "raised")
    subMenu1But(parent = row231, text = "Load", command = loadAllTypes)
    subMenu1But(parent = row231, text = "Remove", command = removeAllTypes)
    subMenu1But(parent = row231, text = "Save", command = saveAllTypes)
    subMenu1But(parent = row231, text = "Export", command = exportall)
    subMenu1But(parent = row231, text = "Rename", command = renameAllTypes)
    subMenu1But(parent = row231, text = "Merge", command = mergeTsOrGap)
    subMenu1But(parent = row231, text = "List", command = refreshDataSetsList)
    tcltk::tkpack(row231, anchor = "nw", fill = "both")
    row232 <- tcltk::ttkframe(KTSEnv$rows2and3, 
                              borderwidth = 0, relief = "raised")
    subMenu1But(parent = row232, text = "Sampling periods", 
                command = anaSamPer)
    subMenu1But(parent = row232, text = "Aggregate", command = aggregateKTS)
    subMenu1But(parent = row232, text = "Cut & resampling", 
                command = selectionTS)
    subMenu1But(parent = row232, text = "Operations", command = composeKTS)
    subMenu1But(parent = row232, text = "Round", command = roundKTS)
    subMenu1But(parent = row232, text = "Scale", command = scaleKTS)
    subMenu1But(parent = row232, text = "Differences", command = diffKTS)
    subMenu1But(parent = row232, text = "Cumulative sum", command = cumuKTS)
    tcltk::tkpack(row232, anchor = "nw", fill = "both")
    assign("row231", row231, envir = KTSEnv)
    assign("row232", row232, envir = KTSEnv)
    assign("activMenu", "tsermenu", envir = KTSEnv)
    loadAllTypes()
  # }
}

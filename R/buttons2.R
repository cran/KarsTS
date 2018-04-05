buttons2 <-
function() {
  if (KTSEnv$activMenu != "gapsetmenu") {
    subMenu2But <- function(parent = NULL, text = "Load", 
                            command = loadAllTypes) {
      buttonSM2 <- tcltk::tkbutton(parent = parent, text = text, width = 5, 
                                   command = command, 
                                   background = "darkolivegreen3", 
                                   foreground = "white", 
                                   font = KTSEnv$KTSFonts$subBt)
      tcltk::tkpack(buttonSM2, side = "left", expand = TRUE, fill = "both")
    }
    try(tcltk::tkdestroy(KTSEnv$row231), silent = TRUE)
    try(tcltk::tkdestroy(KTSEnv$row232), silent = TRUE)
    row231 <- tcltk::ttkframe(KTSEnv$rows2and3, 
                              borderwidth = 0, relief = "raised")
    subMenu2But(parent = row231, text = "Load", command = loadAllTypes)
    subMenu2But(parent = row231, text = "Remove", 
                command = removeAllTypes)
    subMenu2But(parent = row231, text = "Save", command = saveAllTypes)
    subMenu2But(parent = row231, text = "Export", command = exportall)
    subMenu2But(parent = row231, text = "Rename", 
                command = renameAllTypes)
    subMenu2But(parent = row231, text = "Merge", command = mergeTsOrGap)
    subMenu2But(parent = row231, text = "List", 
                command = refreshDataSetsList)
    tcltk::tkpack(row231, anchor = "nw", fill = "both")
    row232 <- tcltk::ttkframe(KTSEnv$rows2and3, 
                              borderwidth = 0, relief = "raised")
    subMenu2But(parent = row232, text = "Gap selection", 
                command = selectionGaps)
    subMenu2But(parent = row232, text = "Artificial random gaps", 
                command = createRandGaps)
    subMenu2But(parent = row232, text = "Artificial specific gaps", 
                command = createSpecGaps)
    subMenu2But(parent = row232, text = "Apply gaps to series", 
                command = applyGap2TSer)
    subMenu2But(parent = row232, text = "Little's MCAR test", 
                command = littleTest)
    
    tcltk::tkpack(row232, anchor = "nw", fill = "both")
    assign("row231", row231, envir = KTSEnv)
    assign("row232", row232, envir = KTSEnv)
    assign("activMenu", "gapsetmenu", envir = KTSEnv)
    loadAllTypes()
  }
}

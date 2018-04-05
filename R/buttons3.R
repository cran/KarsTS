buttons3 <-
function() {
  if (KTSEnv$activMenu != "analymenu") {
    subMenu3But <- function(parent = NULL, text = "Statistics", 
                            command = statisticsKTS) {
      buttonSM3 <- tcltk::tkbutton(parent = parent, text = text, width = 5, 
                                   command = command, 
                                   background = "indianred3", 
                                   foreground = "white", 
                                   font = KTSEnv$KTSFonts$subBt)
      tcltk::tkpack(buttonSM3, side = "left", expand = TRUE, fill = "both")
    }
    try(tcltk::tkdestroy(KTSEnv$row231), silent = TRUE)
    try(tcltk::tkdestroy(KTSEnv$row232), silent = TRUE)
    row231 <- tcltk::ttkframe(KTSEnv$rows2and3, borderwidth = 0, 
                              relief = "raised")
    subMenu3But(parent = row231, text = "Statistics", 
                command = statisticsKTS)
    subMenu3But(parent = row231, text = "Rolling statistics", 
                command = rollStatisticsKTS)
    subMenu3But(parent = row231, text = "Loess decomp.", 
                command = stlplusKTS)
    subMenu3But(parent = row231, text = "Loess smooth.", 
                command = loessKTS)
    subMenu3But(parent = row231, text = "Normality", 
                command = normalityKTS)
    subMenu3But(parent = row231, text = "Stationarity", 
                command = stationarityKTS)
    subMenu3But(parent = row231, text = "Linearity", 
                command = linearityKTS)
    tcltk::tkpack(row231, anchor = "nw", fill = "both")
    row232 <- tcltk::ttkframe(KTSEnv$rows2and3, 
                              borderwidth = 0, relief = "raised")
    subMenu3But(parent = row232, text = "PCA", command = pcaKTS)
    subMenu3But(parent = row232, text = "Mutual info", command = mutInf)
    subMenu3But(parent = row232, text = "Invariants", 
                command = invariantsKTS)
    subMenu3But(parent = row232, text = "Rec. matrix", 
                command = createSimpleRM)
    subMenu3But(parent = row232, text = "Cross rec. matrix", 
                command = createCrossRM)
    subMenu3But(parent = row232, text = "Joint rec. matrix", 
                command = createJointRM)
    subMenu3But(parent = row232, text = "Self-rep. rate", 
                command = determinismKTS)
    subMenu3But(parent = row232, text = "Laminarity", 
                command = laminarityKTS)
    subMenu3But(parent = row232, text = "Theiler's window", 
                command = theilerKTS)
    tcltk::tkpack(row232, anchor = "nw", fill = "both")
    assign("row231", row231, envir = KTSEnv)
    assign("row232", row232, envir = KTSEnv)
    assign("activMenu", "analymenu", envir = KTSEnv)
    refreshDataSetsList(outp = FALSE)
    if (is.null(KTSEnv$dSList$TS) == FALSE) {
      statisticsKTS()
    }
  }
}

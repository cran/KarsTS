buttons4 <-
function() {
  if (KTSEnv$activMenu != "plotmenu") {
    subMenu4But <- function(parent = NULL, text = "Plot ts", 
                            command = plotTimeSeries) {
      buttonSM4 <- tcltk::tkbutton(parent = parent, text = text, width = 5, 
                                   command = command, 
                                   background = "goldenrod3", 
                                   foreground = "white", 
                                   font = KTSEnv$KTSFonts$subBt)
      tcltk::tkpack(buttonSM4, side = "left", expand = TRUE, fill = "both")
    }
    try(tcltk::tkdestroy(KTSEnv$row231), silent = TRUE)
    try(tcltk::tkdestroy(KTSEnv$row232), silent = TRUE)
    row231 <- tcltk::ttkframe(KTSEnv$rows2and3, borderwidth = 0, 
                              relief = "raised")
    subMenu4But(parent = row231, text = "Plot ts", 
                command = plotTimeSeries)
    subMenu4But(parent = row231, text = "Remove points", 
                command = removePoints)
    subMenu4But(parent = row231, text = "Get coordinates", 
                command = getCoordsKTS)
    subMenu4But(parent = row231, text = "Linear correlation", 
                command = linCorrKTS)
    subMenu4But(parent = row231, text = "AMI", 
                command = mutualKTS)
    subMenu4But(parent = row231, text = "Wind Rose", 
                command = windRoseKTS)
    tcltk::tkpack(row231, anchor = "nw", fill = "both")
    row232 <- tcltk::ttkframe(KTSEnv$rows2and3, borderwidth = 0, 
                              relief = "raised")
    subMenu4But(parent = row232, text = "Histogram", command = histKTS)
    subMenu4But(parent = row232, text = "Phase portraits", 
                command = scattTimeSeries)
    subMenu4But(parent = row232, text = "FNN", 
                command = fnnKTS)
    subMenu4But(parent = row232, text = "Ed(1)&Ed(2)", command = E1dAndE2d)
    subMenu4But(parent = row232, text = "Recurrence plot", 
                command = createSimpleRMPlot)
    subMenu4But(parent = row232, text = "Cross rec. plot", 
                command = createCrossRMPlot)
    subMenu4But(parent = row232, text = "Distance plot", 
                command = createDistMatrix)
    subMenu4But(parent = row232, text = "RP", 
                command = RPKTS)
    tcltk::tkpack(row232, anchor = "nw", fill = "both")
    assign("row231", row231, envir = KTSEnv)
    assign("row232", row232, envir = KTSEnv)
    assign("activMenu", "plotmenu", envir = KTSEnv)
    refreshDataSetsList(outp = FALSE)
    if (is.null(KTSEnv$dSList$TS) == FALSE) {
      plotTimeSeries()
    }
  }
}

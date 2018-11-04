buttons5 <-
function() {
  if (KTSEnv$activMenu != "fillmenu") {
    subMenu5But <- function(parent = NULL, text = "Linear", 
                            command = naApproxKTS) {
      buttonSM5 <- tcltk::tkbutton(parent = parent, text = text, width = 5, 
                                   command = command, 
                                   background = "royalblue3",
                                   foreground = "white", 
                                   font = KTSEnv$KTSFonts$subBt)
      tcltk::tkpack(buttonSM5, side = "left", expand = TRUE, fill = "both")
    }
    try(tcltk::tkdestroy(KTSEnv$row231), silent = TRUE)
    try(tcltk::tkdestroy(KTSEnv$row232), silent = TRUE)
    row231 <- tcltk::ttkframe(KTSEnv$rows2and3, borderwidth = 0, 
                              relief = "raised")
    subMenu5But(parent = row231, text = "Stineman's interpolation", 
                command = stinemannKTS)
    subMenu5But(parent = row231, text = "Linear", command = naApproxKTS)
    subMenu5But(parent = row231, text = "Splines", command = naSplinesKTS)
    subMenu5But(parent = row231, text = "ARIMA", command = arimaKalman)
    subMenu5But(parent = row231, text = "Mean value", command = meanValue)
    tcltk::tkpack(row231, anchor = "nw", fill = "both")
    row232 <- tcltk::ttkframe(KTSEnv$rows2and3, borderwidth = 0, 
                              relief = "raised")
    subMenu5But(parent = row232, text = "Multiv.Splines", command = gamKTS)
    subMenu5But(parent = row232, text = "MissForest", command = missForestKTS)
    subMenu5But(parent = row232, text = "Twins", command = fillWithTwins)
    subMenu5But(parent = row232, text = "ARIMAX", command = arimaXKalman)
    subMenu5But(parent = row232, text = "Rm Slope Outliers", 
                command = slopeOutliersBut)
    subMenu5But(parent = row232, text = "Check filling", 
                command = goodnessFilling)
    tcltk::tkpack(row232, anchor = "nw", fill = "both")
    assign("row231", row231, envir = KTSEnv)
    assign("row232", row232, envir = KTSEnv)
    assign("activMenu", "fillmenu", envir = KTSEnv)
    refreshDataSetsList(outp = FALSE)
    if (is.null(KTSEnv$dSList$TS) == FALSE &
        is.null(KTSEnv$dSList$gaps) == FALSE) {
      meanValue()
    }
  }
}

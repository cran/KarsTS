checkIfAnyGapTs <-
function(action = NULL, envirName = KTSEnv) {
  if (class(KTSEnv$dSList$TS) == "character" & 
      class(KTSEnv$dSList$gaps) == "character") {
    eval(call(action), envir = envirName)
  } else {
    if (is.null(KTSEnv$dSList$TS)) {
      tcltk::tkmessageBox(message = paste("There is no time",
                                          "series in the environment"), 
                          icon = "warning")
      loadAllTypes()
    }
    if (is.null(KTSEnv$dSList$gaps)) {
      tcltk::tkmessageBox(message = paste("There is no gap set",
                                          "in the environment"), 
                          icon = "warning")
      loadAllTypes()
    }
  }
}

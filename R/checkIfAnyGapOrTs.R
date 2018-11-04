checkIfAnyGapOrTs <-
function(action = NULL, envirName = KTSEnv) {
  if (class(KTSEnv$dSList$TS) == "character" | 
      class(KTSEnv$dSList$gaps) == "character") {
    eval(call(action), envir = envirName)
  } else {
    tcltk::tkmessageBox(message = paste("There are no time series",
                                        "neither gap sets in the",
                                        "environment"), 
                        icon = "warning")
    loadAllTypes()
  }
}

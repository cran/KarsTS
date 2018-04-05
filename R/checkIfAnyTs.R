checkIfAnyTs <-
function(action = NULL, envirName = KTSEnv) {
  if (class(KTSEnv$dSList$TS) == "character") {
    eval(call(action), envir = envirName)
  } else {
    tcltk::tkmessageBox(message = paste("There is no time series",
                                        "in the environment"), 
                        icon = "warning")
    loadAllTypes()
  }
}

checkIfAny <-
function(action = NULL, envirName = KTSEnv) {
  if (class(c(KTSEnv$dSList$TS, KTSEnv$dSList$gaps, 
              KTSEnv$dSList$rm)) == "character") {
    eval(call(action), envir = envirName)
  } else {
    tcltk::tkmessageBox(message = paste("There are not any data",
                                        "sets in the environment"), 
                        icon = "warning")
    loadAllTypes()
  }
}

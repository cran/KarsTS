checkIfAnyRm <-
function(action = NULL, envirName = KTSEnv) {
  if (class(KTSEnv$dSList$rm) == "character") {
    eval(call(action), envir = envirName)
  } else {
    tcltk::tkmessageBox(message = paste("There is no recurrence matrix",
                                        "in the environment"), 
                        icon = "warning")
    loadAllTypes()
  }
}

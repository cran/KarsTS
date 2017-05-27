createRb <-
function(variable = NULL, dataVector = NULL, 
                     panel = KTSEnv$subPanR4C1) {
  myApplyVector(FUN = createEachRb, dataVector = dataVector, 
                variable = variable, panel = panel)
}

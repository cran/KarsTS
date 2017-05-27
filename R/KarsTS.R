KarsTS <-
function(skipWelcome = FALSE) {
    getScreenSize()
    assign("timeZone", "GMT", envir = KTSEnv)
    loadKarsTSFonts()
    
    if(skipWelcome == FALSE){
      
      wSW <- try(welcomeScreen(), silent = TRUE)
      if (class(wSW) == "try-error") {
        
        try(destroyWelcome, silent = TRUE)
        try(rm(mainPanel.0, envir = KTSEnv), silent = TRUE)
        
      } else {
        
        tcltk::tkwait.window(KTSEnv$mainPanel.0)
        rm(mainPanel.0, envir = KTSEnv)
        
      }
      
    }
    
    assignMultiple(c("heigth4", "width4.1", "width4.2", "titleWidth"), 
                   list(550,300, 50, 30), 
                   envir = KTSEnv)
    mainScreen()
}
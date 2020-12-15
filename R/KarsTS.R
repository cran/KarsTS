KarsTS <-
function(skipWelcome = FALSE) {
  
  # KTSEnv <- new.env(parent = emptyenv())
    # KTSEnv$activButton <- "hello"
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
                   list(550,350, 85, 30), 
                   envir = KTSEnv)
    mainScreen()
}

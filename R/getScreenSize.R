getScreenSize <-
function() {
  
  opeSys <- Sys.info()[[1]]
  
  if(opeSys == "Windows"){
    
    screenWidthWin <- try(system("wmic desktopmonitor get screenwidth", 
                                 intern = TRUE), 
                          silent = TRUE)
    
    if (class(screenWidthWin) != "try-error") {
      
      screenHeightWin <- system("wmic desktopmonitor get screenheight", 
                                intern = TRUE)
      screenSize <- as.numeric(c(screenWidthWin[-c(1, 
                                                   length(screenWidthWin))], 
                                 screenHeightWin[-c(1, 
                                                    length(screenHeightWin))]))
      
      screenSize <- round((2/3) * screenSize)
      names(screenSize) <- c("width", "height")
      assign("screenSize", screenSize, envir = KTSEnv)
      
    }else{
      
      screenSize <- c(900, 500)
      names(screenSize) <- c("width", "height")
      assign("screenSize", screenSize, envir = KTSEnv)
      
    }
    
  }else{
    
    screenSize <- c(900, 500)
    names(screenSize) <- c("width", "height")
    assign("screenSize", screenSize, envir = KTSEnv)
    
  }
  
}

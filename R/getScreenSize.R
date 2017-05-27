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
    
    screenSizeMac <- try(system(paste0("system_profiler ",
                                       "SPDisplaysDataType |grep Resolution"), 
                                intern = TRUE), silent = TRUE)
    
    if (class(screenSizeMac) != "try-error") {
      
      sizeToNumeric <- sapply(strsplit(screenSizeMac, split = NULL), 
                              as.numeric)
      reallyNumeric <- which(is.finite(sizeToNumeric))
      trueCipher <- sizeToNumeric[min(reallyNumeric):max(reallyNumeric)]
      separation <- which(is.na(trueCipher))
      screenWidthMac <- trueCipher[1:(min(separation) - 1)]
      screenHeightMac <- trueCipher[(max(separation) + 1):length(trueCipher)]
      concatenate <- function(X) {
        res <- NULL
        for (i in 1:length(X)) {
          res <- paste0(res, X[i])
        }
        res
      }
      screenWidthMac <- as.numeric(concatenate(as.character(screenWidthMac)))
      screenHeightMac <- as.numeric(concatenate(as.character(screenHeightMac)))
      screenSize <- c(screenWidthMac, screenHeightMac)
      
      
      screenSize <- round((2/3) * screenSize)
      names(screenSize) <- c("width", "height")
      assign("screenSize", screenSize, envir = KTSEnv)
      
    }else{
      
      screenSize <- c(900, 500)
      names(screenSize) <- c("width", "height")
      assign("screenSize", screenSize, envir = KTSEnv)
      
    }
    
  }
  
}

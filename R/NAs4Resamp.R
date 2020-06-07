NAs4Resamp <-
function() {
    N4ROnOk1 <- function() {
      
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                   noValid = NA)
      selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                    noValid = NA)
      perival <- verifyIntEntry(tcltk::tclvalue(KTSEnv$peri), noValid = NA)
      
      if (is.na(selTsName)) {
        
        tcltk::tkmessageBox(message = "Choose a time series", 
                            icon = "warning")
        
      } else if (is.na(selNewName)) {
        
        tcltk::tkmessageBox(message = paste("Give a name for",
                                            "the new time series"), 
                            icon = "warning")
        
      } else if (is.na(perival)) {
        
        tcltk::tkmessageBox(message = paste("Introduce the new sampling",
                                            "period in seconds"), 
                            icon = "warning")
        
      } else {
        
        selTs <- get(selTsName, envir = KTSEnv)
        
        newTStime <- seq(selTs$time[1], selTs$time[nrow(selTs)],perival)
        
        aa <- as.numeric(newTStime)
        bb <- as.numeric(selTs$time)
        aabb <- intersect(aa,bb)
        
        if (setequal(aabb,bb)==FALSE){
          
          tcltk::tkmessageBox(message = paste("The new sampling is not compatible with the original one"), 
                              icon = "warning")
          
        }else{
          
          aabb.Ind <-  apply(as.matrix(1:length(aabb)), 1, 
                             function(x) which(aa == aabb[x]))
          
          
          newTSvalue <- rep(NA,length(aa))   
          newTSvalue[aabb.Ind] <- selTs$value
          newTS <- data.frame(time = newTStime, value = newTSvalue)
          rownames(newTS) <- NULL
          
          assign(paste(selNewName), newTS, KTSEnv)
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANN4R1()
          
        }
        
      }
      
    }
    
    showPANN4R1 <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "UPSAMPLING")
      createTsRb()
      createEntry(labTitle = "Upsampling (s)", textVariableName = "peri", 
                  defaultVal = "")
      createEntry(labTitle = "Output name", textVariableName = "newName")
      createOK(labTitle = "NEXT", action = N4ROnOk1)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANN4R1", 
                 envirName = environment(showPANN4R1))
  }

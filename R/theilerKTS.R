theilerKTS <-
function() {
  
  showPANTheiler <- function() {
    
    createSubPanR4C1()
    createTITLE(labTitle = "APPLY THEILER'S WINDOW")
    createRmRb()
    createEntry(labTitle = "Theiler's window",textVariableName = "theilWin")
    createOK(labTitle = "RUN", action = theilerOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  
  theilerOnOk <- function() {
    
    selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP),
                                 noValid = NA)
    theilerWin <- verifyIntEntry(tcltk::tclvalue(KTSEnv$theilWin), 
                                 noValid = NA)
    
    if (is.na(selRmName)) {
      
      tcltk::tkmessageBox(message = "Choose a recurrence matrix",
                          icon = "warning")
      
    }else if (is.na(theilerWin)){
      
      tcltk::tkmessageBox(message = "Enter the Theiler's window. It must be an integer.",
                          icon = "warning")
      
    }else{
      
      selRm <- get(selRmName, envir = KTSEnv)
      
      if (selRm$type == "cross") {
        tcltk::tkmessageBox(message = paste("Choose a simple or joint",
                                            "recurrence matrix",
                                            "(not cross)"),
                            icon = "warning")
      } else {
        
        newRM <- applyTheiler(RM = selRm, thW = theilerWin)
        assign(paste0(selRmName,"th",theilerWin),newRM, envir = KTSEnv)
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANTheiler()
        
      }
      
    }
    
  }
  
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyRm(action = "showPANTheiler", 
               envirName = environment(showPANTheiler))
  
}

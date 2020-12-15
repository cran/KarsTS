fnnKTS <-
function() {
    fnnOnOk1 <- function() {
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                   noValid = NA)
      embDim <- verifyIntEntry(tcltk::tclvalue(KTSEnv$embDimension), 
                               noValid = NA)
      embDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$embDelays), 
                                 noValid = NA)
      theilerWin <- verifyIntEntry(tcltk::tclvalue(KTSEnv$theilWin), 
                                   noValid = NA)
      escapeFact <- verifyRealEntry(tcltk::tclvalue(KTSEnv$escFact), 
                                    noValid = NA)
      neighDiam <- verifyRealEntry(tcltk::tclvalue(KTSEnv$nDiameter), 
                                   noValid = NA)
      if (is.na(selTsName)) {
        tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
      } else if (is.na(embDim)) {
        tcltk::tkmessageBox(message = paste("Enter the embedding dimension"), 
                            icon = "warning")
      } else if (embDim < 2) {
        tcltk::tkmessageBox(message = paste("The maximum embedding dimension",
                                            "must be an integer greater than 1"), 
                            icon = "warning")
      } else if (is.na(embDelay)) {
        tcltk::tkmessageBox(message = "Enter the delay", icon = "warning")
      } else if (embDelay == 0) {
        tcltk::tkmessageBox(message = paste("The delay must be greater than 0"), 
                            icon = "warning")
      } else if (is.na(theilerWin)) {
        tcltk::tkmessageBox(message = paste("The Theiler window must be an",
                                            "integer greater than",
                                            "or equal to 0"), 
                            icon = "warning")
      } else {
        selTs <- get(tcltk::tclvalue(KTSEnv$selTsP), envir = KTSEnv)
        if (any(is.na(selTs$value))) {
          tcltk::tkmessageBox(message = paste("NAs are not allowed.Make a",
                                              "temporary filling or use a",
                                              "piece of time series",
                                              "without any NAs"), 
                              icon = "warning")
        } else {

            

          if (is.na(escapeFact)) {
            escapeFact <- 10
          }
          if (is.na(neighDiam)) {
            neighDiam <- stats::sd(selTs$value)/10
          }
          fnnResult <- tseriesChaos::false.nearest(selTs$value, m = embDim, 
                                                   d = embDelay, t = theilerWin, 
                                                   rt = escapeFact, 
                                                   eps = neighDiam)
          grDevices::dev.new(noRStudioGD = TRUE)
          tseriesChaos::plot.false.nearest(fnnResult)
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          
        }
      }
    }
    fnnOnOk2 <- function() {
      
      saveThePlot <- function(){
        
        getExtension <- function(selFile) {
          
          selFileSplitRev <- rev(strsplit(selFile, split = NULL)[[1]])
          lastPoint <- min(which(selFileSplitRev == "."))
          
          if(is.finite(lastPoint)){
            
            lengthSelFile <- nchar(selFile)
            exten <- substr(selFile, 
                            lengthSelFile - lastPoint + 2, 
                            lengthSelFile)
            
          }else{
            
            exten <- "none"
            
          }
          
          exten
          
        }
        
        savePopUp <- function(){
          
          onOK <- function() {
            
            filename <- verifyCharEntry(tcltk::tclvalue(entryVar1), noValid = NA)
            if(is.na(filename)){
              filename <- paste0("fig",sample(10000:99999, 1),".tiff")
            }
            KTSEnv$filename <- filename
            
            winW <- verifyIntEntry(tcltk::tclvalue(entryVar2), noValid = NA)
            
            if(is.na(winW)){
              winW <- 15
            }
            KTSEnv$winW <- winW     
            
            winH <- verifyIntEntry(tcltk::tclvalue(entryVar3), noValid = NA)
            if(is.na(winH)){
              winH <- 15
            }
            KTSEnv$winH <- winH
            
            
            resIm <- verifyIntEntry(tcltk::tclvalue(entryVar4), noValid = NA)
            if(is.na(resIm)){
              resIm <- 300
            }
            KTSEnv$resIm <- resIm
            
            tcltk::tkdestroy(KTSEnv$newWin)
            
          }
          
          KTSEnv$newWin <- tcltk::tktoplevel()
          tcltk::tkwm.title(KTSEnv$newWin, "")
          
          entryVar1 <- tcltk::tclVar("")
          ent1 <-tcltk2::tk2entry(KTSEnv$newWin, width = "25",
                                  textvariable = entryVar1)
          text1 <- "File name"
          lab1 <- tcltk2::tk2label(KTSEnv$newWin,
                                   text = text1,
                                   justify = "left")
          
          entryVar2 <- tcltk::tclVar("")
          ent2 <-tcltk2::tk2entry(KTSEnv$newWin, width = "25",
                                  textvariable = entryVar2)
          text2 <- "Width (cm)"
          lab2 <- tcltk2::tk2label(KTSEnv$newWin,
                                   text = text2,
                                   justify = "left")
          
          entryVar3 <- tcltk::tclVar("")
          ent3 <-tcltk2::tk2entry(KTSEnv$newWin, width = "25",
                                  textvariable = entryVar3)
          text3 <- "Height (cm)"
          lab3 <- tcltk2::tk2label(KTSEnv$newWin,
                                   text = text3,
                                   justify = "left")
          
          
          entryVar4 <- tcltk::tclVar("")
          ent4 <-tcltk2::tk2entry(KTSEnv$newWin, width = "25",
                                  textvariable = entryVar4)
          text4 <- "Resolution (ppi)"
          lab4 <- tcltk2::tk2label(KTSEnv$newWin,
                                   text = text4,
                                   justify = "left")
          
          tcltk::tkgrid(lab1,padx = 10, pady = c(15, 5), sticky = "w")
          tcltk::tkgrid(ent1, padx = 10, pady = c(0, 15))
          tcltk::tkgrid(lab2,padx = 10, pady = c(15, 5), sticky = "w")
          tcltk::tkgrid(ent2, padx = 10, pady = c(0, 15))
          tcltk::tkgrid(lab3,padx = 10, pady = c(15, 5), sticky = "w")
          tcltk::tkgrid(ent3, padx = 10, pady = c(0, 15))
          tcltk::tkgrid(lab4,padx = 10, pady = c(15, 5), sticky = "w")
          tcltk::tkgrid(ent4, padx = 10, pady = c(0, 15))
          
          OKbutton <-tcltk::tkbutton(KTSEnv$newWin, text = "OK",
                                     width = -6, command = onOK)
          tcltk::tkgrid(OKbutton, padx = 10, pady = c(5, 15))
          tcltk::tkbind(ent4, "<Return>", onOK)
          tcltk::tkfocus(KTSEnv$newWin)
          
        }
        
        savePopUp()
        tcltk::tkwait.window(KTSEnv$newWin)
        exten <- getExtension(KTSEnv$filename)
        
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        
        if(exten != "tiff" & exten != "png"){ 
          KTSEnv$filename <- paste0(KTSEnv$filename,".tiff")
        }
        
        if( exten == "tiff"){
          
          grDevices::png(filename = KTSEnv$filename,units = "cm",
                         width = KTSEnv$winW, height = KTSEnv$winH,
                         res = KTSEnv$resIm)
          
        }else{
          
          grDevices::png(filename = KTSEnv$filename,units = "cm",
                         width = KTSEnv$winW, height = KTSEnv$winH,
                         res = KTSEnv$resIm) 
          
        }
        
        plotToSave()
        
        grDevices::dev.off()
        
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        
      }
      
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                   noValid = NA)
      embDim <- verifyIntEntry(tcltk::tclvalue(KTSEnv$embDimension), 
                               noValid = NA)
      embDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$embDelays), 
                                 noValid = NA)
      theilerWin <- verifyIntEntry(tcltk::tclvalue(KTSEnv$theilWin), 
                                   noValid = NA)
      escapeFact <- verifyRealEntry(tcltk::tclvalue(KTSEnv$escFact), 
                                    noValid = NA)
      neighDiam <- verifyRealEntry(tcltk::tclvalue(KTSEnv$nDiameter), 
                                   noValid = NA)
      if (is.na(selTsName)) {
        tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
      } else if (is.na(embDim)) {
        tcltk::tkmessageBox(message = paste("Enter the embedding dimension"), 
                            icon = "warning")
      } else if (embDim < 2) {
        tcltk::tkmessageBox(message = paste("The maximum embedding dimension",
                                            "must be an integer greater than 1"), 
                            icon = "warning")
      } else if (is.na(embDelay)) {
        tcltk::tkmessageBox(message = "Enter the delay", icon = "warning")
      } else if (embDelay == 0) {
        tcltk::tkmessageBox(message = paste("The delay must be greater than 0"), 
                            icon = "warning")
      } else if (is.na(theilerWin)) {
        tcltk::tkmessageBox(message = paste("The Theiler window must be an",
                                            "integer greater than",
                                            "or equal to 0"), 
                            icon = "warning")
      } else {
        selTs <- get(tcltk::tclvalue(KTSEnv$selTsP), envir = KTSEnv)
        if (any(is.na(selTs$value))) {
          tcltk::tkmessageBox(message = paste("NAs are not allowed.Make a",
                                              "temporary filling or use a",
                                              "piece of time series",
                                              "without any NAs"), 
                              icon = "warning")
        } else {
          
          
          
          if (is.na(escapeFact)) {
            escapeFact <- 10
          }
          if (is.na(neighDiam)) {
            neighDiam <- stats::sd(selTs$value)/10
          }
          fnnResult <- tseriesChaos::false.nearest(selTs$value, m = embDim, 
                                                   d = embDelay, t = theilerWin, 
                                                   rt = escapeFact, 
                                                   eps = neighDiam)
          
          plotToSave <- function(){
            tseriesChaos::plot.false.nearest(fnnResult)
          }
          saveThePlot()

          
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          
        }
      }
    }
    showPANfnn <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "FALSE NEAREST NEIGHBORS")
      createTsRb()
      createTitle(labTitle = "Parameters")
      createEntry(labTitle = "Maximum embedding dimension", 
                  textVariableName = "embDimension")
      createEntry(labTitle = "Delay (in lags)", 
                  textVariableName = "embDelays")
      createEntry(labTitle = "Theiler window (in lags)", 
                  textVariableName = "theilWin")
      createEntry(labTitle = "Escape factor(optional)", 
                  textVariableName = "escFact")
      createEntry(labTitle = "Neighborhood diameter", 
                  textVariableName = "nDiameter")
      createOK(labTitle = "PLOT", action = fnnOnOk1)
      createOK(labTitle = "PLOT TO FILE", action = fnnOnOk2,width = 14)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANfnn", envirName = environment(showPANfnn))
  }

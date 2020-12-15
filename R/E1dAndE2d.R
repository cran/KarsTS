E1dAndE2d <-
function() {
    showPANed1ed2 <- function() {
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "Ed(1) AND Ed(2)")
        createTsRb()
        createEntry(labTitle = "Delay (in lags)", 
                    textVariableName = "timeDelay")
        createOK(labTitle = "PLOT", action = ed1ed2OnOk1)
        createOK(labTitle = "PLOT TO FILE", action = ed1ed2OnOk2, width = 14)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    ed1ed2OnOk1 <- function() {
      
      E1dAndE2dPlot <- function() {
        LL <- length(selTs$value)
        rs <- nonlinearTseries::estimateEmbeddingDim(time.series = selTs$value, 
                                                     number.points = LL, 
                                                     time.lag = timDelay, 
                                                     max.embedding.dim = 15, 
                                                     threshold = 0.95, 
                                                     max.relative.change = 0.1, 
                                                     do.plot = TRUE, 
                                                     main = "Ed(1) & Ed(2)")
      }
      
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                     noValid = NA)
        timDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$timeDelay), 
                                   noValid = NA)
        if (is.na(selTsName)) {
            tcltk::tkmessageBox(message = "Choose a time series", 
                                icon = "warning")
        } else if (is.na(timDelay)) {
            tcltk::tkmessageBox(message = "Choose a delay", icon = "warning")
        } else if (timDelay == 0) {
            tcltk::tkmessageBox(message = paste("The lag delay must",
                                                "be 1 at least"), 
                                icon = "warning")
        } else {
            selTs <- get(selTsName, envir = KTSEnv)
            if (any(is.na(selTs$value))) {
                tcltk::tkmessageBox(message = paste("The time series",
                                                    "cannot have any NAs"), 
                  icon = "warning")
            } else {
              tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
              grDevices::dev.new(noRStudioGD = TRUE)
              E1dAndE2dPlot()
              tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
            }
        }
    }
    ed1ed2OnOk2 <- function() {
      
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
      
      E1dAndE2dPlot <- function() {
        LL <- length(selTs$value)
        rs <- nonlinearTseries::estimateEmbeddingDim(time.series = selTs$value, 
                                                     number.points = LL, 
                                                     time.lag = timDelay, 
                                                     max.embedding.dim = 15, 
                                                     threshold = 0.95, 
                                                     max.relative.change = 0.1, 
                                                     do.plot = TRUE, 
                                                     main = "Ed(1) & Ed(2)")
      }
      
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                   noValid = NA)
      timDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$timeDelay), 
                                 noValid = NA)
      if (is.na(selTsName)) {
        tcltk::tkmessageBox(message = "Choose a time series", 
                            icon = "warning")
      } else if (is.na(timDelay)) {
        tcltk::tkmessageBox(message = "Choose a delay", icon = "warning")
      } else if (timDelay == 0) {
        tcltk::tkmessageBox(message = paste("The lag delay must",
                                            "be 1 at least"), 
                            icon = "warning")
      } else {
        selTs <- get(selTsName, envir = KTSEnv)
        if (any(is.na(selTs$value))) {
          tcltk::tkmessageBox(message = paste("The time series",
                                              "cannot have any NAs"), 
                              icon = "warning")
        } else {
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
          
          plotToSave <- function(){
            E1dAndE2dPlot()
          }
          saveThePlot()
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        }
      }
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANed1ed2", 
                 envirName = environment(showPANed1ed2))
}

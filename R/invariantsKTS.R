invariantsKTS <-
function() {
  invar1OnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else {
      selTs <- get(selTsName, envir = KTSEnv)
      if (any(is.na(selTs$value))) {
        tcltk::tkmessageBox(message = paste("The time series can't have",
                                            "NAs. Make a preliminary",
                                            "filling"), 
                            icon = "warning")
      } else {
        assign("selTsName", selTsName, envir = KTSEnv)
        showPANinvar2()
      }
    }
  }
  invar2OnOk1 <- function() {
    selDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$delay), noValid = NA)
    selTheilerWin <- verifyIntEntry(tcltk::tclvalue(KTSEnv$theilerWin), 
                                    noValid = NA)
    selcorrel <- verifyIntEntry(tcltk::tclvalue(KTSEnv$correl), noValid = NA)
    selMinEmb <- verifyIntEntry(tcltk::tclvalue(KTSEnv$minEmb), noValid = NA)
    selMaxEmb <- verifyIntEntry(tcltk::tclvalue(KTSEnv$maxEmb), noValid = NA)
    selMinRad <- verifyRealEntry(tcltk::tclvalue(KTSEnv$minRad), noValid = NA)
    selMaxRad <- verifyRealEntry(tcltk::tclvalue(KTSEnv$maxRad), noValid = NA)
    selNumRad <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numRad), noValid = NA)
    if (is.na(selDelay)) {
      tcltk::tkmessageBox(message = paste("Choose the delay",
                                          "for the embedding"), 
                          icon = "warning")
    } else if (is.na(selTheilerWin)) {
      tcltk::tkmessageBox(message = paste("Choose the Theiler window"), 
                          icon = "warning")
    } else if (is.na(selcorrel)) {
      tcltk::tkmessageBox(message = paste("Choose the correltion order"), 
                          icon = "warning")
    } else if (is.na(selMinEmb)) {
      tcltk::tkmessageBox(message = paste("Choose the minimum",
                                          "embedding dimension"), 
                          icon = "warning")
    } else if (is.na(selMaxEmb)) {
      tcltk::tkmessageBox(message = paste("Choose the maximum",
                                          "embedding dimension"), 
                          icon = "warning")
    } else if (is.na(selMinRad)) {
      tcltk::tkmessageBox(message = paste("Choose the minimum radius"), 
                          icon = "warning")
    } else if (is.na(selMaxRad)) {
      tcltk::tkmessageBox(message = paste("Choose the maximum radius"), 
                          icon = "warning")
    } else if (is.na(selNumRad)) {
      tcltk::tkmessageBox(message = paste("Choose the number of",
                                          "radii to use"), 
                          icon = "warning")
    } else {
      selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      resCorrDim <- nonlinearTseries::corrDim(time.series = selTs$value, 
                                              min.embedding.dim = selMinEmb, 
                                              max.embedding.dim = selMaxEmb, 
                                              time.lag = selDelay, 
                                              min.radius = selMinRad, 
                                              max.radius = selMaxRad, 
                                              corr.order = selcorrel, 
                                              n.points.radius = selNumRad, 
                                              theiler.window = selTheilerWin, 
                                              do.plot = FALSE, 
                                              localScalingExp = TRUE)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      if (length(resCorrDim[[1]]) == 0) {
        tcltk::tkmessageBox(message = paste("The output was empty.",
                                            "You can try using less",
                                            "restrictive conditions"), 
                            icon = "warning")
      } else {
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        
        corrMdf <- round(nonlinearTseries::corrMatrix(resCorrDim), 5)
        corrMdf <- as.data.frame(corrMdf)
        colnames(corrMdf) <- signif(nonlinearTseries::radius(resCorrDim), 5)
        if (tcltk::tclvalue(KTSEnv$doCorrSum) == "1") {
          txt1 <- "CORRELATION SUMS MATRIX"
          txt2 <- paste("Time series:", KTSEnv$selTsName)
          txt3 <- paste("Delay:", selDelay)
          txt4 <- paste("Theiler window:", selTheilerWin)
          txt5 <- paste("Correlation order(q):", selcorrel)
          txt6 <- "Each column corresponds to a radius"
          txt7 <- "Each row corresponds to an embedding dimension"
          txtCorrM <- utils::capture.output(print.data.frame(corrMdf))
          txtAll <- c(txt1, txt2, txt3, txt4, txt5, txt6, txt7)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtAll, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtCorrM, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))

          grDevices::dev.new(noRStudioGD = TRUE)
          graphics::plot(resCorrDim)

        }
        if (tcltk::tclvalue(KTSEnv$doSampEntr) == "1") {
          resSampEnt <- nonlinearTseries::sampleEntropy(resCorrDim, 
                                                        do.plot = FALSE)
          sampEntropy <- as.data.frame(round(resSampEnt$sample.entropy, 5))
          icbSiF <- nonlinearTseries::radius(resSampEnt)
          colnames(sampEntropy) <- signif(icbSiF, 5)
          txt1 <- "SAMPLE ENTROPY MATRIX"
          txt2 <- paste("Time series:", KTSEnv$selTsName)
          txt3 <- paste("Delay:", selDelay)
          txt4 <- paste("Theiler window:", selTheilerWin)
          txt5 <- paste("Correlation order (q):", selcorrel)
          txt6 <- "Each column corresponds to a radius"
          txt7 <- "Each row corresponds to an embedding dimension"
          txtSampE <- utils::capture.output(print.data.frame(sampEntropy))
          txtAll <- c(txt1, txt2, txt3, txt4, txt5, txt6, txt7)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtAll, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtSampE, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))

          grDevices::dev.new(noRStudioGD = TRUE)
          graphics::plot(resSampEnt)
          
        }
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        showPANinvar1()
      }
    }
  }
  invar2OnOk2 <- function() {
    
    saveThePlot1 <- function(){
      
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
        tcltk::tkwm.title(KTSEnv$newWin, "Correlation sum and dim")
        
        entryVar1 <- tcltk::tclVar("")
        ent1 <-tcltk2::tk2entry(KTSEnv$newWin, width = "50",
                                textvariable = entryVar1)
        text1 <- "File name"
        lab1 <- tcltk2::tk2label(KTSEnv$newWin,
                                 text = text1,
                                 justify = "left")
        
        entryVar2 <- tcltk::tclVar("")
        ent2 <-tcltk2::tk2entry(KTSEnv$newWin, width = "50",
                                textvariable = entryVar2)
        text2 <- "Width (cm)"
        lab2 <- tcltk2::tk2label(KTSEnv$newWin,
                                 text = text2,
                                 justify = "left")
        
        entryVar3 <- tcltk::tclVar("")
        ent3 <-tcltk2::tk2entry(KTSEnv$newWin, width = "50",
                                textvariable = entryVar3)
        text3 <- "Height (cm)"
        lab3 <- tcltk2::tk2label(KTSEnv$newWin,
                                 text = text3,
                                 justify = "left")
        
        
        entryVar4 <- tcltk::tclVar("")
        ent4 <-tcltk2::tk2entry(KTSEnv$newWin, width = "50",
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
      
      plotToSave1()
      
      grDevices::dev.off()
      
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      
    }
    
    saveThePlot2 <- function(){
      
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
        tcltk::tkwm.title(KTSEnv$newWin, "Entropy")
        
        entryVar1 <- tcltk::tclVar("")
        ent1 <-tcltk2::tk2entry(KTSEnv$newWin, width = "50",
                                textvariable = entryVar1)
        text1 <- "File name"
        lab1 <- tcltk2::tk2label(KTSEnv$newWin,
                                 text = text1,
                                 justify = "left")
        
        entryVar2 <- tcltk::tclVar("")
        ent2 <-tcltk2::tk2entry(KTSEnv$newWin, width = "50",
                                textvariable = entryVar2)
        text2 <- "Width (cm)"
        lab2 <- tcltk2::tk2label(KTSEnv$newWin,
                                 text = text2,
                                 justify = "left")
        
        entryVar3 <- tcltk::tclVar("")
        ent3 <-tcltk2::tk2entry(KTSEnv$newWin, width = "50",
                                textvariable = entryVar3)
        text3 <- "Height (cm)"
        lab3 <- tcltk2::tk2label(KTSEnv$newWin,
                                 text = text3,
                                 justify = "left")
        
        
        entryVar4 <- tcltk::tclVar("")
        ent4 <-tcltk2::tk2entry(KTSEnv$newWin, width = "50",
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
      
      plotToSave2()
      
      grDevices::dev.off()
      
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      
    }
    
    selDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$delay), noValid = NA)
    selTheilerWin <- verifyIntEntry(tcltk::tclvalue(KTSEnv$theilerWin), 
                                    noValid = NA)
    selcorrel <- verifyIntEntry(tcltk::tclvalue(KTSEnv$correl), noValid = NA)
    selMinEmb <- verifyIntEntry(tcltk::tclvalue(KTSEnv$minEmb), noValid = NA)
    selMaxEmb <- verifyIntEntry(tcltk::tclvalue(KTSEnv$maxEmb), noValid = NA)
    selMinRad <- verifyRealEntry(tcltk::tclvalue(KTSEnv$minRad), noValid = NA)
    selMaxRad <- verifyRealEntry(tcltk::tclvalue(KTSEnv$maxRad), noValid = NA)
    selNumRad <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numRad), noValid = NA)
    if (is.na(selDelay)) {
      tcltk::tkmessageBox(message = paste("Choose the delay",
                                          "for the embedding"), 
                          icon = "warning")
    } else if (is.na(selTheilerWin)) {
      tcltk::tkmessageBox(message = paste("Choose the Theiler window"), 
                          icon = "warning")
    } else if (is.na(selcorrel)) {
      tcltk::tkmessageBox(message = paste("Choose the correltion order"), 
                          icon = "warning")
    } else if (is.na(selMinEmb)) {
      tcltk::tkmessageBox(message = paste("Choose the minimum",
                                          "embedding dimension"), 
                          icon = "warning")
    } else if (is.na(selMaxEmb)) {
      tcltk::tkmessageBox(message = paste("Choose the maximum",
                                          "embedding dimension"), 
                          icon = "warning")
    } else if (is.na(selMinRad)) {
      tcltk::tkmessageBox(message = paste("Choose the minimum radius"), 
                          icon = "warning")
    } else if (is.na(selMaxRad)) {
      tcltk::tkmessageBox(message = paste("Choose the maximum radius"), 
                          icon = "warning")
    } else if (is.na(selNumRad)) {
      tcltk::tkmessageBox(message = paste("Choose the number of",
                                          "radii to use"), 
                          icon = "warning")
    } else {
      selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      resCorrDim <- nonlinearTseries::corrDim(time.series = selTs$value, 
                                              min.embedding.dim = selMinEmb, 
                                              max.embedding.dim = selMaxEmb, 
                                              time.lag = selDelay, 
                                              min.radius = selMinRad, 
                                              max.radius = selMaxRad, 
                                              corr.order = selcorrel, 
                                              n.points.radius = selNumRad, 
                                              theiler.window = selTheilerWin, 
                                              do.plot = FALSE, 
                                              localScalingExp = TRUE)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      if (length(resCorrDim[[1]]) == 0) {
        tcltk::tkmessageBox(message = paste("The output was empty.",
                                            "You can try using less",
                                            "restrictive conditions"), 
                            icon = "warning")
      } else {
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        
        corrMdf <- round(nonlinearTseries::corrMatrix(resCorrDim), 5)
        corrMdf <- as.data.frame(corrMdf)
        colnames(corrMdf) <- signif(nonlinearTseries::radius(resCorrDim), 5)
        if (tcltk::tclvalue(KTSEnv$doCorrSum) == "1") {
          txt1 <- "CORRELATION SUMS MATRIX"
          txt2 <- paste("Time series:", KTSEnv$selTsName)
          txt3 <- paste("Delay:", selDelay)
          txt4 <- paste("Theiler window:", selTheilerWin)
          txt5 <- paste("Correlation order(q):", selcorrel)
          txt6 <- "Each column corresponds to a radius"
          txt7 <- "Each row corresponds to an embedding dimension"
          txtCorrM <- utils::capture.output(print.data.frame(corrMdf))
          txtAll <- c(txt1, txt2, txt3, txt4, txt5, txt6, txt7)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtAll, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtCorrM, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          
          plotToSave1 <- function(){
            graphics::plot(resCorrDim)
          }
          saveThePlot1()
          
          
        }
        if (tcltk::tclvalue(KTSEnv$doSampEntr) == "1") {
          resSampEnt <- nonlinearTseries::sampleEntropy(resCorrDim, 
                                                        do.plot = FALSE)
          sampEntropy <- as.data.frame(round(resSampEnt$sample.entropy, 5))
          icbSiF <- nonlinearTseries::radius(resSampEnt)
          colnames(sampEntropy) <- signif(icbSiF, 5)
          txt1 <- "SAMPLE ENTROPY MATRIX"
          txt2 <- paste("Time series:", KTSEnv$selTsName)
          txt3 <- paste("Delay:", selDelay)
          txt4 <- paste("Theiler window:", selTheilerWin)
          txt5 <- paste("Correlation order (q):", selcorrel)
          txt6 <- "Each column corresponds to a radius"
          txt7 <- "Each row corresponds to an embedding dimension"
          txtSampE <- utils::capture.output(print.data.frame(sampEntropy))
          txtAll <- c(txt1, txt2, txt3, txt4, txt5, txt6, txt7)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtAll, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(txtSampE, collapse = "\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          
          plotToSave2 <- function(){
            graphics::plot(resSampEnt)
          }
          saveThePlot2()
          
          
        }
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        showPANinvar1()
      }
    }
  }
  
  showPANinvar1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "INVARIANTS")
    createTsRb()
    createOK(labTitle = "NEXT", action = invar1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  showPANinvar2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "INVARIANTS")
    createNote(labTitle = paste("Time series:", KTSEnv$selTsName), 
               pady = c(10,5))
    createTitle(labTitle = "Inputs")
    createEntry(labTitle = "Delay", textVariableName = "delay")
    createEntry(labTitle = "Theiler window", textVariableName = "theilerWin")
    createEntry(labTitle = "Correlation order (q)", 
                textVariableName = "correl", 
                defaultVal = "2")
    createEntry(labTitle = "Min. embedding dim.", textVariableName = "minEmb", 
                defaultVal = "1")
    createEntry(labTitle = "Max. embedding dim.", textVariableName = "maxEmb", 
                defaultVal = "10")
    createEntry(labTitle = "Min. radius.", textVariableName = "minRad")
    createEntry(labTitle = "Max. radius.", textVariableName = "maxRad")
    createEntry(labTitle = "Number of radii", textVariableName = "numRad")
    createTitle(labTitle = "Outputs")
    createChb(labTitle = "Correlation sum and dimension", 
              variableName = "doCorrSum", 
              defaultVal = "1")
    createChb(labTitle = "KS Entropy", variableName = "doSampEntr", 
              defaultVal = "1")
    createOK(labTitle = "PLOT", action = invar2OnOk1)
    createOK(labTitle = "PLOT TO FILE", action = invar2OnOk2, width = 14)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANinvar1", 
               envirName = environment(showPANinvar1))
}

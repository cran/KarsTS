createCrossRMPlot <-
function() {
  
    showPANcrmp1 <- function() {
      
      createSubPanR4C1()
      createTITLE(labTitle = "CROSS RECURRENCE PLOT")
      createRmRb()
      createOK(labTitle = "NEXT", action = plotcrmpzoom1)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    
    plotcrmpzoom1 <- function() {
      
      selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP),
                                   noValid = NA)
      
      if (is.na(selRmName)) {
        
        tcltk::tkmessageBox(message = "Choose a recurrence matrix",
                            icon = "warning")
      }else{
        
        selRm <- get(selRmName, envir = KTSEnv)
        
        if (selRm$type != "cross") {
          
          tcltk::tkmessageBox(message = paste("Choose a cross",
                                              "recurrence matrix"),
                              icon = "warning")
          
        } else {
          
          KTSEnv$selRmName <- selRmName
          showPANcrmp2()
          
        }
        
      }
      
    }
    
    showPANcrmp2 <- function(){
      
      defName1 <- get(KTSEnv$selRmName, envir = KTSEnv)$tsName[1]
      defName2 <- get(KTSEnv$selRmName, envir = KTSEnv)$tsName[2]
      
      createSubPanR4C1()
      createTITLE(labTitle = "RECURRENCE PLOT")
      
      createEntry(labTitle = "Color",
                  textVariableName = "CoLoR", defaultVal = "darkred")

      createEntry(labTitle = "X Label",
                  textVariableName = "xlab", defaultVal = defName1)
      
      createEntry(labTitle = "Y Label",
                  textVariableName = "ylab", defaultVal = defName2)
      
      createEntry(labTitle = "Labels size",
                  textVariableName = "labSi", defaultVal = "1")
      
      createEntry(labTitle = "Ticks size",
                  textVariableName = "tickSi", defaultVal = "1")
      
      createEntry(labTitle = "Point size", 
                  textVariableName = "poiS", defaultVal = "0.3")
      
      createEntry(labTitle = "Left margin", 
                  textVariableName = "lemar", defaultVal = "5")
      
      createEntry(labTitle = "Lower margin", 
                  textVariableName = "lomar", defaultVal = "5")
      
      createEntry(labTitle = "Ticks location", 
                  textVariableName = "tiLo", defaultVal = "1")
      
      createEntry(labTitle = "Labels location", 
                  textVariableName = "laLo", defaultVal = "3")
      createOK(labTitle = "PLOT", action = plotcrmpzoom2a)
      createOK(labTitle = "PLOT TO FILE", action = plotcrmpzoom2b, width = 14)
      createOK(labTitle = "ZOOM", action = plotSelRm, width = 14)
      
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    
    plotcrmpzoom2a <- function() {
      
      recurrPlot <- function(selRm) {
        
        lengthTS <- KTSEnv$selRm$tsLength - (KTSEnv$selRm$embDim - 1) * KTSEnv$selRm$delay
        format <- findDateFormat(KTSEnv$selRm$tsIni[1], tz = KTSEnv$timeZone)
        tsTime1 <- seq(strptime(KTSEnv$selRm$tsIni[1], format = format,
                                tz = KTSEnv$timeZone),
                       by = KTSEnv$selRm$samPerSec[1], length.out = lengthTS[1])
        format <- findDateFormat(KTSEnv$selRm$tsIni[2], tz = KTSEnv$timeZone)
        tsTime2 <- seq(strptime(KTSEnv$selRm$tsIni[2], format = format,
                                tz = KTSEnv$timeZone),
                       by = KTSEnv$selRm$samPerSec[2], length.out = lengthTS[2])
        
        X <- KTSEnv$selRm$ones$X
        Y <- KTSEnv$selRm$ones$Y
        nRecurrPoints <- nrow(KTSEnv$selRm$ones)
        
        graphics::par(mar = c(KTSEnv$lomars, KTSEnv$lemars, 1, 1),
                      mgp = c(KTSEnv$laLos, KTSEnv$tiLos, 0))
        graphics::plot(tsTime1[X],tsTime2[Y], type = "p",
                       col = KTSEnv$CoLoRs, xlab = KTSEnv$xlabs,
                       ylab = KTSEnv$ylabs, cex = KTSEnv$poiSs, 
                       cex.lab = KTSEnv$labSis,asp = 1,
                       pch = 19, bg = KTSEnv$CoLoRs, 
                       cex.axis = KTSEnv$tickSis)


      }

      selRm <- get(KTSEnv$selRmName, envir = KTSEnv)
      
      KTSEnv$selRm <- get(KTSEnv$selRmName, envir = KTSEnv)
      
      KTSEnv$xlabs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$xlab),
                                      noValid = NA)
      KTSEnv$ylabs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$ylab),
                                      noValid = NA)
      KTSEnv$labSis <- verifyRealEntry(tcltk::tclvalue(KTSEnv$labSi),
                                       noValid = NA)
      KTSEnv$tickSis <- verifyRealEntry(tcltk::tclvalue(KTSEnv$tickSi),
                                        noValid = NA)
      KTSEnv$CoLoRs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$CoLoR),
                                       noValid = NA)
      KTSEnv$poiSs <- verifyRealEntry(tcltk::tclvalue(KTSEnv$poiS),
                                      noValid = NA) 
      KTSEnv$lemars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lemar),
                                      noValid = NA)
      
      KTSEnv$lomars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lomar),
                                      noValid = NA)
      KTSEnv$tiLos <- verifyIntEntry(tcltk::tclvalue(KTSEnv$tiLo),
                                     noValid = NA)
      
      KTSEnv$laLos <- verifyIntEntry(tcltk::tclvalue(KTSEnv$laLo),
                                     noValid = NA)
      
      
      if(is.na(KTSEnv$CoLoR)){KTSEnv$CoLoRs <- "darkred"}
      if(is.na(KTSEnv$poiSs)){KTSEnv$poiSs <- 0.3}
      if(is.na(KTSEnv$xlabs)){KTSEnv$xlabs <- KTSEnv$selRm$tsName[1]}
      if(is.na(KTSEnv$ylabs)){KTSEnv$ylabs <- KTSEnv$selRm$tsName[1]}
      if(is.na(KTSEnv$labSis)){KTSEnv$labSis <- 1}
      if(is.na(KTSEnv$lemars)){KTSEnv$lemars <- 5}
      if(is.na(KTSEnv$lomars)){KTSEnv$lomars <- 5}
      if(is.na(KTSEnv$laLos)){KTSEnv$laLos <- 3}
      if(is.na(KTSEnv$tiLos)){KTSEnv$tiLos <- 1}
      
      lengthTS <- KTSEnv$selRm$tsLength - (KTSEnv$selRm$embDim - 1) * KTSEnv$selRm$delay
      format <- findDateFormat(KTSEnv$selRm$tsIni[1], tz = KTSEnv$timeZone)
      tsTime1 <- seq(strptime(KTSEnv$selRm$tsIni[1], format = format,
                              tz = KTSEnv$timeZone),
                     by = KTSEnv$selRm$samPerSec[1], length.out = lengthTS[1])
      format <- findDateFormat(KTSEnv$selRm$tsIni[2], tz = KTSEnv$timeZone)
      tsTime2 <- seq(strptime(KTSEnv$selRm$tsIni[2], format = format,
                              tz = KTSEnv$timeZone),
                     by = KTSEnv$selRm$samPerSec[2], length.out = lengthTS[2])
      
      grDevices::dev.new(noRStudioGD = TRUE)
      tsPlot <- try(recurrPlot(selRm), silent = TRUE)
      

      if(class(tsPlot) == "try-error"){
        
        tcltk::tkmessageBox(message = paste("The plotting failed",
                                            "probably because the",
                                            "matrix is too large.",
                                            "You can try to ",
                                            "save it to a file directly",
                                            "or to reduce the tolerance"),
                            icon = "warning")
        
        
        grDevices::dev.off()
        
      }
        

    }
    
    plotcrmpzoom2b <- function() {
      
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
          
          entryVar1 <- tcltk::tclVar(KTSEnv$selRmName)
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
      
      plotToSave <- function(){
        
        selRm <- get(KTSEnv$selRmName, envir = KTSEnv)
        
        KTSEnv$selRm <- get(KTSEnv$selRmName, envir = KTSEnv)
        
        KTSEnv$xlabs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$xlab),
                                        noValid = NA)
        KTSEnv$ylabs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$ylab),
                                        noValid = NA)
        KTSEnv$labSis <- verifyRealEntry(tcltk::tclvalue(KTSEnv$labSi),
                                         noValid = NA)
        KTSEnv$tickSis <- verifyRealEntry(tcltk::tclvalue(KTSEnv$tickSi),
                                          noValid = NA)
        KTSEnv$CoLoRs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$CoLoR),
                                         noValid = NA)
        KTSEnv$poiSs <- verifyRealEntry(tcltk::tclvalue(KTSEnv$poiS),
                                        noValid = NA) 
        KTSEnv$lemars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lemar),
                                        noValid = NA)
        
        KTSEnv$lomars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lomar),
                                        noValid = NA)
        KTSEnv$tiLos <- verifyIntEntry(tcltk::tclvalue(KTSEnv$tiLo),
                                       noValid = NA)
        
        KTSEnv$laLos <- verifyIntEntry(tcltk::tclvalue(KTSEnv$laLo),
                                       noValid = NA)
        
        
        if(is.na(KTSEnv$CoLoR)){KTSEnv$CoLoRs <- "darkred"}
        if(is.na(KTSEnv$poiSs)){KTSEnv$poiSs <- 0.3}
        if(is.na(KTSEnv$xlabs)){KTSEnv$xlabs <- KTSEnv$selRm$tsName[1]}
        if(is.na(KTSEnv$ylabs)){KTSEnv$ylabs <- KTSEnv$selRm$tsName[1]}
        if(is.na(KTSEnv$labSis)){KTSEnv$labSis <- 1}
        if(is.na(KTSEnv$lemars)){KTSEnv$lemars <- 5}
        if(is.na(KTSEnv$lomars)){KTSEnv$lomars <- 5}
        if(is.na(KTSEnv$laLos)){KTSEnv$laLos <- 3}
        if(is.na(KTSEnv$tiLos)){KTSEnv$tiLos <- 1}
        
        lengthTS <- KTSEnv$selRm$tsLength - (KTSEnv$selRm$embDim - 1) * KTSEnv$selRm$delay
        format <- findDateFormat(KTSEnv$selRm$tsIni[1], tz = KTSEnv$timeZone)
        tsTime1 <- seq(strptime(KTSEnv$selRm$tsIni[1], format = format,
                                tz = KTSEnv$timeZone),
                       by = KTSEnv$selRm$samPerSec[1], length.out = lengthTS[1])
        format <- findDateFormat(KTSEnv$selRm$tsIni[2], tz = KTSEnv$timeZone)
        tsTime2 <- seq(strptime(KTSEnv$selRm$tsIni[2], format = format,
                                tz = KTSEnv$timeZone),
                       by = KTSEnv$selRm$samPerSec[2], length.out = lengthTS[2])
        

        X <- KTSEnv$selRm$ones$X
        Y <- KTSEnv$selRm$ones$Y
        nRecurrPoints <- nrow(KTSEnv$selRm$ones)
        
        graphics::par(mar = c(KTSEnv$lomars, KTSEnv$lemars, 1, 1),
                      mgp = c(KTSEnv$laLos, KTSEnv$tiLos, 0))
        graphics::plot(tsTime1[X],tsTime2[Y], type = "p",
                       col = KTSEnv$CoLoRs, xlab = KTSEnv$xlabs,
                       ylab = KTSEnv$ylabs, cex = KTSEnv$poiSs, 
                       cex.lab = KTSEnv$labSis,asp = 1,
                       pch = 19, bg = KTSEnv$CoLoRs, 
                       cex.axis = KTSEnv$tickSis)
        
        
        
        
      }
      
      saveThePlot()
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      
      

    }
    
    plotSelRm <- function(){
      

      idkts.srm<- function(selRm, col, cex){
        
        rr <- graphics::locator(n = 1)
        if(class(rr)!="try-error"){
          
          rr <- unlist(rr)
          KTSEnv$indicesToRedden <- rbind(KTSEnv$indicesToRedden,rr)
          if(nrow(KTSEnv$indicesToRedden)==5){
            
            borrar <- KTSEnv$indicesToRedden[1,] 
            KTSEnv$indicesToRedden <- KTSEnv$indicesToRedden[-1,] 
            graphics::points(borrar[1],borrar[2], col = "white", cex=cex)
            
          }
          
          
          graphics::points(KTSEnv$indicesToRedden, col = col, cex=cex)
          
        }
        
        try(idkts.srm(selRm, col, cex), silent = TRUE)
        
        
      }
      
      
      col <- "black"
      cex  <- 2
      try(idkts.srm(selRm=KTSEnv$selRm, col=col, cex=cex), silent = FALSE)
      
      
      
      if(is.null(KTSEnv$indicesToRedden)){
        
        tcltk::tkmessageBox(message = paste("Select, at least, 2 points"),
                            icon = "warning")
        
        
      }else if(nrow(KTSEnv$indicesToRedden)<2){
        
        tcltk::tkmessageBox(message = paste("Select, at least, 2 points"),
                            icon = "warning")
        
      }else{
        
        izq <- min(KTSEnv$indicesToRedden[,1])
        dcha <- max(KTSEnv$indicesToRedden[,1])
        arr <- max(KTSEnv$indicesToRedden[,2])
        aba <- min(KTSEnv$indicesToRedden[,2])
        
        
        lengthTS <- KTSEnv$selRm$tsLength - (KTSEnv$selRm$embDim - 1) * KTSEnv$selRm$delay
        format <- findDateFormat(KTSEnv$selRm$tsIni[1], tz = KTSEnv$timeZone)
        tsTime1 <- seq(strptime(KTSEnv$selRm$tsIni[1], format = format,
                                tz = KTSEnv$timeZone),
                       by = KTSEnv$selRm$samPerSec[1], length.out = lengthTS[1])
        format <- findDateFormat(KTSEnv$selRm$tsIni[2], tz = KTSEnv$timeZone)
        tsTime2 <- seq(strptime(KTSEnv$selRm$tsIni[2], format = format,
                                tz = KTSEnv$timeZone),
                       by = KTSEnv$selRm$samPerSec[2], length.out = lengthTS[2])
        
        tsTimeN1 <- as.numeric(tsTime1)
        tsTimeN2 <- as.numeric(tsTime2)
        ancho <- range(intersect(which(tsTimeN1 >= izq),which(tsTimeN1 <= dcha))) 
        alto <- range(intersect(which(tsTimeN2 >= aba),which(tsTimeN2 <= arr)))

        ZInd <-  c(alto[2],
                   alto[1],
                   ancho[1],
                   ancho[2])
        
        XXX <- KTSEnv$selRm$ones$X
        YYY <- KTSEnv$selRm$ones$Y
        
        xUpper <- which(XXX >= ZInd[3] & XXX <= ZInd[4])
        yLower <- which(YYY >= ZInd[2] & YYY <= ZInd[1])
        newRMOnes <- KTSEnv$selRm$ones[intersect(xUpper,yLower),]
        

        if(nrow(newRMOnes)==0){
          
          tcltk::tkmessageBox(message = "No recurrence point was found",
                              icon = "warning")
          
        }else{
          
        approxRRratio <- (2*nrow(KTSEnv$selRm$ones))/nrow(newRMOnes)
        poiSs1 <- round(KTSEnv$poiSs*approxRRratio^0.3,1)
        if(poiSs1 > 1){poiSs1 <- 1}
        
        grDevices::dev.new(noRStudioGD = TRUE)
        graphics::par(mar = c(KTSEnv$lomars, KTSEnv$lemars, 1, 1),
                      mgp = c(KTSEnv$laLos, KTSEnv$tiLos, 0))

        graphics::plot(tsTime1[newRMOnes[,1]],
                       tsTime2[newRMOnes[,2]],
                       type = "p",asp = 1,
                       col = KTSEnv$CoLoRs,
                       cex = poiSs1,
                       xlab = KTSEnv$xlabs,
                       ylab = KTSEnv$xlabs,
                       cex.lab = KTSEnv$labSis,
                       cex.axis = KTSEnv$tickSis,
                       pch = 19, bg = KTSEnv$CoLoRs)
        

        }
        
        KTSEnv$indicesToRedden <- NULL
        

        
      }
      
      
    }
    
    
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyRm(action = "showPANcrmp1", 
                 envirName = environment(showPANcrmp1))
    
}

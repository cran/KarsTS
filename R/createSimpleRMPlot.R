createSimpleRMPlot <-
function() {
    
    showPANsrmp1 <- function() {
      
      createSubPanR4C1()
      createTITLE(labTitle = "RECURRENCE PLOT")
      createRmRb()
      createOK(labTitle = "NEXT", action = plotsrmpzoom1)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    
    plotsrmpzoom1 <- function() {
      
      selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP),
                                   noValid = NA)
      
      if (is.na(selRmName)) {
        
        tcltk::tkmessageBox(message = "Choose a recurrence matrix",
                            icon = "warning")
      }else{
        
        selRm <- get(selRmName, envir = KTSEnv)
        
        if (selRm$type == "cross" | selRm$type == "fan") {
          
          tcltk::tkmessageBox(message = paste("Choose a simple or joint",
                                              "recurrence matrix"),
                              icon = "warning")
          
        } else {
          
          KTSEnv$selRmName <- selRmName
          showPANsrmp2()
          
        }
        
      }
      
    }
    
    showPANsrmp2 <- function(){
      
      defName <- get(KTSEnv$selRmName, envir = KTSEnv)$tsName[1]
      
      createSubPanR4C1()
      createTITLE(labTitle = "RECURRENCE PLOT")
      
      createEntry(labTitle = "Color",
                  textVariableName = "CoLoR", defaultVal = "darkred")
      
      createEntry(labTitle = "Plot diagonal",
                  textVariableName = "diaYOrN", defaultVal = "No")
      
      createEntry(labTitle = "X Label",
                  textVariableName = "xlab", defaultVal = defName)
      
      createEntry(labTitle = "Y Label",
                  textVariableName = "ylab", defaultVal = defName)
      
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
      
      createOK(labTitle = "PLOT", action = plotsrmpzoom2a)
      createOK(labTitle = "PLOT TO FILE", action = plotsrmpzoom2b, width = 14)
      createOK(labTitle = "ZOOM", action = plotSelRm, width = 14)
      
      
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    
    plotsrmpzoom2a <- function() {
      
      recurrPlot <- function(selRm) {
        
        X <- KTSEnv$selRm$ones$X
        Y <- KTSEnv$selRm$ones$Y
        nRecurrPoints <- nrow(KTSEnv$selRm$ones)
        
        graphics::par(mar = c(KTSEnv$lomars, KTSEnv$lemars, 1, 1),
                      mgp = c(KTSEnv$laLos, KTSEnv$tiLos, 0))
        graphics::plot(tsTime, tsTime, type = "p",
                       col = "white", xlab = KTSEnv$xlabs,
                       ylab = KTSEnv$ylabs, 
                       cex.lab = KTSEnv$labSis, 
                       cex.axis = KTSEnv$tickSis,
                       asp = 1)
        graphics::points(tsTime[X],tsTime[Y],
                         pch = 19, bg = KTSEnv$CoLoRs,
                         type = "p",col = KTSEnv$CoLoRs, cex = KTSEnv$poiSs)
        graphics::points(tsTime[Y],tsTime[X],
                         pch = 19, bg = KTSEnv$CoLoRs,
                         type = "p",col = KTSEnv$CoLoRs, cex = KTSEnv$poiSs)
        
        if(KTSEnv$plotDiag == "Yes"){
          
          graphics::points(tsTime[1:lengthTS],tsTime[1:lengthTS],
                           pch = 19, bg = KTSEnv$CoLoRs,
                           type = "p",col = KTSEnv$CoLoRs, cex = KTSEnv$poiSs)
          
        }
        

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
      KTSEnv$plotDiag <- verifyCharEntry(tcltk::tclvalue(KTSEnv$diaYOrN),
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
      if(is.na(KTSEnv$plotDiag)){KTSEnv$plotDiag <- "No"}
      if(is.na(KTSEnv$lemars)){KTSEnv$lemars <- 5}
      if(is.na(KTSEnv$lomars)){KTSEnv$lomars <- 5}
      if(is.na(KTSEnv$laLos)){KTSEnv$laLos <- 3}
      if(is.na(KTSEnv$tiLos)){KTSEnv$tiLos <- 1}
      
      format <- findDateFormat(KTSEnv$selRm$tsIni[1], tz = KTSEnv$timeZone)
      lengthTS <- KTSEnv$selRm$tsLength - (KTSEnv$selRm$embDim - 1) * KTSEnv$selRm$delay
      lengthTS <- min(lengthTS)
      tsTime <- seq(strptime(KTSEnv$selRm$tsIni[1], format = format,
                             tz = KTSEnv$timeZone),
                    by = KTSEnv$selRm$samPerSec[1], length.out = lengthTS)
      

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
    
    plotsrmpzoom2b <- function() {
      

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
        KTSEnv$plotDiag <- verifyCharEntry(tcltk::tclvalue(KTSEnv$diaYOrN),
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
        if(is.na(KTSEnv$plotDiag)){KTSEnv$plotDiag <- "No"}
        if(is.na(KTSEnv$lemars)){KTSEnv$lemars <- 5}
        if(is.na(KTSEnv$lomars)){KTSEnv$lomars <- 5}
        if(is.na(KTSEnv$laLos)){KTSEnv$laLos <- 3}
        if(is.na(KTSEnv$tiLos)){KTSEnv$tiLos <- 1}
        
        format <- findDateFormat(KTSEnv$selRm$tsIni[1], tz = KTSEnv$timeZone)
        lengthTS <- KTSEnv$selRm$tsLength - (KTSEnv$selRm$embDim - 1) * KTSEnv$selRm$delay
        lengthTS <- min(lengthTS)
        tsTime <- seq(strptime(KTSEnv$selRm$tsIni[1], format = format,
                               tz = KTSEnv$timeZone),
                      by = KTSEnv$selRm$samPerSec[1], length.out = lengthTS)
        
        assign("selRm", selRm, envir = KTSEnv)

        X <- KTSEnv$selRm$ones$X
        Y <- KTSEnv$selRm$ones$Y
        nRecurrPoints <- nrow(KTSEnv$selRm$ones)
        
        graphics::par(mar = c(KTSEnv$lomars, KTSEnv$lemars, 1, 1),
                      mgp = c(KTSEnv$laLos, KTSEnv$tiLos, 0))
        graphics::plot(tsTime, tsTime, type = "p",
                       col = "white", xlab = KTSEnv$xlabs,
                       ylab = KTSEnv$ylabs, 
                       cex.lab = KTSEnv$labSis, 
                       cex.axis = KTSEnv$tickSis,
                       asp = 1)
        graphics::points(tsTime[X],tsTime[Y],
                         pch = 19, bg = KTSEnv$CoLoRs,
                         type = "p",col = KTSEnv$CoLoRs, cex = KTSEnv$poiSs)
        graphics::points(tsTime[Y],tsTime[X],
                         pch = 19, bg = KTSEnv$CoLoRs,
                         type = "p",col = KTSEnv$CoLoRs, cex = KTSEnv$poiSs)
        
        if(KTSEnv$plotDiag == "Yes"){
          
          graphics::points(tsTime[1:lengthTS],tsTime[1:lengthTS],
                           pch = 19, bg = KTSEnv$CoLoRs,
                           type = "p",col = KTSEnv$CoLoRs, cex = KTSEnv$poiSs)
          
        }
        
        
        
        
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

        
        format <- findDateFormat(KTSEnv$selRm$tsIni[1], tz = KTSEnv$timeZone)
        lengthTS <- KTSEnv$selRm$tsLength - (KTSEnv$selRm$embDim - 1) * KTSEnv$selRm$delay
        lengthTS <- min(lengthTS)
        tsTime <- seq(strptime(KTSEnv$selRm$tsIni[1], format = format,
                               tz = KTSEnv$timeZone),
                      by = KTSEnv$selRm$samPerSec[1], length.out = lengthTS)
        
        tsTimeN <- as.numeric(tsTime)
        ancho <- range(intersect(which(tsTimeN >= izq),which(tsTimeN <= dcha))) 
        alto <- range(intersect(which(tsTimeN >= aba),which(tsTimeN <= arr)))
        anchoL <- length(ancho[1]:ancho[2])
        altoL <- length(alto[1]:alto[2])
        
        hacerCuadradoG <- ceiling(abs(diff(c(anchoL,altoL)))/2)
        hacerCuadradoP <- floor(abs(diff(c(anchoL,altoL)))/2)
        
        margenI <- ancho[1]-1
        margenD <- lengthTS-ancho[2]
        margenAb <- alto[1]-1
        margenAr <-  lengthTS-alto[2]
        
        
        if(diff(ancho) > diff(alto)){
          
          if((altoL + hacerCuadradoG + hacerCuadradoP) <= lengthTS){
            
            HaySitioArribaG <- margenAr >= hacerCuadradoG
            HaySitioArribaP <- margenAr >= hacerCuadradoP
            HaySitioAbajoG <- margenAb >= hacerCuadradoG
            HaySitioAbajoP <- margenAb >= hacerCuadradoP
            
            
            if(HaySitioAbajoG == TRUE & HaySitioArribaP== TRUE){
              
              alto[1] <- alto[1]-hacerCuadradoG
              alto[2] <- alto[2]+hacerCuadradoP
              
            }else if(HaySitioArribaG == TRUE & HaySitioAbajoP== TRUE){
              
              alto[1] <- alto[1]-hacerCuadradoP
              alto[2] <- alto[2]+hacerCuadradoG
              
            }else if(HaySitioAbajoG == TRUE & HaySitioArribaP== FALSE){  
              
              alto[1] <- alto[1]- (hacerCuadradoP-margenAr)-hacerCuadradoG
              alto[2] <- lengthTS
              
            }else if(HaySitioAbajoP == TRUE & HaySitioArribaG== FALSE){  
              
              alto[1] <- alto[1]- (hacerCuadradoG-margenAr)-hacerCuadradoP
              alto[2] <- lengthTS
              
            }else if(HaySitioAbajoG == FALSE & HaySitioArribaP== TRUE){  
              
              alto[2] <- alto[2]+ (hacerCuadradoP-margenAb)+hacerCuadradoG
              alto[1] <- 1  
              
            }else if(HaySitioAbajoP == FALSE & HaySitioArribaG== TRUE){  
              
              alto[2] <- alto[2]+ (hacerCuadradoG-margenAb)+hacerCuadradoP
              alto[1] <- 1      
              
            }
            
          }
            
            
            
        }else if(diff(ancho) < diff(alto)){
          
          if((anchoL + hacerCuadradoG + hacerCuadradoP) <= lengthTS){
            
            HaySitioArribaG <- margenAr >= hacerCuadradoG
            HaySitioArribaP <- margenAr >= hacerCuadradoP
            HaySitioAbajoG <- margenAb >= hacerCuadradoG
            HaySitioAbajoP <- margenAb >= hacerCuadradoP
            
            
            if(HaySitioAbajoG == TRUE & HaySitioArribaP== TRUE){
              
              ancho[1] <- ancho[1]-hacerCuadradoG
              ancho[2] <- ancho[2]+hacerCuadradoP
              
            }else if(HaySitioArribaG == TRUE & HaySitioAbajoP== TRUE){
              
              ancho[1] <- ancho[1]-hacerCuadradoP
              ancho[2] <- ancho[2]+hacerCuadradoG
              
            }else if(HaySitioAbajoG == TRUE & HaySitioArribaP== FALSE){  
              
              ancho[1] <- ancho[1]- (hacerCuadradoP-margenAr)-hacerCuadradoG
              ancho[2] <- lengthTS
              
            }else if(HaySitioAbajoP == TRUE & HaySitioArribaG== FALSE){  
              
              ancho[1] <- ancho[1]- (hacerCuadradoG-margenAr)-hacerCuadradoP
              ancho[2] <- lengthTS
              
            }else if(HaySitioAbajoG == FALSE & HaySitioArribaP== TRUE){  
              
              ancho[2] <- ancho[2]+ (hacerCuadradoP-margenAb)+hacerCuadradoG
              ancho[1] <- 1  
              
            }else if(HaySitioAbajoP == FALSE & HaySitioArribaG== TRUE){  
              
              ancho[2] <- ancho[2]+ (hacerCuadradoG-margenAb)+hacerCuadradoP
              ancho[1] <- 1      
              
            }
            
          }
        }
          
           ZInd <-  c(alto[2],
                     alto[1],
                     ancho[1],
                     ancho[2])
          
           XXX <- KTSEnv$selRm$ones$X
           YYY <- KTSEnv$selRm$ones$Y
           XXXYYY <- cbind(XXX,YYY)
           YYYXXX <- cbind(YYY,XXX)
           
           xUpper <- which(XXX >= ZInd[3] & XXX <= ZInd[4])
           yUpper <- which(YYY >= ZInd[2] & YYY <= ZInd[1])
           XXXYYY1 <- XXXYYY[intersect(xUpper,yUpper),]
           
           xLower <- which(YYY >= ZInd[3] & YYY <= ZInd[4])
           yLower <- which(XXX >= ZInd[2] & XXX <= ZInd[1])
           YYYXXX1 <- YYYXXX[intersect(xLower,yLower),]
           
           diagOnes <- intersect(ZInd[3]:ZInd[4],ZInd[2]:ZInd[1])
           diagOnes <- cbind(diagOnes,diagOnes)
           
           newRMOnes <- rbind(XXXYYY1,YYYXXX1)
           newRMOnes <- rbind(newRMOnes,diagOnes)
           
           if(nrow(newRMOnes)==0){
             
             tcltk::tkmessageBox(message = "No recurrence point was found",
                                 icon = "warning")
             
           }else{
             
           newRMOnes <- newRMOnes[order(newRMOnes[,2]),]
           newRMOnes <- newRMOnes[order(newRMOnes[,1]),]
           
           approxRRratio <- (2*nrow(KTSEnv$selRm$ones))/nrow(newRMOnes)
           poiSs1 <- round(KTSEnv$poiSs*approxRRratio^0.3,1)
           if(poiSs1 > 1){poiSs1 <- 1}
           
           try(grDevices::dev.off(), silent = TRUE)
           grDevices::dev.new(noRStudioGD = TRUE)
           graphics::par(mar = c(KTSEnv$lomars, KTSEnv$lemars, 1, 1),
                         mgp = c(KTSEnv$laLos, KTSEnv$tiLos, 0))
           
           
           graphics::plot(tsTime[newRMOnes[,1]],
                          tsTime[newRMOnes[,2]],
                          type = "p", asp = 1,
                          col = KTSEnv$CoLoRs,pch = 19,
                          cex = poiSs1,bg = KTSEnv$CoLoRs,
                          xlab = KTSEnv$xlabs,
                          ylab = KTSEnv$xlabs,
                          cex.lab = KTSEnv$labSis,
                          cex.axis = KTSEnv$tickSis)
           
           }
           
        }
        
      KTSEnv$indicesToRedden <- NULL
          
      }
      
    
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyRm(action = "showPANsrmp1", 
                 envirName = environment(showPANsrmp1))
    
}

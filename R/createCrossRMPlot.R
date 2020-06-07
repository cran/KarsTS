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
      
      defName <- get(KTSEnv$selRmName, envir = KTSEnv)$tsName[1]
      
      createSubPanR4C1()
      createTITLE(labTitle = "RECURRENCE PLOT")
      
      createEntry(labTitle = "Color",
                  textVariableName = "CoLoR", defaultVal = "darkred")

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
      
      
      createEntry(labTitle = "X Scale",defaultVal = "1.5",
                  textVariableName = "xScl")
      
      createEntry(labTitle = "Y Scale",defaultVal = "1.5",
                  textVariableName = "yScl")
      
      
      createOK(labTitle = "PLOT", action = plotcrmpzoom2)
      createOK(labTitle = "SAVE", action = saveThePlot)
      
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    
    plotcrmpzoom2 <- function() {
      
      recurrPlot <- function() {
        
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
                       cex.axis = KTSEnv$tickSis)

        if (exists("fragToZoom", envir = KTSEnv)) {
          
          graphics::polygon(x = c(KTSEnv$fragToZoom[3],
                                  KTSEnv$fragToZoom[3],
                                  KTSEnv$fragToZoom[4],
                                  KTSEnv$fragToZoom[4]),
                            y = c(KTSEnv$fragToZoom[2],
                                  KTSEnv$fragToZoom[1],
                                  KTSEnv$fragToZoom[1],
                                  KTSEnv$fragToZoom[2]),
                            border = "darkcyan", lwd = 2)
          
        }
        
        KTSEnv$parPlotSize <- graphics::par("plt")
        KTSEnv$uC <- graphics::par("usr")
        
      }
      copyPlot <- function() {tkrplot::tkrreplot(tsPlot)}
      onLeftClick <- function(x, y) {
        
        xC <- x
        yC <- y
        width <- tcltk::tclvalue(tcltk::tkwinfo("reqwidth",
                                                tsPlot))
        height <- tcltk::tclvalue(tcltk::tkwinfo("reqheight",
                                                 tsPlot))
        
        width <- as.numeric(width)
        height <- as.numeric( height)
        
        xMin <- KTSEnv$parPlotSize[1] * width
        xMax <- KTSEnv$parPlotSize[2] * width
        yMin <- KTSEnv$parPlotSize[3] * height
        yMax <- KTSEnv$parPlotSize[4] * height
        rangeX <- KTSEnv$uC[2] - KTSEnv$uC[1]
        rangeY <- KTSEnv$uC[4] - KTSEnv$uC[3]
        
        # imgXcr <- (xCr - KTSEnv$uC[1]) * (xMax - xMin)/rangeX + xMin
        # imgYcr <- (yCr - KTSEnv$uC[3]) * (yMax - yMin)/rangeY + yMin
        xC <- as.numeric(xC) + 0.5
        yC <- as.numeric(yC) + 0.5
        yC <- height - yC
        xPlotCoord <- KTSEnv$uC[1] + (xC - xMin) * rangeX/(xMax - xMin)
        yPlotCoord <- KTSEnv$uC[3] + (yC - yMin) * rangeY/(yMax - yMin)
        KTSEnv$touchedPoints <- rbind(KTSEnv$touchedPoints,
                                      c(xPlotCoord,yPlotCoord))
        
      }
      onrelease <- function(x, y) {
        
        leftLimit <- min(KTSEnv$touchedPoints[,1])
        rightLimit <- max(KTSEnv$touchedPoints[,1])
        lowerLimit <- min(KTSEnv$touchedPoints[,2])
        upperLimit <- max(KTSEnv$touchedPoints[,2])
        
        if (all(is.finite(c(leftLimit, rightLimit,
                            upperLimit, lowerLimit)))) {
          
          KTSEnv$fragToZoom <-  c(upperLimit,
                                  lowerLimit,
                                  leftLimit,
                                  rightLimit)

          KTSEnv$touchedPoints <- NULL
          tkrplot::tkrreplot(tsPlot)
          
        }else{
          
          KTSEnv$touchedPoints <- NULL
          
        }
        
      }
      createZoom <- function() {
        
        plotFragment <- function() {
          
          if(all(is.finite(KTSEnv$fragToZoom))){
            
            if(nchar(KTSEnv$selRm$tsIni[1]) > 10){
              iniDate1 <- strptime(KTSEnv$selRm$tsIni[1],
                                  format = "%Y-%m-%d %H:%M:%S",
                                  tz = KTSEnv$timeZone)
            }else{
              iniDate1 <- strptime(KTSEnv$selRm$tsIni[1],
                                  format = "%Y-%m-%d",
                                  tz = KTSEnv$timeZone)
            }
            
            iniDateN1 <- as.numeric(iniDate1)
            time1Recons <- seq(iniDateN1, by = KTSEnv$selRm$samPerSec[1], 
                              length.out = KTSEnv$selRm$tsLength[1])
            
            if(nchar(KTSEnv$selRm$tsIni[2]) > 10){
              iniDate2 <- strptime(KTSEnv$selRm$tsIni[2],
                                   format = "%Y-%m-%d %H:%M:%S",
                                   tz = KTSEnv$timeZone)
            }else{
              iniDate2 <- strptime(KTSEnv$selRm$tsIni[2],
                                   format = "%Y-%m-%d",
                                   tz = KTSEnv$timeZone)
            }
            
            iniDateN2 <- as.numeric(iniDate2)
            time2Recons <- seq(iniDateN2, by = KTSEnv$selRm$samPerSec[2], 
                               length.out = KTSEnv$selRm$tsLength[2])
            
            
            KTSEnv$ZInd <- KTSEnv$fragToZoom
            for (i in 1:2){
              
              dife <- abs(time1Recons - KTSEnv$fragToZoom[i])
              aa <- which(dife == min(dife))
              KTSEnv$ZInd[i] <- which(time1Recons == time1Recons[aa])
              rm(dife,aa)
              
            }
            
            for (i in 3:4){
              
              dife <- abs(time2Recons - KTSEnv$fragToZoom[i])
              aa <- which(dife == min(dife))
              KTSEnv$ZInd[i] <- which(time2Recons == time2Recons[aa])
              rm(dife,aa)
              
            }
            
            XXX <- KTSEnv$selRm$ones$X
            YYY <- KTSEnv$selRm$ones$Y

            xUpper <- which(XXX >= KTSEnv$ZInd[3] & XXX <= KTSEnv$ZInd[4])
            yLower <- which(YYY >= KTSEnv$ZInd[2] & YYY <= KTSEnv$ZInd[1])
            newRMOnes <- KTSEnv$selRm$ones[intersect(xUpper,yLower),]

            lengthTS <- KTSEnv$selRm$tsLength - (KTSEnv$selRm$embDim - 1) * KTSEnv$selRm$delay
            
            format <- findDateFormat(KTSEnv$selRm$tsIni[1], tz = KTSEnv$timeZone)
            tsTime1 <- seq(strptime(KTSEnv$selRm$tsIni[1], format = format,
                                   tz = KTSEnv$timeZone),
                          by = KTSEnv$selRm$samPerSec[1], length.out = lengthTS[1])
            
            
            format <- findDateFormat(KTSEnv$selRm$tsIni[2], tz = KTSEnv$timeZone)
            tsTime2 <- seq(strptime(KTSEnv$selRm$tsIni[2], format = format,
                                   tz = KTSEnv$timeZone),
                          by = KTSEnv$selRm$samPerSec[2], length.out = lengthTS[2])
            
            
            approxRRratio <- (2*nrow(KTSEnv$selRm$ones))/nrow(newRMOnes)
            poiSs1 <- round(KTSEnv$poiSs*approxRRratio^0.3,1)
            if(poiSs1 > 1){poiSs1 <- 1}
            
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
                           cex.axis = KTSEnv$tickSis)
          }
          
        }
        
        copyZoom <- function() {tkrplot::tkrreplot(tsZoom)}
        
        panelZName <- createRandName()
        assign(panelZName, tcltk::tktoplevel(bg = "white"))
        tcltk::tkwm.title(get(panelZName), KTSEnv$selRmName)
        frameZName <- createRandName("frameZName")
        assign(frameZName, tcltk::tkframe(get(panelZName),
                                          borderwidth = 2,
                                          relief = "raised"))
        tcltk::tkgrid(get(frameZName))
        tcltk::tkgrid.configure(get(frameZName), sticky = "n")
        tcltk::tkgrid.columnconfigure(get(frameZName), 0, weight = 0)
        tcltk::tkgrid.rowconfigure(get(frameZName), 0, weight = 0)
        tcltk::tkgrid.rowconfigure(get(frameZName), 1, weight = 0)
        tsZoom <- tkrplot::tkrplot(get(frameZName), fun = plotFragment,
                                   hscale = 3, vscale = 1.5)
        tcltk::tkconfigure(tsZoom, bg = "white")
        copyButton <- tcltk::tkbutton(get(frameZName),
                                      text = "Copy to clipboard",
                                      command = copyZoom)
        tcltk::tkpack(tsZoom, expand = TRUE,
                      fill = "both", anchor = "center")
        tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
        tcltk::tkpack(get(frameZName), expand = TRUE, fill = "both")
        
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
      KTSEnv$xScls <- verifyRealEntry(tcltk::tclvalue(KTSEnv$xScl),
                                      noValid = NA)
      KTSEnv$yScls <- verifyRealEntry(tcltk::tclvalue(KTSEnv$yScl),
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
      if(is.na(KTSEnv$xScls)){KTSEnv$xScl <- 1.5}
      if(is.na(KTSEnv$yScls)){KTSEnv$yScl <- 1.5}
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
      
      assign("selRm", selRm, envir = KTSEnv)
      # xCr <- KTSEnv$selRm$ones$X
      # yCr <- KTSEnv$selRm$ones$Y
      xCr <- tsTime1
      yCr <- tsTime2
      assign("touchedPoints", NULL, envir = KTSEnv)
      
      panelName <- createRandName()
      assign(panelName, tcltk::tktoplevel(bg = "white"))
      tcltk::tkwm.title(get(panelName),
                        paste("Recurrence plot:", KTSEnv$selRmName))
      
      frameName <- createRandName("frameName")
      assign(frameName, tcltk::tkframe(get(panelName), borderwidth = 2,
                                       relief = "raised"))
      
      tcltk::tkgrid(get(frameName))
      tcltk::tkgrid.configure(get(frameName), sticky = "n")
      tcltk::tkgrid.columnconfigure(get(frameName), 0, weight = 0)
      tcltk::tkgrid.rowconfigure(get(frameName), 0, weight = 0)
      tcltk::tkgrid.rowconfigure(get(frameName), 1, weight = 0)
      
      tsPlot <- try(tkrplot::tkrplot(get(frameName),
                                     fun = recurrPlot, 
                                     hscale = KTSEnv$xScls,
                                     vscale = KTSEnv$yScls),
                    silent = TRUE)
      
      if(class(tsPlot) == "try-error"){
        
        tcltk::tkmessageBox(message = paste("The plotting failed",
                                            "probably because the",
                                            "matrix is too large.",
                                            "You can try to ",
                                            "save it to a file directly",
                                            "or to reduce the tolerance"),
                            icon = "warning")
        
      }else{
        
        copyButton <- tcltk::tkbutton(get(frameName),
                                      text = "Copy to clipboard",
                                      command = copyPlot)
        saveButton <- tcltk::tkbutton(get(frameName),
                                      text = "Save to file",
                                      command = saveThePlot)
        zoomButton <- tcltk::tkbutton(get(frameName),
                                      text = "Zoom selected area",
                                      command = createZoom)
        tcltk::tkpack(tsPlot, expand = TRUE,
                      fill = "both", anchor = "center")
        tcltk::tkconfigure(tsPlot, bg = "white")
        tcltk::tkpack(copyButton, expand = TRUE,
                      fill = "both", side = "left")
        tcltk::tkpack(saveButton, expand = TRUE,
                      fill = "both", side = "left")
        tcltk::tkpack(zoomButton, expand = TRUE,
                      fill = "both", side = "left")
        tcltk::tkpack(get(frameName), expand = TRUE, fill = "both")
        tcltk::tkbind(tsPlot, "<B1-Motion>", onLeftClick)
        tcltk::tkbind(tsPlot, "<ButtonRelease-1>", onrelease)
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        
      }
      
    }
    
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
      KTSEnv$xScls <- verifyRealEntry(tcltk::tclvalue(KTSEnv$xScl),
                                      noValid = NA)
      KTSEnv$yScls <- verifyRealEntry(tcltk::tclvalue(KTSEnv$yScl),
                                      noValid = NA)
      KTSEnv$lemars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lemar),
                                      noValid = NA)
      
      KTSEnv$lomars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lomar),
                                      noValid = NA)
      
      KTSEnv$tiLos <- verifyIntEntry(tcltk::tclvalue(KTSEnv$tiLo),
                                     noValid = NA)
      
      KTSEnv$laLos <- verifyIntEntry(tcltk::tclvalue(KTSEnv$laLo),
                                     noValid = NA)
      
      if(is.na(KTSEnv$laLos)){KTSEnv$laLos <- 3}
      if(is.na(KTSEnv$tiLos)){KTSEnv$tiLos <- 1}
      if(is.na(KTSEnv$CoLoR)){KTSEnv$CoLoRs <- "darkred"}
      if(is.na(KTSEnv$poiSs)){KTSEnv$poiSs <- 0.3}
      if(is.na(KTSEnv$xlabs)){KTSEnv$xlabs <- KTSEnv$selRm$tsName[1]}
      if(is.na(KTSEnv$ylabs)){KTSEnv$ylabs <- KTSEnv$selRm$tsName[1]}
      if(is.na(KTSEnv$labSis)){KTSEnv$labSis <- 1}
      if(is.na(KTSEnv$xScls)){KTSEnv$xScl <- 1.5}
      if(is.na(KTSEnv$yScls)){KTSEnv$yScl <- 1.5}
      if(is.na(KTSEnv$lemars)){KTSEnv$lemars <- 5}
      if(is.na(KTSEnv$lomars)){KTSEnv$lomars <- 5}
     
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
      
      graphics::par(mar = c(KTSEnv$lomars, KTSEnv$lemars, 1, 1),
                    mgp = c(KTSEnv$laLos, KTSEnv$tiLos, 0))
      graphics::plot(tsTime1[X], tsTime2[Y], type = "p",
                     col = KTSEnv$CoLoRs, cex = KTSEnv$poiSs, 
                     xlab = KTSEnv$xlabs,
                     ylab = KTSEnv$ylabs, asp = 1,
                     cex.lab = KTSEnv$labSis, 
                     cex.axis = KTSEnv$tickSis)
    }
    
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyRm(action = "showPANcrmp1", 
                 envirName = environment(showPANcrmp1))
    
  }

plotTimeSeries <-
function() {
    
    getFragToZooms <- function(){
      
      findEquivPoints1 <- function(N,ts1,numTime1){
        
        res <- NULL
        tsN <- get(N, envir = KTSEnv)
        numTimeN <- as.numeric(tsN$time)
        
        if(all(numTimeN <= numTime1[1])){
          
          res <- NULL
          
        }else if(all(numTimeN >= numTime1[length(numTime1)])){
          
          res <- NULL
          
        }else{
          
          timeDifIni <- abs(numTimeN-numTime1[1])
          INI <- which(timeDifIni == min(timeDifIni))
          timeDifFin <- abs(numTimeN-numTime1[length(numTime1)])
          FIN <- which(timeDifFin == min(timeDifFin))
          INI <- verifyIntEntry(INI, noValid = NA)
          FIN <- verifyIntEntry(FIN, noValid = NA)
          
          if(is.na(INI) | is.na(FIN)){
            
            res <- NULL
            
          }else if (FIN <= INI){
            
            res <- NULL
            
          }else{
            
            res <- INI:FIN
            
          }
          
        }
        
        res
        
      }
      
      nTsToPlot <- length(KTSEnv$tsToPlotNames)
      ts1 <- get(rev(KTSEnv$tsToPlotNames)[1], envir = KTSEnv)
      ts1 <- ts1[KTSEnv$fragToZoom,]
      numTime1 <- as.numeric(ts1$time)
      fragToZooms <- lapply(KTSEnv$tsToPlotNames,
                            FUN =  findEquivPoints1,
                            ts1 = ts1, numTime1 = numTime1)
      
      fragToZooms
      
    }
    
    createZoom <- function() {
      
      nTsToPlot <- length(KTSEnv$tsToPlotNames)
      
      for(i in 1:nTsToPlot){
        
        ts <- get(KTSEnv$tsToPlotNames[i], envir = KTSEnv)
        newTs <- ts[KTSEnv$fragToZooms[[i]],]
        rownames(newTs) <- NULL
        assign(paste0("tsToZoom",i),newTs,envir = KTSEnv) 
        rm(ts, newTs)
        
      }
      
      KTSEnv$tsToPlotNamesZ <- paste0("tsToZoom",1:nTsToPlot)
      
      plotFragment <- function() {
        
        nTsToPlot <- length(KTSEnv$tsToPlotNamesZ)
        zoomLims <- getPlotLims(nTsToPlot, KTSEnv$tsToPlotNamesZ)
        firstTime <- zoomLims[[1]]
        lastTime <- zoomLims[[2]]
        minValue <- zoomLims[[3]]
        maxValue <- zoomLims[[4]]
        # try(grDevices::dev.off(), silent = TRUE)
        grDevices::dev.new(noRStudioGD = TRUE)
        graphics::plot(c(firstTime, lastTime), 
                       c(minValue, maxValue), 
                       ylim = c(minValue,maxValue), 
                       col = "white", 
                       xlab = KTSEnv$xlabs, 
                       ylab = KTSEnv$ylabs,
                       cex.axis = KTSEnv$tickSis,
                       cex.lab = KTSEnv$labSis)
        
        for (mm in 1:nTsToPlot){
          
          tsToPlot <- get(KTSEnv$tsToPlotNamesZ[mm],envir = KTSEnv)
          
          if(KTSEnv$lineas[mm] == "1"){
            
            graphics::lines(tsToPlot,col = KTSEnv$CoLoRs[mm],lwd = KTSEnv$linWs[mm])
            
          }
          
          if(KTSEnv$puntos[mm] == "1"){
            
            graphics::points(tsToPlot,col = KTSEnv$CoLoRs[mm],
                             cex = KTSEnv$poiSs, pch = 15)
            
          }
          
        }
        
        graphics::legend("topleft", 
                         legend = rev(KTSEnv$tsToPlotNames), 
                         lty = 1,lwd <- rev(KTSEnv$linWs), 
                         bty = "n", ncol = 4, cex = KTSEnv$labSis,
                         col = rev(KTSEnv$CoLoRs), y.intersp = 1)
        
      }
      
      plotFragment()

      
    }
    
    plotVariousTs <- function() {
      
      nTsToPlot <- length(KTSEnv$tsToPlotNames)
      plotLims <- getPlotLims(nTsToPlot, KTSEnv$tsToPlotNames)
      firstTime <- plotLims[[1]]
      lastTime <- plotLims[[2]]
      minValue <- plotLims[[3]]
      maxValue <- plotLims[[4]]
      graphics::plot(c(firstTime, lastTime), 
                     c(minValue, maxValue), 
                     ylim = c(minValue,maxValue), 
                     col = "white", 
                     xlab = KTSEnv$xlabs, 
                     ylab = KTSEnv$ylabs,
                     cex.axis = KTSEnv$tickSis,
                     cex.lab = KTSEnv$labSis)
      
      if (exists("fragToZoom", envir = KTSEnv)) {
        KTSEnv$fragToZooms <- getFragToZooms()
      }
      
      for (mm in 1:nTsToPlot){
        
        tsToPlot <- get(KTSEnv$tsToPlotNames[mm],envir = KTSEnv)
        
        if(KTSEnv$lineas[mm] == "1"){
          
          graphics::lines(tsToPlot,col = KTSEnv$CoLoRs[mm],lwd = KTSEnv$linWs[mm])
          
        }
        
        if (exists("fragToZoom", envir = KTSEnv)) {
          
          graphics::lines(tsToPlot[KTSEnv$fragToZooms[[mm]], ],
                          lwd = KTSEnv$linWs[mm],col = "limegreen")
          
        }
        
        if(KTSEnv$puntos[mm] == "1"){
          
          graphics::points(tsToPlot,col = KTSEnv$CoLoRs[mm],
                           cex = KTSEnv$poiSs, pch = 15)
          
        }
        
        if (exists("fragToZoom", envir = KTSEnv)) {
          
          graphics::points(tsToPlot[KTSEnv$fragToZooms[[mm]], ], 
                           col = "limegreen",cex = KTSEnv$poiSs,
                           pch = 15)
          
        }
        
      }
      
      graphics::legend("topleft", 
                       legend = rev(KTSEnv$tsToPlotNames), 
                       lty = 1,lwd <- rev(KTSEnv$linWs), 
                       bty = "n", ncol = 4, cex = KTSEnv$labSis,
                       col = rev(KTSEnv$CoLoRs), y.intersp = 1)
      
      parPlotSize <- graphics::par("plt")
      assign("parPlotSize", parPlotSize, envir = KTSEnv)
      uC <- graphics::par("usr")
      assign("uC", uC, envir = KTSEnv)
      
    }
    getOrSetParam <- function(){
      
      if(exists("xlabs", envir = KTSEnv) == FALSE){
        KTSEnv$xlabs <- "time"
      }
      if(exists("ylabs", envir = KTSEnv) == FALSE){
        KTSEnv$ylabs <- ""
      }
      if(exists("labSis", envir = KTSEnv) == FALSE){
        KTSEnv$labSis <- 1
      }
      if(exists("tickSis", envir = KTSEnv) == FALSE){
        KTSEnv$tickSis <- 1
      }
      
      default6VarIfNAs()
      
      if(exists("tsToPlotNames", envir = KTSEnv) == FALSE){
        KTSEnv$tsToPlotNames <- getOrderedTS()
      }
      
      nTsToPlot <- length(KTSEnv$tsToPlotNames)
      
      if(exists("CoLoRs", envir = KTSEnv) == FALSE){
        KTSEnv$CoLoRs <- getDefCoLoRs(nTsToPlot)
        
      }
      
      if(exists("lineas", envir = KTSEnv) == FALSE){
        KTSEnv$lineas <- rep("1",nTsToPlot)
      }
      if(exists("puntos", envir = KTSEnv) == FALSE){
        KTSEnv$puntos <- rep("0",nTsToPlot)
      }
      if(exists("linWs", envir = KTSEnv) == FALSE){
        KTSEnv$linWs <- rep(1,nTsToPlot)
      }
      if(exists("poiSs", envir = KTSEnv) == FALSE){
        KTSEnv$poiSs <- rep(1,nTsToPlot)
      }
      
      if(any(is.na(KTSEnv$CoLoRs))){
        aa <- which(is.na(KTSEnv$CoLoRs))
        
        KTSEnv$CoLoRs[aa] <- getDefCoLoRs(nTsToPlot)[aa]
        rm(aa)
      }
      if(any(is.na(KTSEnv$puntos))){
        aa <- which(is.na(KTSEnv$puntos))
        KTSEnv$puntos[aa] <- "0"
        rm(aa)
      }
      if(any(is.na(KTSEnv$lineas))){
        aa <- which(is.na(KTSEnv$lineas))
        KTSEnv$lineas[aa] <- "1"
        rm(aa)
      }
      if(any(is.na(KTSEnv$linWs))){
        aa <- which(is.na(KTSEnv$linWs))
        KTSEnv$linWs[aa] <- 1
        rm(aa)
      }
      if(any(is.na(KTSEnv$poiSs))){
        aa <- which(is.na(KTSEnv$poiSs))
        KTSEnv$poiSs[aa] <- 1
        rm(aa)
      }
      
    }
    getPlotLims <- function(nTsToPlot, tsToPlotNames) {
      firstTime <- Inf
      lastTime <- -Inf
      minValue <- Inf
      maxValue <- -Inf
      for (X in 1:nTsToPlot) {
        timSer <- get(tsToPlotNames[X], envir = KTSEnv)
        if (timSer$time[1] < firstTime) {
          tsWithMinIniTime <- X
          firstTime <- timSer$time[1]
        }
        if (timSer$time[length(timSer$time)] > lastTime) {
          tsWithMaxLastTime <- X
          lastTime <- timSer$time[length(timSer$time)]
        }
        if (min(timSer$value, na.rm = TRUE) < minValue) {
          tsWithMinVal <- X
          minValue <- min(timSer$value, na.rm = TRUE)
        }
        if (max(timSer$value, na.rm = TRUE) > maxValue) {
          tsWithMaxVal <- X
          maxValue <- max(timSer$value, na.rm = TRUE)
        }
      }
      aLittleMargin <- round(0.02 * (maxValue - minValue))
      minValue <- minValue - aLittleMargin
      maxValue <- maxValue + aLittleMargin
      legendSpace <- round(0.2 * (maxValue - minValue))
      maxValue <- maxValue + legendSpace
      res <- list(firstTime = firstTime,lastTime = lastTime, 
                  minValue = minValue, maxValue = maxValue)
      res
      
    }
    default6VarIfNAs <- function(){
      if(is.na(KTSEnv$xlabs)){KTSEnv$xlabs <- "time"}
      if(is.na(KTSEnv$ylabs)){KTSEnv$ylabs <- ""}
      if(is.na(KTSEnv$labSis)){KTSEnv$labSis <- 1}
      if(is.na(KTSEnv$tickSis)){KTSEnv$tickSis <- 1}
    }
    getTStoPlot <- function() {
      
      tsToPlotNames <- NA
      tsselorden <- NA
      tssel <- tsCheckedTF()
      tsselorden <- readMultEntryvalues(KTSEnv$dSList$nTS,
                                        prefix = "tEntscbValue",
                                        type = "integer")
      tsselorden <- tsselorden[which(tssel == TRUE)]
      tsToPlotNames <- KTSEnv$dSList$TS[which(tssel == TRUE)]
      nTsToPlot <- length(tsToPlotNames)
      if (nTsToPlot == 0) {
        tcltk::tkmessageBox(message = paste("Choose at least",
                                            "one time series"),
                            icon = "warning")
      } else if (any(duplicated(tsselorden[which(is.finite(tsselorden))]))) {
        tcltk::tkmessageBox(message = paste("Two time series cannot",
                                            "have the same",
                                            "ordinal number"),
                            icon = "warning")
      } else {
        
        list(tsToPlotNames,tsselorden)
        
      }
      
    } 
    getDefCoLoRs <- function(nTsToPlot) {
      CoLoRsToPlot <- c("blue", "red", "darkgreen", "magenta", "cyan", 
                        "green", "orange", "brown", 
                        "purple", "darkcyan")
      if (nTsToPlot > 10) {
        CoLoRsToPlot <- c(CoLoRsToPlot, rep("black", nTsToPlot - 10))
      } else {
        CoLoRsToPlot <- CoLoRsToPlot[1:nTsToPlot]
      }
      CoLoRsToPlot <- rev(CoLoRsToPlot)
      CoLoRsToPlot
    }
    orderTsForPlotting <- function(nTsToPlot, tsselorden,tsToPlotNames) {
      
      if (all(is.na(tsselorden))) {
        
        tsselorden <- 1:nTsToPlot
        
      }else{
        
        tsselorden <- cbind(1:nTsToPlot, tsselorden)
        tsselorden <- tsselorden[order(tsselorden[, 2],na.last = TRUE),]
        tsselorden <- tsselorden[, 1]
        
      }
      
      tsToPlotNames <- tsToPlotNames[tsselorden]
      tsToPlotNames <- rev(tsToPlotNames)
      
    }
    getOrderedTS <- function() {
      
      X <- getTStoPlot()
      tsToPlotNames <- X[[1]]
      
      if(any(is.na(tsToPlotNames))){
        
        tcltk::tkmessageBox(message = paste("Choose, at least,",
                                            "a time series"),
                            icon = "warning")
        
      }else{
        
        nTsToPlot <- length(tsToPlotNames)
        tsselorden <- X[[2]]
        
        if (nTsToPlot > 1) {
          
          tsToPlotNames <- orderTsForPlotting(nTsToPlot, tsselorden,
                                              tsToPlotNames)
          
        }
        
        tsToPlotNames
        
      }
      
    }
    choosePanelNumber <- function(){
      
      if(exists("CountTS", envir = KTSEnv) == FALSE){
        KTSEnv$CountTS <- 1
      }
      
      showPanParamN()
      
    } 
    advancePanel <- function(){
      
      i <- KTSEnv$CountTS
      nts <- length(KTSEnv$tsToPlotNames)
      getPanelInfo()
      
      if(i != nts){
        
        KTSEnv$CountTS <- KTSEnv$CountTS + 1
        choosePanelNumber()
        
      }else{
        
        rm(CountTS, envir = KTSEnv)
        showPANplotTs()
        
      }
      
    } 
    goBackPanel <- function(){
      
      getPanelInfo()
      KTSEnv$CountTS <- KTSEnv$CountTS - 1
      choosePanelNumber()
      
    } 
    showPanParam1 <- function(){
      
      createSubPanR4C1()
      createTITLE(labTitle = "Parameters")
      
      createEntry(labTitle = "X Label",
                  textVariableName = "xlab")
      
      createEntry(labTitle = "Y Label",
                  textVariableName = "ylab")
      
      createEntry(labTitle = "Labels size",
                  textVariableName = "labSi")
      
      createEntry(labTitle = "Ticks size",
                  textVariableName = "tickSi")
      
      createOK(labTitle = "NEXT", action = goParamPanel2)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    goParamPanel2 <- function(){
      
      KTSEnv$xlabs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$xlab),
                                      noValid = NA)
      
      KTSEnv$ylabs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$ylab),
                                      noValid = NA)
      
      KTSEnv$labSis <- verifyRealEntry(tcltk::tclvalue(KTSEnv$labSi),
                                       noValid = NA)
      
      KTSEnv$tickSis <- verifyRealEntry(tcltk::tclvalue(KTSEnv$tickSi),
                                        noValid = NA)
      
      KTSEnv$tsToPlotNames <- getOrderedTS()
      nts <- length(KTSEnv$tsToPlotNames)
      KTSEnv$puntos <- rep(NA,nts)
      KTSEnv$lineas <- rep(NA,nts)
      s <- rep(NA,nts)
      KTSEnv$defaultTSCoLoRs <- getDefCoLoRs(nts)
      KTSEnv$linWs <- rep(NA,nts)
      KTSEnv$poiSs <- rep(NA,nts)
      KTSEnv$CountTS <- 1
      showPanParamN()
      
    }
    showPanParamN <- function(){
      
      i <- KTSEnv$CountTS
      nts <- length(KTSEnv$tsToPlotNames)
      tsName <- KTSEnv$tsToPlotNames[i]
      
      createSubPanR4C1()
      createTITLE(labTitle = tsName)
      createTitle(labTitle = "Type")
      createChb(labTitle = "Lines", variableName = "lin",
                defaultVal = "1")
      createChb(labTitle = "Points", variableName = "poin",
                defaultVal = "0")
      
      createEntry(labTitle = "Color", 
                  textVariableName = paste0("CoLoR",i))
      
      createEntry(labTitle = "Line width", 
                  textVariableName = paste0("linW",i))
      
      createEntry(labTitle = "Point size", 
                  textVariableName = paste0("poiS",i))
      
      if(i != 1){
        createOK(labTitle = "BACK", action = goBackPanel)
      }
      
      if(i != nts){
        createOK(labTitle = "NEXT", action = advancePanel)
      }else{
        createOK(labTitle = "OK", action = advancePanel)  
      }
      
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    getPanelInfo <- function(){
      
      i <- KTSEnv$CountTS
      
      KTSEnv$puntos[i] <- tcltk::tclvalue(KTSEnv$poin)
      KTSEnv$lineas[i] <- tcltk::tclvalue(KTSEnv$lin)
      
      toEval <- eval(parse(text = paste0("KTSEnv$CoLoR",i)))
      KTSEnv$CoLoRs[i] <- verifyCharEntry(tcltk::tclvalue(toEval),noValid = NA)
      
      toEval <- eval(parse(text = paste0("KTSEnv$linW",i)))
      KTSEnv$linWs[i] <- verifyRealEntry(tcltk::tclvalue(toEval),noValid = NA) 
      
      toEval <- eval(parse(text = paste0("KTSEnv$poiS",i)))
      KTSEnv$poiSs[i] <- verifyRealEntry(tcltk::tclvalue(toEval),noValid = NA) 
      
      if(KTSEnv$puntos[i] == "0" & KTSEnv$lineas[i] == "0"){
        KTSEnv$lineas[i] <- "1"
      }
      
      if(is.na(KTSEnv$CoLoRs[i])){
        KTSEnv$CoLoRs[i] <- KTSEnv$defaultTSCoLoRs[i] 
      }
      
      
      if(is.na(KTSEnv$linWs[i])){KTSEnv$linWs[i] <- 1}
      
      if(is.na(KTSEnv$poiSs[i])){KTSEnv$poiSs[i] <- 1}
      
    }
    showPANplotTs <- function() {
      
      dCh <- rep("0",length(KTSEnv$dSList$TS))
      dEn <- rep("",length(KTSEnv$dSList$TS))
      if(exists("tsToPlotNames", envir = KTSEnv)){
        compa <- compareVecVec(KTSEnv$tsToPlotNames,KTSEnv$dSList$TS)
        aa <- which(compa == TRUE,arr.ind = TRUE)[,1]
        names(aa) <- NULL
        dCh[aa] <- "1"
        dEn[aa] <- getTStoPlot()[[2]]
        if(any(is.na(dEn[aa]))){dEn[aa] <- length(aa):1}
      }
      
      createSubPanR4C1()
      createTITLE(labTitle = "PLOT TIME SERIES")
      createTitle(labTitle = "Time series")
      for(j in 1:length(KTSEnv$dSList$TS)){
        createChbEntry(j, elements = KTSEnv$dSList$TS,
                       prefix = "scbValue", envir = KTSEnv, 
                       dCh = dCh[j], dEn = dEn[j])
      }
      createOK(labTitle = "Parameters",width = 10,action = showPanParam1)
      createOK(labTitle = "PLOT", action = plotpartzoom1, width = 16)
      createOK(labTitle = "ZOOM", action = plotSel, width = 16)
      createOK(labTitle = "PLOT TO FILE", action = plotpartzoom2, width = 16)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    plotpartzoom1 <- function() {
      

      getOrSetParam()
      tsToPlot <- get(rev(KTSEnv$tsToPlotNames)[1],envir = KTSEnv)
      KTSEnv$tsToPlot <- tsToPlot
      

      grDevices::dev.new(noRStudioGD = TRUE)
      plotVariousTs()
      

    }
    plotpartzoom2 <- function() {
      
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
        getOrSetParam()
        tsToPlot <- get(rev(KTSEnv$tsToPlotNames)[1],envir = KTSEnv)
        KTSEnv$tsToPlot <- tsToPlot
        
        xCr <- as.numeric(tsToPlot$time)
        yCr <- tsToPlot$value
        KTSEnv$touchedPoints <- NULL
        plotVariousTs()
        
      }
      saveThePlot()
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      
      
      
    }
    plotSel <- function(){
      
      idkts.pts<- function(selTs){
        
        rr <- try(graphics::identify(selTs$time,selTs$value,n=1, plot = FALSE),
                  silent=TRUE)
        if(class(rr)!="try-error"){
          
          KTSEnv$indicesToRedden <- c(KTSEnv$indicesToRedden,rr)
          if(length(KTSEnv$indicesToRedden)==3){
            
            KTSEnv$indicesToRedden <- KTSEnv$indicesToRedden[-1] 
            
          }
          
          plotVariousTs()
          graphics::abline(v = tsToPlot1$time[KTSEnv$indicesToRedden], col = "green")         

          
        }
        
        try(idkts.pts(selTs), silent = TRUE)
        
        
      }
      
      tsToPlot1 <-  get(rev(KTSEnv$tsToPlotNames)[1], envir = KTSEnv)
      
      idkts.pts(selTs = tsToPlot1)
      
      if(length(KTSEnv$indicesToRedden)!=2){
        
                tcltk::tkmessageBox(message = paste("Select two points"),
                                    icon = "warning")
        
      }else{
      
        KTSEnv$fragToZoom <- sort(KTSEnv$indicesToRedden)[1]:sort(KTSEnv$indicesToRedden)[2]
      KTSEnv$fragToZooms <- getFragToZooms()
      createZoom()
      createTS() #ya funciona
 
      
      }
      
    }
    createTS <- function(){

      createTSPopUp <- function(){
        
        onOK <- function() {
          
          suff <- verifyCharEntry(tcltk::tclvalue(entryVar1), noValid = NA)
          assign("suff",suff,envir = environment(fun = createTS))
          tcltk::tkdestroy(KTSEnv$newWin)
          
        }
        
        KTSEnv$newWin <- tcltk::tktoplevel()
        tcltk::tkwm.title(KTSEnv$newWin, "Create new time series from selection?")
        
        
        text1 <- "Create new time series from selection?"
        lab1 <- tcltk2::tk2label(KTSEnv$newWin,
                                 text = text1,
                                 justify = "left")
        
        text2 <- "Enter the name for creating the time series;"
        lab2 <- tcltk2::tk2label(KTSEnv$newWin,
                                 text = text2,
                                 justify = "left")
        text3 <- "leave it empty otherwise."
        lab3 <- tcltk2::tk2label(KTSEnv$newWin,
                                 text = text3,
                                 justify = "left")
        
        entryVar1 <- tcltk::tclVar("")
        ent1 <-tcltk2::tk2entry(KTSEnv$newWin, width = "25",
                                textvariable = entryVar1)
        
        OKbutton1 <-tcltk::tkbutton(KTSEnv$newWin, text = "OK",
                                    width = -6, command = onOK)
        
        tcltk::tkgrid(lab1,padx = 10, pady = c(15, 0), sticky = "w")
        tcltk::tkgrid(lab2,padx = 10, pady = c(0, 0), sticky = "w")
        tcltk::tkgrid(lab3,padx = 10, pady = c(0, 15), sticky = "w")
        tcltk::tkgrid(ent1, padx = 10, pady = c(0, 15))
        tcltk::tkgrid(OKbutton1, padx = 10, pady = c(5, 5))
        
        tcltk::tkbind(ent1, "<Return>", onOK)
        tcltk::tkfocus(KTSEnv$newWin)
        
      }
      
      
      createTSPopUp()
      tcltk::tkwait.window(KTSEnv$newWin)
      
      if(is.na(suff)==FALSE){
        
        nTsToPlot <- length(KTSEnv$tsToPlotNames)
        newNames <- paste0(KTSEnv$tsToPlotNames,"_",suff)
        
        for(i in 1:nTsToPlot){
          
          ts <- get(KTSEnv$tsToPlotNamesZ[i], envir = KTSEnv)
          assign(newNames[i],ts,envir = KTSEnv) 
          rm(ts)
          
        }
        
      }
      
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANplotTs", 
                 envirName = environment(showPANplotTs))
    
    
  }

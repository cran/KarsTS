scattTimeSeries <-
function() {
    
    showPANplotTs <- function() {
      
      dCh <- rep("0",length(KTSEnv$dSList$TS))
      dEn <- rep("",length(KTSEnv$dSList$TS))
      
      if(exists("tsToPlotNames", envir = KTSEnv)){
        
        compa <- compareVecVec(KTSEnv$tsToPlotNames,KTSEnv$dSList$TS)
        aa <- which(compa == TRUE,arr.ind = TRUE)[,1]
        names(aa) <- NULL
        dCh[aa] <- "1"
        dEn[aa] <- paste0(KTSEnv$embDims,",",KTSEnv$lagDelay)
        
      }
      
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "PHASE PORTRAITS")
      createTitle(labTitle = "Time series")
      for(j in 1:length(KTSEnv$dSList$TS)){
        createChbEntry(j, elements = KTSEnv$dSList$TS,
                       prefix = "scbValue", envir = KTSEnv, 
                       dCh = dCh[j], dEn = dEn[j])
      }
      createOK(labTitle = "Parameters", action = showPanParam, width = 10)
      createOK(labTitle = "PLOT", action = plotTsOnOK1)
      createOK(labTitle = "PLOT TO FILE", action = plotTsOnOK2, width = 14)
      createNote(labTitle = "In the text entries, write the embedding dimension", 
                 pady = c(10, 0))
      createNote(labTitle = "and the delay separated by a comma", pady = c(1, 0))
      createNote(labTitle = "Defaults to no embedding, that is, 1,0",
                 pady = c(1,10))
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    
    getOrSetParam <- function(){
      
      if(exists("xlabs", envir = KTSEnv) == FALSE){
        KTSEnv$xlabs <- NULL
      }
      if(exists("ylabs", envir = KTSEnv) == FALSE){
        KTSEnv$ylabs <- NULL
      }
      if(exists("zlabs", envir = KTSEnv) == FALSE){
        KTSEnv$zlabs <- NULL
      }
      if(exists("labSis", envir = KTSEnv) == FALSE){
        KTSEnv$labSis <- 1
      }
      if(exists("tickSis", envir = KTSEnv) == FALSE){
        KTSEnv$tickSis <- 1
      }
      if(exists("CoLoRs", envir = KTSEnv) == FALSE){
        KTSEnv$CoLoRs <- "rainBow"
      }
      
      if(exists("lineas", envir = KTSEnv) == FALSE){
        KTSEnv$lineas <- "1"
      }
      if(exists("puntos", envir = KTSEnv) == FALSE){
        KTSEnv$puntos <- "0"
      }
      if(exists("linWs", envir = KTSEnv) == FALSE){
        KTSEnv$linWs <- 1
      }
      if(exists("poiSs", envir = KTSEnv) == FALSE){
        KTSEnv$poiSs <- 1
      }
      
      
      
      if(is.null(KTSEnv$xlabs) == FALSE){
        if(is.na(KTSEnv$xlabs)){KTSEnv$xlabs <- NULL}
      }
      if(is.null(KTSEnv$ylabs) == FALSE){
        if(is.na(KTSEnv$ylabs)){KTSEnv$ylabs <- NULL}
      }
      if(is.null(KTSEnv$zlabs) == FALSE){
        if(is.na(KTSEnv$zlabs)){KTSEnv$zlabs <- NULL}
      }
      if(is.na(KTSEnv$labSis)){KTSEnv$labSis <- 1}
      if(is.na(KTSEnv$tickSis)){KTSEnv$tickSis <- 1}
      if(is.na(KTSEnv$CoLoRs)){KTSEnv$CoLoRs <- "rainBow"}
      if(is.na(KTSEnv$puntos)){KTSEnv$puntos <- "0"}
      if(is.na(KTSEnv$lineas)){KTSEnv$lineas <- "1"}
      if(is.na(KTSEnv$linWs)){KTSEnv$linWs <- 1}
      if(is.na(KTSEnv$poiSs)){KTSEnv$poiSs <- 1}
      
      
    }
    
    plotTsOnOK1 <- function() {
      
      getOrSetParam()
      
      getTsToPlot()
      
      nTsToPlot <- length(KTSEnv$tsToPlotNames)
      totDim <- sum(KTSEnv$embDims)
      
      if (nTsToPlot == 0 | nTsToPlot > 3) {
        
        tcltk::tkmessageBox(message = "Choose 1,2, or 3 time series", 
                            icon = "warning")
        
      } else if (totDim < 2 | totDim > 3) {
        
        tcltk::tkmessageBox(message = paste("The total dimension must be 2",
                                            "or 3, that is: 1 time series",
                                            "with embedding dim 2 or 3; 2",
                                            "times series, one of them possibly",
                                            "with emb.dim 2 or, finally, one",
                                            "time series with emb. dim 2 or 3"), 
                            icon = "warning")
        
      }else{
        
        tmComptibility <- matrix(rep(FALSE, 3 * nTsToPlot), 
                                 nTsToPlot, 3)
        firsTS <- get(KTSEnv$tsToPlotNames[1], envir = KTSEnv)
        
        for (i in 1:nTsToPlot) {
          
          tmComptibility[i, ] <- are2TsTimeCompatible(firsTS, 
                                                      get(KTSEnv$tsToPlotNames[i],
                                                          envir = KTSEnv))
        }
        
        if (any(tmComptibility[, 1] == FALSE)) {
          
          tcltk::tkmessageBox(message = paste("The initial date of all the",
                                              "time series must be the same"), 
                              icon = "warning")
          
        } else if (any(tmComptibility[, 2] == FALSE)) {
          
          tcltk::tkmessageBox(message = paste("The sampling period of all",
                                              "the time series must",
                                              "be the same"), 
                              icon = "warning")
          
        } else {
          
          dataMatrix <- getDataMatrix(nTsToPlot,totDim)
          
          CoLoRs <- verifyColor(dataMatrix)
          
          plotPhP1(totDim = totDim, CoLoRs = CoLoRs,dataMatrix = dataMatrix)
          
        }
        
      }
      
    }
    
    showPanParam <- function(){
      
      if(exists("tsToPlotNames", envir = KTSEnv)== FALSE){getTsToPlot()}
      
      createSubPanR4C1()
      createTITLE(labTitle = "Parameters")
      
      createTitle(labTitle = "Type")
      createChb(labTitle = "Lines", variableName = "lin",
                defaultVal = "1")
      createChb(labTitle = "Points", variableName = "poin",
                defaultVal = "0")
      
      createEntry(labTitle = "Color", 
                  textVariableName = "CoLoR")
      
      createEntry(labTitle = "X Label",
                  textVariableName = "xlab")
      
      createEntry(labTitle = "Y Label",
                  textVariableName = "ylab")
      
      createEntry(labTitle = "Z Label",
                  textVariableName = "zlab")
      
      createEntry(labTitle = "Labels size",
                  textVariableName = "labSi")
      
      createEntry(labTitle = "Ticks size",
                  textVariableName = "tickSi")
      
      createEntry(labTitle = "Line width", 
                  textVariableName = "linW")
      
      createEntry(labTitle = "Point size", 
                  textVariableName = "poiS")
      
      
      createOK(labTitle = "OK", action = setParam)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    
    setParam <- function(){
      
      KTSEnv$xlabs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$xlab),
                                      noValid = NA)
      
      KTSEnv$ylabs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$ylab),
                                      noValid = NA)
      
      KTSEnv$zlabs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$zlab),
                                      noValid = NA)
      
      KTSEnv$labSis <- verifyRealEntry(tcltk::tclvalue(KTSEnv$labSi),
                                       noValid = NA)
      
      KTSEnv$tickSis <- verifyRealEntry(tcltk::tclvalue(KTSEnv$tickSi),
                                        noValid = NA)
      
      KTSEnv$puntos <- tcltk::tclvalue(KTSEnv$poin)
      KTSEnv$lineas <- tcltk::tclvalue(KTSEnv$lin)
      
      KTSEnv$CoLoRs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$CoLoR),noValid = NA)
      
      KTSEnv$linWs <- verifyRealEntry(tcltk::tclvalue(KTSEnv$linW),noValid = NA)
      
      KTSEnv$poiSs <- verifyRealEntry(tcltk::tclvalue(KTSEnv$poiS),noValid = NA)
      
      
      if(KTSEnv$puntos == "0" & KTSEnv$lineas == "0"){KTSEnv$lineas <- "1"}
      if(is.na(KTSEnv$CoLoR)){KTSEnv$CoLoRs <- "rainBow"}
      if(is.na(KTSEnv$linWs)){KTSEnv$linWs <- 1}
      if(is.na(KTSEnv$poiSs)){KTSEnv$poiSs <- 1}
      if(is.na(KTSEnv$xlabs)){KTSEnv$xlabs <- NULL}
      if(is.na(KTSEnv$ylabs)){KTSEnv$ylabs <- NULL}
      if(is.na(KTSEnv$zlabs)){KTSEnv$zlabs <- NULL}
      if(is.na(KTSEnv$labSis)){KTSEnv$labSis <- 1}
      if(is.na(KTSEnv$tickSis)){KTSEnv$tickSis <- 1}
      
      showPANplotTs()
      
    }
    
    verifyColor <- function(dataMatrix){
      
      if(KTSEnv$CoLoRs == "rainBow" & KTSEnv$lineas == "0"){
        CoLoRs <- grDevices::rainbow(nrow(dataMatrix))
      }else if(KTSEnv$CoLoRs == "rainbow" & KTSEnv$lineas == "0"){
        CoLoRs <- grDevices::rainbow(nrow(dataMatrix))  
      }else if(KTSEnv$CoLoRs == "rainBow" & KTSEnv$lineas == "1"){
        CoLoRs <- "darkblue"
      }else if(KTSEnv$CoLoRs == "rainbow" & KTSEnv$lineas == "1"){
        CoLoRs <- "darkblue"
      }else{
        CoLoRs <- KTSEnv$CoLoRs
      }
      
      CoLoRs
    }
    
    verifyDimLag <- function(entry) {
      
      res <- c(NA, NA)
      
      if(is.na(entry)){
        
        res <- c(1, 0)
        
      }else{
        
        numberOfCommas <- try(length(which(strsplit(entry, 
                                                    split = NULL)[[1]] == ",")), 
                              silent = TRUE)
        
        if (class(numberOfCommas) == "try-error") {
          res <- c(NA, NA)
        } else if (numberOfCommas != 1) {
          res <- c(NA, NA)
        } else if (strsplit(entry, split = ",")[[1]][1] == ",") {
          res <- c(NA, NA)
        } else if (strsplit(entry, split = ",")[[1]][2] == ",") {
          res <- c(NA, NA)
        } else {
          res <- separateEntry(entry, class1 = verifyIntEntry, 
                               class2 = verifyIntEntry, 
                               noValid = NA)
        }
        
      }
      
      res
      
    }
    
    plotPhP1 <- function(totDim,CoLoRs,dataMatrix){
      
      if (totDim == 2 & KTSEnv$lineas == "1" & KTSEnv$puntos == "1") {
        grDevices::dev.new(noRStudioGD = TRUE)
        graphics::plot(dataMatrix[, 1], dataMatrix[, 2], 
                       xlab = KTSEnv$xlabs, 
                       ylab = KTSEnv$ylabs,
                       cex = KTSEnv$poiSs,
                       cex.axis = KTSEnv$tickSis,
                       cex.lab = KTSEnv$labSis,
                       col = CoLoRs, pch = 15)
        graphics::lines(dataMatrix[, 1], dataMatrix[, 2], 
                        lwd = KTSEnv$linWs,
                        col = CoLoRs)
        
      } else if (totDim == 2 & KTSEnv$lineas == "1" & KTSEnv$puntos == "0") {
        
        grDevices::dev.new(noRStudioGD = TRUE)
        graphics::plot(dataMatrix[, 1], dataMatrix[, 2], type = "l", 
                       xlab = KTSEnv$xlabs, 
                       ylab = KTSEnv$ylabs,
                       lwd = KTSEnv$linWs,
                       cex.axis = KTSEnv$tickSis,
                       cex.lab = KTSEnv$labSis,
                       col = CoLoRs)
        
      } else if (totDim == 2 & KTSEnv$lineas == "0" & KTSEnv$puntos == "1") {
        grDevices::dev.new(noRStudioGD = TRUE)
        graphics::plot(dataMatrix[, 1], dataMatrix[, 2], 
                       xlab = KTSEnv$xlabs, 
                       ylab = KTSEnv$ylabs,
                       cex = KTSEnv$poiSs, pch = 15,
                       cex.axis = KTSEnv$tickSis,
                       cex.lab = KTSEnv$labSis,
                       col = CoLoRs)
        
      } else if (totDim == 3 & KTSEnv$lineas == "1" & KTSEnv$puntos == "1") {
        
        rgl::plot3d(dataMatrix[, 1], dataMatrix[, 2], dataMatrix[, 3], 
                    xlab = KTSEnv$xlabs, 
                    ylab = KTSEnv$ylabs,
                    zlab = KTSEnv$zlabs,
                    cex = KTSEnv$poiSs,pch = 15,
                    cex.axis = KTSEnv$tickSis,
                    cex.lab = KTSEnv$labSis,
                    col = CoLoRs,type = "p")
        rgl::plot3d(dataMatrix[, 1], dataMatrix[, 2], dataMatrix[, 3], 
                    add = TRUE, lwd = KTSEnv$linWs,
                    col = CoLoRs, type = "l")
        
      } else if (totDim == 3 & KTSEnv$lineas == "1" & KTSEnv$puntos == "0") {
        
        rgl::plot3d(dataMatrix[, 1], dataMatrix[, 2], dataMatrix[, 3], 
                    xlab = KTSEnv$xlabs, 
                    ylab = KTSEnv$ylabs,
                    zlab = KTSEnv$zlabs,
                    lwd = KTSEnv$linWs,
                    cex.axis = KTSEnv$tickSis,
                    cex.lab = KTSEnv$labSis,
                    col = CoLoRs,type = "l")
        
      } else if (totDim == 3 & KTSEnv$lineas == "0" & KTSEnv$puntos == "1") {
        
        rgl::plot3d(dataMatrix[, 1], dataMatrix[, 2], dataMatrix[, 3], 
                    xlab = KTSEnv$xlabs, 
                    ylab = KTSEnv$ylabs,
                    zlab = KTSEnv$zlabs,
                    cex = KTSEnv$poiSs,pch = 15,
                    cex.axis = KTSEnv$tickSis,
                    cex.lab = KTSEnv$labSis,
                    col = CoLoRs,type = "p")
        
      }
      
    }
    
    
    getTsToPlot <- function(){
      
      tssel <- tsCheckedTF()
      KTSEnv$tsToPlotNames <- KTSEnv$dSList$TS[which(tssel == TRUE)]
      
      if (any(tssel == TRUE)) {
        
        embDimEntry <- readMultEntryvalues(KTSEnv$dSList$nTS, 
                                           prefix = "tEntscbValue", 
                                           type = "Char")
        
        embDimEntry <- embDimEntry[which(tssel == TRUE)]
        
        dimsAndDelays <- apply(as.matrix(embDimEntry), 1, FUN = verifyDimLag)
        
        if (is.vector(dimsAndDelays)) {
          dimsAndDelays <- as.matrix(dimsAndDelays)
        }
        
        KTSEnv$embDims <- dimsAndDelays[1, ]
        KTSEnv$lagDelay <- dimsAndDelays[2, ]
        
      } 
      
    }
    
    getDataMatrix <- function(nTsToPlot,totDim){
      
      L <- NULL
      for (i in 1:nTsToPlot) {
        L <- c(L, length(get(KTSEnv$tsToPlotNames[i], envir = KTSEnv)$value))
      }
      
      maxL <- max(L)
      
      dataMatrix <- matrix(NA, maxL, totDim)
      colnamesMatrix <- rep(NA, totDim)
      cont <- 1
      for (i in 1:nTsToPlot) {
        
        X <- get(KTSEnv$tsToPlotNames[i], envir = KTSEnv)$value
        if (KTSEnv$embDims[i] > 1) {
          embX <- tseriesChaos::embedd(as.matrix(X), 
                                       m = KTSEnv$embDims[i], 
                                       d = KTSEnv$lagDelay[i])
          
          dataMatrix[1:nrow(embX), cont:(cont + KTSEnv$embDims[i] - 1)] <- embX
          colnamesMatrix[cont:(cont + KTSEnv$embDims[i] - 
                                 1)] <- paste0(KTSEnv$tsToPlotNames[i], 1:KTSEnv$embDims[i])
          cont <- cont + KTSEnv$embDims[i]
          
        } else {
          
          dataMatrix[1:L[i], cont] <- X
          colnamesMatrix[cont] <- KTSEnv$tsToPlotNames[i]
          cont <- cont + 1
        }
        rm(X)
      }
      
      dataMatrix
      
    }
    
    
    plotTsOnOK2 <- function() {
      
      getOrSetParam()
      
      getTsToPlot()
      
      nTsToPlot <- length(KTSEnv$tsToPlotNames)
      totDim <- sum(KTSEnv$embDims)
      
      if (nTsToPlot == 0 | nTsToPlot > 3) {
        
        tcltk::tkmessageBox(message = "Choose 1,2, or 3 time series", 
                            icon = "warning")
        
      } else if (totDim < 2 | totDim > 3) {
        
        tcltk::tkmessageBox(message = paste("The total dimension must be 2",
                                            "or 3, that is: 1 time series",
                                            "with embedding dim 2 or 3; 2",
                                            "times series, one of them possibly",
                                            "with emb.dim 2 or, finally, one",
                                            "time series with emb. dim 2 or 3"), 
                            icon = "warning")
        
      }else{
        
        tmComptibility <- matrix(rep(FALSE, 3 * nTsToPlot), 
                                 nTsToPlot, 3)
        firsTS <- get(KTSEnv$tsToPlotNames[1], envir = KTSEnv)
        
        for (i in 1:nTsToPlot) {
          
          tmComptibility[i, ] <- are2TsTimeCompatible(firsTS, 
                                                      get(KTSEnv$tsToPlotNames[i],
                                                          envir = KTSEnv))
        }
        
        if (any(tmComptibility[, 1] == FALSE)) {
          
          tcltk::tkmessageBox(message = paste("The initial date of all the",
                                              "time series must be the same"), 
                              icon = "warning")
          
        } else if (any(tmComptibility[, 2] == FALSE)) {
          
          tcltk::tkmessageBox(message = paste("The sampling period of all",
                                              "the time series must",
                                              "be the same"), 
                              icon = "warning")
          
        } else {
          
          dataMatrix <- getDataMatrix(nTsToPlot,totDim)
          
          CoLoRs <- verifyColor(dataMatrix)
          
          plotPhP2(totDim = totDim, CoLoRs = CoLoRs,dataMatrix = dataMatrix)
          
        }
        
      }
      
    }
    
    
    plotPhP2 <- function(totDim,CoLoRs,dataMatrix){
      
      
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
      
      if (totDim == 3){
        
        
        tcltk::tkmessageBox(message = paste("This option is not available",
                                            "for 3D plots"), 
                            icon = "warning")
        
      }else{
        
        if (totDim == 2 & KTSEnv$lineas == "1" & KTSEnv$puntos == "1") {
          # grDevices::dev.new(noRStudioGD = TRUE)
          
          thPl <- function(){
            
            graphics::plot(dataMatrix[, 1], dataMatrix[, 2], 
                           xlab = KTSEnv$xlabs, 
                           ylab = KTSEnv$ylabs,
                           cex = KTSEnv$poiSs,
                           cex.axis = KTSEnv$tickSis,
                           cex.lab = KTSEnv$labSis,
                           col = CoLoRs, pch = 15)
            graphics::lines(dataMatrix[, 1], dataMatrix[, 2], 
                            lwd = KTSEnv$linWs,
                            col = CoLoRs)
          }
          
        } else if (totDim == 2 & KTSEnv$lineas == "1" & KTSEnv$puntos == "0") {
          
          # grDevices::dev.new(noRStudioGD = TRUE)
          
          thPl <- function(){
            graphics::plot(dataMatrix[, 1], dataMatrix[, 2], type = "l", 
                           xlab = KTSEnv$xlabs, 
                           ylab = KTSEnv$ylabs,
                           lwd = KTSEnv$linWs,
                           cex.axis = KTSEnv$tickSis,
                           cex.lab = KTSEnv$labSis,
                           col = CoLoRs)
          }
          
        } else if (totDim == 2 & KTSEnv$lineas == "0" & KTSEnv$puntos == "1") {
          # grDevices::dev.new(noRStudioGD = TRUE)
          thPl <- function(){
            graphics::plot(dataMatrix[, 1], dataMatrix[, 2], 
                           xlab = KTSEnv$xlabs, 
                           ylab = KTSEnv$ylabs,
                           cex = KTSEnv$poiSs, pch = 15,
                           cex.axis = KTSEnv$tickSis,
                           cex.lab = KTSEnv$labSis,
                           col = CoLoRs)
          }
          
        }
        
        plotToSave <- function(){
          thPl()
        }
        saveThePlot()
        
        
      }
      
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      
      
      
    }
    
    
    
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANplotTs", 
                 envirName = environment(showPANplotTs))
    
  }

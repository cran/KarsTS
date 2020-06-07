createDistMatrix <-
function () 
{
  showPANsrm1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "DISTANCE MATRIX")
    createTsRb()
    createOK(labTitle = "NEXT", action = srm1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
  }
  
  srm1OnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                 noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    }
    else {
      assign("selTsName", selTsName, envir = KTSEnv)
      showPANsrm2()
    }
  }
  
  showPANsrm2 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "DISTANCE MATRIX")
    createNote(labTitle = paste("Time series:", KTSEnv$selTsName))
    createTitle(labTitle = "Parameters")
    createEntry(labTitle = "Embedding dimension", textVariableName = "embDim", 
                defaultVal = "1")
    createEntry(labTitle = "Delay", textVariableName = "lagDel", 
                defaultVal = "0")
    createTitle(labTitle = "Type of distance")
    typeDist <- tcltk::tclVar("Infinity norm")
    assign("typeDist", typeDist, envir = KTSEnv)
    createRb(variable = KTSEnv$typeDist, dataVector = c("Infinity norm", 
                                                        "Euclidean"))
    createOK(labTitle = "NEXT", action = srm2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
  }
  
  srm2OnOk <- function() {
    
    refreshDataSetsList(outp = FALSE)
    embedDim <- verifyIntEntry(tcltk::tclvalue(KTSEnv$embDim), 
                               noValid = NA)
    lagDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lagDel), 
                               noValid = NA)
    
    typeDi <- tcltk::tclvalue(KTSEnv$typeDist)
    
    if (is.na(embedDim)) {
      
      tcltk::tkmessageBox(message = paste("Choose an ", 
                                          "embedding dimension"), icon = "warning")
    
    }else if (embedDim == 0) {
      
      tcltk::tkmessageBox(message = paste("The embedding", 
                                          "dimension must be", "one or greater"), icon = "warning")
      
    }else if (is.na(lagDelay)) {
      
      tcltk::tkmessageBox(message = "Choose a delay", icon = "warning")
      
    }else if (embedDim > 1 & lagDelay == 0) {
      
      tcltk::tkmessageBox(message = paste("For embedding dimesions", 
                                          "greater than 1, the", "delay must be greater", 
                                          "than 0"), icon = "warning")
      
    } else {
      
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      
   
      getRecurrenceDist <- function (timSer, embedDim, lagDelay){
        
        findRecDist <- function(pointIndex, dataTS, lDataTS) {
          P <- dataTS[pointIndex, ]
          distanceP <- infNorm(P, dataTS)
          distanceP[which(is.infinite(distanceP))] <- NA
          distanceP
        }
              
        if (embedDim == 1) {
          
          dataTS <- matrix(timSer$value)
          
          infNorm <- function(v1, v2) {
            
            abs(v2 - v1)
            
          }
          
        }else {
          
          dataTS <- tseriesChaos::embedd(timSer$value, 
                                         m = embedDim, 
                                         d = lagDelay)
          
          infNorm <- function(v1, v2) {
            
            if(any(class(v2)=="numeric")){v2 <- t(as.matrix(v2))}
            lv2 <- NROW(v2)
            apply(abs(v2 - matrix(rep(v1, each = lv2), lv2, embedDim)), 
                  1, FUN = max, na.rm = FALSE)
            
          }
          
        }
        
        comCa <- which(stats::complete.cases(dataTS))
        indices <- as.matrix(comCa)

        recDists <- lapply(indices, FUN = findRecDist, 
                           dataTS = dataTS, 
                           lDataTS = nrow(dataTS))
        
        if(length(comCa)!= nrow(dataTS)){
          
          recDists1 <- vector("list",nrow(dataTS))
          recDists1[comCa] <- recDists
          inComCa <- setdiff(1:nrow(dataTS),comCa)
          recDists1[inComCa] <- rep(list(rep(NA,nrow(dataTS))),length(inComCa))
          rm(recDists)
          recDists <- recDists1
          
        }
        
        recDists
        
      }
      
      selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
      recDists <- try(getRecurrenceDist(timSer = selTs, 
                                        embedDim = embedDim, 
                                        lagDelay = lagDelay),
                      silent = TRUE)
      
      if(class(recDists)=="try-error"){
        
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANsrm1()
        tcltk::tkmessageBox(message = paste("KarsTS could not calculate the", 
                                            "distance matrix.", 
                                            "Try choosing a shorter", 
                                            "time series"), icon = "warning")
        
      }else{
        
        maxi <- apply(as.matrix(1:length(recDists)), 1,
                      function(ele){max(recDists[[ele]],na.rm = TRUE)})
        
        KTSEnv$maxi <- max(maxi[which(is.finite(maxi))])
        KTSEnv$recDists <- recDists
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        showPANsrm3()
        
      }
      
    }
    
  }
  
  showPANsrm3 <- function() {
    
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "DISTANCE MATRIX")
    createTitle(labTitle = "Parameters")
    createEntry(labTitle = "Minimum distance", textVariableName = "thresh1", 
                defaultVal = "0")
    createEntry(labTitle = "Maximum distance", textVariableName = "thresh2", 
                defaultVal = as.character(KTSEnv$maxi))
    createEntry(labTitle = "Color levels", textVariableName = "newName", 
                defaultVal = "100")
    
    createTitle(labTitle = "White outside the range")
    whiteOrNot <- tcltk::tclVar("No")
    assign("whiteOrNot", whiteOrNot, envir = KTSEnv)
    createRb(variable = KTSEnv$whiteOrNot, dataVector = c("Yes","No"))
    
    createTitle(labTitle = "Colors")
    colGre <- tcltk::tclVar("Color")
    assign("colGre", colGre, envir = KTSEnv)
    createRb(variable = KTSEnv$colGre, dataVector = c("Color","Greys"))

    createEntry(labTitle = "X Label",
                textVariableName = "xlab", defaultVal = KTSEnv$selTsName)
    createEntry(labTitle = "Y Label",
                textVariableName = "ylab", defaultVal = KTSEnv$selTsName)
    createEntry(labTitle = "Labels size",
                textVariableName = "labSi", defaultVal = "1")
    createEntry(labTitle = "Ticks size",
                textVariableName = "tickSi", defaultVal = "1")
    createEntry(labTitle = "Left margin",
                textVariableName = "lemar", defaultVal = "5")
    createEntry(labTitle = "Lower margin",
                textVariableName = "lomar", defaultVal = "5")
    createEntry(labTitle = "Right margin",
                textVariableName = "rimar", defaultVal = "2")
    
    createEntry(labTitle = "Ticks location", 
                textVariableName = "tiLo", defaultVal = "1")
    
    createEntry(labTitle = "Labels location", 
                textVariableName = "laLo", defaultVal = "3")

    createOK(labTitle = "PLOT", action = srm3OnOk)
    createOK(labTitle = "SAVE", action = saveThePlot)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  
  srm3OnOk <- function() {
    
    KTSEnv$minDist <- verifyRealEntry(tcltk::tclvalue(KTSEnv$thresh1), 
                               noValid = NA)
    KTSEnv$maxDist <- verifyRealEntry(tcltk::tclvalue(KTSEnv$thresh2), 
                               noValid = NA)
    KTSEnv$nColors <- verifyIntEntry(tcltk::tclvalue(KTSEnv$newName), 
                              noValid = NA)
    KTSEnv$wYoN <- verifyCharEntry(tcltk::tclvalue(KTSEnv$whiteOrNot), 
                            noValid = NA)
    
    if(exists("colGre", envir = KTSEnv) == FALSE){
      
      KTSEnv$colGre <- verifyCharEntry(tcltk::tclvalue(KTSEnv$colGre), 
                                       noValid = NA)
    }
    KTSEnv$xlabs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$xlab),
                                    noValid = NA)
    KTSEnv$ylabs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$ylab),
                                    noValid = NA)
    KTSEnv$labSis <- verifyRealEntry(tcltk::tclvalue(KTSEnv$labSi),
                                     noValid = NA)
    KTSEnv$tickSis <- verifyRealEntry(tcltk::tclvalue(KTSEnv$tickSi),
                                      noValid = NA)
    KTSEnv$lemars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lemar),
                                    noValid = NA)
    KTSEnv$lomars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lomar),
                                    noValid = NA)
    KTSEnv$rimars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$rimar),
                                    noValid = NA)
    
    KTSEnv$tiLos <- verifyIntEntry(tcltk::tclvalue(KTSEnv$tiLo),
                                   noValid = NA)
    
    KTSEnv$laLos <- verifyIntEntry(tcltk::tclvalue(KTSEnv$laLo),
                                   noValid = NA)
    
    if(is.na(KTSEnv$laLos)){KTSEnv$laLos <- 3}
    if(is.na(KTSEnv$tiLos)){KTSEnv$tiLos <- 1}
    
    if(is.na(KTSEnv$xlabs)){KTSEnv$xlabs <- KTSEnv$selTsName}
    if(is.na(KTSEnv$ylabs)){KTSEnv$ylabs <- KTSEnv$selTsName}
    if(is.na(KTSEnv$labSis)){KTSEnv$labSis <- 1}
    if(is.na(KTSEnv$tickSis)){KTSEnv$tickSis <- 1}
    if(is.na(KTSEnv$lemars)){KTSEnv$lemars <- 5}
    if(is.na(KTSEnv$lomars)){KTSEnv$lomars <- 5}
    if(is.na(KTSEnv$rimars)){KTSEnv$rimars <- 2}
    if (is.na(KTSEnv$nColors)) {KTSEnv$nColors <- 100}
      
    if (is.na(KTSEnv$minDist)) {
      
      tcltk::tkmessageBox(message = paste("Enter the minimum distance"), 
                          icon = "warning")
      
    }else if (is.na(KTSEnv$maxDist)) {
      
      tcltk::tkmessageBox(message = paste("Enter the maximum distance"), 
                          icon = "warning")
      
    } else if (KTSEnv$minDist >= KTSEnv$maxDist) {
      
      tcltk::tkmessageBox(message = paste("The minimum distance must be",
                                          "lower than the maximum"), 
                          icon = "warning")
      
    } else if (KTSEnv$minDist < 0) {
      
      tcltk::tkmessageBox(message = paste("The minimum distance must be",
                                          "equal to or greater than 0"), 
                          icon = "warning")
      
    } else if (KTSEnv$maxDist <= 0) {
      
      tcltk::tkmessageBox(message = paste("The maximum distance must be",
                                          "greater than 0"), 
                          icon = "warning")
      
    } else {
      
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      
      plotTheMatrix <- function(recDists,nColors,rZ,colGre){
        
        dM <- length(recDists)
        
        if(colGre == "Greys"){
          
          myColors <- c("black", "white")
          
        }else{
          
          myColors <- c("magenta", "yellow","cyan")
          
        }
        
        myRamp <- grDevices::colorRampPalette(myColors)(nColors)
      
        if(exists("saveToFile", envir = KTSEnv)){
          
          rm(saveToFile, envir = KTSEnv)
          
        }else{
          
          grDevices::dev.new(noRStudioGD = TRUE)
          
        }
        
        graphics::par(mar = c(KTSEnv$lomars, KTSEnv$lemars, 
                              2, KTSEnv$rimars),
                      mgp = c(KTSEnv$laLos, KTSEnv$tiLos, 0))
        plot3D::image2D(x = 1:dM,
                        y = 1:dM,
                        z = t(matrix(NA,dM,dM)),
                        zlim = rZ,
                        col = myRamp,
                        xaxt = "n",
                        xlab = "",cex.axis = KTSEnv$tickSis,
                        yaxt = "n",
                        ylab = "",
                        colkey = list(cex.axis = KTSEnv$tickSis))
        graphics::axis(side = 1, at = graphics::axTicks(1),
                       labels = selTs$time[graphics::axTicks(1)],
                       cex.axis = KTSEnv$tickSis)
        graphics::axis(side = 2, at = graphics::axTicks(2),
                       labels = selTs$time[graphics::axTicks(2)],
                       cex.axis = KTSEnv$tickSis)
        
        graphics::title(xlab = KTSEnv$xlabs, 
              ylab = KTSEnv$ylabs,cex.lab=KTSEnv$labSis)
        
        if(dM%%2 == 0){
          
          agrupacion <- rep(1:(0.5*dM),each = 2)
          
        }else{
          
          agrupacion <- c(rep(1:(0.5*(dM-1)),each = 2),0.5*(dM-1)) 
          
        }
        
        uA <- unique(agrupacion)
        maxAg <- which(uA==max(uA))
        uA <- uA[-maxAg]
        
        for(jj in uA){
          
          x <- which(agrupacion==jj)
          ma <- cbind(recDists[[x[1]]],recDists[[x[2]]])
          plot3D::image2D(x = x,
                  y = 1:dM,
                  z = t(ma),
                  col = myRamp,
                  zlim = rZ,
                  add = TRUE,
                  colkey = FALSE)
          rm(ma,x)
          
        }
        
        x <- which(agrupacion==maxAg)
        
        if(length(x)==2){
          
          ma <- cbind(recDists[[x[1]]],recDists[[x[2]]])
          
        }else{
          
          ma <- cbind(recDists[[x[1]]],recDists[[x[2]]],recDists[[x[3]]])
          
        }
        
        plot3D::image2D(x = x,
                y = 1:dM,
                z = t(ma),
                col = myRamp,
                zlim = rZ,
                add = TRUE,
                colkey = FALSE)
        rm(ma,x)
        
      }
      
      if(KTSEnv$wYoN == "Yes"){
        
        recDists1 <- KTSEnv$recDists 
        
      }else{
        
        fixARow <- function(nr,distMat,minDist,maxDist){
          
          rowDistMat <- distMat[[nr]]
          rowDistMat[which(rowDistMat < minDist)] <- minDist
          rowDistMat[which(rowDistMat > maxDist)] <- maxDist
          rowDistMat
          
        }
        recDists1 <- lapply(as.matrix(1:length(KTSEnv$recDists)),
                            FUN = fixARow,distMat = KTSEnv$recDists,
                            minDist = KTSEnv$minDist,maxDist = KTSEnv$maxDist)
        
      }
      
      selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
      plotTheMatrix(recDists1, nColors = KTSEnv$nColors, 
                    rZ = c(KTSEnv$minDist,KTSEnv$maxDist), 
                    colGre = KTSEnv$colGre)
      
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
    
    KTSEnv$saveToFile <- TRUE
    
    if( exten == "tiff"){
      
      grDevices::png(filename = KTSEnv$filename,units = "cm",
                      width = KTSEnv$winW, height = KTSEnv$winH,
                      res = KTSEnv$resIm)
      
    }else{
      
      grDevices::png(filename = KTSEnv$filename,units = "cm",
                     width = KTSEnv$winW, height = KTSEnv$winH,
                     res = KTSEnv$resIm) 
      
    }
    
    
    srm3OnOk()
    grDevices::dev.off()
    
    tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
    
  }
  
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANsrm1", envirName = environment(showPANsrm1))
  
}

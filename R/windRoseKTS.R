windRoseKTS <-
function() {
    showPANwRose1 <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "WIND ROSE")
        createTsRb(labTitle = "Magnitude", 
                   variableName = "magTsP")
        createOK(labTitle = "NEXT", action = plotwRose1)
        tcltk::tkpack(KTSEnv$subPanR4C1, 
                      expand = TRUE, fill = "both")
        
    }
    plotwRose1 <- function() {
        magTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$magTsP), 
                                     noValid = NA)
        if (is.na(magTsName)) {
            tcltk::tkmessageBox(message = paste("Choose a magnitude",
                                                "time series"), 
                                icon = "warning")
        } else {
          magTs <- get(magTsName, envir = KTSEnv)
          if (any(is.infinite(magTs$value))) {
            tcltk::tkmessageBox(message = paste("The magnitude cannot",
                                                "have infinite values"), 
                                icon = "warning")
          } else if (any(is.nan(magTs$value))) {
            tcltk::tkmessageBox(message = paste("The magnitude cannot",
                                                "have not a numbers",
                                                "(NaNs); however,NAs are",
                                                "allowed meaning calm"), 
                                icon = "warning")
            } else if (any(magTs$value[which(is.finite(magTs$value))] < 0)) {
                tcltk::tkmessageBox(message = paste("The magnitude cannot",
                                                    "have negative values"), 
                  icon = "warning")
            } else {
                
                assign("magTs", magTs, envir = KTSEnv)
                assign("magTsName", magTsName, envir = KTSEnv)
                showPANwRose2()
            }
        }
    }
    showPANwRose2 <- function() {
        createSubPanR4C1()
        createTITLE(labTitle = "WIND ROSE")
        createTsRb(labTitle = "Direction", variableName = "dirTsP")
        createEntry(labTitle = "Number of bins", 
                    textVariableName = "numberBins")
        createEntry(labTitle = "Number of ticks", 
                    textVariableName = "numberTicks")
        createOK(labTitle = "PLOT", action = plotwRosezoom1)
        createOK(labTitle = "PLOT TO FILE", action = plotwRosezoom2, width = 14)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    plotwRosezoom1 <- function() {
        dirTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$dirTsP), 
                                     noValid = NA)
        nBins <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numberBins), 
                                noValid = NA)
        nTicks <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numberTicks), 
                                 noValid = NA)
        if (is.na(dirTsName)) {
            tcltk::tkmessageBox(message = "choose the direction time series", 
                                icon = "warning")
        } else {
            if (is.na(nBins)) {
                nBins <- 12
            }
            if (is.na(nTicks)) {
                nTicks <- 12
            }
            dirTs <- get(dirTsName, envir = KTSEnv)
            finiteDirTs <- dirTs$value[which(is.finite(dirTs$value))]
            tmComptibility <- are2TsTimeCompatible(dirTs, KTSEnv$magTs)
            if (any(finiteDirTs < 0 | finiteDirTs > 360)) {
                tcltk::tkmessageBox(message = paste("The direction values",
                                                    "must lie between 0",
                                                    "and 360"), 
                  icon = "warning")
            } else if (tmComptibility[1] == FALSE) {
                tcltk::tkmessageBox(message = paste("The initial dates",
                                                    "of the time series",
                                                    "do not match"), 
                  icon = "warning")
            } else if (tmComptibility[2] == FALSE) {
                tcltk::tkmessageBox(message = paste("The sampling period of",
                                                    "the time series",
                                                    "do not match"), 
                  icon = "warning")
            } else if (tmComptibility[3] == FALSE) {
                tcltk::tkmessageBox(message = paste("The length of the",
                                                    "time series do",
                                                    "not match"), 
                  icon = "warning")
            } else {
              windRosePlot <- function() {
                
                colorsWR <- c("blue", "red", "darkgreen", 
                              "magenta", "cyan", "green", 
                              "orange", "brown", "purple", "darkcyan")
                
                mceDB <- circular::circular(dirTs$value, 
                                            units = "degrees", 
                                            template = "geographics")
                
                dataToPlot <- data.frame(dir = mceDB, 
                                         mag = KTSEnv$magTs$value)
                circular::windrose(dataToPlot, bins = nBins, 
                                   main = paste(KTSEnv$magTsName, dirTsName), 
                                   fill.col = colorsWR, 
                                   num.ticks = nTicks, calm = "NA")
              }
              grDevices::dev.new(noRStudioGD = TRUE)
              windRosePlot()
              tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
              
            }
        }
    }
    plotwRosezoom2 <- function() {
        
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
        
        
        dirTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$dirTsP), 
                                     noValid = NA)
        nBins <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numberBins), 
                                noValid = NA)
        nTicks <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numberTicks), 
                                 noValid = NA)
        if (is.na(dirTsName)) {
            tcltk::tkmessageBox(message = "choose the direction time series", 
                                icon = "warning")
        } else {
            if (is.na(nBins)) {
                nBins <- 12
            }
            if (is.na(nTicks)) {
                nTicks <- 12
            }
            dirTs <- get(dirTsName, envir = KTSEnv)
            finiteDirTs <- dirTs$value[which(is.finite(dirTs$value))]
            tmComptibility <- are2TsTimeCompatible(dirTs, KTSEnv$magTs)
            if (any(finiteDirTs < 0 | finiteDirTs > 360)) {
                tcltk::tkmessageBox(message = paste("The direction values",
                                                    "must lie between 0",
                                                    "and 360"), 
                                    icon = "warning")
            } else if (tmComptibility[1] == FALSE) {
                tcltk::tkmessageBox(message = paste("The initial dates",
                                                    "of the time series",
                                                    "do not match"), 
                                    icon = "warning")
            } else if (tmComptibility[2] == FALSE) {
                tcltk::tkmessageBox(message = paste("The sampling period of",
                                                    "the time series",
                                                    "do not match"), 
                                    icon = "warning")
            } else if (tmComptibility[3] == FALSE) {
                tcltk::tkmessageBox(message = paste("The length of the",
                                                    "time series do",
                                                    "not match"), 
                                    icon = "warning")
            } else {
                windRosePlot <- function() {
                    
                    colorsWR <- c("blue", "red", "darkgreen", 
                                  "magenta", "cyan", "green", 
                                  "orange", "brown", "purple", "darkcyan")
                    
                    mceDB <- circular::circular(dirTs$value, 
                                                units = "degrees", 
                                                template = "geographics")
                    
                    dataToPlot <- data.frame(dir = mceDB, 
                                             mag = KTSEnv$magTs$value)
                    circular::windrose(dataToPlot, bins = nBins, 
                                       main = paste(KTSEnv$magTsName, dirTsName), 
                                       fill.col = colorsWR, 
                                       num.ticks = nTicks, calm = "NA")
                }
                
                plotToSave <- function(){
                    windRosePlot()
                }
                saveThePlot()
                tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
                
            }
        }
    }
    
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANwRose1", 
                 envirName = environment(showPANwRose1))
}

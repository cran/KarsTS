removePoints <-
function() {
        selAndPlot <- function() {
            
            thresholdx <- verifyRealEntry(tcltk::tclvalue(KTSEnv$threshx), 
                                          noValid = NA)
            thresholdy <- verifyRealEntry(tcltk::tclvalue(KTSEnv$threshy), 
                                          noValid = NA)
            KTSEnv$selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                                noValid = NA)
            
            KTSEnv$poiSs <- verifyRealEntry(tcltk::tclvalue(KTSEnv$poiS),noValid = NA)
            
            if (is.na(KTSEnv$selTsName)) {
                tcltk::tkmessageBox(message = "Choose a time series", 
                                    icon = "warning")
            } else if (is.na(thresholdx)) {
                tcltk::tkmessageBox(message = paste("Enter a threshold for the",
                                                    "time axis in seconds",
                                                    "(the maximum distance",
                                                    "between the cursor",
                                                    "and the point, at which",
                                                    "the point will be",
                                                    "selected).If too many",
                                                    " points are selected when",
                                                    "you drag on them, the",
                                                    "threshold should be smaller",
                                                    "and vice versa. Try until",
                                                    "you get an",
                                                    "appropiate value"), 
                                    icon = "warning")
                
            } else if (is.na(thresholdy)) {
                tcltk::tkmessageBox(message = paste("Enter a threshold for the Y axis",
                                                    "(the maximum distance",
                                                    "between the cursor",
                                                    "and the point, at which",
                                                    "the point will be",
                                                    "selected).If too many",
                                                    " points are selected when",
                                                    "you drag on them, the",
                                                    "threshold should be smaller",
                                                    "and vice versa. Try until",
                                                    "you get an",
                                                    "appropiate value"), 
                                    icon = "warning")
            } else {
                
                if(is.na(KTSEnv$poiSs)){KTSEnv$poiSs <- 1}
                
                
                idkts.rp<- function(selTs,thresholdx,thresholdy, 
                                 col, poiSs){
                    
                    rr <- try(graphics::identify(selTs$time,selTs$value,n=1, plot = FALSE),
                              silent=TRUE)
                    if(class(rr)!="try-error"){
                        rrrr <- pointsWithinTol (selTs$time[rr],
                                                 selTs$value[rr],
                                                 selTs = selTs,
                                                 thresholdx = thresholdx,
                                                 thresholdy = thresholdy) 
                        KTSEnv$indicesToRedden <- sort(c(rrrr,KTSEnv$indicesToRedden))
                        graphics::points(selTs$time[KTSEnv$indicesToRedden],
                                         selTs$value[KTSEnv$indicesToRedden],
                                         col=col,
                                         cex = poiSs, pch = 19, bg= col)
                        
                        
                        
                        
                    }
                    
                    try(idkts.rp(selTs,thresholdx,thresholdy,col, poiSs),
                        silent = TRUE)
                    
                    
                }
                
                pointsWithinTol <- function(x1,y1,selTs = selTs,
                                            thresholdx = thresholdx,
                                            thresholdy = thresholdy){
                    
                    upperLine <- which(selTs$value < (y1 + thresholdy))
                    lowerLine <- which(selTs$value > (y1 - thresholdy))
                    yBar <- intersect(upperLine,lowerLine)
                    leftLine <- which(as.numeric(selTs$time) > (as.numeric(x1) - thresholdx))
                    rightLine <- which(as.numeric(selTs$time) < (as.numeric(x1) + thresholdx))
                    xBar <- intersect(leftLine,rightLine)
                    redSquareInd <- sort(intersect(xBar,yBar))
                    if(length(redSquareInd)==0){redSquareInd <- NULL}
                    redSquareInd
                    
                    
                }
                
                col = "red"
                try(grDevices::dev.off(), silent = TRUE)
                selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
                grDevices::dev.new(noRStudioGD = TRUE)
                graphics::plot(selTs, cex = KTSEnv$poiSs,pch = 19)
                graphics::points(selTs[KTSEnv$indicesToRedden,],
                                 col = col,cex = KTSEnv$poiSs, pch = 19,bg = col)
                idkts.rp(selTs,thresholdx,thresholdy, 
                      col = col, poiSs = KTSEnv$poiSs)
                
                turnPointsToNAs()
            }
        }
        showPANrmPoints <- function() {
            refreshDataSetsList(outp = FALSE)
            createSubPanR4C1()
            createTITLE(labTitle = "REMOVE POINTS")
            createTsRb()
            createEntry(labTitle = "Name", textVariableName = "newName")
            createEntry(labTitle = "Threshold time (s)", 
                        textVariableName = "threshx", defaultVal = "3600")
            createEntry(labTitle = "Threshold Y", 
                        textVariableName = "threshy", defaultVal = "")
            createEntry(labTitle = "Point size", 
                        textVariableName = "poiS", defaultVal = "1")
            createOK(labTitle = "PLOT", action = selAndPlot)
            createOK(labTitle = "REMOVE POINTS", action = turnPointsToNAs, width = 14)
            tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
            
        }
        turnPointsToNAs <- function(){
            
            turnPointsToNAsPopUp <- function(){
                
                onOK <- function() {
                    
                    resp <- verifyCharEntry(tcltk::tclvalue(entryVar1), noValid = NA)
                    assign("resp",resp,envir = environment(fun = turnPointsToNAsPopUp))
                    tcltk::tkdestroy(KTSEnv$newWin)
                    
                }
                
                KTSEnv$newWin <- tcltk::tktoplevel()
                tcltk::tkwm.title(KTSEnv$newWin, "Remove selected points?")
                text1 <- "Remove selected points?"
                lab1 <- tcltk2::tk2label(KTSEnv$newWin,
                                         text = text1,
                                         justify = "left")
                
                text2 <- "Enter YES or NO"
                lab2 <- tcltk2::tk2label(KTSEnv$newWin,
                                         text = text2,
                                         justify = "left")
 
                entryVar1 <- tcltk::tclVar("")
                ent1 <-tcltk2::tk2entry(KTSEnv$newWin, width = "25",
                                        textvariable = entryVar1)
                
                OKbutton1 <-tcltk::tkbutton(KTSEnv$newWin, text = "OK",
                                            width = -6, command = onOK)
                
                tcltk::tkgrid(lab1,padx = 10, pady = c(15, 0), sticky = "w")
                tcltk::tkgrid(lab2,padx = 10, pady = c(0, 0), sticky = "w")
                tcltk::tkgrid(ent1, padx = 10, pady = c(0, 15))
                tcltk::tkgrid(OKbutton1, padx = 10, pady = c(5, 5))
                
                tcltk::tkbind(ent1, "<Return>", onOK)
                tcltk::tkfocus(KTSEnv$newWin)
                
            }
            
            
            turnPointsToNAsPopUp()
            tcltk::tkwait.window(KTSEnv$newWin)

            
            if(is.na(resp)){
                
                resp1 <- "createDefault"
                tcltk::tkmessageBox(message = paste("The option provided",
                                                    "was not valid.",
                                                    "In order to preserve",
                                                    "your work,",
                                                    "the time series",
                                                    "was created, regardless."),
                                    icon = "warning")
                
            }else if(resp=="NO" | resp=="no" | resp=="No" | resp=="N"| resp=="n"){
                
                resp1 <- "NO"
                
            }else if(resp=="YES" | resp=="yes" | resp=="Yes" | resp=="Y"| resp=="y"){
                
                resp1 <- "YES"
                
            }else{
                
                resp1 <- "createDefault" 
                tcltk::tkmessageBox(message = paste("The option provided",
                                                    "was not valid.",
                                                    "In order to preserve",
                                                    "your work,",
                                                    "the time series",
                                                    "was created, regardless."),
                                    icon = "warning")
                
                
            }
            
            if(resp1 != "NO"){
                
                if (length(KTSEnv$indicesToRedden) == 0) {
                    tcltk::tkmessageBox(message = paste("No point was",
                                                        "selected"),
                                        icon = "warning")
                    
                } else {
                    
                    newName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName),
                                               noValid = NA)
                    
                    verifyCharEntry(tcltk::tclvalue(KTSEnv$newName), 
                                    noValid = NA)
                    selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
                    
                    if (is.na(newName)) {
                        newName <- paste0(KTSEnv$selTsName, "_pr")
                        
                        tcltk::tkmessageBox(message = paste("The time series",
                                                            "of removed values",
                                                            "will be named using",
                                                            "the suffix pr"),
                                            icon = "warning")
                        
                    } else if (newName == "overwrite series") {
                        newName <- KTSEnv$selTsName
                    }
                    
                    KTSEnv$indicesToRedden <- sort(KTSEnv$indicesToRedden)
                    selTs1 <- selTs
                    selTs1$value[KTSEnv$indicesToRedden] <- NA
                    if(all(is.na(selTs1$value)==TRUE)){
                        
                        tcltk::tkmessageBox(message = paste("All the values were",
                                                            "removed from the",
                                                            "time series"), 
                                            icon = "warning")
                        
                    }else{
                        assign(newName, selTs1, envir = KTSEnv)
                        grDevices::dev.new(noRStudioGD = TRUE)
                        graphics::plot(selTs1, cex = KTSEnv$poiSs, pch = 19,
                                       main = newName)
                    }
                    
                    
                }
            }
        } 
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        
        checkIfAnyTs(action = "showPANrmPoints", 
                     envirName = environment(showPANrmPoints))
    }

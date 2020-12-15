linCorrKTS <-
function() {
  lcorrOnOk1 <- function() {
    maxLag <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numlag), 
                             noValid = NA)
    tssel <- tsCheckedTF()
    checkedTS <- which(tssel == TRUE)
    lCheckedTS <- length(checkedTS)
    if (lCheckedTS < 1 | lCheckedTS > 2) {
      tcltk::tkmessageBox(message = paste("Choose one or",
                                          "two time series"), 
                          icon = "warning")
    } else if (is.na(maxLag)) {
      tcltk::tkmessageBox(message = paste("Choose the maximum",
                                          "delay (lags)"), 
                          icon = "warning")
    } else if (lCheckedTS == 1) {
      selTsName <- KTSEnv$dSList$TS[checkedTS]
      selTs <- get(selTsName, envir = KTSEnv)
      if (any(is.na(selTs$value))) {
        tcltk::tkmessageBox(message = paste("The time series must",
                                            "not contain any NAs"), 
                            icon = "warning")
      } else {
        acfPacfPlot <- function() {
          graphics::par(mfrow = c(2, 1))
          graphics::par(pty = "m")
          graphics::par(mar = c(4, 4, 2, 1))
          stats::acf(selTs$value, lag.max = maxLag, type = "correlation", 
                     main = selTsName, ylab = "ACF", xlab = "Lag", 
                     plot = TRUE, na.action = stats::na.pass, 
                     demean = TRUE)
          graphics::par(mar = c(4, 4, 2, 1))
          respacf <- stats::pacf(selTs$value, na.action = stats::na.pass, 
                                 lag.max = maxLag, main = "", 
                                 ylab = "PACF", xlab = "Lag")
        }
        
        grDevices::dev.new(noRStudioGD = TRUE)
        acfPacfPlot()
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        
      }
    } else if (lCheckedTS == 2) {
      selTsName <- KTSEnv$dSList$TS[checkedTS]
      selTs1 <- get(selTsName[1], envir = KTSEnv)
      selTs2 <- get(selTsName[2], envir = KTSEnv)
      timecompatibility <- are2TsTimeCompatible(selTs1, selTs2)
      if (any(is.na(selTs1$value)) | any(is.na(selTs2$value))) {
        tcltk::tkmessageBox(message = paste("The time series must",
                                            "not contain any NAs"), 
                            icon = "warning")
      } else if (timecompatibility[2] == FALSE) {
        tcltk::tkmessageBox(message = paste("Both time series must have",
                                            "the same sampling period"), 
                            icon = "warning")
      } else {
        if (timecompatibility[1] == FALSE) {
          lagTS <- diff(as.numeric(selTs1$time[1], selTs2$time[1]))
          tcltk::tkmessageBox(message = paste("There is a lag of ", lagTS, 
                                              "seconds between the",
                                              "time series",
                                              ". Take it into consideration"), 
                              icon = "warning")
        }
        ccfPlot <- function() {
          stats::ccf(selTs1$value, selTs2$value, 
                     type = "correlation", lag.max = maxLag, 
                     na.action = stats::na.pass, plot = TRUE, 
                     col = "midnightblue", 
                     main = paste(selTsName[1], "and", selTsName[2]))
          
        }
        grDevices::dev.new(noRStudioGD = TRUE)
        ccfPlot()
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      }
    }
  }
  lcorrOnOk2 <- function() {
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
    maxLag <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numlag), 
                             noValid = NA)
    tssel <- tsCheckedTF()
    checkedTS <- which(tssel == TRUE)
    lCheckedTS <- length(checkedTS)
    if (lCheckedTS < 1 | lCheckedTS > 2) {
      tcltk::tkmessageBox(message = paste("Choose one or",
                                          "two time series"), 
                          icon = "warning")
    } else if (is.na(maxLag)) {
      tcltk::tkmessageBox(message = paste("Choose the maximum",
                                          "delay (lags)"), 
                          icon = "warning")
    } else if (lCheckedTS == 1) {
      selTsName <- KTSEnv$dSList$TS[checkedTS]
      selTs <- get(selTsName, envir = KTSEnv)
      if (any(is.na(selTs$value))) {
        tcltk::tkmessageBox(message = paste("The time series must",
                                            "not contain any NAs"), 
                            icon = "warning")
      } else {
        acfPacfPlot <- function() {
          graphics::par(mfrow = c(2, 1))
          graphics::par(pty = "m")
          graphics::par(mar = c(4, 4, 2, 1))
          stats::acf(selTs$value, lag.max = maxLag, type = "correlation", 
                     main = selTsName, ylab = "ACF", xlab = "Lag", 
                     plot = TRUE, na.action = stats::na.pass, 
                     demean = TRUE)
          graphics::par(mar = c(4, 4, 2, 1))
          respacf <- stats::pacf(selTs$value, na.action = stats::na.pass, 
                                 lag.max = maxLag, main = "", 
                                 ylab = "PACF", xlab = "Lag")
        }
        
        plotToSave <- function(){
          acfPacfPlot()
        }
        saveThePlot()
        
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
        
      }
    } else if (lCheckedTS == 2) {
      selTsName <- KTSEnv$dSList$TS[checkedTS]
      selTs1 <- get(selTsName[1], envir = KTSEnv)
      selTs2 <- get(selTsName[2], envir = KTSEnv)
      timecompatibility <- are2TsTimeCompatible(selTs1, selTs2)
      if (any(is.na(selTs1$value)) | any(is.na(selTs2$value))) {
        tcltk::tkmessageBox(message = paste("The time series must",
                                            "not contain any NAs"), 
                            icon = "warning")
      } else if (timecompatibility[2] == FALSE) {
        tcltk::tkmessageBox(message = paste("Both time series must have",
                                            "the same sampling period"), 
                            icon = "warning")
      } else {
        if (timecompatibility[1] == FALSE) {
          lagTS <- diff(as.numeric(selTs1$time[1], selTs2$time[1]))
          tcltk::tkmessageBox(message = paste("There is a lag of ", lagTS, 
                                              "seconds between the",
                                              "time series",
                                              ". Take it into consideration"), 
                              icon = "warning")
        }
        ccfPlot <- function() {
          stats::ccf(selTs1$value, selTs2$value, 
                     type = "correlation", lag.max = maxLag, 
                     na.action = stats::na.pass, plot = TRUE, 
                     col = "midnightblue", 
                     main = paste(selTsName[1], "and", selTsName[2]))
          
        }
        plotToSave <- function(){
          ccfPlot()
        }
        saveThePlot()
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      }
    }
  }
  showPANlcorr <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "LINEAR CORRELATION")
    createTsChb()
    createEntry(labTitle = "Max. number of lags", 
                textVariableName = "numlag")
    createOK(labTitle = "PLOT", action = lcorrOnOk1)
    createOK(labTitle = "PLOT TO FILE", action = lcorrOnOk2, width = 14)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANlcorr", envirName = environment(showPANlcorr))
}

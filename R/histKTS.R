histKTS <-
function() {
  histOnOk1 <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), noValid = NA)
    nBars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numberBars), noValid = NA)
    KTSEnv$xScls <- verifyRealEntry(tcltk::tclvalue(KTSEnv$xScl),
                                    noValid = NA)
    
    KTSEnv$yScls <- verifyRealEntry(tcltk::tclvalue(KTSEnv$yScl),
                                    noValid = NA)
    if(is.na(KTSEnv$xScls)){KTSEnv$xScls <- 1}
    if(is.na(KTSEnv$yScls)){KTSEnv$yScls <- 1}
    
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else {
      selTs <- get(selTsName, envir = KTSEnv)
      if (is.na(nBars)) {
        numnBars <- diff(range(selTs$value, na.rm = TRUE))
        dennBars1 <- stats::IQR(selTs$value, na.rm = TRUE)
        dennBars2 <- length(selTs$value)^(1/3)
        dennBars <- 2 * dennBars1/dennBars2
        nBars <- numnBars/dennBars
        nBars <- round(nBars)
      }
      plotHist <- function() {
        histResult <- graphics::hist(selTs$value, nBars, plot = FALSE)
        graphics::plot(histResult, ylab = "Probability density", xlab = "", 
                       main = paste("Histogram of ", selTsName), 
                       col = "limegreen", freq = FALSE)
        x = seq(min(selTs$value, na.rm = TRUE), max(selTs$value, na.rm = TRUE), 
                length.out = 100)
        graphics::curve(stats::dnorm(x, mean(selTs$value, na.rm = TRUE), 
                                     stats::sd(selTs$value, na.rm = TRUE)), 
                        add = TRUE, col = "darkred")
        
      }
      
      grDevices::dev.new(noRStudioGD = TRUE)
      plotHist()
      # copyPlot <- function() {
      #   tkrplot::tkrreplot(tsPlot)
      # }
      # panelName <- createRandName()
      # assign(panelName, tcltk::tktoplevel(bg = "white"))
      # tcltk::tkwm.title(get(panelName), "Histogram")
      # tsPlot <- tkrplot::tkrplot(get(panelName), fun = plotHist,
      #                            hscale = KTSEnv$xScls, 
      #                            vscale = KTSEnv$yScls)
      # copyButton <- tcltk::tkbutton(get(panelName), 
      #                               text = "Copy to clipboard", 
      #                               command = copyPlot)
      # tcltk::tkpack(tsPlot, expand = TRUE, fill = "both", anchor = "center")
      # tcltk::tkconfigure(tsPlot, bg = "white")
      # tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
    }
  }
  histOnOk2 <- function() {
    
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
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), noValid = NA)
    nBars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numberBars), noValid = NA)
    KTSEnv$xScls <- verifyRealEntry(tcltk::tclvalue(KTSEnv$xScl),
                                    noValid = NA)
    
    KTSEnv$yScls <- verifyRealEntry(tcltk::tclvalue(KTSEnv$yScl),
                                    noValid = NA)
    if(is.na(KTSEnv$xScls)){KTSEnv$xScls <- 1}
    if(is.na(KTSEnv$yScls)){KTSEnv$yScls <- 1}
    
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else {
      selTs <- get(selTsName, envir = KTSEnv)
      if (is.na(nBars)) {
        numnBars <- diff(range(selTs$value, na.rm = TRUE))
        dennBars1 <- stats::IQR(selTs$value, na.rm = TRUE)
        dennBars2 <- length(selTs$value)^(1/3)
        dennBars <- 2 * dennBars1/dennBars2
        nBars <- numnBars/dennBars
        nBars <- round(nBars)
      }
      plotHist <- function() {
        histResult <- graphics::hist(selTs$value, nBars, plot = FALSE)
        graphics::plot(histResult, ylab = "Probability density", xlab = "", 
                       main = paste("Histogram of ", selTsName), 
                       col = "limegreen", freq = FALSE)
        x = seq(min(selTs$value, na.rm = TRUE), max(selTs$value, na.rm = TRUE), 
                length.out = 100)
        graphics::curve(stats::dnorm(x, mean(selTs$value, na.rm = TRUE), 
                                     stats::sd(selTs$value, na.rm = TRUE)), 
                        add = TRUE, col = "darkred")
        
      }
      
      
      plotToSave <- function(){
        plotHist()
      }
      saveThePlot()
      
    
      # copyPlot <- function() {
      #   tkrplot::tkrreplot(tsPlot)
      # }
      # panelName <- createRandName()
      # assign(panelName, tcltk::tktoplevel(bg = "white"))
      # tcltk::tkwm.title(get(panelName), "Histogram")
      # tsPlot <- tkrplot::tkrplot(get(panelName), fun = plotHist,
      #                            hscale = KTSEnv$xScls, 
      #                            vscale = KTSEnv$yScls)
      # copyButton <- tcltk::tkbutton(get(panelName), 
      #                               text = "Copy to clipboard", 
      #                               command = copyPlot)
      # tcltk::tkpack(tsPlot, expand = TRUE, fill = "both", anchor = "center")
      # tcltk::tkconfigure(tsPlot, bg = "white")
      # tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
    }
  }
  showPANhist <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "HISTOGRAM")
    createTsRb()
    createEntry(labTitle = "Approx. number of bars", 
                textVariableName = "numberBars")
    createEntry(labTitle = "X Scale",textVariableName = "xScl")
    
    createEntry(labTitle = "Y Scale",textVariableName = "yScl")
    createOK(labTitle = "PLOT", action = histOnOk1)
    createOK(labTitle = "PLOT TO FILE", action = histOnOk2, width = 14)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANhist", 
               envirName = environment(showPANhist))
}

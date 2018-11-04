cleanEnvir <-
function(envir = KTSEnv) {
  
  candidates <- c("CombiType", "OBErrors", "UF", "alfa", 
                  "candidToRedden", "cenday", "theScalar",
                  "cenhour", "cenmins", "cenmonth", "censecs", 
                  "cenyear", "correl", "cri1cbValue","saveToFile","xlim", 
                  "cri2cbValue", "cri3cbValue", "cri4cbValue", 
                  "cri5cbValue", "degreepol","NB1","NB2","Meth1","Meth2", 
                  "delay", "delsel", "depla0", "doCorrSum", "doSampEntr", 
                  "lin", "poin","lemar","lemars","lomar","lomars", 
                  "embDelays", "embDim", "embDimension", "embsel", 
                  "escFact", "extrap", "finday", 
                  "finhour", "finmins", "finmonth", "finsecs", "finyear", 
                  "fragToZoom","fragToZooms", "gapselec","ZInd", 
                  "gapselecname", "grP", "indicesToRedden", 
                  "iniday", "tsToPlot", 
                  "inihour", "inimins", "inimonth", "inisecs", "iniyear", 
                  "inpCheckedInd", 
                  "lagDel", "lagDelay", "lagDif", "lengthGap", "madila", 
                  "magniTSNames", "maxEmb", 
                  "maxIter", "maxIterations", "maxRad", "maxcbValue", 
                  "maxiSize", "maxniter", 
                  "mecbValue", "medcbValue", "mediaona", "mininumNObs", 
                  "minEmb", "minLength", 
                  "minRad", "mincbValue", "miniSize", "nDiameter", 
                  "nTrees", "naDens", "naTreatment", 
                  "naTreatmentSel", "namesCols", "ncolPredata", "newName", 
                  "nnmgap", "notAll", 
                  "nuf", "numRad", "numberBars", "numberGaps", 
                  "smoothTSName", "numberTrees", 
                  "numlag", "outCheckedInd", "panelName", "parPlotSize", 
                  "peri", "period","diaYOrN", "nuMlag",
                  "perival", "predTSNames", "q1cbValue", "q3cbValue", 
                  "ressMeVal", "rmaselec", 
                  "rmaselecname", "scaleYN", "sdcbValue", "seasonality", 
                  "seawin", "selNewName", 
                  "selgap", "selrma", "selrma1", "selrma2", "selser", 
                  "selser0", "selser1", 
                  "selser2", "selsizeWindow", "seriesel", 
                  "seriesel1name", "seriesel2name", 
                  "serieselname", "sigdi0", "signifLev", "sizeSurr", 
                  "sizeWindow", "slopeTSNames", 
                  "span", "specGap", "statisType", "statisTypeSel", 
                  "tailornot", "theilWin", 
                  "theilerWin", "thresh", "timeDelay", "tiparch", 
                  "touchedPoints", "trendwin", 
                  "tsPlot", "uC", "ventanaper", "win1", 
                  "win2", "win3", "win4", "win5", 
                  "win6", "win7", "wingap", "selIniVal", "selCenterOrNot", 
                  "selGap", "selGapName", 
                  "selGapP", "selRm", "selRmName", "selRmP", "selRmP1", 
                  "selRmP2", "selTs", 
                  "selTs1Name", "selTs2Name", "selTsName", "selTsP", 
                  "selTsP0", "selTsP1","tiLo","tiLos","laLo","laLos",
                  "selTsP2", "tsWithGapsName", "txt1", "txt2", 
                  "rmFromFilling", "rmFromFill", 
                  "magTsP", "magTs", "magTsName", "dirTsP", "numberBins", 
                  "numberTicks", "regTsName", 
                  "regTS", "bsE", "fxE", "perE",
                  "tsToPlotNames","lineas","puntos","lin","poin",
                  "tsToPlotNamesZ","xlabs","ylabs","zlabs","tickSis",
                  "labSis","CoLoRs","typeDist","typeDi",
                  "linWs", "poiSs","suff","filename","winW","winH",
                  "xScls","yScls","CountTS","defaultTSCoLoRs")
  
  removeIfExists(candidates, envir = envir)
  
  patron1 <- ls(pattern = "*cbValue", envir = envir)
  if (length(patron1) > 0) {
    rm(list = patron1, envir = envir)
  }
  
  patron2 <- ls(pattern = "*rbValue", envir = envir)
  if (length(patron2) > 0) {
    rm(list = patron2, envir = envir)
  }
  
  patron3 <- ls(pattern = "linW*", envir = envir)
  if (length(patron3) > 0) {
    rm(list = patron3, envir = envir)
  }
  
  patron5 <- ls(pattern = "poiS*", envir = envir)
  if (length(patron5) > 0) {
    rm(list = patron5, envir = envir)
  }
  
  patron6 <- ls(pattern = "tsToZoom*", envir = envir)
  if (length(patron6) > 0) {
    rm(list = patron6, envir = envir)
    
  }
  
  patron7 <- ls(pattern = "CoLoR*", envir = envir)
  if (length(patron7) > 0) {
    rm(list = patron7, envir = envir)
  }
  
  patron8 <- ls(pattern = "tEntscbValue*", envir = envir)
  if (length(patron8) > 0) {
    rm(list = patron8, envir = envir)
  }

  if(exists("mainPanel", envir = KTSEnv)){
    
    try(tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr"),
        silent = TRUE)
    
  }
  
}

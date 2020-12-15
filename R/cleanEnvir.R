cleanEnvir <-
function(envir = KTSEnv) {
  
  candidates <- c("alfa", "bsE", "cenday",
                  "cenhour", "cenmins", "cenmonth","censecs",
                  "cenyear", "CoLoRs", "CombiType", "correl", 
                  "CountTS", "cri1cbValue","cri2cbValue","cri3cbValue",  
                  "cri4cbValue","cri5cbValue","defaultTSCoLoRs", "degreepol", 
                  "delay",  "delsel", "depla0", "diaYOrN",
                  "dirTsP", "doCorrSum", "doSampEntr", "embDelays", 
                  "embDim", "embDimension", "embsel", "escFact",
                  "extrap", "filename","finday", "finhour",
                  "finmins", "finmonth","finsecs", "finyear",
                  "fragToZoom", "fragToZooms","fxE", "gapselec", 
                  "gapselecname", "grP", "indicesToRedden", "iniday", 
                  "inihour", "inimins", "inimonth","inisecs",
                  "iniyear", "inpCheckedInd",  "labSis", "lagDel", 
                  "lagDelay","lagDif", "laLo", "laLos", 
                  "lemar",  "lemars", "lengthGap", "lin", 
                  "lin", "lineas", "linWs",  "lomar", 
                  "lomars", "madila", "magniTSNames", "magTs", 
                  "magTsName", "magTsP", "maxcbValue", "maxEmb", 
                  "maxiSize","maxIter", "maxIterations",  "maxniter", 
                  "maxRad", "mecbValue", "medcbValue", "mediaona", 
                  "Meth1",  "Meth2",  "mincbValue", "minEmb", 
                  "mininumNObs","miniSize","minLength", "minRad", 
                  "naDens", "namesCols", "naTreatment","naTreatmentSel", 
                  "NB1", "NB2", "ncolPredata","nDiameter", 
                  "newName", "nnmgap", "notAll", "nTrees", 
                  "nuf", "numberBars", "numberBins", "numberGaps",
                  "numberTicks","numberTrees","numlag", "nuMlag", 
                  "numRad", "OBErrors","outCheckedInd",  "panelName", 
                  "perE", "peri", "period", 
                  "perival", "poin", "poin", "poiSs", 
                  "predTSNames","puntos", "q1cbValue", "q3cbValue", 
                  "regTS",  "regTsName","resp", "ressMeVal", "rmaselec", 
                  "rmaselecname", "rmFromFill", "rmFromFilling",  "saveToFile",
                  "scaleYN", "sdcbValue", "seasonality","seawin", 
                  "selCenterOrNot", "selgap", "selGap", "selGapName",
                  "selGapP", "selIniVal", "selNewName", "selRm", 
                  "selrma", "selrma1", "selrma2", "selRmName", 
                  "selRmP", "selRmP1", "selRmP2", "selser", 
                  "selser0", "selser1", "selser2", "selsizeWindow", 
                  "selTs",  "selTs1Name", "selTs2Name", "selTsName", 
                  "selTsP", "selTsP0", "selTsP1", "selTsP2",
                  "seriesel","seriesel1name",  "seriesel2name",  "serieselname",  
                  "sigdi0", "signifLev", "sizeSurr","sizeWindow",
                  "slopeTSNames", "smoothTSName", "span", "specGap",
                  "statisType", "statisTypeSel",  "suff", "tailornot", 
                  "theilerWin", "theilWin","theScalar", "thresh", 
                  "tickSis", "tiLo", "tiLos",  "timeDelay", 
                  "tiparch", "trendwin","tsPlot", 
                  "tsToPlot","tsToPlotNames",  "tsToPlotNamesZ", "tsWithGapsName", 
                  "txt1", "txt2", "typeDi", "typeDist", 
                  "UF",  "ventanaper", "win1",  
                  "win2", "win3", "win4", "win5",  
                  "win6", "win7", "wingap", "winH",  
                  "winW", "xlabs",  "xlim", 
                  "ylabs",  "zlabs")
  
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

cleanEnvir <-
function(envir = KTSEnv) {
  candidates <- c("CombiType", "OBErrors", "UF", "alfa", 
                  "candidToRedden", "cenday", 
                  "cenhour", "cenmins", "cenmonth", "censecs", 
                  "cenyear", "correl", "cri1cbValue", 
                  "cri2cbValue", "cri3cbValue", "cri4cbValue", 
                  "cri5cbValue", "degreepol", 
                  "delay", "delsel", "depla0", "doCorrSum", "doSampEntr", 
                  "ele1cbValue", "ele2cbValue", 
                  "embDelays", "embDim", "embDimension", "embsel", 
                  "escFact", "extrap", "finday", 
                  "finhour", "finmins", "finmonth", "finsecs", "finyear", 
                  "fragToZoom", "gapselec", 
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
                  "peri", "period", 
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
                  "selTsP0", "selTsP1", 
                  "selTsP2", "tsWithGapsName", "txt1", "txt2", 
                  "rmFromFilling", "rmFromFill", 
                  "magTsP", "magTs", "magTsName", "dirTsP", "numberBins", 
                  "numberTicks", "regTsName", 
                  "regTS", "bsE", "fxE", "perE")
  removeIfExists(candidates, envir = envir)
  patron1 <- ls(pattern = "*cbValue")
  if (length(patron1) > 0) {
    rm(list = patron1, envir = KTSEnv)
  }
  patron2 <- ls(pattern = "*rbValue")
  if (length(patron2) > 0) {
    rm(list = patron2, envir = KTSEnv)
  }
}


makeGlobal <- c("aboutKTSButton","activMenu","alfa", "bsE", 
                "cenday","cenhour",  "cenmins", 
                "cenmonth", "censecs",  "cenyear",  "CoLoRs",  
                "CombiType","correl","CountTS",  "createSubPanR4C3",
                "cri1cbValue", "cri2cbValue", "cri3cbValue", "cri4cbValue",
                "cri5cbValue", "defaultTSCoLoRs", "degreepol","delay",
                "delsel","dentro","dentroplot",  "depla0",  
                "dirTsP","doCorrSum","doSampEntr",  "dSList",  
                "embDelays","embDim","embDimension","embsel",  
                "escFact",  "extrap","fan",  "fanOption",  
                "fiAmNe","filename", "finday","finhour", 
                "finmins",  "finmonth", "finsecs",  "finyear", 
                "fragToZoom",  "fragToZooms", "frameForButtons0", "fxE", 
                "gapselec", "gapselecname","grP",  "heigth4", 
                "helpButton",  "indicesToRedden",  "iniday","inihour", 
                "inimins",  "inimonth", "inisecs",  "iniyear", 
                "inpCheckedInd", "KTSEnv","KTSFonts", "labSis",  
                "lagDel","lagDelay", "lagDif","lemar",
                "lemars","lengthGap","lin",  "lin", 
                "lineas","linWs","lomar","lomars",  
                "madila","magniTSNames","magTs","magTsName",  
                "magTsP","mainPanel","mainPanel.0", "maxcbValue", 
                "maxEmb","maxiSize", "maxIter",  "maxIterations",
                "maxniter", "maxRad","mecbValue","medcbValue", 
                "mediaona", "menu1Button", "menu2Button", "menu3Button",
                "menu4Button", "menu5Button", "mincbValue",  "minEmb",  
                "mininumNObs", "miniSize", "minLength","minRad",  
                "naDens","namesCols","naTreatment", "naTreatmentSel",  
                "ncolPredata", "nDiameter","newName",  "newWin",  
                "nnmgap","nombre","notAll","nTrees",  
                "nuf",  "numberBars",  "numberBins",  "numberGaps", 
                "numberTicks", "numberTrees", "numlag","nuMlag",  
                "numRad","OBErrors", "origTSName",  "outCheckedInd",
                "panelName","perE", "peri", "period",  
                "perival",  "poin", "poin", "pointToFill",
                "poiSs","predTSNames", "puntos","q1cbValue",  
                "q3cbValue","resIm","resp","ressMeVal","rmaselec",
                "rmaselecname","rmFromFill",  "rmFromFilling", "row1",
                "row11","row4", "row4.col1","row4.col2",  
                "row4.col3","row5", "rows2and3","saveReportBut",
                "saveToFile",  "scaleYN",  "screenSize",  "sdcbValue",  
                "seasonality", "seawin","selCenterOrNot","selgap",  
                "selGap","selGapName",  "selGapP",  "selIniVal",  
                "selNewName",  "selRm","selrma","selrma1", 
                "selrma2",  "selRmName","selRmP","selRmP1", 
                "selRmP2",  "selser","selser0",  "selser1", 
                "selser2",  "selsizeWindow", "selTs","selTs1Name", 
                "selTs2Name",  "selTsName","selTsP","selTsP0", 
                "selTsP1",  "selTsP2",  "seriesel", "seriesel1name",
                "seriesel2name", "serieselname","sigdi0","signifLev",  
                "sizeSurr", "sizeWindow",  "slopeTSNames","spaceForTxtW", 
                "spaceForTxtW5", "span", "specGap",  "statisType", 
                "statisTypeSel", "subPanR4C1",  "suff", "tailornot",  
                "theilerWin",  "theilWin", "theScalar","thresh",  
                "threshx",  "threshy",  "tickSis",  "timeDelay",  
                "timeZone", "tiparch",  "tipo", "tkrp",
                "tkrp1","tkrp2","tkrp3","trendwin",
                "tsPlot","tsToPlotNames", "tsToPlotNamesZ","tsWithGapsName",  
                "txt1", "txt2", "txtWidget","txtWidget5", 
                "typeDi","typeDist", "UF","umbral",  
                "ventanaper",  "width4.1", "width4.2", "widthRow1",  
                "win1", "win2", "win3", "win4",
                "win5", "win6", "win7", "wingap",  
                "winH", "winW", "xlabs","xlim",
                "xScr", "xScr5","ylabs","yScr",
                "yScr5","zlabs")


#' @import utils
utils::globalVariables(makeGlobal)

KTSEnv <- new.env()

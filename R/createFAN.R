createFAN <-
function() {
    
    showPANfan1 <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "FAN RECURRENCE MATRIX")
      assign("fanOption", tcltk::tclVar("Modify existing matrix"), envir = KTSEnv)
      opList <- c("Modify existing matrix",
                  "Create from time series")
      for(ope in opList){
        createEachRb(labTitle = ope, variable = KTSEnv$fanOption)
      }
      
      createOK(labTitle = "NEXT", action = fanOnOk1)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    showPANfan2a <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "FAN RECURRENCE MATRIX")
      createRmRb()
      createNote(labTitle = "Choose the time series from which the matrix originated")
      createTsRb()
      createEntry(labTitle = "Maximum number of neighbors",
                  textVariableName = "fiAmNe", defaultVal = "")
      createEntry(labTitle = "Name", textVariableName = "newName")
      createOK(labTitle = "RUN", action = fanOnOk2a)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    showPANfan2b1 <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "FAN RECURRENCE MATRIX")
      createTsRb()
      createEntry(labTitle = "Maximum number of neighbors",
                  textVariableName = "fiAmNe", defaultVal = "")
      
      createOK(labTitle = "NEXT", action = fanOnOk2b1)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    showPANfan2b2 <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "FAN RECURRENCE MATRIX")
      createNote(labTitle = paste("Time series:", KTSEnv$selTsName))
      createTitle(labTitle = "Parameters")
      createEntry(labTitle = "Embedding dimension",
                  textVariableName = "embDim", defaultVal = "1")
      createEntry(labTitle = "Delay", textVariableName = "lagDel",
                  defaultVal = "0")
      createEntry(labTitle = "Tolerance", textVariableName = "thresh")
      createEntry(labTitle = "Theiler's window",textVariableName = "theilWin")
      createEntry(labTitle = "Name", textVariableName = "newName")
      createOK(labTitle = "RUN", action = fanOnOk2b2)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    fanOnOk1 <- function() {
      
      fanOp <- tcltk::tclvalue(KTSEnv$fanOption)
      if (fanOp == "Modify existing matrix") {
        
        checkIfAnyRm(action = "showPANfan2a",
                     envirName = environment(fanOnOk1))
        
      }else if(fanOp == "Create from time series"){
        
        checkIfAnyTs(action = "showPANfan2b1",
                     envirName = environment(fanOnOk1))
        
      }else{
        
        tcltk::tkmessageBox(message = "Choose an option",
                            icon = "warning")
        
      }
      
    }
    fanOnOk2a <- function() {
      
      getRP4FAN <- function(selTs,selRm){
        
        rebuildCol <- function(indCol, X1, Y1) {
          sort(c(Y1[which(X1 == indCol)], X1[which(Y1 == indCol)]))
        }
        
  
        if (selRm$embDim == 1) { 
          
          dataTS <- matrix(selTs$value)
          infNorm <- function(v1, v2) {
            abs(v2 - v1)
          }
          
        } else {
          
          dataTS <- tseriesChaos::embedd(selTs$value,
                                         m = selRm$embDim,
                                         d = selRm$delay)
          
          infNorm <- function(v1, v2) {
            
            if(any(class(v2)=="numeric")){v2 <- t(as.matrix(v2))}
            lv2 <- NROW(v2)
            apply(abs(v2 - matrix(rep(v1, each = lv2), lv2, selRm$embDim)),
                  1,FUN = max, na.rm = FALSE)
            
          }
          
        }
        
        colsWithRP <- sort(union(unique(selRm$ones$X),unique(selRm$ones$Y)))
        colsWithRP <- as.matrix(colsWithRP)
        
        recPointsX <- vector("list", length(colsWithRP))
        recPointsY <- vector("list", length(colsWithRP))
        
        cont <- 1
        
        for( indCol in colsWithRP){
          
          reCol <- rebuildCol(indCol = indCol, X1 = selRm$ones$X, Y1 =  selRm$ones$Y)
          
          reColL <- length(reCol)
          
          if (reColL > 0 & reColL <= fan){
           
            recPointsY[[cont]] <- reCol
            
            recPointsX[[cont]] <- rep(indCol, reColL)
            
            
          }else if(reColL > fan){
            
            P <- dataTS[indCol, ]
            pointsOtherThanP <- dataTS[reCol, ]
            distanceP <- infNorm(P, pointsOtherThanP)
            distanceP[which(is.finite(distanceP)== FALSE)] <- NA
            ordenados <- sort(distanceP, index.return = TRUE, na.last = NA)
          
            Lordenados <- length(ordenados$ix)
            
            if(Lordenados > 0 & Lordenados <= fan){
              
              recPointsY[[cont]] <- reCol
              
              recPointsX[[cont]] <- rep(indCol, reColL) 
              
            }else if(Lordenados > fan){
              
              recPointsY[[cont]] <- sort(reCol[ordenados$ix[1:fan]])
              
              recPointsX[[cont]] <- rep(indCol, fan) 
              
              
            }
            
            rm(P, pointsOtherThanP, distanceP, ordenados,Lordenados)
            
            }
          
          rm(reCol, reColL)
          cont <- cont + 1
          
        }
        
        
        if(length(recPointsY) == 0){
          
          res <- list(recPointsX = NULL, recPointsY = NULL)
          
        }else{
          
          recPointsX <- unlist(recPointsX)
          recPointsY <- unlist(recPointsY)
          res <- list(recPointsX = recPointsX, recPointsY =recPointsY)
          
        }
        
        res
        
      }
      
      
      refreshDataSetsList(outp = FALSE)
      selRmName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selRmP),
                                   noValid = NA)
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP),
                                   noValid = NA)
      fan <- verifyIntEntry(tcltk::tclvalue(KTSEnv$fiAmNe),
                            noValid = NA)
      selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName),
                                    noValid = NA)
      
      if (is.na(selRmName)) {
        tcltk::tkmessageBox(message = "Choose a recurrence matrix",
                            icon = "warning")
      }else if (is.na(selTsName)) {
          tcltk::tkmessageBox(message = "Choose a time series",
                              icon = "warning")
      } else if (is.na(selNewName)) {
        tcltk::tkmessageBox(message = paste("Enter a name for the",
                                            "recurrence matrix "),
                            icon = "warning")
      } else if (is.na(fan)) {
        tcltk::tkmessageBox(message = paste("Choose an integer ",
                                            "maximum amount of neighbors"),
                            icon = "warning")
      } else if (fan <= 0) {
        tcltk::tkmessageBox(message = paste("The maximum amount of neighbors",
                                            "must be a positive integer"),
                            icon = "warning")
      }else{
        
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        selRm <- get(selRmName, envir = KTSEnv)
        selTs <- get(selRm$tsName,envir = KTSEnv)
        
        compRes <- areTsRmTimeCompatible(selTs, selRm)
        
        
        if(selRm$type != "simple"){
          
          tcltk::tkmessageBox(message = paste("Choose a simple recurrence matrix"),
                              icon = "warning")
          
          
        }else if(any(compRes)==FALSE){
          

          tcltk::tkmessageBox(message = paste("Choose the time series from which the matrix originated"),
                              icon = "warning")        
          
        }else{
          
          RP4FAN <- getRP4FAN(selTs = selTs,selRm = selRm)
          recPointsX <- RP4FAN$recPointsX
          recPointsY <- RP4FAN$recPointsY
          
          if (length(recPointsY) == 0) {
            
            tcltk::tkmessageBox(message = paste("No recurrence",
                                                "point was found"),
                                icon = "warning")
            tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
            cleanEnvir()
            refreshDataSetsList(outp = FALSE)
            showPANfan2a()
            
          } else {
            
            recPoints <- as.data.frame(cbind(recPointsX, recPointsY))
            colnames(recPoints) <- c("X", "Y")
            recMatrix <- selRm
            recMatrix$ones <- recPoints
            recMatrix$type <- "fan"
            assign(selNewName, recMatrix, envir = KTSEnv)
            tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
            cleanEnvir()
            refreshDataSetsList(outp = FALSE)
            showPANfan1()
            
          }
          
        }
        
      }
      
    }
    fanOnOk2b1 <- function() {
      
      refreshDataSetsList(outp = FALSE)
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP),
                                   noValid = NA)
      fan <- verifyIntEntry(tcltk::tclvalue(KTSEnv$fiAmNe),
                            noValid = NA)
      
      if (is.na(selTsName)) {
        tcltk::tkmessageBox(message = "Choose a time series",
                            icon = "warning")
      } else if (is.na(fan)) {
        tcltk::tkmessageBox(message = paste("Choose an integer",
                                            "maximum amount of neighbors"),
                            icon = "warning")
      } else if (fan <= 0) {
        tcltk::tkmessageBox(message = paste("The maximum amount of neighbors",
                                            "must be a positive integer"),
                            icon = "warning")
      }else{
        
        assign("selTsName", selTsName, envir = KTSEnv)
        assign("fan", fan, envir = KTSEnv)
        showPANfan2b2()
        
      }
      
    }
    fanOnOk2b2 <- function() {
      
      refreshDataSetsList(outp = FALSE)
      fan <- KTSEnv$fan
      embedDim <- verifyIntEntry(tcltk::tclvalue(KTSEnv$embDim),
                                 noValid = NA)
      lagDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lagDel),
                                 noValid = NA)
      threshold <- verifyRealEntry(tcltk::tclvalue(KTSEnv$thresh),
                                   noValid = NA)
      theilerWin <- verifyIntEntry(tcltk::tclvalue(KTSEnv$theilWin),
                                   noValid = NA)
      selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$newName),
                                    noValid = NA)
      if (is.na(embedDim)) {
        tcltk::tkmessageBox(message = paste("Choose an ",
                                            "embedding dimension"),
                            icon = "warning")
      } else if (embedDim == 0) {
        tcltk::tkmessageBox(message = paste("The embedding",
                                            "dimension must be",
                                            "one or greater"),
                            icon = "warning")
      } else if (is.na(lagDelay)) {
        tcltk::tkmessageBox(message = "Choose a delay", icon = "warning")
      } else if (embedDim > 1 & lagDelay == 0) {
        tcltk::tkmessageBox(message = paste("For embedding dimesions",
                                            "greater than 1, the",
                                            "delay must be greater",
                                            "than 0"),
                            icon = "warning")
      } else if (is.na(threshold)) {
        tcltk::tkmessageBox(message = "Choose a tolerance",
                            icon = "warning")
      }else if (is.na(theilerWin)){
        
        tcltk::tkmessageBox(message = "Enter the Theiler's window. It must be an integer.",
                            icon = "warning")
        
      } else if (is.na(selNewName)) {
        tcltk::tkmessageBox(message = paste("Enter a name for the",
                                            "recurrence matrix "),
                            icon = "warning")
      } else {
        
        tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
        selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
        
        res <- getFANRM2(selTs = selTs,
                         embedDim = embedDim,
                         lagDelay = lagDelay,
                         threshold = threshold,
                         theilerWin = theilerWin, 
                         fan = fan)
        
        recPointsX <- res$recPointsX
        recPointsY <- res$recPointsY
        
        if (length(recPointsY) == 0) {
          
          tcltk::tkmessageBox(message = paste("No recurrence",
                                              "point was found"),
                              icon = "warning")
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANfan1()
          
        } else {
          
          recPoints <- as.data.frame(cbind(recPointsX, recPointsY))
          colnames(recPoints) <- c("X", "Y")
          recMatrix <- list(ones = recPoints, tol = threshold,
                            tsName = KTSEnv$selTsName,
                            embDim = embedDim, delay = lagDelay,
                            dist = "inf_norm",
                            tsLength = nrow(selTs),
                            samPerSec = diff(as.numeric(selTs$time[1:2])),
                            tsIni = as.character(selTs$time[1]),
                            type = "fan")
          assign(selNewName, recMatrix, envir = KTSEnv)
          tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANfan1()
          
        }
        
      }
      
    }
    
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    showPANfan1()
    
    
    
  }

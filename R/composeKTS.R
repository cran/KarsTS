composeKTS <-
function() {
    
    getName <- function(selOp,tsToCompose){
      
      if(selOp == "Add" | selOp == "Multiply"){
        
        selNewName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$NewName),
                                      noValid = NA)
        if(is.na(selNewName)){ selNewName <- "operationResult"}
        
      }else{
        
        opList <- c("Add", "Multiply","Subtract", "Divide","Absolute value",
                    "Opposite","Reciprocal","Ln","Log10",
                    "Exp","10^","Add scalar", "Multiply by scalar",
                    "Raise to scalar","Remove points")
        opListShort <- c("Add", "Mult","Sub", "Div","AbsV",
                         "Opp","Rec","Ln","Log10",
                         "Exp","10ts","AddS", "MultS","RaiS","rmP")
        
        selNewName <- paste0(tsToCompose,
                             opListShort[which(opList == selOp)])
        
      }
      
      selNewName
      
    }
    areAllTsCompatible <- function(ntsToCompose, tsToCompose) {
      
      if (ntsToCompose == 2) {
        tmComptibility <- are2TsTimeCompatible(get(tsToCompose[1], 
                                                   envir = KTSEnv), 
                                               get(tsToCompose[2], 
                                                   envir = KTSEnv))
        tmComptibility <- t(as.matrix(tmComptibility))
      } else {
        firstTS <- get(tsToCompose[1], envir = KTSEnv)
        tmComptibility <- matrix(rep(FALSE, 3 * ntsToCompose), 
                                 ntsToCompose, 3)
        for (i in 2:ntsToCompose) {
          tmComptibility[i, ] <- are2TsTimeCompatible(firstTS, 
                                                      get(tsToCompose[i], 
                                                          envir = KTSEnv))
        }
        tmComptibility <- tmComptibility[-1, ]
        if (is.vector(tmComptibility)) {
          tmComptibility <- as.matrix(t(tmComptibility))
        }
      }
      tmComptibility
    }
    buildTsMatrix <- function(tsToCompose) {
      matrixTs <- NULL
      for (i in tsToCompose) {
        matrixTs <- cbind(matrixTs, get(i, envir = KTSEnv)$value)
      }
      matrixTs
    }
    showPANcompo1 <- function() {
      createSubPanR4C1()
      createTITLE(labTitle = "OPERATIONS")
      assign("operator", tcltk::tclVar("AddTS"), envir = KTSEnv)
      opList <- c("Add", "Multiply","Subtract", "Divide","Absolute value",
                  "Opposite","Reciprocal","Ln","Log10",
                  "Exp","10^","Add scalar", "Multiply by scalar",
                  "Raise to scalar","Remove points")
      
      for(ope in opList){
        createEachRb(labTitle = ope, variable = KTSEnv$operator)  
      }
      createOK(labTitle = "NEXT", action = compo1OnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    compo1OnOk <- function() {
      selOp <- tcltk::tclvalue(KTSEnv$operator)
      if (selOp == "Subtract" | selOp == "Divide") {
        showPANcompo2B1()
      }else if(selOp == "Remove points"){
        showPANcompo2C()
      }else{
        showPANcompo2A()
      }
    }
    showPANcompo2A <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      selOp <- tcltk::tclvalue(KTSEnv$operator)
      createTITLE(labTitle = selOp)
      createTsChb()
      if(any(c("Add scalar","Multiply by scalar","Raise to scalar") == selOp)){
        createEntry(labTitle = "Scalar", textVariableName = "theScalar")
      }
      if(selOp == "Add" | selOp == "Multiply"){
        createEntry(labTitle = "Name", textVariableName = "NewName")
      }
      createOK(labTitle = "RUN", action = compo2AOnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    }
    showPANcompo2B1 <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      selOp <- tcltk::tclvalue(KTSEnv$operator)
      createTITLE(labTitle = selOp)
      if(selOp == "Subtract"){labTi1 <- "Minuend"}else{labTi <- "Numerator"}
      createTsRb(labTitle = labTi1, variableName = "selTsP1")
      createOK(labTitle = "NEXT", action = showPANcompo2B2)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    }
    showPANcompo2B2 <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      selOp <- tcltk::tclvalue(KTSEnv$operator)
      createTITLE(labTitle = selOp)
      if(selOp == "Subtract"){labTi2 <- "Subtrahend"}else{labTi <- "Denominator"}
      createTsRb(labTitle = labTi2, variableName = "selTsP2")
      createEntry(labTitle = "Name (optional)", textVariableName = "NewName")
      createOK(labTitle = "RUN", action = compo2BOnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    }
    showPANcompo2C <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "Remove points")
      createTsChb()
      createTitle(labTitle = "Condition")
      cond <- tcltk::tclVar(">")
      assign("cond", cond, envir = KTSEnv)
      createRb(variable = KTSEnv$cond, 
               dataVector = c(">",">=","<","<=","="))
      createEntry(labTitle = "Scalar", textVariableName = "theScalar")
      createOK(labTitle = "RUN", action = compo2COnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    }
    compo2AOnOk <- function() {
      
      tssel <- tsCheckedTF()
      if (all(tssel == FALSE)) {
        tcltk::tkmessageBox(message = paste("Choose at least", 
                                            "a time series"), 
                            icon = "warning")
      }else {
        
        selOp <- tcltk::tclvalue(KTSEnv$operator)
        
        tsToCompose <- KTSEnv$dSList$TS[which(tssel == TRUE)]
        ntsToCompose <- length(tsToCompose)
        
        selNewName <- getName(selOp,tsToCompose)
        
        if(any(c("Add scalar","Multiply by scalar",
                 "Raise to scalar") == selOp)){
          
          theScalar <- verifyRealEntry(tcltk::tclvalue(KTSEnv$theScalar),
                                       noValid = NA)
          
          if(is.na(theScalar)){
            
            tcltk::tkmessageBox(message = paste("The scalar", 
                                                "must be a single real number"), 
                                icon = "warning")
            
          }else{
            
            for( i in 1:ntsToCompose){
              
              time = get(tsToCompose[i], envir = KTSEnv)$time
              value = get(tsToCompose[i], envir = KTSEnv)$value
              
              if(selOp == "Add scalar"){
                value <- value^theScalar
              }else if(selOp == "Mult scalar"){
                value <- value*theScalar
              }else{
                value <- value^theScalar
              }
              
              aa <- which(is.finite(value) == FALSE & is.na(value) == FALSE)
              if(length(aa) > 0){
                value[aa] <- NA
                tcltk::tkmessageBox(message = paste("Some NaN, Inf or -Inf",
                                                    "were produced and",
                                                    "replaced by NAs"),
                                    icon = "warning") 
              }
              newTS <- data.frame(time = time, value = value)
              assign( selNewName[i],newTS, envir = KTSEnv)
              
              rm(time, value,newTS,aa)
            }
            
            cleanEnvir()
            refreshDataSetsList(outp = FALSE)
            showPANcompo1()
            
          }
          
        }else{
          
          if (selOp == "Add" | selOp == "Multiply") {
            
            if(ntsToCompose < 2){
              tcltk::tkmessageBox(message = paste("Select, at least,",
                                                  "two time series"),
                                  icon = "warning")
              
            }else{
              
              tmComptibility <- areAllTsCompatible(ntsToCompose,tsToCompose)
              
              if (any(tmComptibility[, 1] == FALSE)) {
                tcltk::tkmessageBox(message = paste("The initial date of",
                                                    "all the time series",
                                                    "must be the same"),
                                    icon = "warning")
              } else if (any(tmComptibility[, 2] == FALSE)) {
                tcltk::tkmessageBox(message = paste("The sampling period of",
                                                    "all the time series",
                                                    "must be the same"),
                                    icon = "warning")
              } else if (any(tmComptibility[, 3] == FALSE)) {
                tcltk::tkmessageBox(message = paste("All time series must",
                                                    "have the same length"),
                                    icon = "warning")
                
              }else{
                
                matrixTs <- buildTsMatrix(tsToCompose)
                
                time <- get(tsToCompose[1], envir = KTSEnv)$time
                
                if (selOp == "Add") {
                  value <- rowSums(matrixTs)
                }else{
                  value <- apply(matrixTs, 1, prod)
                }
                
                aa <- which(is.finite(value) == FALSE & is.na(value) == FALSE)
                if(length(aa) > 0){
                  value[aa] <- NA
                  tcltk::tkmessageBox(message = paste("Some NaN, Inf or -Inf",
                                                      "were produced and",
                                                      "replaced by NAs"),
                                      icon = "warning") 
                }
                
                newTS <- data.frame(time = time, value = value)
                assign( selNewName,newTS, envir = KTSEnv)
                
                cleanEnvir()
                refreshDataSetsList(outp = FALSE)
                showPANcompo1()
                
              }
              
            }
            
          }else{
            
            for( i in 1:ntsToCompose){
              
              time <- get(tsToCompose[i], envir = KTSEnv)$time
              value <- get(tsToCompose[i], envir = KTSEnv)$value
              
              if (selOp == "Opposite") {
                value <- -value
              }else if (selOp == "Reciprocal") {
                value <- 1/value
              }else if (selOp == "Ln") {
                value <- log(value)
              }else if (selOp == "Log10") {
                value <- log10(value)
              }else if (selOp == "Exp") {
                value <- exp(value)
              }else if (selOp == "Absolute value") {
                value <- abs(value)
              }else{
                value <- 10^value
              }
              
              aa <- which(is.finite(value) == FALSE & is.na(value) == FALSE)
              if(length(aa) > 0){
                value[aa] <- NA
                tcltk::tkmessageBox(message = paste("Some NaN, Inf or -Inf",
                                                    "were produced and",
                                                    "replaced by NAs"),
                                    icon = "warning") 
              }
              newTS <- data.frame(time = time, value = value)
              assign( selNewName[i],newTS, envir = KTSEnv)
              
              rm(time, value,newTS,aa)
              
            }
            
            cleanEnvir()
            refreshDataSetsList(outp = FALSE)
            showPANcompo1()
            
          }
          
        }
        
      }
      
    }
    compo2BOnOk <- function() {
      
      selTsName1 <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP1), 
                                    noValid = NA)
      selTsName2 <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP2), 
                                    noValid = NA)
      
      tsToCompose <- c(selTsName1,selTsName2)
      tmComptibility <- areAllTsCompatible(2,tsToCompose)
      
      if (any(tmComptibility[, 1] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The initial date of",
                                            "all the time series",
                                            "must be the same"),
                            icon = "warning")
      } else if (any(tmComptibility[, 2] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The sampling period of",
                                            "all the time series",
                                            "must be the same"),
                            icon = "warning")
      } else if (any(tmComptibility[, 3] == FALSE)) {
        tcltk::tkmessageBox(message = paste("All time series must",
                                            "have the same length"),
                            icon = "warning")
        
      }else{
        
        
        selOp <- tcltk::tclvalue(KTSEnv$operator)
        selNewName <- getName(selOp,tsToCompose)
        time = get(selTsName1, envir = KTSEnv)$time
        value1 = get(selTsName1, envir = KTSEnv)$value
        value2 = get(selTsName2, envir = KTSEnv)$value
        
        if(selOp == "Subtract"){
          value <- value1 - value2
        }else if(selOp == "Divide"){
          value <- value1/value2
        }
        
        aa <- which(is.finite(value) == FALSE & is.na(value) == FALSE)
        if(length(aa) > 0){
          value[aa] <- NA
          tcltk::tkmessageBox(message = paste("Some NaN, Inf or -Inf",
                                              "were produced and",
                                              "replaced by NAs"),
                              icon = "warning") 
        }
        
        newTS <- data.frame(time = time, value = value)
        assign( selNewName,newTS, envir = KTSEnv)
        
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANcompo1()
        
      }
      
    }
    compo2COnOk <- function() {
      
      tssel <- tsCheckedTF()
      if (all(tssel == FALSE)) {
        tcltk::tkmessageBox(message = paste("Choose at least", 
                                            "a time series"), 
                            icon = "warning")
      }else {
        
        selOp <- tcltk::tclvalue(KTSEnv$operator)
        
        tsToCompose <- KTSEnv$dSList$TS[which(tssel == TRUE)]
        ntsToCompose <- length(tsToCompose)
        
        selNewName <- getName(selOp,tsToCompose)
        
        theScalar <- verifyRealEntry(tcltk::tclvalue(KTSEnv$theScalar),
                                     noValid = NA)
        
        if(is.na(theScalar)){
          
          tcltk::tkmessageBox(message = paste("The scalar", 
                                              "must be a single real number"), 
                              icon = "warning")
          
        }else{
          
          condi <- tcltk::tclvalue(KTSEnv$cond)
          
          for( i in 1:ntsToCompose){
            
            time = get(tsToCompose[i], envir = KTSEnv)$time
            value = get(tsToCompose[i], envir = KTSEnv)$value
            
            if(condi == ">"){
              bb <- which(value > theScalar)
            }else if(condi == ">="){
              bb <- which(value >= theScalar) 
            }else if(condi == "<"){
              bb <- which(value < theScalar) 
            }else if(condi == "<="){
              bb <- which(value <= theScalar)
            }else if(condi == "="){
              bb <- which(value == theScalar) 
            }
            
            
            if(length(bb) == 0){
              tcltk::tkmessageBox(message = paste("No points to remove in", 
                                                  tsToCompose[i]), 
                                  icon = "warning")
              
            }else{
              
              value[bb] <- NA 
              newTS <- data.frame(time = time, value = value)
              assign( selNewName[i],newTS, envir = KTSEnv)
              
              rm(time,value,newTS,bb)
            }
          }
          
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANcompo1()
          
        }
        
      }
      
    }
    
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANcompo1", 
                 envirName = environment(showPANcompo1))
 
  }

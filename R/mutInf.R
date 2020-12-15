mutInf <-
function() {
    
    getRollMI <- function(xDisc,lInterv,timeTS){
      
      TS1value <- xDisc[[1]]
      TS2value <- xDisc[[2]]
      
      if(lInterv%%2 == 1){
        
        lInterv <- lInterv + 1
        tcltk::tkmessageBox(message = paste("The sliding window must be even",
                                            "so, it was increased by one"), 
                            icon = "warning")
        
      }
      
      getMutInf1 <- function(i, TS1value, 
                             TS2value = TS1value,
                             lInterv = NULL){
        
        interv <- i:(i+lInterv)
        x <- TS1value[interv]
        y <- TS2value[interv]
        
        if(any(is.na(x)) | any(is.na(y))){
          mm <- NA
        }else{
          mm <- try(infotheo::mutinformation(X = x, Y = y, 
                                             method = "emp"), 
                    silent = TRUE)
          
          if(class(mm) == "try-error"){mm = NA}
          
        }
        
        mm
        
      }
      
      LL <- min(length(TS1value),length(TS2value))
      
      rollMutInf12 <- apply(as.matrix(1:(LL - lInterv)),1, 
                            FUN = getMutInf1,TS1value = TS1value,
                            TS2value = TS2value,lInterv = lInterv)
      
      rollMutInf12 <- c(rep(NA,0.5*lInterv),rollMutInf12,rep(NA,0.5*lInterv))
      

      rollMutInf11 <- apply(as.matrix(1:(LL - lInterv)),1, 
                            FUN = getMutInf1,TS1value = TS1value,
                            TS2value = TS1value,lInterv = lInterv)
      
      rollMutInf11 <- c(rep(NA,0.5*lInterv),rollMutInf11,rep(NA,0.5*lInterv))
      
      
      rollMutInf22 <- apply(as.matrix(1:(LL - lInterv)),1, 
                            FUN = getMutInf1,TS1value = TS2value,
                            TS2value = TS2value,lInterv = lInterv)
      
      rollMutInf22 <- c(rep(NA,0.5*lInterv),rollMutInf22,rep(NA,0.5*lInterv))
      
      
      assign(paste0(KTSEnv$selTsName[1],"_",KTSEnv$selTsName[2],
                    "MI",lInterv+1),
             data.frame(time = timeTS,value = rollMutInf12),
             envir = KTSEnv)
      assign(paste0(KTSEnv$selTsName[1],"ME",lInterv+1),
             data.frame(time = timeTS,value = rollMutInf11),
             envir = KTSEnv)
      assign(paste0(KTSEnv$selTsName[2],"ME",lInterv+1),
             data.frame(time = timeTS,value = rollMutInf22),
             envir = KTSEnv)
      
      
    }
    
    discretizeVars <- function(X, disc = NULL, nbins = NULL, X1Names = NULL){

      discrVars <- vector("list",length(X))
      
      for(nV in 1:length(X)){
        
        interRes <- infotheo::discretize(X[[nV]], 
                                         disc = disc[nV], 
                                         nbins = nbins[nV])           
        interRes <- unclass(interRes)$X
        discrVars[[nV]] <- interRes
        rm(interRes)
        
      }
      
      names(discrVars) <- X1Names
      
      discrVars
      
    }
    
    getBinLims <- function(X,XDisc){
      
      getDiscrtLimits <- function(X0,X,rf){
        
        limiInf <- NULL
        limiSup <- NULL
        
        for(le in rf){
          
          elesInBin <- X0[which(X == le)]
          
          if(length(elesInBin)==0){
            
            limiInf <- c(limiInf,NA)
            
            limiSup <- c(limiSup,NA)
            
          }else{
            
            limiInf <- c(limiInf,
                         min(elesInBin, na.rm = TRUE))
            
            limiSup <- c(limiSup,
                         max(elesInBin, na.rm = TRUE))
            
          }
          
          rm(elesInBin)
          
        }
        
        limis <- cbind(limiInf,limiSup)
        
      }
      
      limsBins <- vector("list", length(X))
      
      for(nV in 1:length(X)){
        
        xNv0 <- X[[nV]]
        xNvD <- XDisc[[nV]]
        # xNv <- unclass(xNvD)$X
        rf <- sort(unique(xNvD))
        
        limis <- getDiscrtLimits(xNv0,xNvD,rf)
        limis1 <- c(limis[1,1],limis[,2])
        names(limis1) <- NULL
        
        limsBins[[nV]] <- limis1
        
        rm(xNv0,xNvD,limis,rf)
        
      }
      
      limsBins
    }
    
    tableDisvVar <- function(X,XDisc){
      
      tablas <- vector("list", 0.5*(length(X)^2 - length(X)))
      cont <- 0
      
      for(i in 1:length(X)){
        
        for(j in 1:length(X)){
          
          if( i > j){
            
            cont <- cont + 1
            tablas[[cont]] <- table(XDisc[[i]], XDisc[[j]])
            
          }
          
        }
        
      }
      
      tablas
      
    }
    
    RPKTS2OnOk <- function() {
      
      slidWin <- verifyIntEntry(tcltk::tclvalue(KTSEnv$winW), noValid = NA)
      Meth1 <- verifyCharEntry(tcltk::tclvalue(KTSEnv$Me1), noValid = NA)
      Meth2 <- verifyCharEntry(tcltk::tclvalue(KTSEnv$Me2), noValid = NA)
      Nbins1 <- verifyIntEntry(tcltk::tclvalue(KTSEnv$NB1), noValid = NA)
      Nbins2 <- verifyIntEntry(tcltk::tclvalue(KTSEnv$NB2), noValid = NA)
      methodList <- c("equal frequency", "equal width", "global width")
      
      if( is.na(Meth1) | is.na(Meth1)){
        
        tcltk::tkmessageBox(message = paste("The methods available are",
                                            "equal frequency, equal width,",
                                            "and global width"), 
                            icon = "warning")
        
      }else if(all(compareVecVec(methodList,c(Meth1,Meth2)) == FALSE)){
        
        tcltk::tkmessageBox(message = paste("The methods available are",
                                            "equal frequency, equal width,",
                                            "and global width"), 
                            icon = "warning")
        
      }else if(is.na(Nbins1) | is.na(Nbins2)){
        
        tcltk::tkmessageBox(message = "Enter an integer number of bins", 
                            icon = "warning")
        
      }else{
        
        X <- list(get(KTSEnv$selTsName[1], envir = KTSEnv)$value,
                  get(KTSEnv$selTsName[2], envir = KTSEnv)$value)
        
        methodList1 <- c("equalfreq","equalwidth","globalequalwidth")
        disc <- c(methodList1[which(methodList == Meth1)],
                  methodList1[which(methodList == Meth2)])
        
        discreteVar1Var2 <- discretizeVars(X = X, disc = disc, 
                                           nbins = c(Nbins1,Nbins2),
                                           X1Names = KTSEnv$selTsName)
        
        binLims <- getBinLims(X = X, XDisc = discreteVar1Var2)
        tableDisc <- tableDisvVar(X = X, XDisc = discreteVar1Var2)
        
        V1V1 <- infotheo::mutinformation(X = discreteVar1Var2[[1]], 
                                         Y = discreteVar1Var2[[1]], 
                                         method = "emp")
        V2V2 <- infotheo::mutinformation(X = discreteVar1Var2[[2]], 
                                         Y = discreteVar1Var2[[2]], 
                                         method = "emp")
        V1V2 <- infotheo::mutinformation(X = discreteVar1Var2[[1]], 
                                         Y = discreteVar1Var2[[2]], 
                                         method = "emp")
        
        
        txt <- c(paste(" Time Series:", KTSEnv$selTsName[1],",",
                       KTSEnv$selTsName[2]), 
                 paste(KTSEnv$selTsName[1], "marginal entropy:", 
                       round(V1V1,4)),
                 paste(KTSEnv$selTsName[2], "marginal entropy:", 
                       round(V2V2,4)), 
                 paste(KTSEnv$selTsName[1], "mutual information:", 
                       round(V1V2,4)), 
                 paste(KTSEnv$selTsName[1],"discretization:", 
                       Meth1,"method,",Nbins1, "bins"), 
                 paste(" Bins breaks:",
                       paste(round(binLims[[1]],2), collapse = ",")),
                 paste(KTSEnv$selTsName[2],"discretization:", 
                       Meth2,"method,",Nbins2, "bins"),
                 paste(" Bins breaks:",
                       paste(round(binLims[[2]],2), collapse = ",")),
                 paste("Coincidences"))
                
        tD <- as.data.frame.matrix(tableDisc[[1]])
        txt1 <- utils::capture.output(print.data.frame(tD))
        
        writeMethodTitle("MUTUAL INFORMATION")
        tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                        paste(txt, collapse = "\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                        paste(txt1, collapse = "\n"))
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
        endingLines()
        
        if(is.na(slidWin)==FALSE){
          
          timeTS <- get(KTSEnv$selTsName[1], envir = KTSEnv)$time
          getRollMI(xDisc = discreteVar1Var2,lInterv = slidWin,
                    timeTS = timeTS)
          
        }
        
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        showPANRPKTS1()

      }
      
    }

    showPANRPKTS2 <- function() {
      
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "MUTUAL INFORMATION")
      createTitle(labTitle = KTSEnv$selTsName[1])
      createEntry(labTitle = "Method", textVariableName = "Me1", 
                  defaultVal = "equal frequency")
      createEntry(labTitle = "Number of bins", textVariableName = "NB1")
      createTitle(labTitle = KTSEnv$selTsName[2])
      createEntry(labTitle = "Method", textVariableName = "Me2", 
                  defaultVal = "equal frequency")
      createEntry(labTitle = "Number of bins", textVariableName = "NB2")
      
      createTitle(labTitle = "")
      createEntry(labTitle = "Use sliding window", 
                  textVariableName = "winW", defaultVal = "No")
      
      
      
      createNote(labTitle = "Methods:equal frequency, equal width, global width")
      createOK(labTitle = "RUN", action = RPKTS2OnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    
    RPKTS1OnOk <- function() {
      
      tssel <- tsCheckedTF()
      checkedTS <- which(tssel == TRUE)
      lCheckedTS <- length(checkedTS)
      
      if (lCheckedTS != 2) {
        
        tcltk::tkmessageBox(message = paste("Choose two time series"), 
                            icon = "warning")
        
      }else{
        
        selTsName <- KTSEnv$dSList$TS[checkedTS]
        selTs1 <- get(selTsName[1], envir = KTSEnv)
        selTs2 <- get(selTsName[2], envir = KTSEnv)
        timecompatibility <- are2TsTimeCompatible(selTs1, selTs2)
        
        if (timecompatibility[2] == FALSE) {
          
          tcltk::tkmessageBox(message = paste("Both time series must have",
                                              "the same sampling period"), 
                              icon = "warning")
          
        }else{
          
          if (timecompatibility[1] == FALSE) {
            lagTS <- diff(as.numeric(selTs1$time[1], selTs2$time[1]))
            tcltk::tkmessageBox(message = paste("There is a lag of ", lagTS, 
                                                "seconds between the",
                                                "time series",
                                                ". Take it into consideration"), 
                                icon = "warning")
          }
          
          KTSEnv$selTsName <- selTsName
          showPANRPKTS2()
          
        }
        
      }
      
    }
    
    showPANRPKTS1 <- function() {
      
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "MUTUAL INFORMATION")
      createTsChb()
      createOK(labTitle = "NEXT", action = RPKTS1OnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANRPKTS1", 
                 envirName = environment(showPANRPKTS1))
    
  }

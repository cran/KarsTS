normalityKTS <-
function() {
  rand1OnOk <- function() {
    
    tssel <- tsCheckedTF()
    
    if (all(tssel == FALSE)) {
      
      tcltk::tkmessageBox(message = "Choose at least a time series", 
                          icon = "warning")
      
    }else{
      
      testNormUni <- function(timSer, timSerName) {
        
        tcltk::tkinsert(KTSEnv$txtWidget, "end", timSerName)
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
        
        for (j in c("CVM", "Lillie", "AD")) {
          
          result <- try(MVN::mvn(timSer, univariateTest = j, desc = FALSE), 
                        silent = TRUE)
          
          if (class(result) != "try-error") {
            
            txtNorm1 <- c(paste(result$univariateNormality[1,1],
                                "p_value:", 
                                round(result$univariateNormality[1,4],2)))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", txtNorm1)
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
            rm(txtNorm1)
            
          }
          
          rm(result)
          
        }
        
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
        
      }
      
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      tsToTestNames <- KTSEnv$dSList$TS[which(tssel == TRUE)]
      nTS <- length(tsToTestNames)
      writeMethodTitle("NORMALITY TESTS")
      tcltk::tkinsert(KTSEnv$txtWidget, "end", "Univariate normality tests")
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
      
      for (i in tsToTestNames) {
        
        testNormUni(get(i, envir = KTSEnv)$value, i)
        
      }
      
      if (nTS > 1) {
        
        tmComptibility <- matrix(rep(FALSE, 3 * nTS), nTS, 3)
        
        for (i in 2:nTS) {
          tmComptibility[i, ] <- are2TsTimeCompatible(get(tsToTestNames[1],
                                                             envir = KTSEnv), 
                                                         get(tsToTestNames[i], 
                                                             envir = KTSEnv))
        }
        
        tmComptibility <- tmComptibility[-1, ]
        
        if (all(tmComptibility == TRUE)) {
          
          tsToTest <- get(tsToTestNames[1], envir = KTSEnv)$value
          
          for (i in tsToTestNames[2:nTS]) {
            
            tsToTest <- cbind(tsToTest, get(i, envir = KTSEnv)$value)
            
          }
          
          colnames(tsToTest) <- tsToTestNames
          
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          "Multivariate normality tests")
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                          paste(tsToTestNames, collapse = ","))
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
          
          # result <- try(MVN::hzTest(tsToTest, cov = TRUE, qqplot = FALSE), 
          #               silent = TRUE)
          result <- try(MVN::mvn(tsToTest, mvnTest = "hz"), 
                        silent = TRUE)
          
          if (class(result) == "try-error") {
            
            txtNorm1 <- "Henze-Zirkler's Normality Test: failed"
            
          } else {
            
            txtNorm1 <- c(paste("Henze-Zirkler's Normality Test", 
                                "p_value:", 
                                round(result$multivariateNormality[1,3], 2)))
            
          }
          
          tcltk::tkinsert(KTSEnv$txtWidget, "end", txtNorm1)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          rm(result, txtNorm1)
          
          result <- try(MVN::mvn(tsToTest, mvnTest = "mardia",
                                 covariance = TRUE), 
                        silent = TRUE)
          
          if (class(result) == "try-error") {
            
            txtNorm1 <- "Mardia's Normality Test: failed"
            
          } else {
            
            aabb <- as.numeric(as.character(result$multivariateNormality[,3]))
            txtNorm1 <- c(paste("Mardia's Normality Test", 
                                "skew/kurt p_value:", 
                                round(aabb[1], 2), 
                                round(aabb[2], 2)))
          }
          
          tcltk::tkinsert(KTSEnv$txtWidget, "end", txtNorm1)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          rm(result, txtNorm1)
          
        }
        
      }
      
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
      endingLines()
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      showPANrand1()
      
    }
    
  }
  
  showPANrand1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "NORMALITY TESTS")
    createTsChb()
    createOK(labTitle = "RUN", action = rand1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANrand1", 
               envirName = environment(showPANrand1))
}

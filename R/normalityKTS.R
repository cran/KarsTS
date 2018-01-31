normalityKTS <-
function() {
  rand1OnOk <- function() {
    tssel <- tsCheckedTF()
    if (all(tssel == FALSE)) {
      tcltk::tkmessageBox(message = "Choose at least a time series", 
                          icon = "warning")
    } else {
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      tsToTestNames <- KTSEnv$dSList$TS[which(tssel == TRUE)]
      nTS <- length(tsToTestNames)
      writeMethodTitle("NORMALITY TESTS")
      tcltk::tkinsert(KTSEnv$txtWidget, "end", "Univariate normality tests")
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
      testNormUni <- function(timSer, timSerName) {
        tcltk::tkinsert(KTSEnv$txtWidget, "end", timSerName)
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
        for (j in c("CVM", "Lillie", "AD")) {
          result <- try(MVN::uniNorm(timSer, type = j, desc = FALSE), 
                        silent = TRUE)
          if (class(result) != "try-error") {
            result <- unclass(result$Normality)
            txtNorm1 <- c(paste(result$method, "p_value:", 
                                round(result$p.value,2)))
            tcltk::tkinsert(KTSEnv$txtWidget, "end", txtNorm1)
            tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
            rm(txtNorm1)
          }
          rm(result)
        }
        tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
      }
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
          
          
          result <- try(MVN::hzTest(tsToTest, cov = TRUE, qqplot = FALSE), 
                        silent = TRUE)
          if (class(result) == "try-error") {
            txtNorm1 <- "Henze-Zirkler's Normality Test: failed"
          } else {
            txtNorm1 <- c(paste("Henze-Zirkler's Normality Test", "p_value:", 
                                round(attr(result, "p.value"), 2)))
          }
          tcltk::tkinsert(KTSEnv$txtWidget, "end", txtNorm1)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
          rm(result, txtNorm1)
          
          result <- try(MVN::mardiaTest(tsToTest, cov = TRUE, qqplot = FALSE), 
                        silent = TRUE)
          if (class(result) == "try-error") {
            txtNorm1 <- "Mardia's Normality Test: failed"
          } else {
            txtNorm1 <- c(paste("Mardia's Normality Test", 
                                "skew/kurt p_value:", 
                                round(attr(result, "p.value.skew"), 2), 
                                round(attr(result, "p.value.kurt"), 2)))
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

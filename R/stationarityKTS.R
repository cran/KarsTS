stationarityKTS <-
function() {
  statioOnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                 noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    } else {
      selTs <- get(selTsName, envir = KTSEnv)
      if (any(is.na(selTs$value))) {
        tcltk::tkmessageBox(message = paste("The time series had",
                                            "NAs and they were",
                                            "ignored to run the tests.",
                                            "Take it into consideration"), 
                            icon = "warning")
      }
      value <- selTs$value[which(is.finite(selTs$value))]
      resultPP <- unclass(stats::PP.test(value))
      resultbox <- unclass(stats::Box.test(value, type = "Ljung-Box"))
      resultadf <- unclass(tseries::adf.test(value, 
                                             alternative = "stationary"))
      resultkpss <- unclass(tseries::kpss.test(value))
      txt1 <- paste("STATIONARITY TESTS FOR:", selTsName, "\n")
      txt2 <- c(resultPP[[4]], paste("P-value:", round(resultPP[[3]], 2)), 
                "H0: The detrended time series is not stationary", 
                paste("Truncation lag parameter:", resultPP[[2]]), "\n")
      txt3 <- c(resultbox[[4]], paste("P-value", round(resultbox[[3]]), 2), 
                "H0: There are no self-correlations", 
                paste("Truncation lag parameter", resultbox[[2]]), "\n")
      txt4 <- c(resultadf[[5]], paste("P-value", round(resultadf[[4]], 2)), 
                "H0: The detrended time series is not stationary", 
                paste0("H1:", resultadf[[3]]), 
                paste("Lag order", resultadf[[2]]), "\n")
      txt5 <- c(resultkpss[[4]], paste("P-value", round(resultkpss[[3]],2)), 
                "H0: The time series is white noise", 
                paste("Truncation lag parameter", resultkpss[[2]]))
      txt <- c(txt2, txt3, txt4, txt5)
      writeMethodTitle(txt1)
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste(txt, collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
      endingLines()
      cleanEnvir()
      showPANstatio()
    }
  }
  showPANstatio <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "STATIONARITY TESTS")
    createTsRb()
    createOK(labTitle = "RUN", action = statioOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANstatio", 
               envirName = environment(showPANstatio))
}

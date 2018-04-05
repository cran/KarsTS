statisticsKTS <-
function() {
  statOnOk <- function() {
    refreshDataSetsList(outp = FALSE)
    tssel <- tsCheckedTF()
    if (all(tssel == FALSE)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    } else {
      tsToAnalize <- KTSEnv$dSList$TS[which(tssel == TRUE)]
      getStatisticsEach <- function(X) {
        res <- getStatistics(get(X, envir = KTSEnv))
      }
      result <- myApplyVector(FUN = getStatisticsEach, 
                              dataVector = tsToAnalize, 
                              out.ncols = 7)
      result <- as.data.frame(t(result))
      rownames(result) <- c("Min.", "1st.Qu.", "Median", 
                            "Mean", "3rd.Qu.", 
                            "Max.", "St.Dev")
      colnames(result) <- tsToAnalize
      txt <- utils::capture.output(print.data.frame(result))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", "STATISTICS")
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", date())
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste(txt, collapse = "\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
      endingLines()
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      showPANstat()
    }
  }
  showPANstat <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "STATISTICS")
    createTsChb()
    createOK(labTitle = "RUN", action = statOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANstat", envirName = environment(showPANstat))
}

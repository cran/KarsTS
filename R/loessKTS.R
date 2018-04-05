loessKTS <-
function() {
  loess1OnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP))
    span <- verifyRealEntry(tcltk::tclvalue(KTSEnv$alfa), noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    } else if (is.na(span)) {
      tcltk::tkmessageBox(message = "Give a value for alpha", 
                          icon = "warning")
    } else {
      assignMultiple(c("selTsName", "span"), list(selTsName, span), 
                     envir = KTSEnv)
      showPANloess2()
    }
  }
  loess2OnOk <- function() {
    refreshDataSetsList(outp = FALSE)
    allowExtrap <- tcltk::tclvalue(KTSEnv$extrap)
    degreePol <- verifyIntEntry(tcltk::tclvalue(KTSEnv$degreepol), 
                                noValid = NA)
    tssel <- tsCheckedTF()
    predictorTS <- KTSEnv$dSList$TS[which(tssel == TRUE)]
    nPredictorTS <- length(predictorTS)
    selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
    if (nPredictorTS == 0) {
      tmComptibility <- matrix(rep(TRUE, 3), 1, 3)
    } else {
      tmComptibility <- matrix(rep(FALSE, 3 * nPredictorTS), 
                               nPredictorTS, 3)
      for (i in 1:nPredictorTS) {
        tmComptibility[i, ] <- are2TsTimeCompatible(selTs, 
                                                    get(predictorTS[i], 
                                                        envir = KTSEnv))
      }
    }
    if (any(tmComptibility[, 1] == FALSE)) {
      tcltk::tkmessageBox(message = paste("The initial date of all the",
                                          "time series must be the same"), 
                          icon = "warning")
    } else if (any(tmComptibility[, 2] == FALSE)) {
      tcltk::tkmessageBox(message = paste("The sampling period of all",
                                          "the time series must",
                                          "be the same"), 
                          icon = "warning")
    } else if (any(tmComptibility[, 3] == FALSE)) {
      tcltk::tkmessageBox(message = paste("All time series must",
                                          "have the same length"), 
                          icon = "warning")
    } else if (nPredictorTS > 4) {
      tcltk::tkmessageBox(message = paste("Only up to four",
                                          "predictor time series can be used"), 
                          icon = "warning")
    } else {
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      lSeriesel <- nrow(selTs)
      if (any(predictorTS == KTSEnv$selTsName)) {
        predictorTS[which(predictorTS == KTSEnv$selTsName)] <- "timeAsPred"
      }
      timeAsPred <- data.frame(time = 1:lSeriesel, value = 1:lSeriesel)
      if (is.na(degreePol)) {
        degreePol <- 2
      }
      if (allowExtrap == "Yes") {
        superf <- "direct"
      } else {
        superf <- "interpolate"
      }
      if (nPredictorTS == 0) {
        dataReady <- data.frame(time = timeAsPred$time, value = selTs$value)
        sp <- superf
        lssR <- stats::loess(dataReady$value ~ dataReady$time, 
                             data = dataReady, 
                             span = KTSEnv$span, degree = degreePol, 
                             na.action = stats::na.exclude, 
                             control = stats::loess.control(surface = sp))
      } else {
        dataReady <- selTs$value
        for (j in 1:nPredictorTS) {
          dataReady <- cbind(dataReady, 
                             get(predictorTS[j], envir = KTSEnv)$value)
        }
        dataReady <- as.data.frame(dataReady)
        namesDR <- c("Y", "X1", "X2", "X3", "X4")[1:(nPredictorTS + 1)]
        colnames(dataReady) <- namesDR
        lssR <- stats::loess(Y ~ ., data = dataReady, span = KTSEnv$span, 
                             degree = degreePol, 
                             na.action = stats::na.exclude, 
                             control = stats::loess.control(surface = superf))
      }
      complDR <- which(stats::complete.cases(dataReady) == TRUE)
      if (length(complDR) == lSeriesel) {
        smoothedTS <- data.frame(time = selTs$time, value = lssR$fitted)
      } else {
        smoothedTS <- selTs
        smoothedTS$value[complDR] <- lssR$fitted
      }
      asCharSpan <- as.character(KTSEnv$span)
      asCharSpan1 <- strsplit(asCharSpan, split = NULL)
      if (any(asCharSpan1[[1]] == "-")) {
        asCharSpan1[[1]][which(asCharSpan1[[1]] == "-")] <- "m"
        asCharSpan <- paste0(asCharSpan1[[1]], collapse = "")
      }
      assign(paste0(KTSEnv$selTsName, "_lss", asCharSpan), 
             smoothedTS, envir = KTSEnv)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      showPANloess1()
    }
  }
  showPANloess1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "LOESS SMOOTHING")
    createTsRb(labTitle = "Time series to smooth")
    createEntry(labTitle = "Alpha", textVariableName = "alfa")
    createOK(labTitle = "NEXT", action = loess1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  showPANloess2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "LOESS SMOOTHING")
    createTsChb(labTitle = "Predictor time series")
    createEntry(labTitle = "Polynomials degree", 
                textVariableName = "degreepol", 
                defaultVal = "2")
    createTitle(labTitle = "Allow extrapolation")
    assign("extrap", tcltk::tclVar("No"), envir = KTSEnv)
    createRb(variable = KTSEnv$extrap, dataVector = c("Yes", "No"))
    createOK(labTitle = "RUN", action = loess2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANloess1", 
               envirName = environment(showPANloess1))
}

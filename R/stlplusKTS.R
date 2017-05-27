stlplusKTS <-
function() {
  showPANstlKTS1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "LOESS SEASONAL DECOMPOSITION")
    createTsRb()
    createOK(labTitle = "NEXT", action = stlKTS1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  stlKTS1OnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                 noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else {
      assign("selTsName", selTsName, envir = KTSEnv)
      showPANstlKTS2()
    }
  }
  showPANstlKTS2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "LOESS SEASONAL DECOMPOSITION")
    createTitle(labTitle = "Main features (in lags)")
    createEntry(labTitle = "Seasonality", textVariableName = "seasonality")
    createEntry(labTitle = "Seasonal window", textVariableName = "seawin")
    createEntry(labTitle = "Trend window", textVariableName = "trendwin")
    createTitle(labTitle = "Type of decomposition")
    assign("type", tcltk::tclVar("Additive"), envir = KTSEnv)
    createRb(variable = KTSEnv$type, 
             dataVector = c("Additive", "Multiplicative"))
    createTitle(labTitle = "Trend decomposition(optional)")
    createEntry(labTitle = "Window,name", textVariableName = "win1")
    createEntry(labTitle = "Window,name", textVariableName = "win2")
    createEntry(labTitle = "Window,name", textVariableName = "win3")
    createEntry(labTitle = "Window,name", textVariableName = "win4")
    createEntry(labTitle = "Window,name", textVariableName = "win5")
    createEntry(labTitle = "Window,name", textVariableName = "win6")
    createEntry(labTitle = "Window,name", textVariableName = "win7")
    createOK(labTitle = "RUN", action = stlKTS2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  stlKTS2OnOk <- function() {
    verifyWinName <- function(X, noValid = NA) {
      result <- list(win = noValid, name = noValid)
      if (X == "") {
        result <- list(win = NA, name = NA)
      } else if (is.character(X)) {
        numberOfCommas <- length(which(strsplit(X, 
                                                split = NULL)[[1]] == ","))
        if (numberOfCommas == 1) {
          CommaPosition <- which(strsplit(X, split = NULL)[[1]] == ",")
          if (CommaPosition != 1 & CommaPosition != nchar(X)) {
            splitted <- separateEntry(X, class1 = verifyIntEntry, 
                                      class2 = verifyCharEntry, 
                                      noValid = "noValid")
            result <- list(win = verifyIntEntry(splitted[1], 
                                                noValid = "noValid"), 
                           name = verifyCharEntry(splitted[2], 
                                                  noValid = "noValid"))
          }
        }
      }
      result
    }
    transfIfMult <- function(selTs, addOrMult) {
      if (addOrMult == "Multiplicative") {
        if (any(selTs$value <= 0)) {
          assign("addOrMult", "Additive", envir = KTSEnv)
          tcltk::tkmessageBox(message = paste("The multiplicative",
                                              "decomposition requires a",
                                              "strictly positive time",
                                              "series, so the type was",
                                              "changed to additive"), 
                              icon = "warning")
        } else {
          selTs$value <- log(selTs$value)
          selTs$value[which(is.finite(selTs$value) == FALSE)] <- NA
        }
      }
      selTs
    }
    addOrMult <- tcltk::tclvalue(KTSEnv$type)
    seasonality1 <- verifyIntEntry(tcltk::tclvalue(KTSEnv$seasonality), 
                                   noValid = NA)
    seaWindow <- verifyIntEntry(tcltk::tclvalue(KTSEnv$seawin), noValid = NA)
    trendWindow <- verifyIntEntry(tcltk::tclvalue(KTSEnv$trendwin), 
                                  noValid = NA)
    winName1 <- verifyWinName(tcltk::tclvalue(KTSEnv$win1), 
                              noValid = "noValid")
    winName2 <- verifyWinName(tcltk::tclvalue(KTSEnv$win2), 
                              noValid = "noValid")
    winName3 <- verifyWinName(tcltk::tclvalue(KTSEnv$win3), 
                              noValid = "noValid")
    winName4 <- verifyWinName(tcltk::tclvalue(KTSEnv$win4), 
                              noValid = "noValid")
    winName5 <- verifyWinName(tcltk::tclvalue(KTSEnv$win5), 
                              noValid = "noValid")
    winName6 <- verifyWinName(tcltk::tclvalue(KTSEnv$win6), 
                              noValid = "noValid")
    winName7 <- verifyWinName(tcltk::tclvalue(KTSEnv$win7), 
                              noValid = "noValid")
    allWinNames <- list(winName1$win, winName2$win, 
                        winName3$win, winName4$win, 
                        winName5$win, winName6$win, winName7$win, 
                        winName1$name, winName2$name, 
                        winName3$name, winName4$name, winName5$name, 
                        winName6$name, winName7$name)
    allWins <- list(winName1$win, winName2$win, winName3$win, 
                    winName4$win, winName5$win, 
                    winName6$win, winName7$win)
    allNames <- list(winName1$name, winName2$name, winName3$name,
                     winName4$name, 
                     winName5$name, winName6$name, winName7$name)
    validWinsInd <- setdiff(1:7, 
                            which(is.na(allWins) | allWins == "noValid"))
    validNamesInd <- setdiff(1:7, 
                             which(is.na(allNames) | allNames == "noValid"))
    validWinNamesInd <- intersect(validWinsInd, validNamesInd)
    if (is.na(seasonality1)) {
      tcltk::tkmessageBox(message = paste("Enter the seasonality (in lags)"),
                          icon = "warning")
    } else if (seasonality1 < 4) {
      tcltk::tkmessageBox(message = paste("Each period must have at least",
                                          "four measurements,that is,",
                                          "the seasonality has to be",
                                          "four or greater"), 
                          icon = "warning")
    } else if (is.na(seaWindow)) {
      tcltk::tkmessageBox(message = paste("Enter the seasonal",
                                          "window (in lags)"), 
                          icon = "warning")
    } else if (is.na(trendWindow)) {
      tcltk::tkmessageBox(message = paste("Enter the trend",
                                          "window (in lags)"), 
                          icon = "warning")
    } else if (any(allWinNames == "noValid")) {
      tcltk::tkmessageBox(message = paste("Some optional smoothing",
                                          "parameters have not an",
                                          "appropiate format. You",
                                          "must enter an integer, a",
                                          "comma and a name, without",
                                          "any spaces or leave the",
                                          "entry completely empty"), 
                          icon = "warning")
    } else if (all(is.na(allWinNames))) {
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
      selTs <- transfIfMult(selTs, addOrMult)
      loessResult <- stlplus::stlplus(selTs$value, t = selTs$time, 
                                      n.p = seasonality1, 
                                      s.window = seaWindow, s.degree = 2, 
                                      t.window = trendWindow, t.degree = 2, 
                                      inner = 2, outer = 10, fc.degree = NULL, 
                                      fc.window = NULL, fc.name = NULL)
      if (addOrMult == "Multiplicative") {
        loessResult$data[, 2:4] <- exp(loessResult$data[, 2:4])
      }
      assign(paste0(KTSEnv$selTsName, "Sea"), 
             data.frame(time = selTs$time, value = loessResult$data$seasonal),
             envir = KTSEnv)
      assign(paste0(KTSEnv$selTsName, "Tr"), 
             data.frame(time = selTs$time, value = loessResult$data$trend),
             envir = KTSEnv)
      assign(paste0(KTSEnv$selTsName, "Rem"), 
             data.frame(time = selTs$time, value = loessResult$data$remainder),
             envir = KTSEnv)
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      showPANstlKTS1()
    } else {
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
      selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
      selTs <- transfIfMult(selTs, addOrMult)
      FCW <- unlist(allWins[validWinNamesInd])
      FClab <- unlist(allNames[validWinNamesInd])
      FCD <- rep(2, length(FCW))
      loessResult <- stlplus::stlplus(selTs$value, t = selTs$time, 
                                      n.p = seasonality1, 
                                      s.window = seaWindow, s.degree = 2, 
                                      t.window = trendWindow, t.degree = 2, 
                                      inner = 2, outer = 10, fc.degree = FCD, 
                                      fc.window = FCW, fc.name = FClab)
      numberCompos <- 2 + length(validWinNamesInd)
      if (addOrMult == "Multiplicative") {
        loessResult$data[, 2] <- exp(loessResult$data[, 2])
        loessResult$fc <- exp(loessResult$fc)
      }
      assign(paste0(KTSEnv$selTsName, "Sea"), 
             data.frame(time = selTs$time, value = loessResult$data$seasonal), 
             envir = KTSEnv)
      assign(paste0(KTSEnv$selTsName, "Rem"), 
             data.frame(time = selTs$time, value = loessResult$fc$remainder), 
             envir = KTSEnv)
      for (j in 1:(ncol(loessResult$fc) - 1)) {
        assign(paste0(KTSEnv$selTsName, FClab[j]), 
               data.frame(time = selTs$time, value = loessResult$fc[, j]), 
               envir = KTSEnv)
      }
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
      showPANstlKTS1()
    }
  }
  tcltk::tkmessageBox(message = paste("If your time series is huge,",
                                      "the process may take a some minutes.",
                                      "Save your objects and results",
                                      "before running this function"), 
                      icon = "warning")
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANstlKTS1", 
               envirName = environment(showPANstlKTS1))
}

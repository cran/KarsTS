linearityKTS <-
function() {
    sdtMessage2sA = function(data.statistic, surrogates.statistics, K) {
        sorted.values = sort(c(data.statistic, surrogates.statistics))
        min.st = sorted.values[K]
        len = length(sorted.values)
        max.st = sorted.values[[len - K + 1]]
        txt0 <- " Null Hypothesis: Data comes from a linear stochastic process"
        if (data.statistic <= min.st) {
            txt1 <- c(" Reject Null hypothesis:", 
                      paste(" Original data's stat is",
                            "significant smaller than surrogates' stats"))
        } else {
            if (data.statistic >= max.st) {
                txt1 <- paste(" Reject Null hypothesis:", 
                          " Original data's stat is significantly",
                          "larger than surrogates test stats")
            } else {
                txt1 <- " Accept Null hypothesis"
            }
        }
        txtRes <- c(txt0, txt1)
    }
    
    trickFun <- function(dataToTest,selSLev,lagDelay){
      nonlinearTseries::surrogateTest(time.series = dataToTest, 
                                      significance = selSLev, 
                                      one.sided = FALSE, K = 20, 
                                      FUN = nonlinearTseries::timeAsymmetry2, 
                                      tau = lagDelay, 
                                      verbose = FALSE, 
                                      do.plot = FALSE)
    }
    
    lintestsOnOk <- function() {
        selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                     noValid = NA)
        lagDelay <- verifyIntEntry(tcltk::tclvalue(KTSEnv$delay), 
                                   noValid = NA)
        selSLev <- verifyRealEntry(tcltk::tclvalue(KTSEnv$signifLev), 
                                        noValid = NA)
        if (is.na(selTsName)) {
            tcltk::tkmessageBox(message = "Choose a time series", 
                                icon = "warning")
        } else if (is.na(lagDelay)) {
            tcltk::tkmessageBox(message = paste("Choose the delay for",
                                                "the surrogates test"), 
                icon = "warning")
        } else if (is.na(selSLev)) {
            tcltk::tkmessageBox(message = paste("Choose the significance",
                                                "level for the",
                                                "surrogates test"), 
                icon = "warning")
        } else {
            selTs <- get(selTsName, envir = KTSEnv)
            if (any(is.na(selTs$value))) {
                tcltk::tkmessageBox(message = paste("The time series",
                                                    "can't have NAs. ",
                                                    "Make a preliminary",
                                                    "filling"), 
                  icon = "warning")
            } else {
              tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
              dataToTest <- selTs$value
              sr <- trickFun(dataToTest,selSLev,lagDelay)
              surTestTxt <- sdtMessage2sA(sr$data.statistic, 
                                          sr$surrogates.statistics, 
                                          K = 20)
              nLTr <- nonlinearTseries::nonlinearityTest(dataToTest, 
                                                         verbose = FALSE)
              txt0 <- paste("LINEARITY TESTS FOR:", selTsName, "\n")
              txt01 <- c("Surrogates test", 
                         " Based on 20 surrogates", 
                         " Two sided", 
                         paste(" Significance level:", selSLev), 
                         paste(" Delay:", lagDelay), 
                         surTestTxt, "\n")
              txt1 <- c("Teraesvirta's neural network test", 
                        " Null hypothesis: Linearity in mean", 
                        paste(" X-squared = ", 
                              round(nLTr$Terasvirta$statistic, 4), 
                              " df = ", nLTr$Terasvirta$parameter, 
                              " p-value = ", 
                              round(nLTr$Terasvirta$p.value, 4)), 
                        "\n")
              txt2 <- c("White neural network test", 
                        " Null hypothesis: Linearity in mean", 
                        paste(" X-squared = ", 
                              round(nLTr$White$statistic, 4),
                              " df = ",nLTr$White$parameter, 
                              " p-value = ", 
                              round(nLTr$White$p.value), 4), 
                        "\n")
              txt3 <- c("Keenan's one-degree test for nonlinearity", 
                        paste(" Null hypothesis: The time",
                              "series follows some AR process"), 
                        paste(" F-stat = ", 
                              round(nLTr$Keenan$test.stat, 4), 
                              " p-value = ", 
                              round(nLTr$Keenan$p.value, 4)), 
                        "\n")
              txt4 <- c("McLeod-Li test", 
                        paste(" Null hypothesis:",
                              "The time series follows some ARIMA process"), 
                        paste(" Maximum p-value = ", 
                              round(max(unlist(nLTr$McLeodLi)),4)), 
                        "\n")
              txt5 <- c("Tsay's Test for nonlinearity", 
                        paste(" Null hypothesis: The time",
                              "series follows some AR process"), 
                        paste(" F-stat = ", 
                              round(nLTr$Tsay$test.stat, 4), 
                              " p-value = ", 
                              round(nLTr$Tsay$p.value, 4)), 
                        "\n")
              txt6 <- c(paste("Likelihood ratio test for",
                              "threshold nonlinearity"), 
                        paste(" Null hypothesis: The time",
                              "series follows some AR process"), 
                        paste(" Alternativce hypothesis: The",
                              "time series follows some TAR process"), 
                        paste(" X-squared = ", 
                              round(nLTr$TarTest$test.stat, 4), 
                              " p-value = ", 
                              round(nLTr$TarTest$p.value, 4)), 
                        "\n")
              txt <- c(txt0, date(), "\n", txt01, txt1, txt2, 
                       txt3, txt4, txt5,txt6)
              tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                              paste(txt, collapse = "\n"))
              tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
              endingLines()
              cleanEnvir()
              tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
              showPANlintests()
            }
        }
    }
    showPANlintests <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "LINEARITY TESTS")
        createTsRb()
        createTitle(labTitle = "Parameters for the surrogates test")
        createEntry(labTitle = "Delay", textVariableName = "delay")
        createEntry(labTitle = "Significance level", 
                    textVariableName = "signifLev", 
                    defaultVal = "0.05")
        createOK(labTitle = "RUN", action = lintestsOnOk)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    tcltk::tkmessageBox(message = paste("The process might take from",
                                        "seconds to some minutes.",
                                        "If your time series has more than",
                                        "roughly 45000 measurements,",
                                        "the programme can collapse"), 
        icon = "warning")
    checkIfAnyTs(action = "showPANlintests", 
                 envirName = environment(showPANlintests))
}

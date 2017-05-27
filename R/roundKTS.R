roundKTS <-
function() {
    roundOnOk <- function() {
        refreshDataSetsList(outp = FALSE)
        tssel <- tsCheckedTF()
        signi <- verifyIntEntry(tcltk::tclvalue(KTSEnv$sigdi0), 
                                noValid = NA)
        decim <- verifyIntEntry(tcltk::tclvalue(KTSEnv$depla0), 
                                noValid = NA)
        if (all(tssel == FALSE)) {
            tcltk::tkmessageBox(message = "Choose at least a time series", 
                                icon = "warning")
        } else if (is.na(signi) & is.na(decim)) {
            tcltk::tkmessageBox(message = paste("Choose a number of",
                                                "significant digits",
                                                "or decimal places"), 
                icon = "warning")
        } else {
            if (is.na(signi) == FALSE & is.na(decim) == FALSE) {
                tcltk::tkmessageBox(message = paste("You chose both",
                                                    "significant digits",
                                                    "and decimal places,",
                                                    "therefore only the",
                                                    "significant digits",
                                                    "were taken on account"), 
                  icon = "warning")
            }
            tsToRoundNames <- KTSEnv$dSList$TS[which(tssel == TRUE)]
            roundedTsNames <- paste0(tsToRoundNames, "_rnd")
            roundAndAssign <- function(TsToRoundNames, roundedTsNames) {
                for (i in 1:length(tsToRoundNames)) {
                  TS <- get(tsToRoundNames[i], envir = KTSEnv)
                  TS$value <- round(TS$value, digits = decim)
                  assign(roundedTsNames[i], TS, envir = KTSEnv)
                  rm(TS)
                }
            }
            signifAndAssign <- function(tsToRoundNames, roundedTsNames) {
                for (i in 1:length(tsToRoundNames)) {
                  TS <- get(tsToRoundNames[i], envir = KTSEnv)
                  TS$value <- signif(TS$value, digits = signi)
                  assign(roundedTsNames[i], TS, envir = KTSEnv)
                  rm(TS)
                }
            }
            if (is.na(signi) == FALSE) {
                signifAndAssign(tsToRoundNames, roundedTsNames)
            } else {
                roundAndAssign(tsToRoundNames, roundedTsNames)
            }
            cleanEnvir()
            refreshDataSetsList(outp = FALSE)
            showPANround()
        }
    }
    showPANround <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "ROUND")
        createTsChb()
        rntsst3 <- tcltk::tklabel(KTSEnv$subPanR4C1, 
                                  text = "Enter one of the next options", 
            font = KTSEnv$KTSFonts$T2)
        tcltk::tkgrid(rntsst3, sticky = "nw", 
                      padx = c(10, 10), pady = c(5, 2))
        createEntry(labTitle = "Significant digits",
                    textVariableName = "sigdi0")
        createEntry(labTitle = "Decimal places", 
                    textVariableName = "depla0")
        createOK(labTitle = "RUN", action = roundOnOk)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANround", 
                 envirName = environment(showPANround))
}

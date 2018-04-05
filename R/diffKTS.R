diffKTS <-
function() {
    diffOnOk <- function() {
        refreshDataSetsList(outp = FALSE)
        tssel <- tsCheckedTF()
        selLagDif <- verifyIntEntry(tcltk::tclvalue(KTSEnv$lagDif), 
                                    noValid = NA)
        selCenterOrNot <- tcltk::tclvalue(KTSEnv$centerOrNot)
        newNames <- paste0(KTSEnv$dSList$TS, "_dff")
        if (all(tssel == FALSE)) {
            tcltk::tkmessageBox(message = paste("Choose a time series,",
                                                " at least"), 
                                icon = "warning")
        } else if (is.na(selLagDif)) {
            tcltk::tkmessageBox(message = "Introduce an integer lag", 
                                icon = "warning")
        } else {
            getdiffTimser <- function(Ind, selLagDif, selCenterOrNot) {
                timSer <- get(KTSEnv$dSList$TS[Ind], envir = KTSEnv)
                if (selLagDif != 1 | selCenterOrNot == "No") {
                  
                  timSer$value <- c(diff(timSer$value, lag = selLagDif), 
                                    rep(NA, selLagDif))
                  timSer <- timSer[1:(nrow(timSer) - selLagDif), ]
                  
                } else {
                  
                  resDiff <- diff(timSer$value, lag = selLagDif)
                  L <- length(resDiff)
                  resDiff1 <- rep(NA, 2 * L + 1)
                  resDiff1[seq(2, length(resDiff1), 2)] <- resDiff
                  resDiff2 <- zoo::na.spline(resDiff1, 
                                             xout = seq(1, 
                                                        length(resDiff1), 2), 
                                             na.rm = FALSE, maxgap = 1)
                  timSer$value <- resDiff2
                  
                }
                
                assign(newNames[Ind], timSer, envir = KTSEnv)
            }
            selTimSerInd <- which(tssel == TRUE)
            if (selLagDif != 1 & selCenterOrNot == "Yes") {
                selCenterOrNot == "No"
                tcltk::tkmessageBox(message = paste("The option center",
                                                    "time series is only",
                                                    "available when the",
                                                    "lag is 1"), 
                  icon = "warning")
            }
            for (i in selTimSerInd) {
                getdiffTimser(i, selLagDif, selCenterOrNot)
            }
            cleanEnvir()
            refreshDataSetsList(outp = FALSE)
            showPANdiff()
        }
    }
    showPANdiff <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "DIFFERENCES")
        if (class(KTSEnv$dSList$TS) == "character") {
            createTsChb()
        }
        createEntry(labTitle = "Lag", textVariableName = "lagDif", 
                    defaultVal = "1")
        createTitle(labTitle = "Center times series")
        assign("centerOrNot", tcltk::tclVar("No"), envir = KTSEnv)
        createEachRb(labTitle = "Yes", variable = KTSEnv$centerOrNot)
        createEachRb(labTitle = "No", variable = KTSEnv$centerOrNot)
        
        createOK(labTitle = "RUN", action = diffOnOk)
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANdiff", 
                 envirName = environment(showPANdiff))
}

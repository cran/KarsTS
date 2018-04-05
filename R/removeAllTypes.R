removeAllTypes <-
function() {
    removeOnOk <- function() {
        refreshDataSetsList(outp = FALSE)
        if (is.null(KTSEnv$dSList$nTS) == FALSE) {
            tssel <- tsCheckedTF()
            if (any(tssel) == TRUE) {
                rm(list = KTSEnv$dSList$TS[which(tssel == TRUE)], 
                   envir = KTSEnv)
            }
        }
        if (is.null(KTSEnv$dSList$nGaps) == FALSE) {
            gsel <- gapCheckedTF()
            if (any(gsel) == TRUE) {
                rm(list = KTSEnv$dSList$gaps[which(gsel == TRUE)], 
                   envir = KTSEnv)
            }
        }
        if (is.null(KTSEnv$dSList$nRM) == FALSE) {
            rsel <- rmCheckedTF()
            if (any(rsel) == TRUE) {
                rm(list = KTSEnv$dSList$rm[which(rsel == TRUE)], 
                   envir = KTSEnv)
            }
        }
        cleanEnvir()
        refreshDataSetsList(outp = FALSE)
        if (exists("subPanR4C1")) {
            tcltk::tkdestroy(subPanR4C1)
        }
        checkIfAny(action = "showPANremove", 
                   envirName = environment(showPANremove))
    }
    showPANremove <- function() {
        refreshDataSetsList(outp = FALSE)
        createSubPanR4C1()
        createTITLE(labTitle = "REMOVE")
        createOK(labTitle = "RUN", action = removeOnOk)
        if (class(KTSEnv$dSList$TS) == "character") {
            createTsChb()
        }
        if (class(KTSEnv$dSList$gaps) == "character") {
            createGapChb()
        }
        if (class(KTSEnv$dSList$rm) == "character") {
            createRmChb()
        }
        
        createNote(labTitle = "This will remove the data from the environment", 
                   pady = c(15,0))
        createNote(labTitle = "not from your hard disk", pady = c(0, 5))
        tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
        
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAny(action = "showPANremove", 
               envirName = environment(showPANremove))
}

readMultEntryvalues <-
function(nElements, prefix = "entValue", 
                                type = "character") {
    readMultEntryvalues <- function(ind, prefix = prefix, type = type) {
        if (type == "integer") {
          result <- verifyIntEntry(tcltk::tclvalue(get(paste0(prefix, ind), 
                                                       envir = KTSEnv)), 
                                   noValid = NA)
        } else if (type == "real") {
          result <- verifyRealEntry(tcltk::tclvalue(get(paste0(prefix, ind), 
                                                        envir = KTSEnv)), 
                                    noValid = NA)
        } else {
          result <- verifyCharEntry(tcltk::tclvalue(get(paste0(prefix, ind), 
                                                        envir = KTSEnv)), 
                                    noValid = NA)
        }
    }
    result <- myApplyVector(FUN = readMultEntryvalues, 
                            dataVector = 1:nElements, 
                            prefix = prefix, type = type)
}

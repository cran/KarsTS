writeMethodTitle <-
function(titleMethod) {
    tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                    paste(c(titleMethod, date()), collapse = "\n"))
    tcltk::tkinsert(KTSEnv$txtWidget, "end", "\n\n")
}

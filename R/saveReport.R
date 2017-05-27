saveReport <-
function() {
  
  if (exists("txtWidget", envir = KTSEnv)) {
    
    fileName <- paste0("report", substr(date(), 21, 25),
                       substr(date(), 5, 7), 
                       substr(date(), 9, 10), substr(date(), 12, 13), 
                       substr(date(), 15, 16), 
                       substr(date(), 18, 19), ".txt")
    X <- tcltk::tkget(KTSEnv$txtWidget, "0.0", "end")
    utils::write.table(tcltk::tclvalue(X), file = fileName)
    
  }
  
}

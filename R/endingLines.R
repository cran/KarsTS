endingLines <-
function() {
  
  endL <- paste0("***************************************",
                 "*************************")
  tcltk::tkinsert(KTSEnv$txtWidget, "end", endL, collapse = "\n")
  tcltk::tkinsert(KTSEnv$txtWidget, "end", endL, collapse = "\n")
  tcltk::tkinsert(KTSEnv$txtWidget, "end", endL, collapse = "\n")
  tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
  tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
  tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\n\n"))
}

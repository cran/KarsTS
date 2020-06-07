writeMethodSummary <-
function(filledNasTable, remainingNAsInGap, 
                               selTsName, selGapName, 
                               selGap) {
  txt2 <- paste("Time series:", selTsName)
  txt3 <- paste("Gap set:", selGapName)
  txt33 <- paste("Gap set length:", length(selGap$gaps))
  if (length(remainingNAsInGap) == 0) {
    txt4 <- "All NAs within the gap set were interpolated"
  } else {
    txt4 <- paste0("Number of remaining gaps (within the gap set):", 
                   length(remainingNAsInGap))
  }
  if (is.null(filledNasTable)) {
    txt5 <- "No NAs could be filled"
    txt6 <- NULL
  } else {
    txt5 <- "Gaps filled:"
    txt6 <- utils::capture.output(print.data.frame(filledNasTable))
  }
  txtAll <- c(txt2, txt3, txt33, txt4, txt5)
  tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                  paste(txtAll, collapse = "\\n"))
  tcltk::tkinsert(KTSEnv$txtWidget, "end", "\\n\\n")
  tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                  paste(txt6, collapse = "\\n"))
  tcltk::tkinsert(KTSEnv$txtWidget, "end", "\\n\\n")
}

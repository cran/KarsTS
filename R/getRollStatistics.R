getRollStatistics <-
function(selTs, selTsName, slidingWin, 
                              tailsTS = FALSE, selStatisTF) {
  if (slidingWin%%2 == 0) {
    slidingWin <- slidingWin + 1
    tcltk::tkmessageBox(message = paste("The sliding window has to be odd", 
                                        ", so it was increased by one"), 
                        icon = "warning")
  }
  halfWindowNAs <- rep(NA, 0.5 * (slidingWin - 1))
  if (tailsTS == TRUE) {
    partial <- TRUE
  } else {
    partial <- FALSE
    addTails <- function(newSer, halfWindowNAs) {
      c(halfWindowNAs, newSer, halfWindowNAs)
    }
  }
  if (selStatisTF$Min == TRUE) {
    statis1 <- zoo::rollapply(selTs$value, slidingWin, min, na.rm = TRUE, 
                              align = "center", partial = partial)
    statis1[which(is.finite(statis1) == FALSE)] <- NA
    if (tailsTS == FALSE) {
      statis1 <- addTails(statis1, halfWindowNAs)
    }
    assign(paste0(selTsName, "Min"), 
           data.frame(time = selTs$time, value = statis1), 
           envir = KTSEnv)
  }
  if (selStatisTF$FQ == TRUE) {
    statis2 <- zoo::rollapply(selTs$value, slidingWin, 
                              stats::quantile, probs = 0.25, 
                              na.rm = TRUE, align = "center", 
                              partial = partial)
    statis2[which(is.finite(statis2) == FALSE)] <- NA
    if (tailsTS == FALSE) {
      statis2 <- addTails(statis2, halfWindowNAs)
    }
    assign(paste0(selTsName, "Q1"), 
           data.frame(time = selTs$time, value = statis2), 
           envir = KTSEnv)
  }
  if (selStatisTF$Median == TRUE) {
    statis3 <- zoo::rollapply(selTs$value, slidingWin, 
                              stats::median, na.rm = TRUE, 
                              align = "center", partial = partial)
    statis3[which(is.finite(statis3) == FALSE)] <- NA
    if (tailsTS == FALSE) {
      statis3 <- addTails(statis3, halfWindowNAs)
    }
    assign(paste0(selTsName, "Median"), 
           data.frame(time = selTs$time, value = statis3), 
           envir = KTSEnv)
  }
  if (selStatisTF$Mean == TRUE) {
    statis4 <- zoo::rollapply(selTs$value, slidingWin, mean, 
                              na.rm = TRUE, align = "center", 
                              partial = partial)
    statis4[which(is.finite(statis4) == FALSE)] <- NA
    if (tailsTS == FALSE) {
      statis4 <- addTails(statis4, halfWindowNAs)
    }
    assign(paste0(selTsName, "Mean"), 
           data.frame(time = selTs$time, value = statis4), 
           envir = KTSEnv)
  }
  if (selStatisTF$TQ == TRUE) {
    statis5 <- zoo::rollapply(selTs$value, slidingWin, 
                              stats::quantile, probs = 0.75, 
                              na.rm = TRUE, align = "center", 
                              partial = partial)
    statis5[which(is.finite(statis5) == FALSE)] <- NA
    if (tailsTS == FALSE) {
      statis5 <- addTails(statis5, halfWindowNAs)
    }
    assign(paste0(selTsName, "Q3"), 
           data.frame(time = selTs$time, value = statis5), 
           envir = KTSEnv)
  }
  if (selStatisTF$Max == TRUE) {
    statis6 <- zoo::rollapply(selTs$value, slidingWin, 
                              max, na.rm = TRUE, align = "center", 
                              partial = partial)
    statis6[which(is.finite(statis6) == FALSE)] <- NA
    if (tailsTS == FALSE) {
      statis6 <- addTails(statis6, halfWindowNAs)
    }
    assign(paste0(selTsName, "Max"), 
           data.frame(time = selTs$time, value = statis6), 
           envir = KTSEnv)
  }
  if (selStatisTF$Sd == TRUE) {
    statis7 <- zoo::rollapply(selTs$value, slidingWin, 
                              stats::sd, na.rm = TRUE, 
                              align = "center", partial = partial)
    statis7[which(is.finite(statis7) == FALSE)] <- NA
    if (tailsTS == FALSE) {
      statis7 <- addTails(statis7, halfWindowNAs)
    }
    assign(paste0(selTsName, "Sd"), 
           data.frame(time = selTs$time, value = statis7), 
           envir = KTSEnv)
  }
}

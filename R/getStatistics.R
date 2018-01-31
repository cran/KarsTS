getStatistics <-
function(selTs) {
  basicStatistics <- c(min(selTs$value, na.rm = TRUE), 
                       stats::quantile(selTs$value, probs = 0.25, 
                                       na.rm = TRUE), 
                       stats::median(selTs$value, na.rm = TRUE), 
                       mean(selTs$value, na.rm = TRUE), 
                       stats::quantile(selTs$value, probs = 0.75, 
                                       na.rm = TRUE), 
                       max(selTs$value, na.rm = TRUE), 
                       stats::sd(selTs$value, na.rm = TRUE))
  names(basicStatistics) = c("Min.", "1st.Qu.", "Median", 
                             "Mean", "3rd.Qu.", "Max.","St.Dev")
  basicStatistics
}

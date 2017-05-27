histKTS <-
function() {
  histOnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), noValid = NA)
    nBars <- verifyIntEntry(tcltk::tclvalue(KTSEnv$numberBars), noValid = NA)
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", icon = "warning")
    } else {
      selTs <- get(selTsName, envir = KTSEnv)
      if (is.na(nBars)) {
        numnBars <- diff(range(selTs$value, na.rm = TRUE))
        dennBars1 <- stats::IQR(selTs$value, na.rm = TRUE)
        dennBars2 <- length(selTs$value)^(1/3)
        dennBars <- 2 * dennBars1/dennBars2
        nBars <- numnBars/dennBars
        nBars <- round(nBars)
      }
      plotHist <- function() {
        histResult <- graphics::hist(selTs$value, nBars, plot = FALSE)
        graphics::plot(histResult, ylab = "Probability density", xlab = "", 
                       main = paste("Histogram of ", selTsName), 
                       col = "limegreen", freq = FALSE)
        x = seq(min(selTs$value, na.rm = TRUE), max(selTs$value, na.rm = TRUE), 
                length.out = 100)
        graphics::curve(stats::dnorm(x, mean(selTs$value, na.rm = TRUE), 
                                     stats::sd(selTs$value, na.rm = TRUE)), 
                        add = TRUE, col = "darkred")
        
      }
      copyPlot <- function() {
        tkrplot::tkrreplot(tsPlot)
      }
      panelName <- createRandName()
      assign(panelName, tcltk::tktoplevel(bg = "white"))
      tcltk::tkwm.title(get(panelName), "Histogram")
      tsPlot <- tkrplot::tkrplot(get(panelName), fun = plotHist)
      copyButton <- tcltk::tkbutton(get(panelName), 
                                    text = "Copy to clipboard", 
                                    command = copyPlot)
      tcltk::tkpack(tsPlot, expand = TRUE, fill = "both", anchor = "center")
      tcltk::tkconfigure(tsPlot, bg = "white")
      tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
      tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
    }
  }
  showPANhist <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "HISTOGRAM")
    createTsRb()
    createEntry(labTitle = "Approx. number of bars", 
                textVariableName = "numberBars")
    createOK(labTitle = "RUN", action = histOnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANhist", 
               envirName = environment(showPANhist))
}

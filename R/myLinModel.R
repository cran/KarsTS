myLinModel <-
function(observed, predicted) {
    linModel <- try(stats::lm(predicted ~ 0 + observed), silent = TRUE)
    if (class(linModel) == "try-error") {
        warning("It was not possible to fit a linear model")
    } else {
      sumLinModel <- summary(linModel, correlation = TRUE)
      otherErrEstim <- getOtherErrEstim(observed, predicted)
      monYsup <- t(stats::coefficients(sumLinModel)[1,c(1, 2, 4)])
      goodnessFit <- t(data.frame(sumLinModel$r.squared, 
                                  monYsup, 
                                  sumLinModel$sigma, 
                                  stats::pf(sumLinModel$fstatistic[1], 
                                            sumLinModel$fstatistic[2], 
                                            sumLinModel$fstatistic[3], 
                                            lower.tail = F), 
                                  stats::cor(observed, predicted, 
                                             use = "complete.obs"), 
                                  stats::confint(linModel, 
                                                 conf.level = 0.95), 
                                  t(otherErrEstim)))
      rownames(goodnessFit) <- c("Explained var", "Slope coeff", 
                                 "Slope coeff err", "Slope coeff p-val", 
                                 "Residual stand err", "P-val", 
                                 "Correlation", "Confid interv 5%", 
                                 "Confid interv 95%", "Rel RMSE", 
                                 "Mean abs error", "Bias error")
      lmInflu <- stats::lm.influence(linModel)
      leve <- sort(lmInflu$hat, decreasing = TRUE, index.return = TRUE)
      sig <- sort(abs(lmInflu$sigma - sumLinModel$sigma), 
                  decreasing = TRUE, index.return = TRUE)
      coe <- sort(abs(lmInflu$coefficients), decreasing = TRUE, 
                  index.return = TRUE)
      wei <- sort(lmInflu$wt.res, decreasing = TRUE, 
                  index.return = TRUE)
      predictConf <- stats::predict(linModel, interval = "confidence", 
                                    level = 0.95)
      newO <- seq(min(observed, na.rm = TRUE), max(observed, na.rm = TRUE), 
                  length.out = 100)
      predictPred <- stats::predict(linModel, 
                                    newdata = data.frame(observed = newO), 
                                    interval = "prediction", level = 0.95)
        
        plotLinModel <- function() {
            graphics::par(mfrow = c(2, 2))
            graphics::plot(linModel)
        }
        copyPlot <- function() {
            tkrplot::tkrreplot(lmPlot)
        }
        nameWin <- paste0("panel", sample(10000:99999, 1))
        assign(nameWin, tcltk::tktoplevel(bg = "white"))
        tcltk::tkwm.title(get(nameWin), "Linear fit")
        lmPlot <- tkrplot::tkrplot(get(nameWin), fun = plotLinModel, 
                                   hscale = 3, vscale = 1.5)
        copyButton <- tcltk::tkbutton(get(nameWin), 
                                      text = "Copy to clipboard", 
                                      command = copyPlot)
        tcltk::tkpack(lmPlot, expand = TRUE, fill = "both", 
                      anchor = "center")
        tcltk::tkconfigure(lmPlot, bg = "white")
        tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
        
        plotOP <- function() {
          graphics::plot(observed, predicted, xlab = "Observed", 
                         ylab = "Predicted", 
                         ylim = c(min(predictPred, na.rm = TRUE), 
                                  max(predictPred, na.rm = TRUE)), 
                         asp = 1)
            graphics::abline(linModel, col = "red")
            graphics::lines(newO, predictPred[, 3], 
                            lty = "dashed", col = "blue")
            graphics::lines(newO, predictPred[, 2], 
                            lty = "dashed", col = "blue")
            graphics::lines(observed, predictConf[, 3], 
                            lty = "dashed", col = "cyan")
            graphics::lines(observed, predictConf[, 2], 
                            lty = "dashed", col = "cyan")
            graphics::abline(linModel, col = "red")
            graphics::points(observed[leve$ix[1:5]], 
                             predicted[leve$ix[1:5]], col = "purple")
            graphics::points(observed[wei$ix[1:5]], 
                             predicted[wei$ix[1:5]], col = "yellow")
            graphics::points(observed[sig$ix[1:5]], 
                             predicted[sig$ix[1:5]], col = "orange")
            graphics::points(observed[coe$ix[1:5]], 
                             predicted[coe$ix[1:5]], col = "green")
            bx <- graphics::par("usr")
            graphics::legend(x = bx[1:2], 
                             y = bx[4] + c(0, 0.2 * (bx[4] - bx[3])), 
                             xpd = TRUE, 
                             legend = c("fit", "pred.conf", "leverage", 
                                        "weights","sigma", "slope"), 
                             col = c("red", "blue", "purple", 
                                     "yellow", "orange","green"), 
                             ncol = 3, pch = 1)
        }
        copyPlot <- function() {
            tkrplot::tkrreplot(opPlot)
        }
        nameWin <- paste0("panel", sample(10000:99999, 1))
        assign(nameWin, tcltk::tktoplevel(bg = "white"))
        tcltk::tkwm.title(get(nameWin), "Observed vs predicted")
        opPlot <- tkrplot::tkrplot(get(nameWin), fun = plotOP, 
                                   hscale = 2, vscale = 1.2)
        copyButton <- tcltk::tkbutton(get(nameWin), 
                                      text = "Copy to clipboard", 
                                      command = copyPlot)
        tcltk::tkpack(opPlot, expand = TRUE, fill = "both", anchor = "center")
        tcltk::tkconfigure(opPlot, bg = "white")
        tcltk::tkpack(copyButton, expand = TRUE, fill = "both")
        
        goodnessFit
    }
}

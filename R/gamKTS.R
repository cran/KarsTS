gamKTS <-
function() {
  gamKTS1OnOk <- function() {
    selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP))
    if (is.na(selTsName)) {
      tcltk::tkmessageBox(message = "Choose a time series", 
                          icon = "warning")
    } else {
      KTSEnv$selTsName <- selTsName
      showPANgamKTS2()
    }
  }
  gamKTS2OnOk <- function() {
    refreshDataSetsList(outp = FALSE)
    tssel <- tsCheckedTF()
    predictorTS <- KTSEnv$dSList$TS[which(tssel == TRUE)]
    nPredictorTS <- length(predictorTS)
    selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
    if (any(predictorTS == KTSEnv$selTsName)) {
      
      tcltk::tkmessageBox(message = paste("The time series",
                                          "to fill cannot be",
                                          "one of the predictors"), 
                          icon = "warning")
      
    } else if (nPredictorTS == 0) {
      tcltk::tkmessageBox(message = paste("Choose, at least,",
                                          "a predictor time series"), 
                          icon = "warning")
    } else if (nPredictorTS > 3) {
      tcltk::tkmessageBox(message = paste("The maximum number of",
                                          "predictor time series is 3"), 
                          icon = "warning")
    } else {
      tmComptibility <- matrix(rep(FALSE, 3 * nPredictorTS), nPredictorTS, 
                                  3)
      for (i in 1:nPredictorTS) {
        tmComptibility[i, ] <- are2TsTimeCompatible(selTs, 
                                                       get(predictorTS[i], 
                                                           envir = KTSEnv))
      }
      
      if (any(tmComptibility[, 1] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The initial date of all the",
                                            "time series must be the same"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 2] == FALSE)) {
        tcltk::tkmessageBox(message = paste("The sampling period of all",
                                            "the time series must",
                                            "be the same"), 
                            icon = "warning")
      } else if (any(tmComptibility[, 3] == FALSE)) {
        tcltk::tkmessageBox(message = paste("All time series must",
                                            "have the same length"), 
                            icon = "warning")
      } else {
        KTSEnv$predictorTS <- predictorTS
        showPANgamKTS3()
      }
    }
  }
  gamKTS3OnOk <- function() {
    
    fx <- tcltk::tclvalue(KTSEnv$fxE)
    bs <- verifyCharEntry(tcltk::tclvalue(KTSEnv$bsE), noValid = NA)
    per <- verifyRealEntry(tcltk::tclvalue(KTSEnv$perE), noValid = NA)
    selTs <- get(KTSEnv$selTsName, envir = KTSEnv)
    gapToUse <- gapForSelMethod(KTSEnv$selTsName, selTs)
    selGap <- gapToUse$selGap
    selGapName <- gapToUse$selGapName
    nasInSelTs <- which(is.na(selTs$value))
    tmComptibility <- areTsGapTimeCompatible(selTs, selGap)
    
    if (length(nasInSelTs) == 0) {
      tcltk::tkmessageBox(message = paste("The selected time",
                                          "series contains no NAs"), 
                          icon = "warning")
    } else if (length(selGap$gaps) == 0) {
      tcltk::tkmessageBox(message = "The gap set is empty", icon = "warning")
    } else if (length(setdiff(union(selGap$gaps, nasInSelTs), 
                              nasInSelTs)) != 0) {
      tcltk::tkmessageBox(message = paste("Some NAs in the gap set",
                                          "do not exist in the time series.",
                                          "Check that the selected gap set",
                                          "comes from the selected",
                                          "time series"), 
                          icon = "warning")
    } else if (tmComptibility[1] == FALSE) {
      tcltk::tkmessageBox(message = paste("The initial date of the time",
                                          "series and the one stored in",
                                          "the gap set do not match"), 
                          icon = "warning")
    } else if (tmComptibility[2] == FALSE) {
      tcltk::tkmessageBox(message = paste("The sampling period of the",
                                          "time series and the one stored",
                                          "in the gap set do not match"), 
                          icon = "warning")
    } else if (tmComptibility[3] == FALSE) {
      tcltk::tkmessageBox(message = paste("The time series is shorter",
                                          "than some indices stored",
                                          "in the set of gaps"), 
                          icon = "warning")
    } else {
      
      getPiece <- function(gapLims, per, X, Y) {
        if (is.vector(X)) {
          X <- as.matrix(X)
        }
        L <- round(per * (gapLims[2] - gapLims[1] + 1), 0)
        P <- (gapLims[1] - L):(gapLims[2] + L)
        P <- intersect(P, 1:nrow(X))
        
        lP <- length(P)
        if (lP > 0) {
          X <- X[P, ]
          Y <- Y[P]
        } else {
          X <- NA
          Y <- NA
        }
        
        res <- list(X1 = X, Y1 = Y, predLims = P)
      }
      
      multSpline <- function(XP, YP, per = 1, bs = "cr", fx = TRUE) {
        
        TI <- 1:length(YP)
        
        if (is.vector(XP)) {
          gamMod <- try(mgcv::gam(YP ~ te(XP, TI, bs = bs, fx = fx)), 
                        silent = TRUE)
          
          if (any(class(gamMod) == "try-error")) {
            stop("The gam model failed")
          } else {
            res <- try(stats::predict(gamMod, newdata = data.frame(XP, TI)), 
                       silent = TRUE)
            if (class(res) == "try-error") {
              stop("The prediction failed")
            } else {
              res
            }
          }
          
        } else if (ncol(XP) == 2) {
          XP1 <- XP[, 1]
          XP2 <- XP[, 2]
          gamMod <- try(mgcv::gam(YP ~ te(XP1, XP2, TI, bs = bs, fx = fx)), 
                        silent = TRUE)
          
          if (any(class(gamMod) == "try-error")) {
            stop("The gam model failed")
          } else {
            res <- try(stats::predict(gamMod, 
                                      newdata = data.frame(XP1, XP2,TI)), 
                       silent = TRUE)
            if (class(res) == "try-error") {
              stop("The prediction failed")
            } else {
              res
            }
          }
        } else if (ncol(XP) == 3) {
          XP1 <- XP[, 1]
          XP2 <- XP[, 2]
          XP3 <- XP[, 3]
          gamMod <- try(mgcv::gam(YP ~ te(XP1, XP2, XP3, TI, 
                                          bs = bs, fx = fx)), 
                        silent = TRUE)
          
          if (any(class(gamMod) == "try-error")) {
            stop("The gam model failed")
          } else {
            res <- try(stats::predict(gamMod, 
                                      newdata = data.frame(XP1, XP2,XP3, TI)), 
                       silent = TRUE)
            if (class(res) == "try-error") {
              stop("The prediction failed")
            } else {
              res
            }
          }
        }
      }
      
      placePrediction <- function(YTimSer, gapLims, gamPred, predLims) {
        predLims1 <- gapLims[1] - predLims[1]
        aa <- gapLims[1]:gapLims[2]
        YTimSer$value[aa] <- gamPred[predLims1:(predLims1 + 
                                                  gapLims[2] - gapLims[1])]
        YTimSer
      }
      
      multSplinInterp <- function(X, YTimSer, gapsInYnotInX, per = 1, 
                                  bs = "cr",fx = TRUE) {
        
        gapMatrix <- groupIndices(gapsInYnotInX)
        nG <- nrow(gapMatrix)
        res <- vector("list", nG)
        gamPred <- vector("list", nG)
        
        for (i in 1:nrow(gapMatrix)) {
          
          res[[i]] <- getPiece(gapLims = gapMatrix[i, 1:2], per = per, X = X, 
                               Y = YTimSer$value)
          
          gamPred[[i]] <- try(multSpline(XP = res[[i]]$X1, YP = res[[i]]$Y1, 
                                         per = per, bs = bs, fx = fx), 
                              silent = TRUE)
          
        }
        
        YTimSerPred <- YTimSer
        
        for (j in 1:nrow(gapMatrix)) {
          
          if (is.numeric(gamPred[[j]])) {
            YTimSerPred <- placePrediction(YTimSerPred, 
                                           gapLims = gapMatrix[j,1:2], 
                                           gamPred = gamPred[[j]], 
                                           predLims = res[[j]]$predLims)
          }
        }
        
        YTimSerPred
        
      }
      
      if (is.na(bs)) {
        bs <- "cr"
        tcltk::tkmessageBox(message = paste("The smoothing basis",
                                            "entry was not valied and",
                                            "it defaulted to cr"), 
                            icon = "warning")
      }
      
      if (is.na(per)) {
        per <- 100
        tcltk::tkmessageBox(message = paste("The window was not valid",
                                            "and it defaulted to 100%"), 
                            icon = "warning")
      }
      
      if (fx == "df") {
        fx <- TRUE
      } else {
        fx <- FALSE
      }
      
      per <- 0.01 * per
      lPred <- length(KTSEnv$predictorTS)
      
      if (lPred == 1) {
        X <- get(KTSEnv$predictorTS, envir = KTSEnv)$value
      } else {
        X <- NULL
        for (i in 1:lPred) {
          X <- cbind(X, get(KTSEnv$predictorTS[i], envir = KTSEnv)$value)
        }
      }
      
      
      filledTS <- multSplinInterp(X = X, YTimSer = selTs, 
                                  gapsInYnotInX = selGap$gaps, 
                                  per = per, bs = bs, fx = fx)
      
      filledTS$value[which(is.nan(filledTS$value))] <- NA
      
      assign(paste0(KTSEnv$selTsName, "_", selGapName, "_mSpl"), 
             filledTS, envir = KTSEnv)
      gapsAfterFill <- getGapsAfterFill(filledTS, selGap, 
                                        envir = environment(gamKTS1OnOk))
      remainingNAsInGap <- gapsAfterFill$remainingNAsInGap
      filledNasTable <- gapsAfterFill$filledNasTable
      writeMethodTitle("MULTIVARIATE SPLINE INTERPOLATION")
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      "GAM model with tensor product smooth ")
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      paste("Filled time series:", KTSEnv$selTsName))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", 
                      paste("Predictor time series:", 
                            KTSEnv$predictorTS, collapse = ","))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("Regression:", fx))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("Smoothing basis:", bs))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("Window:", 100 * per, 
                                                     "% gap length"))
      tcltk::tkinsert(KTSEnv$txtWidget, "end", paste("\\n"))
      writeMethodSummary(filledNasTable, remainingNAsInGap, KTSEnv$selTsName, 
                         selGapName, selGap)
      endingLines()
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      showPANgamKTS1()
    }
  }
  
  showPANgamKTS1 <- function() {
    refreshDataSetsList(outp = FALSE)
    createSubPanR4C1()
    createTITLE(labTitle = "MULTIVARIATE SPLINES")
    createTsRb(labTitle = "Time series to fill")
    createOK(labTitle = "NEXT", action = gamKTS1OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  showPANgamKTS2 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "MULTIVARIATE SPLINES")
    createTsChb(labTitle = "Predictor time series")
    createOK(labTitle = "NEXT", action = gamKTS2OnOk)
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  showPANgamKTS3 <- function() {
    createSubPanR4C1()
    createTITLE(labTitle = "MULTIVARIATE SPLINES")
    if (is.null(KTSEnv$dSList$gaps) == FALSE) {
      createGapRb()
    }
    createEntry(labTitle = "Window (% gap length)", textVariableName = "perE")
    
    createTitle(labTitle = "Type of regression")
    assign("fxE", tcltk::tclVar("df"), envir = KTSEnv)
    createRb(variable = KTSEnv$fxE, dataVector = c("df", "penalized"))
    
    createEntry(labTitle = "Smoothing basis", textVariableName = "bsE")
    
    createOK(labTitle = "RUN", action = gamKTS3OnOk)
    
    createNote(labTitle = "The window can be greater than 100%")
    createNote(labTitle = "The smoothing basis is usually cr or tp ", 
               pady = c(5,1))
    createNote(labTitle = "cr: cubic regression spline ", pady = c(0, 1))
    createNote(labTitle = "tp: plate regression spline ", pady = c(0, 10))
    
    tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
    
  }
  
  cleanEnvir()
  refreshDataSetsList(outp = FALSE)
  checkIfAnyTs(action = "showPANgamKTS1", 
               envirName = environment(showPANgamKTS1))
}

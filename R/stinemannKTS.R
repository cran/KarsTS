stinemannKTS <-
function() {
    
    na.stinterp <- function(object, 
                            along = stats::time(object), na.rm = TRUE, ...){
      along <- as.numeric(along)
      na.stinterp.0 <- function(y) {
        na <- is.na(y)
        if(all(!na)) return(y)
        y[na] <- stinterp(along[!na], as.numeric(y[!na]), along[na], ...)$y
        return(y)
      }
      object[] <- if (length(dim(object)) == 0) na.stinterp.0(object)
      else apply(object, 2, na.stinterp.0)
      if (na.rm) stats::na.omit(object) else object
    }
    
    
    "stinterp" <- function (x, y, xout, yp, 
                            method = c("scaledstineman", 
                                       "stineman", 
                                       "parabola")){
      
      if (missing(x) || missing(y) || missing(xout)) 
          stop("Wrong number of input arguments, x, y and xout must be specified")
        if (!is.vector(x) || !is.vector(y) || !is.vector(xout) || 
            !is.numeric(x) || !is.numeric(y) || !is.numeric(xout)) 
          stop("x, y and xout must be numeric vectors")
        if (length(x) < 2) 
          stop("x must have 2 or more elements")
        if (length(x) != length(y)) 
          stop("x must have the same number of elements as y")
        if (any(is.na(x)) || any(is.na(x)) || any(is.na(xout))) 
          stop("NAs in x, y or xout are not allowed")
        if (!missing(yp)) {
          if (!is.vector(yp) || !is.numeric(yp)) 
            stop("yp must be a numeric vector")
          if (length(y) != length(yp)) 
            stop("When specified, yp must have the same number of elements as y")
          if (any(is.na(yp)))
            stop("NAs in yp are not allowed")
          if (!missing(method)) 
            stop("Method should not be specified if yp is given")
        }
        dx <- diff(x)
        dy <- diff(y)
        if (any(dx <= 0)) 
          stop("The values of x must strictly increasing")
        
        #calculation of slopes if needed
        if (missing(yp)) {
          yp <- switch(match.arg(method), # allows 4 partial argument matching 
                       scaledstineman = stinemanSlopes(x,y, scale = TRUE), 
                       stineman = stinemanSlopes(x, y, scale = FALSE), 
                       parabola = parabolaSlopes(x, y))
        }
        
        # preparations
        m <- length(x)
        m1 <- m - 1
        s <- dy/dx
        k <- length(xout)
        
        ix <- findInterval(xout, x, rightmost.closed = TRUE)
        
        # For edgepoints allow extrapolation 
        # within a tiny range (set by machine precision).
        
        epx <- 5 * (.Machine$double.eps) * diff(range(x))
        ix[min(x) - epx <= xout & xout <= min(x)] <- 1
        ix[max(x) <= xout & xout <= max(x) + epx] <- m1
        idx <- 1 <= ix & ix <= m1
        ix1 <- ix[idx]
        ix2 <- ix1 + 1
        
        # computation of the interpolant for 
        # the three cases dyo1dyo2 ==, > and < 0
        
        dxo1 <- xout[idx] - x[ix1]
        dxo2 <- xout[idx] - x[ix2]
        y0o <- y[ix1] + s[ix1] * dxo1
        dyo1 <- (yp[ix1] - s[ix1]) * dxo1
        dyo2 <- (yp[ix2] - s[ix1]) * dxo2
        dyo1dyo2 <- dyo1 * dyo2
        yo <- y0o
        # linear interpolation is sufficient for m=2 unless slopes are given,
        # then nothing more is done
        if (m > 2 || !missing(yp)) { 
          id <- dyo1dyo2 > 0
          yo[id] <- y0o[id] + dyo1dyo2[id]/(dyo1[id] + dyo2[id])
          id <- dyo1dyo2 < 0
          yo[id] <- y0o[id] + dyo1dyo2[id] * (dxo1[id] + dxo2[id])/(dyo1[id] - 
                                                                      dyo2[id])/((dx[ix1])[id])
        }
        
        # return the results
        
        yout <- rep(NA, k)
        yout[idx] <- yo
        list(x = xout, y = yout)
      }
    
    
    "parabolaSlopes" <-
      function (x, y) 
      {
        m <- length(x)
        m1 <- m - 1
        dx <- diff(x)
        dy <- diff(y)
        dydx = dy/dx
        if (m == 2) {
          yp <- rep(dydx, 2)
        } else {
          yp <- c((dydx[1]*(2*dx[1]+dx[2])-dydx[2]*dx[1])/(dx[1]+dx[2]),
                  (dydx[-m1]*dx[-1] + dydx[-1]*dx[-m1])/(dx[-1] + dx[-m1]),
                  (dydx[m1]*(2*dx[m1]+dx[m1-1])-dydx[m1-1]*dx[m1])/(dx[m1]+dx[m1-1]))
        }
        yp
      }
    
    
    
    "stinemanSlopes" <-
      function (x, y, scale = FALSE) 
      {
        m <- length(x)
        m1 <- m - 1
        if (m == 2) {
          yp <- rep(diff(y)/diff(x), 2)
        }
        else {
          if (scale) {
            sx <- diff(range(x))
            sy <- diff(range(y))
            if (sy <= 0) 
              sy <- 1
            x <- x/sx
            y <- y/sy
          }
          dx <- diff(x)
          dy <- diff(y)
          yp <- rep(NA, m)
          dx2dy2p <- dx[-1]^2 + dy[-1]^2
          dx2dy2m <- dx[-m1]^2 + dy[-m1]^2
          yp[2:m1] <- (dy[-m1] * dx2dy2p + dy[-1] * dx2dy2m)/(dx[-m1] * 
                                                                dx2dy2p + dx[-1] * dx2dy2m)
          s <- dy[1]/dx[1]
          if ((s >= 0 && s >= yp[2]) || (s <= 0 && s <= yp[2])) 
            yp[1] <- 2 * s - yp[2]
          else yp[1] <- s + abs(s) * (s - yp[2])/(abs(s) + abs(s - 
                                                                 yp[2]))
          s <- dy[m1]/dx[m1]
          if ((s >= 0 && s >= yp[m1]) || (s <= 0 && s <= yp[m1])) 
            yp[m] <- 2 * s - yp[m1]
          else yp[m] <- s + abs(s) * (s - yp[m1])/(abs(s) + abs(s - 
                                                                  yp[m1]))
          if (scale) 
            yp <- yp * sy/sx
        }
        yp
      }
    
    stineOnOk <- function() {
      selTsName <- verifyCharEntry(tcltk::tclvalue(KTSEnv$selTsP), 
                                   noValid = NA)
      peri <- verifyIntEntry(tcltk::tclvalue(KTSEnv$peri),
                             noValid = NA)
      
      
      stinepack::na.stinterp(c(1,NA,3:10))
      stinepack::stinemanSlopes(1:13,sin(1:13),scale=TRUE)
      stinepack::parabolaSlopes(1:13,sin(1:13))
      stinepack::stinterp(1:13,sin(1:13),3)
      
      if (is.na(selTsName)) {
        tcltk::tkmessageBox(message = "Choose a time series", 
                            icon = "warning")
      } else {
        selTs <- get(selTsName, envir = KTSEnv)
        gapToUse <- gapForSelMethod(selTsName, selTs)
        selGap <- gapToUse$selGap
        selGapName <- gapToUse$selGapName
        nasInSelTs <- which(is.na(selTs$value))
        tmComptibility <- areTsGapTimeCompatible(selTs, selGap)
        if (length(nasInSelTs) == 0) {
          tcltk::tkmessageBox(message = paste("The selected time",
                                              "series contains no NAs"), 
                              icon = "warning")
        } else if (length(selGap$gaps) == 0) {
          tcltk::tkmessageBox(message = "The gap set is empty", 
                              icon = "warning")
        } else if (length(setdiff(union(selGap$gaps, nasInSelTs), nasInSelTs)) != 
                   0) {
          tcltk::tkmessageBox(message = paste("Some NAs in the gap",
                                              "set do not exist in the time",
                                              "series. Check that the selected",
                                              " gap set comes from the",
                                              "selected time series"), 
                              icon = "warning")
        } else if (tmComptibility[1] == FALSE) {
          tcltk::tkmessageBox(message = paste("The initial date of the",
                                              "time series and the one",
                                              "stored in the gap",
                                              "set do not match"), 
                              icon = "warning")
        } else if (tmComptibility[2] == FALSE) {
          tcltk::tkmessageBox(message = paste("The sampling period of",
                                              "the time series and the",
                                              "one stored in the gap",
                                              "set do not match"), 
                              icon = "warning")
        } else if (tmComptibility[3] == FALSE) {
          tcltk::tkmessageBox(message = paste("The time series is shorter",
                                              "than some indices",
                                              "stored in the set of gaps"), 
                              icon = "warning")
        } else {
          
          
          
          if(is.na(peri)){peri <- 1}
          
          filledTS <- selTs
          
          for (ppp in 1:peri){
            
            posi.ppp <- seq(ppp, nrow(selTs),peri)
            
            gg <- rep(3,nrow(selTs))
            gg[selGap$gaps] <- 2
            gg <- gg[posi.ppp]
            gg <- which(gg == 2)
            
            if(length(gg) > 0){
              
              filledTS.i <- selTs[posi.ppp,]
              rownames(filledTS.i) <- NULL
              
              filledData <- na.stinterp(object = filledTS.i$value, 
                                        along = filledTS.i$time, 
                                        na.rm = FALSE)
              
              
              filledTS.i$value[gg] <- filledData[gg]
              
              
              filledTS$value[posi.ppp] <- filledTS.i$value
              
              rm(filledTS.i)
              
            }
            
            rm(gg,posi.ppp)
            
          }
          

          assign(paste0(selTsName, "_", selGapName, "_sti"), 
                 filledTS, envir = KTSEnv)
          gapsAfterFill <- getGapsAfterFill(filledTS, selGap, 
                                            envir = environment(stineOnOk))
          remainingNAsInGap <- gapsAfterFill$remainingNAsInGap
          filledNasTable <- gapsAfterFill$filledNasTable
          if(peri == 1){
            txtPeri <- "Period: none (1)"
          }else{
            txtPeri <- paste("Period:", peri) 
          }
          writeMethodTitle("STINEMANN'S INTERPOLATION")
          tcltk::tkinsert(KTSEnv$txtWidget, "end", txtPeri)
          tcltk::tkinsert(KTSEnv$txtWidget, "end", "\n")
          writeMethodSummary(filledNasTable, remainingNAsInGap, 
                             selTsName, 
                             selGapName, selGap)
          endingLines()
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANstine()
        }
      }
    }
    showPANstine <- function() {
      refreshDataSetsList(outp = FALSE)
      createSubPanR4C1()
      createTITLE(labTitle = "STINEMANN'S INTERPOLATION")
      if (is.null(KTSEnv$dSList$gaps) == FALSE) {
        createGapRb()
      }
      createTsRb()
      createEntry(labTitle = "Period", textVariableName = "peri",
                  defaultVal = "1")
      createOK(labTitle = "RUN", action = stineOnOk)
      tcltk::tkpack(KTSEnv$subPanR4C1, expand = TRUE, fill = "both")
      
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    checkIfAnyTs(action = "showPANstine", 
                 envirName = environment(showPANstine))
  }

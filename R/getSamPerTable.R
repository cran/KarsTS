getSamPerTable <-
function(timSer, sampPer) {
  timSerNoNas <- timSer[which(is.na(timSer$value) == FALSE), ]
  numNoNaTime <- as.numeric(timSerNoNas$time)
  numAllTime0 <- as.numeric(timSer$time) - numNoNaTime[1]
  numNoNaTime0 <- numNoNaTime - numNoNaTime[1]
  difnumNoNaTime0 <- diff(numNoNaTime0)
  getGroupsInNoNas <- function(difnumNoNaTime0) {
    result <- NULL
    for (i in sampPer) {
      rawIndices <- which(difnumNoNaTime0 == i)
      grouped <- groupIndices(rawIndices)
      result <- rbind(result, cbind(rep(i, nrow(grouped)), grouped))
      rm(rawIndices, grouped)
    }
    result[, 3] <- result[, 3] + 1
    result[, 4] <- result[, 4] + 1
    result <- result[order(result[, 2]), ]
    result
  }
  groupNoNas <- getGroupsInNoNas(difnumNoNaTime0)
  getGroupsInAll <- function(groupNoNas, numNoNaTime0, numAllTime0) {
    pcol <- which(compareVecVec(numNoNaTime0[groupNoNas[, 2]], numAllTime0), 
                  arr.ind = TRUE)[, 1]
    scol <- which(compareVecVec(numNoNaTime0[groupNoNas[, 3]], numAllTime0), 
                  arr.ind = TRUE)[, 1]
    groupInAll <- cbind(groupNoNas[, 1], pcol, scol, groupNoNas[, 4])
    rownames(groupInAll) <- NULL
    colnames(groupInAll) <- c("samp.per", "ini", "fin", "N")
    groupInAll
  }
  groupInAll <- getGroupsInAll(groupNoNas, numNoNaTime0, numAllTime0)
  addTrueGaps <- function(groupInAll, numAllTime0, timSer) {
    fmf <- diff(numAllTime0[1:2])
    naa <- nrow(groupInAll)
    getIniGap <- function(groupInAll, fmf) {
      firstIni <- groupInAll[1, 2]
      if (firstIni != 1) {
        add0 <- data.frame(samp.per = fmf, ini = 1, 
                           fin = firstIni - 1, N = firstIni - 1)
        rownames(add0) <- NULL
      } else {
        add0 <- NULL
      }
      add0
    }
    add0 <- getIniGap(groupInAll, fmf)
    getFinGap <- function(groupInAll, fmf, timSer) {
      lastFin <- groupInAll[nrow(groupInAll), 3]
      if (lastFin != nrow(timSer)) {
        addF <- data.frame(samp.per = fmf, ini = lastFin + 1, 
                           fin = nrow(timSer), 
                           N = nrow(timSer) - lastFin)
        rownames(addF) <- NULL
      } else {
        addF <- NULL
      }
      addF
    }
    addF <- getFinGap(groupInAll, fmf, timSer)
    getInBetweenGaps <- function(groupInAll, fmf) {
      jumpsInd <- groupInAll[-1, 2] - groupInAll[-naa, 3]
      jumpsSampPer <- groupInAll[-naa, 1]
      rest <- jumpsInd%%jumpsSampPer
      quotient <- jumpsInd/jumpsSampPer
      addNonMultiples <- function(rest, groupInAll, fmf) {
        nmI <- which(rest != 0)
        if (length(nmI) > 0) {
          leviia <- groupInAll[(nmI + 1), 2] - 1 - groupInAll[nmI, 3]
          addNonMult <- data.frame(samp.per = fmf, 
                                   ini = groupInAll[nmI, 3] + 1, 
                                   fin = groupInAll[(nmI + 1), 2] - 1, 
                                   N = leviia)
          rownames(addNonMult) <- NULL
        } else {
          addNonMult <- NULL
        }
        addNonMult
      }
      ana2 <- addNonMultiples(rest, groupInAll, fmf)
      addMultiples <- function(rest, groupInAll, fmf, quotient, jumpsInd) {
        mI <- which(rest == 0 & jumpsInd != 0)
        if (length(mI) > 0) {
          nsfrpq <- groupInAll[(mI + 1), 2] - 2 * quotient[mI] - 
            groupInAll[mI, 3] + 1
          addMult <- data.frame(samp.per = groupInAll[mI, 1], 
                                ini = groupInAll[mI, 3] + quotient[mI], 
                                fin = groupInAll[(mI + 1), 2] - quotient[mI], 
                                N = nsfrpq)
          rownames(addMult) <- NULL
        } else {
          addMult <- NULL
        }
        addMult
      }
      ana3 <- addMultiples(rest, groupInAll, fmf, quotient, jumpsInd)
      anaInBetween <- rbind(ana2, ana3)
      anaInBetween
    }
    addIB <- getInBetweenGaps(groupInAll, fmf)
    allToAdd <- rbind(add0, addIB, addF)
    addGapRows <- function(allToAdd, groupInAll) {
      groupInAll <- data.frame(groupInAll, 
                               type = rep("data", nrow(groupInAll)))
      if (length(allToAdd) > 0) {
        allToAdd <- data.frame(allToAdd, type = rep("NAs", nrow(allToAdd)))
        allRowsPresent <- rbind(groupInAll, allToAdd)
      } else {
        allRowsPresent <- groupInAll
      }
      rownames(allRowsPresent) <- NULL
      allRowsPresent <- allRowsPresent[order(allRowsPresent$ini), ]
      allRowsPresent
    }
    allRowsPresent <- addGapRows(allToAdd, groupInAll)
    allRowsPresent
  }
  allRowsPresent <- addTrueGaps(groupInAll, numAllTime0, timSer)
  indices2Dates <- function(allRowsPresent, timSer) {
    withDates <- data.frame(sp.min = allRowsPresent$samp.per/60, 
                            ini = timSer$time[allRowsPresent$ini], 
                            fin = timSer$time[allRowsPresent$fin], 
                            N = allRowsPresent$N, 
                            type = allRowsPresent$type)
  }
  withDates <- indices2Dates(allRowsPresent, timSer)
  row.names(withDates) <- NULL
  withDates
}

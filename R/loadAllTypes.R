loadAllTypes <-
function() {
    getExtension <- function(selFile) {
        selFileSplitRev <- rev(strsplit(selFile, split = NULL)[[1]])
        lastPoint <- min(which(selFileSplitRev == "."))
        lengthSelFile <- nchar(selFile)
        exten <- substr(selFile, lengthSelFile - lastPoint + 2, 
                        lengthSelFile)
        exten
    }
    getAlreadyLoaded <- function(DStoLoad) {
      
        loadedDS <- c(KTSEnv$dSList$TS, KTSEnv$dSList$gaps, KTSEnv$dSList$rm)
        alreadyLoaded <- intersect(loadedDS, DStoLoad)
        alreadyLoaded
        
    }
    importDS <- function() {
      importOnOk <- function() {
        selSepLoad <- verifyCharEntry(tcltk::tclvalue(KTSEnv$sepLoad), 
                                      noValid = NA)
        selDateFormat <- verifyCharEntry(tcltk::tclvalue(KTSEnv$dateFormatRb), 
                                         noValid = NA)
        selFile <- try(file.choose(), silent = TRUE)
        
        if (class(selFile) != "try-error") {
          exten <- getExtension(selFile)
          
          if (exten == "csv" & is.na(selSepLoad)) {
            tcltk::tkmessageBox(message = paste("The file extension is",
                                                "csv and you chose",
                                                "no separator.",
                                                "The loading may fail"), 
                                icon = "warning")
            selSepLoad <- ","
          } else if (exten == "txt" & is.na(selSepLoad)) {
            selSepLoad <- ""
          }
          if (nchar(selSepLoad) > 1) {
            tcltk::tkmessageBox(message = paste("The separator",
                                                "must be one byte"), 
                                icon = "warning")
          } else if (exten == "R" | exten == "RData") {
            tcltk::tkmessageBox(message = paste("To load R or RData files,",
                                                "use the other button"), 
                                icon = "warning")
          } else if (exten != "csv" & exten != "txt") {
            tcltk::tkmessageBox(message = paste("Allowed types of file",
                                                "in KarsTS are: .R, .RData,",
                                                ".csv and .txt"), 
                                icon = "warning")
          } else {
            tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
            preRead <- function(selFile, sep = selSepLoad, 
                                condis = c(2, 6, 4, 5, 11, 11)) {
              preData <- utils::read.table(selFile, header = FALSE, sep = sep, 
                                           numerals = "no.loss", skip = 2, 
                                           fill = TRUE)
              ncolPredata <- ncol(preData)
              likelyType <- "likelyTS"
              if (ncolPredata == condis[2]) {
                if (length(which(is.na(preData[, condis[3]]) == FALSE)) == 1 & 
                    length(which(is.na(preData[, condis[4]]) == FALSE)) == 1) {
                  likelyType <- "likelyGap"
                }
              } else if (ncolPredata == condis[5]) {
                if (any(c("cross", "joint", "simple") == 
                        preData[1, condis[6]])) {
                  likelyType <- "likelyRM"
                }
              }
              assign("ncolPredata", ncolPredata, envir = KTSEnv)
              likelyType
            }
            getDateFormat <- function(selDateFormat) {
              
              dateFormLabels <- c("m/d/YYYY H:M", "YYYY/m/d H:M", 
                                  "YYYY-m-d H:M", 
                                  "m-d-YYYY H:M", "d/m/YYYY H:M", 
                                  "YYYY/d/m H:M", "YYYY-d-m H:M", 
                                  "d-m-YYYY H:M")
              dateFormats <- c("%m/%d/%Y %H:%M", "%Y/%m/%d %H:%M",
                               "%Y-%m-%d %H:%M", 
                               "%m-%d-%Y %H:%M", "%d/%m/%Y %H:%M", 
                               "%Y/%d/%m %H:%M", "%Y-%d-%m %H:%M", 
                               "%d-%m-%Y %H:%M")
              dateFormat <- dateFormats[which(dateFormLabels == selDateFormat)]
              
            }
            auxloadts1 <- function(selFile, selDateFormat, ext = "csv", 
                                   tz = KTSEnv$timeZone, 
                                   sep = selSepLoad, colclass = NULL) {
              separateTimeData <- function(readData, ext) {
                if (ext == "csv") {
                  timeTSChar <- readData[, 1]
                  dataTS <- data.matrix(readData[2:KTSEnv$ncolPredata])
                } else if (ext == "txt") {
                  timeTSChar <- paste(readData[, 1], readData[, 2])
                  dataTS <- data.matrix(readData[3:KTSEnv$ncolPredata])
                  KTSEnv$namesCols <- KTSEnv$namesCols[-1]
                  assign("ncolPredata", KTSEnv$ncolPredata - 1, 
                         envir = KTSEnv)
                }
                list(timeTSChar = timeTSChar, dataTS = dataTS)
              }
              isOrderAlright <- function(timeDates) {
                diffTime <- diff(as.numeric(timeDates))
                uniqueDiffs <- sort(unique(diffTime))
                result <- TRUE
                if (any(uniqueDiffs <= 0)) {
                  badElement <- min(which(diffTime <= 0) + 1)
                  tcltk::tkmessageBox(message = paste("Bad order;",
                                                      "check the row", 
                                                      badElement, 
                                                      "in your time",
                                                      "series dates"), 
                                      icon = "warning")
                  result <- FALSE
                }
                result
              }
              areJumpsAlright <- function(timeDates, sampPer) {
                diffTime <- diff(as.numeric(timeDates))
                unDiffTime <- unique(diffTime)
                remain <- unDiffTime%%sampPer
                remain <- remain[is.finite(remain)]
                result <- TRUE
                if (any(remain != 0)) {
                  non0Remain <- remain[remain != 0][1]
                  badElement <- min(which(diffTime%%sampPer == non0Remain)) + 1
                  tcltk::tkmessageBox(message = paste("Some time jumps are",
                                                      "not multiples of the",
                                                      "minimum sampling",
                                                      " period. Check,at",
                                                      "least, the position", 
                                                      badElement, "in your",
                                                      "time series dates"), 
                                      icon = "warning")
                  result <- FALSE
                }
                result
              }
              analizeJumps <- function(timeTS, minSampPer) {
                timeTS <- as.numeric(timeTS)
                lastTime <- timeTS[length(timeTS)]
                timeTS <- c(timeTS, lastTime + 2 * minSampPer, lastTime + 4 * 
                              minSampPer)
                onesToInsert <- diff(timeTS)/minSampPer - 1
                posIns <- which(onesToInsert != 0)
                repairedPiece <- rep(0, posIns[1])
                nextPL <- rep(0, posIns[2] - posIns[1])
                laamu <- length(posIns) - 1
                for (i in 1:laamu) {
                  pieceToInsert <- rep(1, onesToInsert[posIns[i]])
                  repairedPiece <- c(repairedPiece, pieceToInsert, nextPL)
                  if (is.finite(posIns[i + 2])) {
                    nextPL <- rep(0, posIns[(i + 2)] - posIns[i + 1])
                  }
                }
                trueLength <- length(repairedPiece) - 2
                forRelvar <- repairedPiece[1:trueLength]
              }
              homoSampPer <- function(indices0or1, dataTS) {
                if (class(dataTS) == "data.frame") {
                  dataTS <- data.matrix(dataTS)
                } else if (is.vector(dataTS) == TRUE) {
                  dataTS <- matrix(dataTS)
                }
                noNAspositions <- which(indices0or1 == 0)
                fixedData <- matrix(NA, length(indices0or1), ncol(dataTS))
                fixedData[noNAspositions, ] <- dataTS
                fixedData
              }
              createTsDataFrame <- function(timeDates, dataTS, 
                                            sampPer, ext = "csv") {
                dataCompleted <- homoSampPer(analizeJumps(timeDates, sampPer), 
                                             dataTS)
                timeComplete <- seq(timeDates[1], 
                                    timeDates[length(timeDates)], 
                                    sampPer)
                TSready <- cbind(timeComplete, as.data.frame(dataCompleted))
                colnames(TSready) <- KTSEnv$namesCols
                TSready
              }
              TSready <- NA
              readData <- utils::read.table(selFile, header = FALSE, 
                                            sep = sep, 
                                            numerals = "no.loss", 
                                            colClasses = colclass, 
                                            fill = TRUE, skip = 2)
              if (ncol(readData) == 1) {
                tcltk::tkmessageBox(message = paste("Probably the separator",
                                                    "is wrong or maybe your",
                                                    "file has only",
                                                    "one column"), 
                                    icon = "warning")
              } else {
                namesCols <- utils::read.table(selFile, header = FALSE, 
                                               sep = sep, 
                                               numerals = "no.loss", 
                                               skip = 1, fill = TRUE)
                assign("namesCols", as.vector(as.matrix(namesCols[1, ])), 
                       envir = KTSEnv)
                
                lToRm <- length(c(which(is.na(readData[, 1])), 
                                  which(readData[, 1] == "")))
                readData <- readData[which(is.na(readData[, 1]) == FALSE), ]
                readData <- readData[which(readData[, 1] != ""), ]
                if (lToRm > 0) {
                  tcltk::tkmessageBox(message = paste(lToRm, "rows were",
                                                      "removed because their",
                                                      "times were",
                                                      "empty or NA"), 
                                      icon = "warning")
                }
                timeData <- separateTimeData(readData = readData, ext = ext)
                timeTSChar <- timeData$timeTSChar
                dataTS <- timeData$dataTS
                dateFormat <- getDateFormat(selDateFormat)
                timeDates <- strptime(timeTSChar, format = dateFormat, 
                                      tz = KTSEnv$timeZone)
                if (all(is.na(timeDates))) {
                  tcltk::tkmessageBox(message = paste("The selected date",
                                                      "format does not match",
                                                      "the date format",
                                                      "in your file"), 
                                      icon = "warning")
                } else if (any(is.na(timeDates))) {
                  tcltk::tkmessageBox(message = paste("The selected date",
                                                      "format does not",
                                                      "match some date",
                                                      "formats in your file.",
                                                      "Verify that the date",
                                                      "format is the same",
                                                      "through the whole",
                                                      "time series"), 
                                      icon = "warning")
                } else {
                  orderAlright <- isOrderAlright(timeDates)
                  sampPer <- min(unique(diff(as.numeric(timeDates))), 
                                 na.rm = TRUE)
                  jumpsAlright <- areJumpsAlright(timeDates, sampPer)
                  if (orderAlright == TRUE & jumpsAlright == TRUE) {
                    TSready <- createTsDataFrame(timeDates, dataTS, sampPer, 
                                                 ext = exten)
                  }
                }
                
              }
              
              TSready
            }
            auxloadgap1 <- function(selFile, selDateFormat, ext = "csv", 
                                    tz = KTSEnv$timeZone, 
                                    sep = selSepLoad, colclass = NULL) {
              GapReady <- NA
              GapReady <- utils::read.table(selFile, header = FALSE, 
                                            sep = sep, 
                                            numerals = "no.loss", 
                                            colClasses = colclass, 
                                            skip = 2, fill = TRUE)
              if (ext == "txt") {
                GapReady[1, 2] <- paste(GapReady[1, 2], GapReady[1, 3])
                GapReady[1, 4] <- paste(GapReady[1, 4], GapReady[1, 5])
                GapReady <- GapReady[, -c(3, 5)]
              }
              
              if (ncol(GapReady) == 1) {
                tcltk::tkmessageBox(message = paste("Probably the separator",
                                                    "is wrong or maybe your",
                                                    "file has only",
                                                    "one column"), 
                                    icon = "warning")
              } else {
                nameGap <- utils::read.table(selFile, header = FALSE, 
                                             sep = sep, 
                                             numerals = "no.loss", 
                                             skip = 0, fill = TRUE)
                assign("nameGap", nameGap[1, 1], envir = KTSEnv)
                
              }
              GapReady
            }
            auxloadrma1 <- function(selFile, selDateFormat, ext = "csv", 
                                    tz = KTSEnv$timeZone, 
                                    sep = selSepLoad, colclass = NULL) {
              
              rmReady <- NA
              rmReady <- utils::read.table(selFile, header = FALSE, 
                                           sep = sep, 
                                           numerals = "no.loss", 
                                           colClasses = colclass, skip = 2, 
                                           fill = TRUE)
              nameRm <- utils::read.table(selFile, header = FALSE, 
                                          sep = sep, 
                                          numerals = "no.loss", skip = 0, 
                                          fill = TRUE)
              assign("nameRm", nameRm[1, 1], envir = KTSEnv)
              
              if (ext == "txt") {
                rmReady[, 10] <- paste(rmReady[, 10], rmReady[, 11])
                rmReady <- rmReady[, -11]
              }
              rmReady
            }
            
            if (exten == "csv" & (selSepLoad == "" | selSepLoad == " ")) {
              
              tcltk::tkmessageBox(message = paste("The separators for",
                                                  "csv files use to be comma",
                                                  "or semicolon.",
                                                  "The loading may fail"), 
                                  icon = "warning")
              
            } else if (exten == "txt" & (selSepLoad == ";" | 
                                         selSepLoad == ",")) {
              
              tcltk::tkmessageBox(message = paste("You chose commas or",
                                                  "semicolons as separators",
                                                  "of a txt. The loading",
                                                  "may fail"), 
                                  icon = "warning")
              
            } else if (selSepLoad != ";" & selSepLoad != "," & selSepLoad != 
                       "" & selSepLoad != " ") {
              
              tcltk::tkmessageBox(message = paste("The separator is unusual.",
                                                  " The loading may fail"), 
                                  icon = "warning")
              
            }
            
            if (exten == "csv") {
              condis <- c(2, 6, 4, 5, 11, 11)
              classesGap <- c("numeric", rep("character", 2), 
                              rep("numeric", 2), "character")
              classesRm <- c(rep("numeric", 3), "character", 
                             rep("numeric",2), "character", 
                             rep("numeric", 2), rep("character", 2))
            } else if (exten == "txt") {
              condis <- c(3, 8, 6, 7, 12, 12)
              classesGap <- c("numeric", rep("character", 4), 
                              rep("numeric",2), "character")
              classesRm <- c(rep("numeric", 3), "character", 
                             rep("numeric",2), "character", 
                             rep("numeric", 2), rep("character", 3))
            }
            likelyType <- preRead(selFile, condis = condis)
            
            if (likelyType == "likelyTS") {
              if (exten == "csv") {
                clasesser <- c("character", rep("numeric", 
                                                KTSEnv$ncolPredata - 1))
              } else if (exten == "txt") {
                clasesser <- c("character", "character", 
                               rep("numeric", KTSEnv$ncolPredata - 2))
              }
              
              DStoLoad <- auxloadts1(selFile, selDateFormat, 
                                     ext = exten, colclass = clasesser)
              if (class(DStoLoad) != "data.frame") {
                tcltk::tkmessageBox(message = paste("The time series does",
                                                    "not have the",
                                                    "proper format"), 
                                    icon = "warning")
                
              } else {
                
                eodbn <- paste0(KTSEnv$namesCols[2:KTSEnv$ncolPredata])
                alreadyLoaded <- getAlreadyLoaded(eodbn)
                for (gg in 2:KTSEnv$ncolPredata) {
                  if (any(alreadyLoaded == KTSEnv$namesCols[gg])) {
                    tcltk::tkmessageBox(message = paste(KTSEnv$namesCols[gg], 
                                                        "already exists in",
                                                        "the environment",
                                                        "and it will",
                                                        "not be loaded"), 
                                        icon = "warning")
                    
                  } else {
                    newTS <- DStoLoad[, c(1, gg)]
                    colnames(newTS) <- c("time", "value")
                    assign(paste0(KTSEnv$namesCols[gg]), newTS, 
                           envir = KTSEnv)
                    rm(newTS)
                  }
                }
                
              }
            } else if (likelyType == "likelyGap") {
              DStoLoad <- try(auxloadgap1(selFile, selDateFormat, 
                                          ext = exten, 
                                          colclass = classesGap), 
                              silent = TRUE)
              if (class(DStoLoad) != "data.frame") {
                tcltk::tkmessageBox(message = paste("The set of gaps does",
                                                    "not have a",
                                                    "proper format"), 
                                    icon = "warning")
              } else {
                alreadyLoaded <- getAlreadyLoaded(paste0(KTSEnv$nameGap))
                if (length(alreadyLoaded) > 0) {
                  tcltk::tkmessageBox(message = paste("The data set was",
                                                      "not loaded because", 
                                                      paste(alreadyLoaded, 
                                                            collapse = ","), 
                                                      "already exists in",
                                                      "the environment"), 
                                      icon = "warning")
                } else {
                  dateFormat <- getDateFormat(selDateFormat)
                  timeDates <- strptime(DStoLoad[1, 2:3], format = dateFormat, 
                                        tz = KTSEnv$timeZone)
                  if (all(is.na(timeDates))) {
                    tcltk::tkmessageBox(message = paste("The selected date",
                                                        "format does not",
                                                        "match the date",
                                                        "format in",
                                                        "your file"), 
                                        icon = "warning")
                  } else if (any(is.na(timeDates))) {
                    tcltk::tkmessageBox(message = paste("The selected date",
                                                        "format does not",
                                                        "match some date",
                                                        "formats in your",
                                                        "file. Verify that",
                                                        "the date format is",
                                                        "the same through",
                                                        "the whole",
                                                        "time series"), 
                                        icon = "warning")
                  } else {
                    newGap <- list(gaps = DStoLoad[, 1], 
                                   tsIni = DStoLoad[1,2], 
                                   tsEnd = DStoLoad[1, 3], 
                                   samPerMin = DStoLoad[1, 4], 
                                   tsLength = DStoLoad[1, 5], 
                                   tsName = DStoLoad[1, 6])
                    assign(paste0(KTSEnv$nameGap), newGap, KTSEnv)
                  }
                }
              }
            } else if (likelyType == "likelyRM") {
              DStoLoad <- try(auxloadrma1(selFile, selDateFormat, ext = exten, 
                                          colclass = classesRm), silent = TRUE)
              if (class(DStoLoad) != "data.frame") {
                tcltk::tkmessageBox(message = paste("The recurrence matrix",
                                                    "does not have a",
                                                    "proper format"), 
                                    icon = "warning")
              } else {
                alreadyLoaded <- getAlreadyLoaded(paste0(KTSEnv$nameRm))
                if (length(alreadyLoaded) > 0) {
                  tcltk::tkmessageBox(message = paste("The data set was",
                                                      "not loaded because", 
                                                      paste(alreadyLoaded, 
                                                            collapse = ","), 
                                                      "already exists",
                                                      "in the environment"), 
                                      icon = "warning")
                } else {
                  newRM <- list(ones = DStoLoad[, 1:2], 
                                tol = DStoLoad[, 3], 
                                tsName = DStoLoad[, 4], 
                                embDim = DStoLoad[, 5], 
                                delay = DStoLoad[,6], 
                                dist = DStoLoad[, 7], 
                                tsLength = DStoLoad[, 8], 
                                samPerSec = DStoLoad[, 9], 
                                tsIni = DStoLoad[, 10], 
                                type = DStoLoad[1, 11])
                  for (j in c(2, 4, 5, 7, 8)) {
                    newRM[[j]] <- newRM[[j]][which(is.na(newRM[[j]]) == FALSE)]
                  }
                  for (j in c(3, 6, 9, 10)) {
                    flarsu <- intersect(which(newRM[[j]] != ""), 
                                        which(newRM[[j]] != " "))
                    newRM[[j]] <- newRM[[j]][flarsu]
                    rm(flarsu)
                  }
                  colnames(newRM$ones) <- c("X", "Y")
                  
                  dateFormat <- getDateFormat(selDateFormat)
                  timeDates <- strptime(newRM$tsIni, format = dateFormat, 
                                        tz = KTSEnv$timeZone)
                  if (is.na(timeDates)) {
                    tcltk::tkmessageBox(message = paste("The selected date",
                                                        "format does",
                                                        "not match",
                                                        "the date format",
                                                        "in your file"), 
                                        icon = "warning")
                  } else {
                    assign(paste0(KTSEnv$nameRm), newRM, KTSEnv)
                  }
                }
              }
            } else {
              tcltk::tkmessageBox(message = paste("The type of data",
                                                  "set is unclear"), 
                                  icon = "warning")
            }
            tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
            
          }
          tcltk::tkmessageBox(message = "Done", icon = "warning")
          cleanEnvir()
          refreshDataSetsList(outp = FALSE)
          showPANloadDS()
        }
      }
      showPANimport <- function() {
        refreshDataSetsList(outp = FALSE)
        subPanR4C1 <- createSubPanR4C1()
        createTITLE(labTitle = "IMPORT CSV OR TXT")
        createEntry(labTitle = "Separator", textVariableName = "sepLoad")
        
        dateFormLabels <- c("m/d/YYYY H:M", "YYYY/m/d H:M", 
                            "YYYY-m-d H:M", "m-d-YYYY H:M", 
                            "d/m/YYYY H:M", "YYYY/d/m H:M", 
                            "YYYY-d-m H:M", "d-m-YYYY H:M")
        assign("dateFormatRb", tcltk::tclVar("YYYY-m-d H:M"), envir = KTSEnv)
        createRb(variable = KTSEnv$dateFormatRb, dataVector = dateFormLabels)
        createOK(labTitle = "CHOOSE FILE", action = importOnOk, width = 20)
        tcltk::tkpack(subPanR4C1, expand = TRUE, fill = "both")
        assign("subPanR4C1", subPanR4C1, envir = KTSEnv)
      }
      cleanEnvir()
      refreshDataSetsList(outp = FALSE)
      showPANimport()
    }
    loadDSOnOk <- function() {
        refreshDataSetsList(outp = FALSE)
        selFile <- try(file.choose(), silent = TRUE)
        if (class(selFile) != "try-error") {
            exten <- getExtension(selFile)
            if (exten == "csv") {
                tcltk::tkmessageBox(message = paste("To load csv files,",
                                                    "use the other button"), 
                  icon = "warning")
            } else if (exten == "txt") {
                tcltk::tkmessageBox(message = paste("To load txt files, use",
                                                    "the other button"), 
                  icon = "warning")
            } else if (exten != "R" & exten != "RData") {
                tcltk::tkmessageBox(message = paste("Allowed types of file",
                                                    "in KarsTS are: .R,"
                                                    ,".RData, .csv and .txt"), 
                  icon = "warning")
            } else {
                tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "watch")
                DSNames <- load(selFile)
                alreadyLoaded <- getAlreadyLoaded(DSNames)
                isTSAlright <- function(DStoLoad, DSNames, jj) {
                  result <- FALSE
                  if (any(class(DStoLoad[, 1]) != c("POSIXct", "POSIXt"))) {
                    tcltk::tkmessageBox(message = paste("Error loading", 
                                                        DSNames[jj], 
                                                        ". The dates in",
                                                        "the fist column",
                                                        "of any time series",
                                                        "must be in",
                                                        "POSIXct POSIXt",
                                                        "format"), 
                                        icon = "warning")
                  } else if (ncol(DStoLoad) < 2) {
                    tcltk::tkmessageBox(message = paste("Error loading", 
                                                        DSNames[jj], 
                                                        ". The time series",
                                                        "must have","at least",
                                                        "two columns:",
                                                        "dates and data"), 
                                        icon = "warning")
                  } else {
                    result <- TRUE
                  }
                }
                isGapAlright <- function(DStoLoad, gapColnames, DSNames, jj) {
                  if (all(names(DStoLoad) == gapColnames)) {
                    result <- TRUE
                  } else {
                    tcltk::tkmessageBox(message = paste("Error loading", 
                                                        DSNames[jj], 
                                                        ". The elements of",
                                                        "the gap set do not",
                                                        "have the proper",
                                                        "names"), 
                                        icon = "warning")
                    result <- FALSE
                  }
                  result
                }
                isRmAlright <- function(DStoLoad, rmColnames, DSNames, jj) {
                  if (all(names(DStoLoad) == rmColnames)) {
                    result <- TRUE
                  } else {
                    tcltk::tkmessageBox(message = paste("Error loading", 
                                                        DSNames[jj], 
                                                        ". The elements of",
                                                        "the recurrence",
                                                        "matrix do not have",
                                                        "the proper names"), 
                                        icon = "warning")
                    result <- FALSE
                  }
                  result
                }
                loadTSs <- function(DStoLoad) {
                  nDStoLoad <- ncol(DStoLoad)
                  namesDS <- names(DStoLoad)
                  for (gg in 2:nDStoLoad) {
                    newTS <- DStoLoad[, c(1, gg)]
                    colnames(newTS) <- c("time", "value")
                    assign(namesDS[gg], newTS, envir = KTSEnv)
                  }
                }
                gapColnames <- c("gaps", "tsIni", "tsEnd", "samPerMin", 
                                 "tsLength", "tsName")
                rmColnames <- c("ones", "tol", "tsName", "embDim", "delay", 
                                "dist","tsLength", "samPerSec", 
                                "tsIni", "type")
                for (jj in 1:length(DSNames)) {
                  if (any(alreadyLoaded == DSNames[jj])) {
                    tcltk::tkmessageBox(message = paste(DSNames[jj], 
                                                        "already exists in",
                                                        "the environment",
                                                        "and it will",
                                                        "not be loaded"), 
                      icon = "warning")
                  } else {
                    DStoLoad <- get(DSNames[jj])
                    if (class(DStoLoad) == "data.frame") {
                      TSAlright <- isTSAlright(DStoLoad, DSNames, jj)
                      if (TSAlright == TRUE) {
                        loadTSs(DStoLoad)
                      }
                    } else if (class(DStoLoad) == "list") {
                      if (names(DStoLoad)[1] == "gaps") {
                        gapAlright <- isGapAlright(DStoLoad, gapColnames, 
                                                   DSNames,jj)
                        if (gapAlright == TRUE) {
                          assign(DSNames[jj], DStoLoad, KTSEnv)
                        }
                      } else if (names(DStoLoad)[1] == "ones") {
                        rmAlright <- isRmAlright(DStoLoad, rmColnames, 
                                                 DSNames, jj)
                        if (rmAlright == TRUE) {
                          assign(DSNames[jj], DStoLoad, KTSEnv)
                        }
                      } else {
                        tcltk::tkmessageBox(message = paste("Error loading", 
                                                            DSNames[jj], 
                                                            ". The data set",
                                                            "format is not",
                                                            "proper"), 
                                            icon = "warning")
                      }
                    }
                    rm(DStoLoad)
                  }
                }
                tcltk::tkconfigure(KTSEnv$mainPanel, cursor = "left_ptr")
                tcltk::tkmessageBox(message = "Done", icon = "warning")
                refreshDataSetsList(outp = FALSE)
            }
        }
    }
    showPANloadDS <- function() {
        refreshDataSetsList(outp = FALSE)
        subPanR4C1 <- createSubPanR4C1()
        createTITLE(labTitle = "LOAD")
        createOK(labTitle = "LOAD R FILE", action = loadDSOnOk, width = 30)
        createOK(labTitle = "IMPORT CSV OR TXT", 
                 action = importDS, width = 30)
        createNote(labTitle = "Press the List button to see a summary", 
                   pady = c(15,0))
        createNote(labTitle = "of the loaded data sets", pady = c(5, 15))
        
        tcltk::tkpack(subPanR4C1, expand = TRUE, fill = "both")
        assign("subPanR4C1", subPanR4C1, envir = KTSEnv)
    }
    cleanEnvir()
    refreshDataSetsList(outp = FALSE)
    showPANloadDS()
}

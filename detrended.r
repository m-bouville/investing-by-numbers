#default values of parameters:
setDetrendedDefaultValues <- function() {
   def$detrendedInputDF           <<- "dat"
   def$detrendedInputName         <<- "TR"
   def$detrendedAvgOver           <<- 30L
   def$detrendedMedianAlloc       <<- 90
   def$detrendedInterQuartileAlloc<<- 97
   def$typicalDetrended           <<- paste0("detrended", def$detrendedInputName, "avg", def$detrendedAvgOver, "__", 
                                             def$detrendedMedianAlloc, "_", def$detrendedInterQuartileAlloc)
}

calcDetrended <- function(inputDF, inputName, detrendedName) {
   if (inputDF=="dat")             logInput <- log( dat[, inputName] )
   else if (inputDF=="signal")     logInput <- log( signal[, inputName] )
   else if (inputDF=="alloc")      logInput <- log( alloc[, inputName] )
   else if (inputDF=="TR")         logInput <- log( TR[, inputName] )
   else if (inputDF=="next30yrs")  logInput <- log( next30yrs[, inputName] )
   else stop("data frame ", inputDF, " not recognized")
   
   fitPara <- regression(TR$numericDate[], logInput[])
   a <- fitPara[[1]]
   b <- fitPara[[2]]
#   print( c(a, b) )
   addNumColToDat(detrendedName)
   dat[, detrendedName] <<- logInput[] - (a + b * TR$numericDate[])
}


## Average CAPE over 'avgOver' months
calcAvgDetrended <- function(detrendedName, avgOver=def$detrendedAvgOver) {

   requireColInDat(detrendedName)
   avgDetrendedName <- paste0(detrendedName, "avg", avgOver)
   addNumColToDat(avgDetrendedName)
   for(i in 1:(avgOver-1)) dat[i, avgDetrendedName] <<- NA # not enough data to calculate average
   for(i in avgOver:numData) dat[i, avgDetrendedName] <<- mean(dat[(i-avgOver+1):i, detrendedName])  
}


CalcDetrendedSignal <- function(inputDF, inputName, medianAlloc, interQuartileAlloc, strategyName, avgOver) {
   
   if (interQuartileAlloc==100) interQuartileAlloc <- 100-1e-3
   
   b <- tan(pi*(medianAlloc/100-.5))
   tan2A <- tan(pi*interQuartileAlloc/100)
   a <- sqrt(1/tan2A^2 + 1 + b^2) - 1/tan2A
   
   detrendedName <- strategyName
#   if (!detrendedName %in% colnames(dat)) 
   {
      calcDetrended(inputDF=inputDF, inputName=inputName, detrendedName=detrendedName)
      if( is.numeric(avgOver) ) {
         if( !paste0(detrendedName,"avg",avgOver) %in% colnames(dat) ) 
            calcAvgDetrended(detrendedName, avgOver)
         detrendedName <- paste0(detrendedName, "avg", avgOver) 
      }
      
      addNumColToSignal(strategyName)
      
      bearish <- quantile(dat[, detrendedName], 0.25, na.rm=T)[[1]]
      bullish <- quantile(dat[, detrendedName], 0.75, na.rm=T)[[1]]
      
      signal[, strategyName] <<- a * ( 2 * (dat[, detrendedName]-bullish) / (bearish-bullish) - 1 ) + b
   }
}


createDetrendedStrategy <- function(inputDF="dat", inputName="TR", avgOver=def$detrendedAvgOver, 
                                    medianAlloc=60, interQuartileAlloc=80,
                                    strategyName="", futureYears=def$futureYears, 
                                    tradingCost=def$tradingCost, force=F) {   
   if (strategyName=="") {
      if( is.numeric(avgOver) )
         strategyName <- paste0("detrended", inputName, "avg", avgOver, "__", medianAlloc, "_", interQuartileAlloc)
      else strategyName <- paste0("detrended", inputName, "__", medianAlloc, "_", interQuartileAlloc)
   }
   
    if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
       CalcDetrendedSignal(inputDF=inputDF, inputName=inputName, medianAlloc=medianAlloc, 
                           interQuartileAlloc=interQuartileAlloc, strategyName=strategyName, avgOver=avgOver)
      calcAllocFromSignal(strategyName)
      addNumColToTR(strategyName)  
      startIndex = avgOver # max(months, sum(is.na(alloc[ ,strategyName])))+1
      calcStrategyReturn(strategyName, startIndex)
   } 
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index]    <<- strategyName
      parameters$type[index]        <<- "detrended"
      parameters$subtype[index]     <<- inputName
      parameters$inputDF[index]     <<- inputDF
      parameters$inputName[index]   <<- inputName
      parameters$startIndex[index]  <<- startIndex
      parameters$avgOver[index]     <<- avgOver
      parameters$medianAlloc[index] <<- medianAlloc
      parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
#    calcTRnetOfTradingCost(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)      
}


searchForOptimalDetrended <- function(inputDF="dat", inputName="TR", minAvgOver=30, maxAvgOver=30, byAvgOver=0, 
                                     minMed=80, maxMed=95, byMed=5, minIQ=97, maxIQ=99, byIQ=1, 
                                     futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                     minTR=def$valueMinTR, maxVol=def$valueMaxVol, maxDD2=def$valueMaxDD2, 
                                     minTO=def$valueMinTO, CPUnumber=def$CPUnumber, force=F) {
   
   print(paste0("strategy                |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   detrendedName <- paste0("detrended", inputName)
   calcDetrended(inputDF=inputDF, inputName=inputName, detrendedName) 
   for (avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver)) {
      calcAvgDetrended(detrendedName, avgOver=avgOver)
      for ( med in seq(minMed, maxMed, by=byMed) )       
         for ( IQ in seq(minIQ, maxIQ, by=byIQ) ) {
            strategyName <- paste0(detrendedName, "avg", avgOver, "_", med, "_", IQ)
            
            createDetrendedStrategy(inputDF=inputDF, inputName=inputName, avgOver=avgOver, strategyName=strategyName, 
                                    medianAlloc=med, interQuartileAlloc=IQ, futureYears=futureYears, force=force)

            showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                   minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
         }
      plotAllReturnsVsFour()
   }
   print("")
   showSummaryForStrategy(def$typicalDetrended)
}




#default values of parameters:
setReversalDefaultValues <- function() {
   def$reversalInputDF            <<- "dat"
   def$reversalInputName          <<- "TR"
   def$reversalAvgOver            <<- 9L
   def$reversalReturnToMean       <<- 10
   def$reversalMedianAlloc        <<- 99
   def$reversalInterQuartileAlloc <<- 10
   def$typicalReversal            <<- paste0("reversal_", def$reversalInputName, "_", 
                                             def$reversalAvgOver, "__", def$reversalReturnToMean, "_", 
                                             def$reversalMedianAlloc, "_", def$reversalInterQuartileAlloc)
}


## calculating trend reversal (essentially a second derivative)
calcReversal <- function(inputDF, inputName, avgOver=def$reversalAvgOver, reversalName) {
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="signal") input <- signal[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")
   
   addNumColToDat(reversalName)
   dat[1:(2*avgOver), reversalName] <<- NA
   for(i in (2*avgOver+1):numData) 
      dat[i, reversalName] <<- log( input[i] * input[i-2*avgOver] / input[i-avgOver]^2 )
}


calcReversalSignal <- function(signal0, medianAlloc, interQuartileAlloc, strategyName) {
   
#    reversalName <- paste0("reversal_", inputName, "_", avgOver)
#    if (!reversalName %in% colnames(dat)) 
#       calcReversal(inputDF=inputDF, inputName=inputName, avgOver=avgOver, reversalName=reversalName)
   addNumColToSignal(strategyName)
   
   bearish <- quantile(signal0, 0.75, na.rm=T)[[1]] # backwards compared to others (high is good)
   bullish <- quantile(signal0, 0.25, na.rm=T)[[1]]

   if (interQuartileAlloc==100) interQuartileAlloc <- 100-1e-3
   
   b <- tan(pi*(medianAlloc/100-.5))
   tan2A <- tan(pi*interQuartileAlloc/100)
   a <- sqrt(1/tan2A^2 + 1 + b^2) - 1/tan2A
   
   signal[, strategyName] <<- a * ( 2 * (signal0-bullish) / (bearish-bullish) - 1 ) + b
}


## The trend reversal strategy does not find out whether prices are going up (time to be in the market),
## or going down (we should be out of the market).
## Instead it finds out whether a rise or fall is starting.
## So when the signal is positive, instead of _setting_ the allocation to a high value, we _increase_ its value.
## For this reason the algoritm is rather different from other strategies.
createReversalStrategy <- function(inputDF=def$reversalInputDF, inputName=def$reversalInputName, 
                                   avgOver=def$reversalAvgOver, returnToMean=def$reversalReturnToMean, 
                                   medianAlloc=def$reversalMedianAlloc, interQuartileAlloc=def$reversalInterQuartileAlloc, 
                                   strategyName="", futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   reversalName <- paste0("reversal_", inputName, "_", avgOver)
   if (strategyName=="") 
      strategyName <- paste0(reversalName, "__", returnToMean, "_", medianAlloc, "_", interQuartileAlloc)
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      if (!reversalName %in% colnames(dat)) 
         calcReversal(inputDF=inputDF, inputName=inputName, avgOver=avgOver, reversalName=reversalName)
      
      for(i in (2*avgOver+2):numData)
         dat[i, reversalName] <- dat[i, reversalName] - mean(dat[1:i-1, reversalName], na.rm=T)
      ## dat[, reversalName] = dat[, reversalName] - average of dat[, reversalName]
      ## i.e. we ensure that the average of dat[, reversalName] is 0
      ## Note that we use the average known at the time, 
      ## i.e. we do not use what would have been future information at the time.   
      
      signal0 <- numeric(numData)
      signal0 <- NA
      signal0[2*avgOver+2] <- 0
      for (i in (2*avgOver+3):numData ) 
         signal0[i] <- signal0[i-1] + dat[i, reversalName] - returnToMean/100 * signal0[i-1]
      ## 'signal0' is essentially an integral of dat[, reversalName]
      ## 'returnToMean' brings 'signal0' back towards 0 over time, to avoid a long-term drift.
      
      calcReversalSignal(signal0, medianAlloc=medianAlloc, 
                         interQuartileAlloc=interQuartileAlloc, strategyName=strategyName)      
      
      requireColInSignal(strategyName)
      addNumColToAlloc(strategyName)
      calcAllocFromSignal(strategyName)
      
      addNumColToTR(strategyName)  
      startIndex = 2*avgOver+1 + 12 # padding to allow settling down
      calcStrategyReturn(strategyName, startIndex)
   } 
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index]    <<- strategyName
      parameters$type[index]        <<- "reversal"
      parameters$subtype[index]     <<- inputName
      parameters$inputDF[index]     <<- inputDF
      parameters$inputName[index]   <<- inputName
      parameters$startIndex[index]  <<- startIndex
      parameters$medianAlloc[index] <<- medianAlloc
      parameters$interQuartileAlloc[index] <<- interQuartileAlloc
      parameters$avgOver[index]     <<- avgOver
      parameters$name1[index]       <<- "returnToMean"
      parameters$value1[index]      <<- returnToMean      
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
}


searchForOptimalReversal <-function(inputDF=def$reversalInputDF, inputName=def$reversalInputName, 
                                    minAvgOver=9L, maxAvgOver=12L, byAvgOver=3L, 
                                    minRTM=0, maxRTM=20, byRTM=5,
                                    minMed=10, maxMed=99, byMed=20, minIQ=10, maxIQ=90, byIQ=20, 
                                    futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                    minTR=def$technicalMinTR, maxVol=def$technicalMaxVol, maxDD2=2.2, 
                                    minTO=def$technicalMinTO, force=F) {
   lastTimePlotted <- proc.time()  
   print(paste0("strategy                 |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )

   for ( avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver) ) 
      for ( RTM in seq(minRTM, maxRTM, by=byRTM) ) {
         for ( med in seq(minMed, maxMed, by=byMed) ) 
            for ( IQ in seq(minIQ, maxIQ, by=byIQ) ) {
               strategyName <- paste0("reversal_", inputName, "_", avgOver, "_", RTM, "_", med, "_", IQ)
               
               createReversalStrategy(inputDF=inputDF, inputName=inputName, avgOver=avgOver, returnToMean=RTM, 
                                      medianAlloc=med, interQuartileAlloc=IQ, strategyName=strategyName, force=force)                  
               showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                      minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
            }
         if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 5 ) { # we replot only if it's been a while
            plotAllReturnsVsFour()
            lastTimePlotted <- proc.time()
         }
      }
   print("")
   showSummaryForStrategy(def$typicalReversal)
   plotAllReturnsVsFour()
}

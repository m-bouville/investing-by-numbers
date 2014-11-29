

############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
##    reversal.r generates a strategy     ##
##        based on trend reversal         ##
##   (essentially a second derivative)    ##
##                                        ##
############################################


#default values of parameters:
setReversalDefaultValues <- function() {
   def$reversalInputDF       <<- "dat"
   def$reversalInputName     <<- "TR"
   
   def$reversalAvgOver1      <<-   9L
   def$reversalReturnToMean1 <<-  13.5
   def$reversalBearish1      <<- -50
   def$reversalBullish1      <<- -50     # DD^2: 50
   def$typicalReversal1      <<- paste0("reversal_", def$reversalAvgOver1, "_", def$reversalReturnToMean1, "_", 
                                        def$reversalBearish1, "_", def$reversalBullish1)
   
   def$reversalAvgOver2      <<- 11L     # 6L
   def$reversalReturnToMean2 <<- 17      # 4.2
   def$reversalBearish2      <<-  2      # 3.6
   def$reversalBullish2      <<-  2      # 4   
   def$typicalReversal2      <<- paste0("reversal_", def$reversalAvgOver2, "_", def$reversalReturnToMean2, "_", 
                                        def$reversalBearish2, "_", def$reversalBullish2)
}


## calculating trend reversal (essentially a second derivative)
calcReversal <- function(inputDF, inputName, avgOver=def$reversalAvgOver, reversalName) {
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="signal")     input <- signal[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")
   
   addNumColToDat(reversalName)
   dat[1:(2*avgOver), reversalName] <<- NA
   for(i in (2*avgOver+1):numData) 
      dat[i, reversalName] <<- log( input[i] * input[i-2*avgOver] / input[i-avgOver]^2 )
}


calcReversalSignal <- function(rawSignal,                                
                               bearish=def$BollBearish, bullish=def$BollBullish, 
                               signalMin=def$signalMin, signalMax=def$signalMax,
                               strategyName, startIndex=2*def$reversalAvgOver+1) {
   
   bearish=bearish/100
   bullish=bullish/100

   addNumColToSignal(strategyName)
   calcSignalForStrategy(strategyName, input=rawSignal, bearish=bearish, bullish=bullish,
                         signalMin=signalMin, signalMax=signalMax, startIndex=startIndex ) 
}


## The trend reversal strategy does not find out whether prices are going up (time to be in the market),
## or going down (we should be out of the market).
## Instead it finds out whether a rise or fall is starting.
## So when the signal is positive, instead of _setting_ the allocation to a high value, we _increase_ its value.
## For this reason the algoritm is rather different from other strategies.
createReversalStrategy <- function(inputDF=def$reversalInputDF, inputName=def$reversalInputName, 
                                   avgOver=def$reversalAvgOver, returnToMean=def$reversalReturnToMean, 
                                   bearish=def$momentumBearish, bullish=def$momentumBullish, 
                                   signalMin=def$signalMin, signalMax=def$signalMax,
                                   strategyName="", type="reversal", 
                                   futureYears=def$futureYears, costs, 
                                   coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {

   reversalName <- paste0("reversal_", inputName, "_", avgOver)
   if (strategyName=="") 
      strategyName <- paste0("reversal_", avgOver, "_", returnToMean, "_", bearish, "_", bullish)
   if (bearish==bullish) bullish = bearish + 1e-3 # bear==bull creates problems
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      if (!reversalName %in% colnames(dat)) 
         calcReversal(inputDF=inputDF, inputName=inputName, avgOver=avgOver, reversalName=reversalName)
      
      for(i in (2*avgOver+2):numData)
         dat[i, reversalName] <- dat[i, reversalName] - mean(dat[1:i-1, reversalName], na.rm=T)
      ## dat[, reversalName] = dat[, reversalName] - average of dat[, reversalName]
      ## i.e. we ensure that the average of dat[, reversalName] is 0
      ## Note that we use the average known at the time, 
      ## i.e. we do not use what would have been future information at the time.   
      
      rawSignal <- numeric(numData)
      rawSignal <- NA
      rawSignal[2*avgOver+2] <- 0
      for (i in (2*avgOver+3):numData ) 
         rawSignal[i] <- rawSignal[i-1] + dat[i, reversalName] - returnToMean/100 * rawSignal[i-1]
      ## 'rawSignal' is essentially an integral of dat[, reversalName]
      ## 'returnToMean' brings 'rawSignal' back towards 0 over time, to avoid a long-term drift.
      
      if (inputName=="TR")
         startIndex <- 2*avgOver+1 + 12 # padding to allow settling down
      else {
         i=1
         while ( is.na(dat[i, inputName]) )
            i <- i+1
         startIndex <- i + 2*avgOver + 12
      }
      
      calcReversalSignal(rawSignal, bearish=bearish, bullish=bullish, 
                         signalMin=signalMin, signalMax=signalMax, 
                         startIndex=startIndex, strategyName=strategyName)      
      
      requireColInSignal(strategyName)
      addNumColToAlloc(strategyName)
      calcAllocFromSignal(strategyName)
      
      addNumColToTR(strategyName)  
      calcStrategyReturn(strategyName, startIndex)
   } 
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index]    <<- strategyName
      if (type=="search") {
         parameters$type[index]        <<- "search"
         parameters$subtype[index]     <<- "reversal"        
      } else {
         parameters$type[index]        <<- "reversal"
         parameters$subtype[index]     <<- inputName
      }
      parameters$inputDF[index]     <<- inputDF
      parameters$inputName[index]   <<- inputName
      parameters$startIndex[index]  <<- startIndex
      parameters$bearish[index]     <<- bearish
      parameters$bullish[index]     <<- bullish  
      parameters$avgOver[index]     <<- avgOver
      parameters$name1[index]       <<- "returnToMean"
      parameters$value1[index]      <<- returnToMean      
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, costs=costs,
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
}


calcOptimalReversal <- function(inputDF=def$reversalInputDF, inputName=def$reversalInputName, 
                                minAvgOver=8L, maxAvgOver=10L, byAvgOver=1L, 
                                minRTM = 10, maxRTM = 16, byRTM=1,
                                minBear=-54, maxBear=-42, byBear=2, 
                                minDelta=0,  maxDelta=2, byDelta=0.5, 
                                futureYears=def$futureYears, costs=def$tradingCost, 
                                minTR=0, maxVol=20, maxDD2=5, minTO=0.7, minScore=14.8, countOnly,
                                xMinVol, xMaxVol, xMinDD2, xMaxDD2,
                                col=F, plotType="symbols", nameLength=25, plotEvery=def$plotEvery, force=F) {
   counterTot <- 0; counterNew <- 0
   lastTimePlotted <- proc.time()  
   
   for ( avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver) ) 
      for ( RTM in seq(minRTM, maxRTM, by=byRTM) ) 
         for ( bear in seq(minBear, maxBear, by=byBear) ) {     
            for ( delta in seq(minDelta, maxDelta, by=byDelta) ) {
               bull = bear + delta
               strategyName <- paste0("reversal_", avgOver, "_", RTM, "_", bear, "_", bull)
               
               counterTot <- counterTot + 1 
               if(countOnly) {
                  if ( !(strategyName %in% colnames(TR)) | !(strategyName %in% colnames(alloc)) )
                     counterNew <- counterNew + 1                  
               } else {
                  createReversalStrategy(inputDF=inputDF, inputName=inputName, avgOver=avgOver, returnToMean=RTM, 
                                      bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                                      strategyName=strategyName, costs=costs, force=force)                  
                  showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                      minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                                      nameLength=nameLength, force=F)
               }
            }
            if ( !countOnly && (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > plotEvery ) { 
               # we replot only if it's been a while
               plotAllReturnsVsTwo(col=col, searchPlotType=plotType, 
                                   xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2)
               lastTimePlotted <- proc.time()
            }
         }
   if(countOnly)
      print (paste0("Running ", counterTot, " parameter sets (", counterNew, " new)"))
}


searchForOptimalReversal <- function(inputDF=def$reversalInputDF, inputName=def$reversalInputName, 
                                    minAvgOver=8L, maxAvgOver=10L, byAvgOver=1L,
                                    minRTM=   13,  maxRTM=    15,  byRTM=    1,
                                    minBear= -52,  maxBear=  -48,  byBear=   1,
                                    minDelta=  0,  maxDelta=   1,  byDelta=  0.2, 
                                    futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
                                    minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.7, minScore=7.2,
                                    xMinVol=13, xMaxVol=17, xMinDD2=3, xMaxDD2=8.5,
                                    col=F, plotType="symbols", nameLength=27, plotEvery=def$plotEvery, force=F) {

   # calculate how many parameter sets will be run
   calcOptimalReversal(inputDF, inputName, minAvgOver, maxAvgOver, byAvgOver, 
                       minRTM, maxRTM, byRTM, minBear, maxBear, byBear, minDelta, maxDelta, byDelta, 
                       futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore, countOnly=T,
                       xMinVol, xMaxVol, xMinDD2, xMaxDD2, col, plotType, nameLength, plotEvery, force)
   
   dashes <- displaySummaryHeader(futureYears=futureYears, nameLength=nameLength)
   
   # actually calculating
   calcOptimalReversal(inputDF, inputName, minAvgOver, maxAvgOver, byAvgOver, 
                       minRTM, maxRTM, byRTM, minBear, maxBear, byBear, minDelta, maxDelta, byDelta, 
                       futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore, countOnly=F,
                       xMinVol, xMaxVol, xMinDD2, xMaxDD2, col, plotType, nameLength, plotEvery, force)
   
   print(dashes)
   showSummaryForStrategy(def$typicalReversal1, nameLength=nameLength, costs=costs)
   showSummaryForStrategy(def$typicalReversal2, nameLength=nameLength, costs=costs)
   plotAllReturnsVsTwo(col=col, searchPlotType=plotType,
                       xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2)
}


searchForTwoOptimalReversal <-function(plotType="symbols", force=F) {
   print("Reversal 1")
   searchForOptimalReversal(minAvgOver=8L, maxAvgOver=10L, byAvgOver=1L,
                            minRTM=   13,  maxRTM=    14.5,byRTM=    0.5,
                            minBear= -53,  maxBear=  -49,  byBear=   0.5,
                            minDelta=  0,  maxDelta=   1,  byDelta=  0.5, minScore=7.85 )
   print("")
   
   print("Reversal 2")
   searchForOptimalReversal(minAvgOver=10L, maxAvgOver=12L, byAvgOver=1L,
                            minRTM=    17,  maxRTM=    21,  byRTM=    1,
                            minBear=   -2,  maxBear=    3,  byBear=   1,
                            minDelta=   0,  maxDelta=   1,  byDelta=  1, minScore=7.8 )
#    searchForOptimalReversal(minAvgOver=5L,  maxAvgOver= 7L,  byAvgOver=1L,
#                             minRTM=    4.,  maxRTM=     4.4, byRTM=    0.2,
#                             minBear=   3.,  maxBear=    4.2, byBear=   0.4,
#                             minDelta=  0,   maxDelta=   1.2, byDelta=  0.4, minScore=7.29 )
}

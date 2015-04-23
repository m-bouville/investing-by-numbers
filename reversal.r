

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
   
   # optimized with costs = 2%
   def$reversalAvgOver1      <<-  10L   #  9L    
   def$reversalReturnToMean1 <<-  17    # 13.5
   def$reversalBearish1      <<- -52.5  #-50
   def$reversalBullish1      <<- -52.5  #-50
   def$reversalYoyoOffset1   <<-  2L    #  1L
   def$reversalYoyoPenalty1  <<-  1     #  0
   typical$reversal1         <<- nameReversalStrategy(
         def$reversalInputName, def$reversalAvgOver1, def$reversalReturnToMean1,
         def$reversalBearish1,  def$reversalBullish1, def$reversalYoyoOffset1,   def$reversalYoyoPenalty1)
   
#    # optimized with costs = 2%
#    def$reversalAvgOver2      <<- 11L     # 11L
#    def$reversalReturnToMean2 <<- 21.5    # 21
#    def$reversalBearish2      <<- -0.5    #  0
#    def$reversalBullish2      <<- -0.5    #  0   
#    def$reversalYoyoOffset2   <<-  3L     #  1L
#    def$reversalYoyoPenalty2  <<-  1      #  0
#    typical$reversal2         <<- nameReversalStrategy(
#       def$reversalInputName, def$reversalAvgOver2, def$reversalReturnToMean2,
#       def$reversalBearish2,  def$reversalBullish2, def$reversalYoyoOffset2,   def$reversalYoyoPenalty2)
   
   # optimized with costs = 2%
   def$reversalAvgOver2      <<- 10L   # 6L
   def$reversalReturnToMean2 <<- 33.   # 4.7
   def$reversalBearish2      <<- 19.   # 4.6
   def$reversalBullish2      <<- 19.   # 4.6
   def$reversalYoyoOffset2   <<- 17L
   def$reversalYoyoPenalty2  <<-  1
   typical$reversal2         <<- nameReversalStrategy(
      def$reversalInputName, def$reversalAvgOver2, def$reversalReturnToMean2,
      def$reversalBearish2,  def$reversalBullish2, def$reversalYoyoOffset2,   def$reversalYoyoPenalty2)
   
   #    def$reversalAvgOver2      <<-  6L     # optimized with costs = 2%  # 6L
   #    def$reversalReturnToMean2 <<-  4.2    # 4.2
   #    def$reversalBearish2      <<-  3.4    # 3.6
   #    def$reversalBullish2      <<-  3.4    # 4   
   #    typical$reversal2      <<- paste0("reversal_", def$reversalAvgOver2, "_", def$reversalReturnToMean2, "_", 
   #                                         def$reversalBearish2, "_", def$reversalBullish2)
}



nameReversalStrategy <- function(inputName="TR", avgOver, RTM, bearish, bullish, yoyoOffset=1, yoyoPenalty=0) { 
   strategyName <- paste0("reversal_", avgOver, "_", RTM, "_", bearish, "_", bullish)
   if (yoyoOffset>=2 && yoyoPenalty>0)
      strategyName <- paste0(strategyName, "_", yoyoOffset, "_", yoyoPenalty)
   if (inputName!="TR")
      strategyName <- paste0(strategyName, "__", inputName)
   return(strategyName)
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


calcReversalSignal <- function(rawSignal, bearish=def$BollBearish, bullish=def$BollBullish, 
                               signalMin=def$signalMin, signalMax=def$signalMax,
                               strategyName, startIndex=2*def$reversalAvgOver+1, yoyoOffset=1L, yoyoPenalty=0) {
   
   bearish=bearish/100
   bullish=bullish/100
   
   addNumColToSignal(strategyName)
   calcSignalForStrategy(strategyName, input=rawSignal, bearish=bearish, bullish=bullish,
                         signalMin=signalMin, signalMax=signalMax, startIndex=startIndex,
                         yoyoOffset=yoyoOffset, yoyoPenalty=yoyoPenalty) 
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
                                   coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
                                   yoyoOffset=1L, yoyoPenalty=0, force=F) {
   
   reversalName <- paste0("reversal_", inputName, "_", avgOver)
   if(strategyName=="") 
      strategyName <- nameReversalStrategy(inputName, avgOver, returnToMean, 
                                           bearish, bullish, yoyoOffset, yoyoPenalty)
   
   if (bearish==bullish) bullish = bearish + 1e-3 # bear==bull creates problems
   
   ## Calculating startIndex
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="signal")     input <- signal[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")
   startIndex <- sum(is.na(input)) + 2*avgOver + 2
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      if (!reversalName %in% colnames(dat)) 
         calcReversal(inputDF=inputDF, inputName=inputName, avgOver=avgOver, reversalName=reversalName)
      
      for(i in startIndex:numData)
         dat[i, reversalName] <- dat[i, reversalName] - mean(dat[1:i-1, reversalName], na.rm=T)
      ## dat[, reversalName] = dat[, reversalName] - average of dat[, reversalName]
      ## i.e. we ensure that the average of dat[, reversalName] is 0
      ## Note that we use the average known at the time, 
      ## i.e. we do not use what would have been future information at the time.   
      
      rawSignal <- numeric(numData)
      rawSignal <- NA
      rawSignal[startIndex] <- 0
      for (i in (startIndex+1):numData ) 
         rawSignal[i] <- rawSignal[i-1] + dat[i, reversalName] - returnToMean/100 * rawSignal[i-1]
      ## 'rawSignal' is essentially an integral of dat[, reversalName]
      ## 'returnToMean' brings 'rawSignal' back towards 0 over time, to avoid a long-term drift.
      
      #       if (inputName=="TR")
      #          startIndex <- 2*avgOver+1 + 12 # padding to allow settling down
      #       else {
      #          i=1
      #          while ( is.na(dat[i, inputName]) )
      #             i <- i+1
      #          startIndex <- i + 2*avgOver + 12 + 100
      #       }
      
      calcReversalSignal(rawSignal, bearish=bearish, bullish=bullish, 
                         signalMin=signalMin, signalMax=signalMax, 
                         startIndex=startIndex, strategyName=strategyName,
                         yoyoOffset=yoyoOffset, yoyoPenalty=yoyoPenalty)      
      
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
      if(inputName=="TR")
         if (type=="training") {
            parameters$type[index]    <<- "training"
            parameters$subtype[index] <<- "reversal"        
         } else {
            parameters$type[index]    <<- "reversal"
            parameters$subtype[index] <<- inputName
         }
      else if(substr(inputName, 1, 4)=="CAPE")
         if (type=="training") {
            parameters$type[index]    <<- "training"
            parameters$subtype[index] <<- "reversal_CAPE"
         } else {
            parameters$type[index]    <<- "reversal_CAPE"
            parameters$subtype[index] <<- inputName
         }      
      else if(substr(inputName, 1, 9)=="detrended")
         if (type=="training") {
            parameters$type[index]    <<- "training"
            parameters$subtype[index] <<- "reversal_detrended"
         } else {
            parameters$type[index]    <<- "reversal_detrended"
            parameters$subtype[index] <<- inputName
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
         minAvgOver,    maxAvgOver,    byAvgOver,     minRTM,        maxRTM,        byRTM,
         minBear,       maxBear,       byBear,        minDelta,      maxDelta,      byDelta,
         minYoyoOffset, maxYoyoOffset, byYoyoOffset,  minYoyoPenalty, maxYoyoPenalty, byYoyoPenalty,
         futureYears=def$futureYears, costs=def$tradingCost, 
         minTR=0, maxVol=20, maxDD2=5, minTO=0.7, minScore=14.8, countOnly,
         xMinVol, xMaxVol, xMinDD2, xMaxDD2, type="reversal",
         col=F, plotType="symbols", nameLength=25, plotEvery=def$plotEvery, force=F) {
   
   counterTot <- 0; counterNew <- 0
   lastTimePlotted <- proc.time()  
   
   # creating ranges that allow to sample the parameter space broadly initially
   rangeAvgOver  <- createRange(minAvgOver,   maxAvgOver,   byAvgOver)
   rangeRTM      <- createRange(minRTM,       maxRTM,       byRTM)
   rangeBear     <- createRange(minBear,      maxBear,      byBear)
   rangeYoyoOffset<-createRange(minYoyoOffset,maxYoyoOffset,byYoyoOffset)
   rangeDelta    <- createRange(minDelta,     maxDelta,     byDelta)   
   
   for (delta in rangeDelta) {
      if (!countOnly && minDelta!=maxDelta) print(paste("  Starting delta =", delta))
      for (avgOver in rangeAvgOver) {
         if (!countOnly && minAvgOver!=maxAvgOver) print(paste("   Starting avgOver =", avgOver))
         for (RTM in rangeRTM) {
            if (!countOnly && minRTM!=maxRTM && minDelta==maxDelta && minAvgOver==maxAvgOver)
               print(paste("  Starting RTM =", RTM))
            for (bear in rangeBear) {
               bull = bear + delta
               
               for (yoyoOffset in rangeYoyoOffset) {
                  if (yoyoOffset>1) # if yoyoOffset==1 there is no yoyo penalization: no need for a whole range
                     rangeYoyoPenalty <- createRange(minYoyoPenalty, maxYoyoPenalty, byYoyoPenalty)
                  else rangeYoyoPenalty <- minYoyoPenalty 
                  
                  for (yoyoPenalty in rangeYoyoPenalty) {
                     if (yoyoOffset==1 || yoyoPenalty==0) {# if no yoyo prevention because of _either_ parameter
                        yoyoOffset<-1                     #   then no yoyo prevention because of _both_ parameter.
                        yoyoPenalty<-0                    # This prevents several equivalent parameter couples
                     }                                     #   (e.g. doing both offset=1, penalty=1 and offset=2, penalty=0)
                     
                     strategyName <- nameReversalStrategy(inputName, avgOver, RTM, bear, bull, yoyoOffset, yoyoPenalty)
                     
                     counterTot <- counterTot + 1 
                     if(countOnly) {
                        if ( !(strategyName %in% colnames(TR)) | !(strategyName %in% colnames(alloc)) )
                           counterNew <- counterNew + 1                  
                     } else {
                        createReversalStrategy(inputDF=inputDF, inputName=inputName, avgOver=avgOver, returnToMean=RTM, 
                                               bearish=bear, bullish=bull, yoyoOffset=yoyoOffset, yoyoPenalty=yoyoPenalty,
                                               signalMin=def$signalMin, signalMax=def$signalMax,
                                               strategyName=strategyName, type=type, costs=costs, force=force)                  
                        showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                               minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                                               nameLength=nameLength, force=F)
                     }
                     if ( !countOnly && (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > plotEvery ) { 
                        # we replot only if it's been a while
                        plotAllReturnsVsTwo(col=col, trainingPlotType=plotType, 
                                            xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2)
                        lastTimePlotted <- proc.time()
                     }
                  }
               }
            }
         }
      }
   }
   if(countOnly)
      print (paste0("Running ", counterTot, " parameter sets (", counterNew, " new)"))
}


searchForOptimalReversal <- function(inputDF=def$reversalInputDF, inputName=def$reversalInputName, 
         minAvgOver=    8L, maxAvgOver=   10L, byAvgOver=    1L,
         minRTM=       13,  maxRTM=       15,  byRTM=        1,
         minBear=     -52,  maxBear=     -48,  byBear=       1,
         minDelta=      0,  maxDelta=      1,  byDelta=      0.2, 
         minYoyoOffset= 1L, maxYoyoOffset= 5L, byYoyoOffset= 2L, 
         minYoyoPenalty=1,  maxYoyoPenalty=1,  byYoyoPenalty=0.4,  # 1 generally works best
         futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
         minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.7, minScore=11,
         xMinVol=13, xMaxVol=16.5, xMinDD2=4, xMaxDD2=9,
         type="training", col=F, plotType="symbols", nameLength=32, plotEvery=def$plotEvery, 
         referenceStrategies=c(typical$reversal1, typical$reversal2), force=F) {
   
   if (dataSplit != "training") 
      warning("Doing training in '", dataSplit, "' mode.", immediate.=T)
   if (costs < 1/100) 
      warning("costs = ", costs*100, "%.", immediate.=T)
   
   cleanUpStrategies()
   
   # calculate how many parameter sets will be run
   calcOptimalReversal(inputDF, inputName, 
         minAvgOver=    minAvgOver,    maxAvgOver=    maxAvgOver,    byAvgOver=    byAvgOver,
         minRTM=        minRTM,        maxRTM=        maxRTM,        byRTM=        byRTM, 
         minBear   =    minBear,       maxBear   =    maxBear,       byBear   =    byBear,
         minDelta  =    minDelta,      maxDelta  =    maxDelta,      byDelta  =    byDelta,
         minYoyoOffset= minYoyoOffset, maxYoyoOffset= maxYoyoOffset, byYoyoOffset= byYoyoOffset, 
         minYoyoPenalty=minYoyoPenalty,maxYoyoPenalty=maxYoyoPenalty,byYoyoPenalty=byYoyoPenalty,
         futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore, countOnly=T,type=type,
         xMinVol, xMaxVol, xMinDD2, xMaxDD2, col, plotType, nameLength, plotEvery, force)
   
   dashes <- displaySummaryHeader(futureYears=futureYears, nameLength=nameLength)
   
   # actually calculating
   calcOptimalReversal(inputDF, inputName, 
         minAvgOver=    minAvgOver,    maxAvgOver=    maxAvgOver,    byAvgOver=    byAvgOver,
         minRTM=        minRTM,        maxRTM=        maxRTM,        byRTM=        byRTM, 
         minBear   =    minBear,       maxBear   =    maxBear,       byBear   =    byBear,
         minDelta  =    minDelta,      maxDelta  =    maxDelta,      byDelta  =    byDelta,
         minYoyoOffset= minYoyoOffset, maxYoyoOffset= maxYoyoOffset, byYoyoOffset= byYoyoOffset, 
         minYoyoPenalty=minYoyoPenalty,maxYoyoPenalty=maxYoyoPenalty,byYoyoPenalty=byYoyoPenalty,
         futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore, countOnly=F,type=type,
         xMinVol, xMaxVol, xMinDD2, xMaxDD2, col, plotType, nameLength, plotEvery, force)
   
   print(dashes)
   for ( i in 1:length(referenceStrategies) )
      showSummaryForStrategy(referenceStrategies[i], nameLength=nameLength, costs=costs)
   plotAllReturnsVsTwo(col=col, trainingPlotType=plotType, costs=costs,
                       xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2)
}


searchForThreeOptimalReversal <-function(minScore1=14.92, minScore2=15.7, minScore3=14.9, 
                                         do1=T, do2=T, do3=T, plotType="symbols", 
                                         costs=def$tradingCost+def$riskAsCostTechnical, force=F) {
   if(do1) {
      print("Reversal 1")
      searchForOptimalReversal(minAvgOver=    8L, maxAvgOver=   11L,  byAvgOver=    1L,
                               minRTM=       15.5,maxRTM=       17.5, byRTM=        0.5,
                               minBear=     -53,  maxBear=     -51.5, byBear=       0.5,
                               minYoyoOffset= 2L, maxYoyoOffset= 3L,  byYoyoOffset= 1L, 
                               minYoyoPenalty=1,  maxYoyoPenalty=1,   byYoyoPenalty=0.4,
                               minDelta=      0,  maxDelta=      1,   byDelta=      0.5, 
                               referenceStrategies=typical$reversal1, costs=costs, minScore=minScore1 )
   }
   if( do1 && (do2||do3) ) { # needed only if there is something to seperate
      print("")
      print("****************************************************************************************************")
      print("")
   }
   if(do2) {
      print("Reversal 2")
      searchForOptimalReversal(minAvgOver=   10L, maxAvgOver=   12L, byAvgOver=    1L,
                               minRTM=       20.5,maxRTM=       22,  byRTM=        0.5,
                               minBear=      -1,  maxBear=       0.5,byBear=       0.5,
                               minYoyoOffset= 1L, maxYoyoOffset= 4L, byYoyoOffset= 1L, 
                               minYoyoPenalty=1,  maxYoyoPenalty=1,  byYoyoPenalty=0.4, 
                               minDelta=      0,  maxDelta=      0.5,byDelta=      0.5, 
                               referenceStrategies=typical$reversal2, costs=costs, minScore=minScore2 )
   }
   if( do3 && (do1||do2) ) { # needed only if there is something to seperate
      print("")
      print("****************************************************************************************************")
      print("")
   }
   if(do3) {
      print("Reversal 3")
      searchForOptimalReversal(minAvgOver=    5L,  maxAvgOver=    7L,  byAvgOver=    1L,
                               minRTM=        3.6, maxRTM=        5.2, byRTM=        0.4,
                               minBear=       2.2, maxBear=       4.2, byBear=       0.4,
                               minYoyoOffset= 3L,  maxYoyoOffset= 7L,  byYoyoOffset= 2L, 
                               minYoyoPenalty=1,   maxYoyoPenalty=1,   byYoyoPenalty=0.4, 
                               minDelta=      0,   maxDelta=      0.,  byDelta=      0.2, 
                               referenceStrategies=c(typical$reversal1, typical$reversal2), 
                               costs=costs, minScore=minScore3 )
   }
}

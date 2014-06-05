#default values of parameters:
setBollDefaultValues <- function() {
   def$BollInputDF          <<- "dat"
   def$BollInputName        <<- "TR"
   def$BollAvgOver          <<- 18L
   def$BollBearish          <<- -1
   def$BollBullish          <<- -0.3
#    def$BollMedianAlloc      <<- 99
#    def$BollInterQuartileAlloc <<- 25
   def$typicalBoll          <<- paste0("Boll_", def$BollInputName, "_", def$BollAvgOver, "__", 
                                       -def$BollBearish, "_", def$BollBullish)
}

calcBollSignal <- function(inputDF=def$BollInputDF, inputName=def$BollInputName, avgOver=def$BollAvgOver, 
                           bearish=def$BollBearish, bullish=def$BollBullish, 
                           signalMin=def$signalMin, signalMax=def$signalMax,
                           strategyName="", force=F) {

   BollBandsName <- paste0(inputName, avgOver)
   avgName <- paste0("avg_", BollBandsName)
   SDname <- paste0("SD_", BollBandsName)
   
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="signal")     input <- signal[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")
   
   #    time1 <- proc.time()
   if ( !(avgName %in% colnames(dat)) | !(SDname %in% colnames(dat)) | force) {# if data do not exist yet or we force recalculation:
      addNumColToDat(avgName)
      addNumColToDat(SDname)
      dat[1:(avgOver-1), avgName] <<- NA # not enough data to calculate average or SD
      dat[1:(avgOver-1), SDname]  <<- NA 
      for(i in avgOver:numData) {
         dat[i, avgName] <<- mean(input[(i-avgOver+1):i])  
         dat[i, SDname]  <<-   sd(input[(i-avgOver+1):i])
      }
   }      
   #    print( c( "Bollinger - calc-avg-&SD time:", round(summary(proc.time())[[1]] - time1[[1]] , 2) ) )
   
   #    time1 <- proc.time()    
   if ( !(strategyName %in% colnames(signal)) | force) {# if data do not exist yet or we force recalculation:
      rawSignal <- numeric(numData)
      rawSignal <- (input - dat[, avgName]) / dat[, SDname]
      
      #       bearish <- quantile(signal[, strategyName], 0.75, na.rm=T)[[1]]
      #       bullish <- quantile(signal[, strategyName], 0.25, na.rm=T)[[1]]
      #       if (interQuartileAlloc==100) interQuartileAlloc <- 100-1e-3
      #       
      #       b <- tan(pi*(medianAlloc/100-.5))
      #       tan2A <- tan(pi*interQuartileAlloc/100)
      #       a <- sqrt(1/tan2A^2 + 1 + b^2) - 1/tan2A
      
#       print(summary(rawSignal))
      startIndex <- sum(is.na(rawSignal)) + 1
      calcSignalForStrategy(strategyName, input=rawSignal, bearish=bearish, bullish=bullish,
                            signalMin=signalMin, signalMax=signalMax, startIndex=startIndex ) 

#       signal[, strategyName] <<- a * ( 2 * (signal[, strategyName]-bullish) / (bearish-bullish) - 1 ) + b
   }
   #    print( c( "Bollinger - compare-to-bands time:", round(summary(proc.time())[[1]] - time1[[1]] , 2) ) )
}


createBollStrategy <- function(inputDF=def$BollInputDF, inputName=def$BollInputName, avgOver=def$BollAvgOver, 
                               bearish=def$BollBearish, bullish=def$BollBullish, 
                               signalMin=def$signalMin, signalMax=def$signalMax,
                               strategyName="", type="", futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {

   if(strategyName=="")  
      strategyName <- paste0("Boll_", inputName, "_", avgOver, "__", -bearish, "_", bullish)
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      #       time0 <- proc.time()
      calcBollSignal(inputDF=inputDF, inputName=inputName, avgOver=avgOver, 
                     bearish=bearish, bullish=bullish, 
                     signalMin=signalMin, signalMax=signalMax,
                     strategyName=strategyName, force=force)
      #       print( c( "Bollinger - calcBollSignal() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )
      
      #       time0 <- proc.time()
      calcAllocFromSignal(strategyName)
      #       print( c( "Bollinger - calcAllocFromSignal() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )
            
      #       time0 <- proc.time()
      addNumColToTR(strategyName)
      startIndex <- sum(is.na(alloc[, strategyName])) + 1
      calcStrategyReturn(strategyName, startIndex)
      #       print( c( "Bollinger - calcStrategyReturn() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )
   }
   
#    time0 <- proc.time()
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      if (type=="search") {
         parameters$type[index]        <<- "search"
         parameters$subtype[index]     <<- "Bollinger"        
      } else {
         parameters$type[index]        <<- "Bollinger"
         parameters$subtype[index]     <<- inputName
      }
      parameters$startIndex[index] <<- startIndex
      parameters$inputDF[index]    <<- inputDF
      parameters$inputName[index]  <<- inputName
      parameters$bearish[index]    <<- bearish
      parameters$bullish[index]    <<- bullish      
      #       parameters$medianAlloc[index] <<-  medianAlloc
#       parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
      parameters$avgOver[index]    <<-  avgOver
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
#    calcTRnetOfTradingCost(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)      
# print( c( "Bollinger - stats time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
}


searchForOptimalBoll <- function(inputDF="dat", inputName="TR", minAvgOver=21L, maxAvgOver=21L, byAvgOver=3L, 
                                 minBear=-1.4, maxBear=-0.6, byBear=0.1, 
                                 minBull=-0.4, maxBull=0.4, byBull=0.1, 
                                 futureYears=def$futureYears, tradingCost=def$tradingCost, type="search", 
                                 minTR=def$technicalMinTR, maxVol=def$technicalMaxVol, maxDD2=def$technicalMaxDD2, 
                                 minTO=def$technicalMinTO, col=F, plotType="symbols", force=F) {
   
   lastTimePlotted <- proc.time()
   print(paste0("strategy         |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )

   for ( avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver) ) {
      for ( bear in seq(minBear, maxBear, by=byBear) ) {      
         for ( bull in seq(minBull, maxBull, by=byBull) ) {
             if (bull < bear + 1e-3 ) 
               {
               strategyName <- paste0("Boll_", inputName, "_", avgOver, "_", -bear, "_", bull)
#                if (bull > bear - 1e-3  ) bull <- bear - 1e-3 # bear=bull creates problems
               
               createBollStrategy(inputDF, inputName, avgOver=avgOver, type=type,
                                  bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                                  strategyName=strategyName, futureYears=futureYears, force=force)
               
               showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                      minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
            }
         }
         if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 5 ) { # we replot only if it's been a while
            plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
            lastTimePlotted <- proc.time()
         }
      }
   }
   print("")
   showSummaryForStrategy(def$typicalBoll)
   plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
}




############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
##    Bollinger.r generates a strategy    ##
##        based on Bollinger bands        ##
##                                        ##
############################################



#default values of parameters:
setBollDefaultValues <- function() {
   def$BollInputDF    <<- "dat"
   def$BollInputName  <<- "TR"
   
   def$BollAvgOver1    <<-  14L
   def$BollBearish1    <<- -31.5      # DD^2: -31
   def$BollBullish1    <<- -31.5      # DD^2: -31
   def$typicalBoll1    <<- paste0("Boll_", def$BollAvgOver1, "_", def$BollBearish1, "_", def$BollBullish1)
   
   def$BollAvgOver2    <<- 10L
   def$BollBearish2    <<- 46.5       # DD^2: 46
   def$BollBullish2    <<- 46.5
   def$typicalBoll2    <<- paste0("Boll_", def$BollAvgOver2, "_", def$BollBearish2, "_", def$BollBullish2)
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
   
   if ( !(avgName %in% colnames(dat)) | !(SDname %in% colnames(dat)) | force) {
      # if data do not exist yet or we force recalculation:
      addNumColToDat(avgName)
      addNumColToDat(SDname)
      dat[1:(avgOver-1), avgName] <<- NA # not enough data to calculate average or SD
      dat[1:(avgOver-1), SDname]  <<- NA 
      for(i in avgOver:numData) {
         dat[i, avgName] <<- mean(input[(i-avgOver+1):i])  
         dat[i, SDname]  <<-   sd(input[(i-avgOver+1):i])
      }
   }      

   if ( !(strategyName %in% colnames(signal)) | force) {
      # if data do not exist yet or we force recalculation:
      rawSignal <- numeric(numData)
      rawSignal <- (input - dat[, avgName]) / dat[, SDname]

      startIndex <- sum(is.na(rawSignal)) + 1
      calcSignalForStrategy(strategyName, input=rawSignal, bearish=bearish, bullish=bullish,
                            signalMin=signalMin, signalMax=signalMax, startIndex=startIndex ) 
   }
}


createBollStrategy <- function(inputDF=def$BollInputDF, inputName=def$BollInputName, avgOver=def$BollAvgOver, 
                               bearish=def$BollBearish, bullish=def$BollBullish, 
                               signalMin=def$signalMin, signalMax=def$signalMax,
                               strategyName="", type="", futureYears=def$futureYears, costs=def$tradingCost, 
                               coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {
   if(strategyName=="") {
      if (inputName=="TR")
         strategyName <- paste0("Boll_", avgOver, "_", bearish, "_", bullish)
      else
         strategyName <- paste0("Boll_", inputName, "_", avgOver, "_", bearish, "_", bullish)
   }
  
   if (bullish == bearish) bullish <- bearish + 1e-3 # bearish==bullish creates problems
   bearish <- bearish/100
   bullish <- bullish/100
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      calcBollSignal(inputDF=inputDF, inputName=inputName, avgOver=avgOver, 
                     bearish=bearish, bullish=bullish, 
                     signalMin=signalMin, signalMax=signalMax,
                     strategyName=strategyName, force=force)

      calcAllocFromSignal(strategyName)

      addNumColToTR(strategyName)
      startIndex <- sum(is.na(alloc[, strategyName])) + 1
      calcStrategyReturn(strategyName, startIndex)
   }
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      if(inputName=="TR")
         if (type=="search") {
            parameters$type[index]    <<- "search"
            parameters$subtype[index] <<- "Bollinger"        
         } else {
            parameters$type[index]    <<- "Bollinger"
            parameters$subtype[index] <<- inputName
         }
      else if(substr(inputName, 1, 4)=="CAPE")
         if (type=="search") {
            parameters$type[index]    <<- "search"
            parameters$subtype[index] <<- paste0("Boll_CAPE")
         } else {
            parameters$type[index]    <<- paste0("Boll_CAPE")
            parameters$subtype[index] <<- inputName
         }      
      else if(substr(inputName, 1, 9)=="detrended")
         if (type=="search") {
            parameters$type[index]    <<- "search"
            parameters$subtype[index] <<- paste0("Boll_detrended")
         } else {
            parameters$type[index]    <<- paste0("Boll_detrended")
            parameters$subtype[index] <<- inputName
         }
      parameters$startIndex[index] <<- startIndex
      parameters$inputDF[index]    <<- inputDF
      parameters$inputName[index]  <<- inputName
      parameters$bearish[index]    <<- bearish
      parameters$bullish[index]    <<- bullish  
      parameters$avgOver[index]    <<- avgOver
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, costs=costs,
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
}


calcOptimalBoll <- function(inputDF, inputName, minAvgOver, maxAvgOver, byAvgOver, 
                            minBear, maxBear, byBear, minDelta, maxDelta, byDelta,  
                            futureYears, costs, type, 
                            minTR, maxVol, maxDD2, minTO, minScore, coeffTR, coeffVol, coeffDD2,
                            xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly,
                            col, plotType, nameLength, plotEvery, force) {
   
   counterTot <- 0; counterNew <- 0
   lastTimePlotted <- proc.time()
   
   for ( avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver) ) {
      for ( bear in seq(minBear, maxBear, by=byBear) ) {      
         for ( delta in seq(minDelta, maxDelta, by=byDelta) ) {
            bull = bear + delta               
            if (inputName=="TR")
               strategyName <- paste0("Boll_", avgOver, "_", bear, "_", bull)
            else
               strategyName <- paste0("Boll_", inputName, "_", avgOver, "_", bear, "_", bull)
           
            
            counterTot <- counterTot + 1 
            if(countOnly) {
               if ( force || !(strategyName %in% colnames(TR)) || !(strategyName %in% colnames(alloc)) )
                  counterNew <- counterNew + 1                  
            } else {
               createBollStrategy(inputDF, inputName, avgOver=avgOver, type=type,
                               bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                               strategyName=strategyName, futureYears=futureYears, force=force)
            
               showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                   minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                                   nameLength=nameLength, force=F) 
            }
            if ( !countOnly && (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > plotEvery ) { 
               # we replot only if it's been a while
               plotAllReturnsVsTwo(col=col, searchPlotType=plotType, 
                                   xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2)
               lastTimePlotted <- proc.time()
            }
         }
      }
   }
   if(countOnly)
      print (paste0("Running ", counterTot, " parameter sets (", counterNew, " new)"))
}


searchForOptimalBoll <- function(inputDF="dat", inputName="TR", 
                                 minAvgOver= 13L, maxAvgOver= 15L, byAvgOver= 1L, 
                                 minBear=   -34,  maxBear=   -30,  byBear=    0.5, 
                                 minDelta=    0,  maxDelta=    2,  byDelta=   0.5,  
                                 futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
                                 minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.6, minScore=7.3,
                                 coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
                                 xMinVol=12.5, xMaxVol=16.5, xMinDD2=3, xMaxDD2=11,
                                 type="search", col=F, plotType="symbols", 
                                 nameLength=20, plotEvery=def$plotEvery, 
                                 referenceStrategies=c(def$typicalBoll1, def$typicalBoll2), force=F) {
 
   if (dataSplit != "search") 
      warning("Doing a search for parameters in '", dataSplit, "' mode.", immediate.=T)
   
   cleanUpStrategies()
   
   # calculate how many parameters sets will be run
   calcOptimalBoll(inputDF, inputName, minAvgOver, maxAvgOver, byAvgOver, 
                   minBear, maxBear, byBear, minDelta, maxDelta, byDelta,  
                   futureYears, costs, type, 
                   minTR, maxVol, maxDD2, minTO, minScore, coeffTR, coeffVol, coeffDD2,
                   xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly=T,
                   col, plotType, nameLength, plotEvery, force)
   
   dashes <- displaySummaryHeader(futureYears=futureYears, nameLength=nameLength)
   
   # actually calculating
   calcOptimalBoll(inputDF, inputName, minAvgOver, maxAvgOver, byAvgOver, 
                   minBear, maxBear, byBear, minDelta, maxDelta, byDelta,  
                   futureYears, costs, type, 
                   minTR, maxVol, maxDD2, minTO, minScore, coeffTR, coeffVol, coeffDD2,
                   xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly=F,
                   col, plotType, nameLength, plotEvery, force)
      
   print(dashes)
   for ( i in 1:length(referenceStrategies) )
      showSummaryForStrategy(referenceStrategies[i], nameLength=nameLength, costs=costs)
   plotAllReturnsVsTwo(col=col, searchPlotType=plotType,
                       xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2)
}


searchForTwoOptimalBoll <- function(minScore1=7, minScore2= 6, plotType="symbols", force=F) {
   print("Bollinger 1...")
   searchForOptimalBoll(minAvgOver= 13L, maxAvgOver =15L, byAvgOver=1L, 
                        minBear   =-37,  maxBear   =-25,  byBear  = .5, 
                        minDelta  =  0,  maxDelta  =  3,  byDelta = .5, minScore=minScore1)
   print("")
   print("Bollinger 2...")
   searchForOptimalBoll(minAvgOver=  9L, maxAvgOver= 12L, byAvgOver=1L, 
                        minBear   = 42,  maxBear   = 52,  byBear  = .5, 
                        minDelta  =  0,  maxDelta  =  2,  byDelta = .5, minScore=minScore2)
}


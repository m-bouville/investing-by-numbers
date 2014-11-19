

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
   def$BollBearish1    <<- -31
   def$BollBullish1    <<- -31
   def$typicalBoll1    <<- paste0("Boll_", def$BollInputName, "_", def$BollAvgOver1, "__", 
                                 def$BollBearish1, "_", def$BollBullish1)
   
   def$BollAvgOver2    <<- 10L
   def$BollBearish2    <<- 49
   def$BollBullish2    <<- 49
   def$typicalBoll2    <<- paste0("Boll_", def$BollInputName, "_", def$BollAvgOver2, "__", 
                                  def$BollBearish2, "_", def$BollBullish2)
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

   if ( !(strategyName %in% colnames(signal)) | force) {# if data do not exist yet or we force recalculation:
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
   if(strategyName=="")  
      strategyName <- paste0("Boll_", inputName, "_", avgOver, "__", bearish, "_", bullish)
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
      parameters$avgOver[index]    <<-  avgOver
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, costs=costs,
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
}


searchForOptimalBoll <- function(inputDF="dat", inputName="TR", 
                                 minAvgOver= 13L, maxAvgOver =15L, byAvgOver=1L, 
                                 minBear   =-34,  maxBear   =-30,  byBear  = 0.5, 
                                 minDelta  =  0,  maxDelta  =  2,  byDelta = 0.5,  
                                 futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, type="search", 
                                 minTR=0, maxVol=20, maxDD2=4, minTO=0.7, minScore=14.78,
                                 xMinVol=def$xMinVol, xMaxVol=17, xMinDD2=0.5, xMaxDD2=1.3,
                                 col=F, plotType="symbols", nameLength=21, plotEvery=def$plotEvery, force=F) {
   
   lastTimePlotted <- proc.time()
   print(paste0("strategy              |  TR   ", futureYears, 
                " yrs: med, 5%| vol. alloc: avg, now|TO yrs| DD^2 | score") )
   print("----------------------+-------+--------------+-------+-------------+------+------+------")
      
   for ( avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver) ) {
      for ( bear in seq(minBear, maxBear, by=byBear) ) {      
         for ( delta in seq(minDelta, maxDelta, by=byDelta) ) {
            bull = bear + delta               
            strategyName <- paste0("Boll_", inputName, "_", avgOver, "_", bear, "_", bull)
               
            createBollStrategy(inputDF, inputName, avgOver=avgOver, type=type,
                               bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                               strategyName=strategyName, futureYears=futureYears, force=force)
               
            showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                   minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                                   nameLength=nameLength, force=F)            
            if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > plotEvery ) { # we replot only if it's been a while
               plotAllReturnsVsTwo(col=col, searchPlotType=plotType, 
                                   xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2)
               lastTimePlotted <- proc.time()
            }
         }
      }
   }
   print("----------------------+-------+--------------+-------+-------------+------+------+------")
   showSummaryForStrategy(def$typicalBoll, nameLength=nameLength, costs=costs)
   plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
}


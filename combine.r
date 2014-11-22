
############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
##         combine.r combines             ##
##   the signals of several strategies    ##
##                                        ##
############################################


#default values of parameters:
setCombinedDefaultValues <- function() {
   ## When optimizing combined strategies, we value diversity through coeffEntropy.
   ##   This is to avoid idiosyncrasies: if what was the best strategy when fitting 
   ##   is bad when testing, then other strategies can counterbalance it to an extent.
   ##   Ceteris paribus, the combination of 4 strategies with similar weights is safer than a single strategy.
   def$coeffEntropyTechnical <<- 3
   def$coeffEntropyValue     <<- 3
   def$coeffEntropyBalanced  <<- 1
   
   def$technicalStrategies  <<- c(def$typicalSMA, def$typicalBoll1, def$typicalBoll2, def$typicalReversal)
   def$technicalFractions   <<- c(20, 24, 25, 31)
   def$typicalTechnical     <<- "technical"
   for (i in 1:length(def$technicalFractions) )
      def$typicalTechnical  <<- paste0(def$typicalTechnical, "_", def$technicalFractions[i])
   
   def$valueStrategies      <<- c(def$typicalCAPE1, def$typicalCAPE2, def$typicalDetrended)
   def$valueFractions       <<- c(23, 41, 36)
   def$typicalValue         <<- "value"
   for (i in 1:length(def$valueFractions) )
      def$typicalValue      <<- paste0(def$typicalValue, "_", def$valueFractions[i])
   
   def$balancedStrategies   <<- c(def$typicalTechnical, def$typicalValue)
   def$balancedFractions    <<- c(60, 40)
   def$balancedCombineMode  <<- "weighted"
   if (def$balancedCombineMode == "weighted") {
      def$typicalBalanced   <<- "balanced"
      for (i in 1:length(def$balancedFractions) )
         def$typicalBalanced<<- paste0(def$typicalBalanced, "_", def$balancedFractions[i])
   }
   else def$typicalBalanced <<- paste0("balanced_", def$balancedCombineMode)
   
   def$balancedStrategies1  <<- c(def$typicalSMA, def$typicalBoll1, def$typicalBoll2, def$typicalReversal,
                                  def$typicalCAPE1, def$typicalCAPE2, def$typicalDetrended)
   def$balancedFractions1   <<- c(12, 14.4, 15, 18.6, 9.2, 16.4, 14.4)
   # mimics: technicalFractions=c(20, 24, 25, 31); valueFractions=c(23, 41, 36); balancedFractions=c(60, 40)
   def$balancedCombineMode1 <<- "weighted"
   if (def$balancedCombineMode1 == "weighted") {
      def$typicalBalanced1   <<- "balanced"
      for (i in 1:length(def$balancedFractions1) )
         def$typicalBalanced1<<- paste0(def$typicalBalanced1, "_", def$balancedFractions1[i])
   }
   else def$typicalBalanced1 <<- paste0("balanced_", def$balancedCombineMode1)
   
   def$balancedStrategies2  <<- c(def$typicalSMA, def$typicalBoll1, def$typicalBoll2, def$typicalReversal,
                                 def$typicalCAPE1, def$typicalCAPE2, def$typicalDetrended)
   def$balancedFractions2   <<- c(11, 16, 14.5, 24, 4.5, 10, 20)      # coeffEntropy = 0.5
   #def$balancedFractions2   <<- c(13.5, 15, 14.5, 16.5, 11.5, 13, 16) # coeffEntropy = 1
   #def$balancedFractions2   <<- c(14, 15.5, 14.5, 17, 10, 12, 17)     # coeffEntropy = 2
   def$balancedCombineMode2 <<- "weighted"
   if (def$balancedCombineMode2 == "weighted") {
      def$typicalBalanced2   <<- "balanced"
      for (i in 1:length(def$balancedFractions2) )
         def$typicalBalanced2<<- paste0(def$typicalBalanced2, "_", def$balancedFractions2[i])
   }
   else def$typicalBalanced2 <<- paste0("balanced_", def$balancedCombineMode2)
   
}


## Calculate the weighted average of the signals of several strategies
calcCombinedStrategySignal_weighted <- function(inputStrategyName, fraction, strategyName, force=F) {
   
   if ( length(inputStrategyName) != length(fraction) )
      stop("inputStrategyName and fraction must have the same length.")      
   numLoops <- length(inputStrategyName)
   
   sumCoeff <- sum(fraction)
   if (abs(sumCoeff-1)>1e-6) stop(paste("Sum of coefficients must be 1, not", sumCoeff))
   
   for(i in 1:numLoops)
      requireColInSignal(inputStrategyName[i])

   if (!(strategyName %in% colnames(signal)) | force) { # if data do not exist yet or we force recalculation:   
      addNumColToSignal(strategyName)
      
   signal[, strategyName] <<- 0
   for (i in 1:numLoops) 
      if(fraction[i]!=0) 
         signal[, strategyName] <<- signal[, strategyName] + fraction[i]*signal[, inputStrategyName[i] ]
   }    
}


## Combine the signals of several strategies 
## by getting into the stock market if any of the strategies wants in.
calcCombinedStrategySignal_or <- function(inputStrategyName, strategyName, force=F) {
   
   numLoops <- length(inputStrategyName)   
   for (i in 1:numLoops) requireColInSignal(inputStrategyName[i])
   
   if (!(strategyName %in% colnames(signal)) | force) { # if data do not exist yet or we force recalculation:   
      addNumColToSignal(strategyName)
      for (i in 1:numData) {
         signal[i, strategyName] <<- max( signal[i, inputStrategyName ] )
         signal[i, strategyName] <<- max( min(signal[i, strategyName], def$signalMax), def$signalMin)
      }
   }
}


## Combine the signals of several strategies 
## by getting into the stock market if all strategies want in.
calcCombinedStrategySignal_and <- function(inputStrategyName, strategyName, force=F) {   
   
   numLoops <- length(inputStrategyName)   
   for (i in 1:numLoops) requireColInSignal(inputStrategyName[i])
   
   if (!(strategyName %in% colnames(signal)) | force) { # if data do not exist yet or we force recalculation:   
      addNumColToSignal(strategyName)
      for (i in 1:numData) {
         signal[i, strategyName] <<- min( signal[i, inputStrategyName ] )
         signal[i, strategyName] <<- max( min(signal[i, strategyName], def$signalMax), def$signalMin)
      }
   }
}
   
## Combine the signals of (exactly) 2 strategies dynamically.
## The output signal moves towards the input instead of being set to it
calcCombinedStrategySignal_dynamic <- function(inputStrategyName,
                                           strategyName, speed, force=F) {   
   
   if ( length(inputStrategyName) > 2 )
      stop("Only two strategies can be used with combineMode==\'dynamic\'.")   
   
   requireColInSignal(inputStrategyName[1])
   requireColInSignal(inputStrategyName[2])
   
   if (!(strategyName %in% colnames(signal)) | force) { # if data do not exist yet or we force recalculation:      
      ## Find which strategy is slower and which faster
      if(stats$turnover[ which(stats$strategy == inputStrategyName[1]) ] > 
            stats$turnover[ which(stats$strategy == inputStrategyName[2]) ]) {
         slowStrat <- alloc[, inputStrategyName[1] ]
         fastStrat <- alloc[, inputStrategyName[2] ]      
      } else {
         slowStrat <- alloc[, inputStrategyName[2] ]
         fastStrat <- alloc[, inputStrategyName[1] ]      
      }     
      
      addNumColToSignal(strategyName)
      startIndex <- max( sum(is.na(slowStrat)), sum(is.na(fastStrat)) ) + 1
      signal[1:(startIndex-1), strategyName] <<- NA
      signal[startIndex, strategyName] <<- ( slowStrat[startIndex] + fastStrat[startIndex] ) / 2
#       target <- NULL # which strategy we move towards
      speed <- speed/100
      
      for (i in (startIndex+1):numData) {
         minimum <- min( slowStrat[i], fastStrat[i] )
         maximum <- max( slowStrat[i], fastStrat[i] )
         #          print (c(i, minimum, maximum, signal[i-1, strategyName]))
         if( signal[i-1, strategyName] < minimum) { # if below both, increase the value of the signal
            signal[i, strategyName] <<- minimum + 1e-6 # add a bit to avoid pinning
#             target <- NULL
         }
         #signal[i-1, strategyName] - 0.2 * (signal[i-1, strategyName]-minimum)
         else if( signal[i-1, strategyName] > maximum) { # if above both, decrease the value of the signal
            signal[i, strategyName] <<- maximum - 1e-6 # add a bit to avoid pinning
#             target <- NULL
         }
         #signal[i-1, strategyName] - 0.2 * (signal[i-1, strategyName]-maximum)
         else { # if between them, go towards the one that moved away
#             if( is.null(target) ) { # if there is no target yet and we need one: we set one
#                if (abs(fastStrat[i]-signal[i-1, strategyName]) >= abs(slowStrat[i]-signal[i-1, strategyName]) )
#                   target <- "fast" # the strategy that moved away is the fast one: it is now our target
#                else target <- "slow"
#             }
#             if (target == "fast")
#                signal[i, strategyName] <<- signal[i-1, strategyName] + 
#             speed * (fastStrat[i]-signal[i-1, strategyName])
#             else if (target == "slow")
#                signal[i, strategyName] <<- signal[i-1, strategyName] + 
# speed * (slowStrat[i]-signal[i-1, strategyName])
#             else stop(target, " is not a valid value for \'target\'.")
            signal[i, strategyName] <<- signal[i-1, strategyName] + speed * (fastStrat[i]-signal[i-1, strategyName])
            #          signal[i, strategyName] <<- max(min(signal[i, strategyName], maximum), minimum)
         }
      }
   }
}


## Combine the signals of several strategies
calcCombinedStrategySignal <- function
      (inputStrategyName, fraction, 
       strategyName, subtype, combineMode="weighted", speed="", force=F) {
   
   if (combineMode=="weighted")   
      calcCombinedStrategySignal_weighted(inputStrategyName, fraction, strategyName, force)
   else if (combineMode=="or") 
      calcCombinedStrategySignal_or(inputStrategyName, strategyName=strategyName, force)
   else if (combineMode=="and") 
      calcCombinedStrategySignal_and(inputStrategyName, strategyName=strategyName, force)
   else if (combineMode=="dynamic") {
      calcCombinedStrategySignal_dynamic(inputStrategyName, strategyName=strategyName, speed=speed, force)
   }
   else stop(combineMode, " is not a valid value for combineMode.")
}


combineStrategies <- function
      (inputStrategyName, fraction="", 
       strategyName="", type="combined", subtype, combineMode="weighted", speed=0,
       futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
       coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {
   
   if ( combineMode=="weighted" && length(inputStrategyName) != length(fraction) )
      stop("inputStrategyName and fraction must have the same length.")         
   numLoops <- length(inputStrategyName)
         
   if(strategyName=="") {
      if (combineMode=="weighted") {
         strategyName <- paste0(subtype)
         for (i in 1:numLoops) strategyName <- paste0(strategyName, "_", fraction[i])
     }
      else if (combineMode=="or") 
         strategyName <- paste0(subtype, "_or") 
      else if (combineMode=="and") 
         strategyName <- paste0(subtype, "_and")      
      else if (combineMode=="dynamic") 
         strategyName <- paste0(subtype, "_dyn", speed)      
   }
   
   if (!(strategyName %in% colnames(alloc)) | force) {   
      calcCombinedStrategySignal(inputStrategyName=inputStrategyName, fraction=fraction/100, 
                                 strategyName=strategyName, subtype=subtype, combineMode=combineMode, speed=speed, force=force)
      calcAllocFromSignal(strategyName)
   }
   
   startIndex <- 0
   for (i in 1:numLoops) { # startIndex = max(startIndex_i)
      index <- parameters$startIndex[ which(parameters$strategy == inputStrategyName[i]) ]
      if ( length(index)==0 || is.na(index) ) index <- 0
      if ( index > startIndex )
      startIndex <- index
   }
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      addNumColToTR(strategyName)
      calcStrategyReturn(strategyName, startIndex)
   }
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index]   <<- strategyName
      parameters$type[index]       <<- type
      parameters$subtype[index]    <<- subtype
      parameters$startIndex[index] <<- startIndex
      
      parameters$inputStrategyName1[index] <<- inputStrategyName[1]
      parameters$inputStrategyName2[index] <<- inputStrategyName[2]
      if(numLoops >= 3) parameters$inputStrategyName3[index] <<- inputStrategyName[3]
      if(numLoops >= 4) parameters$inputStrategyName4[index] <<- inputStrategyName[4]
      if(numLoops >= 5) parameters$inputStrategyName5[index] <<- inputStrategyName[5]
      if(numLoops >= 6) parameters$inputStrategyName6[index] <<- inputStrategyName[6]
      if(numLoops >= 7) parameters$inputStrategyName7[index] <<- inputStrategyName[7]
      if (combineMode=="weighted") {
         parameters$fraction1[index] <<- fraction[1]
         parameters$fraction2[index] <<- fraction[2]
         if(numLoops >= 3) parameters$fraction3[index] <<- fraction[3]
         if(numLoops >= 4) parameters$fraction4[index] <<- fraction[4]         
         if(numLoops >= 5) parameters$fraction5[index] <<- fraction[5]
         if(numLoops >= 6) parameters$fraction6[index] <<- fraction[6]         
         if(numLoops >= 7) parameters$fraction7[index] <<- fraction[7]         
      }
      parameters$name1[index] <<- "combineMode"
      parameters$value1[index] <<- combineMode
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, costs=costs,
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   
   indexStats      <- which(stats$strategy == strategyName)
   indexParameters <- which(parameters$strategy == strategyName)
   stats$type[indexStats]    <<- parameters$type[indexParameters]
   stats$subtype[indexStats] <<- parameters$subtype[indexParameters]
   
   entropy <- 0
   if (combineMode=="weighted") 
      for (i in 1:numLoops)
         if (fraction[i]>0) entropy <- entropy - fraction[i]/100 * log(fraction[i]/100)
   stats$entropy[indexStats] <<- entropy
   
}

searchForOptimalCombined <- function
      (inputStrategyName, minF, maxF, byF,
       futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
       type="search", subtype, speed=0, coeffEntropy=def$coeffEntropy,
       minTR=0, maxVol=20, maxDD2=5, minTO=0, minScore=0, 
       coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
       CPUnumber=def$CPUnumber, plotType="dots", 
       col, combineMode="weighted", nameLength, plotEvery=def$plotEvery, 
       xMinVol=def$minVol, xMaxVol=def$maxVol, xMinDD2=def$minDD2, xMaxDD2=def$maxDD2, 
       yMin=def$yTRmin, yMax=def$yTRmax, countOnly, force=F) {
  
   counterTot <- 0; counterNew <- 0
   lastTimePlotted <- proc.time()
   maxLength <- 7
   if (dat$numericDate[numData] > 2000) # not 'search' time range
      warning("The data set goes all the way to year ", floor(dat$numericDate[numData]), immediate. = T)
   
   if ( max( length(inputStrategyName), length(minF), length(maxF), length(byF) ) != 
        min( length(inputStrategyName), length(minF), length(maxF), length(byF) ) )
      stop("inputStrategyName, minF, maxF and byF must have the same length.")
   
   numLoops <- length(inputStrategyName)
   
   while( maxF[numLoops]==0 ) { # if maxF=0 then that fraction will always be 0, we can ignore it
      minF[numLoops] = 0
      byF [numLoops] = 0
      warning(paste("Strategy number", numLoops, "will not be used (maxF=0).") )
      numLoops <- numLoops-1
   }
   if (numLoops > maxLength)
      stop(paste("Only", maxLength, "strategies can be handled.") )
   
   if(numLoops < maxLength) {
      minF[(numLoops+1):maxLength] <- 0 # pad with 0 to have maxLength elements (1 per loop)
      maxF[(numLoops+1):maxLength] <- 0 # beyond numLoops, this will give "f6 in seq(0, 0, by=0)", i.e. f6=0
      byF [(numLoops+1):maxLength] <- 0
   }

   if (combineMode=="or" | combineMode=="all" ) {
      combineStrategies(inputStrategyName, strategyName=paste0(subtype,"_or"), type=type, subtype=subtype, 
                        costs=costs, combineMode="or", force=force)   
      showSummaryForStrategy(strategyName=paste0(subtype,"_or"), futureYears=futureYears, costs=costs, 
                             minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                             coeffEntropy=coeffEntropy, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, 
                             nameLength=nameLength, force=F)
   }
   
   if (combineMode=="and" | combineMode=="all" ) {
      combineStrategies(inputStrategyName, strategyName=paste0(subtype,"_and"), type=type, subtype=subtype, 
                        costs=costs, combineMode="and", force=force)   
      showSummaryForStrategy(strategyName=paste0(subtype,"_and"), futureYears=futureYears, costs=costs, 
                             minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                             coeffEntropy=coeffEntropy, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, 
                             nameLength=nameLength, force=F)
   }

   if (combineMode=="dynamic" | (combineMode=="all" && numLoops<=2) ) {
      combineStrategies(inputStrategyName, strategyName=paste0(subtype,"_dyn",speed), type=type, subtype=subtype, 
                        costs=costs, combineMode="dynamic", speed=speed, force=force)   
      showSummaryForStrategy(strategyName=paste0(subtype,"_dyn",speed), futureYears=futureYears, costs=costs, 
                             minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                             coeffEntropy=coeffEntropy, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, 
                             nameLength=nameLength, force=F)
   }
   
   if (combineMode=="weighted" | combineMode=="all" )
      for(f1 in seq(minF[1], maxF[1], by=byF[1])) 
         for(f2 in seq(minF[2], maxF[2], by=byF[2])) 
            for(f3 in seq(minF[3], maxF[3], by=byF[3])) 
               for(f4 in seq(minF[4], maxF[4], by=byF[4])) 
                  for(f5 in seq(minF[5], maxF[5], by=byF[5])) 
                     for(f6 in seq(minF[6], maxF[6], by=byF[6]))
                        for(f7 in seq(minF[7], maxF[7], by=byF[7])) {
                           
                           f <- c(f1, f2, f3, f4, f5, f6, f7)
                           f <- f[1:numLoops]
                           if( byF[numLoops]==100 ) 
                              f[numLoops] <- 100 - sum(f[1:(numLoops-1)]) # enforcing a sum of 100%
                              
                           if ( sum(f)==100 && sum(abs(f))==100 && f[numLoops]>=minF[numLoops] && f[numLoops]<=maxF[numLoops]) { 
                              # only is the sum of weigths is 100% does it make sense
                              strategyName <- paste0(subtype)
                              for (i in 1:numLoops) strategyName <- paste0(strategyName, "_", f[i])
                              
                              counterTot <- counterTot + 1 
                              if(countOnly) {
                                 if ( !(strategyName %in% colnames(TR)) | !(strategyName %in% colnames(alloc)) )
                                    counterNew <- counterNew + 1                  
                              } else {
                                 combineStrategies(inputStrategyName, f, strategyName=strategyName, costs=costs,
                                                type=type, subtype=subtype, combineMode="weighted", force=force)
                              
                                 showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                                     minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                                                     coeffEntropy=coeffEntropy, coeffTR=coeffTR, 
                                                     coeffVol=coeffVol, coeffDD2=coeffDD2, nameLength=nameLength, force=F)
                              }
                              if ( !countOnly && (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > plotEvery ) { 
                                 # we replot only if it's been a while
                                 plotAllReturnsVsTwo(col=col, searchPlotType=plotType, costs=costs,
                                                     xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2,
                                                     yMin=yMin, yMax=yMax)
                                 lastTimePlotted <- proc.time()
                              }
                        }
                     }
   ## we replot only if we have not just plotted (literally) a second ago
   if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 1 ) 
      plotAllReturnsVsTwo(col=col, searchPlotType=plotType, costs=costs, 
                          xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2,
                          yMin=yMin, yMax=yMax) 
   if(countOnly)
      print (paste0("Running ", counterTot, " parameter sets (", counterNew, " new)"))
}


searchForOptimalTechnical <- function(inputStrategyName = c(def$typicalSMA, 
                                       def$typicalBoll1, def$typicalBoll2, def$typicalReversal), 
   minF = c(16, 14, 20, 24), 
   maxF = c(24, 28, 28, 36), 
   byF  = c( 2,  2,  2,100L), 
   futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
   type="search", subtype="technical", coeffEntropy=def$coeffEntropyTechnical, 
   minTR=0, maxVol=16, minTO=0.6, maxDD2=1.2, minScore=16,
   col=F, CPUnumber=def$CPUnumber, plotType="dots", 
   combineMode="all", nameLength=23, plotEvery=def$plotEvery, 
   xMinVol=13, xMaxVol=17, xMinDD2=0.5, xMaxDD2=1.3, force=F) {
   
   # calculate how many parameters sets will be run
   searchForOptimalCombined(inputStrategyName=inputStrategyName, minF=minF, maxF=maxF, byF=byF, 
                            futureYears=futureYears, costs=costs, type=type, subtype=subtype,  coeffEntropy=coeffEntropy,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, combineMode=combineMode, 
                            nameLength=nameLength, plotEvery=plotEvery, countOnly=T,
                            xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2, force=force) 

   print(paste0("strategy                |  TR   ", futureYears, " yrs: med, 5%| vol. alloc: avg, now|TO yrs | DD^2 | score") )
   print("------------------------+-------+--------------+-------+-------------+-------+------+------")

   # actually calculating
   searchForOptimalCombined(inputStrategyName=inputStrategyName, minF=minF, maxF=maxF, byF=byF, 
                            futureYears=futureYears, costs=costs, type=type, subtype=subtype,  coeffEntropy=coeffEntropy,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, combineMode=combineMode, 
                            nameLength=nameLength, plotEvery=plotEvery, countOnly=F,
                            xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2, force=force) 
   print("------------------------+-------+--------------+-------+-------------+-------+------+------")
   showSummaryForStrategy(def$typicalTechnical, coeffEntropy=coeffEntropy, nameLength=nameLength, costs=costs)
}

searchForOptimalValue <- function
      (inputStrategyName = c(def$typicalCAPE1, def$typicalCAPE2, def$typicalDetrended), 
       minF = c(25, 30, 37), 
       maxF = c(31, 35, 42), 
       byF =  c( 1,  1,100L), 
       futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
       type="search", subtype="value", coeffEntropy=def$coeffEntropyValue, 
       minTR=0, maxVol=20, minTO=4, maxDD2=2, minScore=8.6,
       col=F, CPUnumber=def$CPUnumber, plotType="dots", combineMode="all", 
       nameLength=17, plotEvery=def$plotEvery, 
       xMinVol=def$minVol, xMaxVol=def$maxVol, xMinDD2=def$minDD2, xMaxDD2=def$maxDD2, force=F) {
    
   # calculate how many parameters sets will be run
   searchForOptimalCombined(inputStrategyName=inputStrategyName, minF=minF, maxF=maxF, byF=byF, 
                                  futureYears=futureYears, costs=costs, type=type, subtype=subtype, coeffEntropy=coeffEntropy, 
                                  minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                                  col=col, CPUnumber=CPUnumber, plotType=plotType, combineMode=combineMode, 
                                  nameLength=nameLength, plotEvery=plotEvery, countOnly=T,
                                  xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2, force=force) 
         
   print(paste0("strategy          |  TR   ", futureYears, " yrs: med, 5%| vol. alloc: avg, now|TO yrs | DD^2 | score") )
   print("------------------+-------+--------------+-------+-------------+-------+------+------")
         
   # actually calculating
   searchForOptimalCombined(inputStrategyName=inputStrategyName, minF=minF, maxF=maxF, byF=byF, 
                            futureYears=futureYears, costs=costs, type=type, subtype=subtype, coeffEntropy=coeffEntropy, 
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, combineMode=combineMode, 
                            nameLength=nameLength, plotEvery=plotEvery, countOnly=F,
                            xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2, force=force) 

   print("------------------+-------+--------------+-------+-------------+-------+------+------")
   showSummaryForStrategy(def$typicalValue, coeffEntropy=coeffEntropy, nameLength=nameLength, costs=costs)
}

searchForOptimalBalanced <- function(
      inputStrategyName = c(def$typicalTechnical, def$typicalValue), 
       minF = c(40, 25), maxF = c(100, 100), byF = c(1, 1),
       futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
       type="search", subtype="balanced", speed=0, coeffEntropy=def$coeffEntropyBalanced, 
       minTR=5, maxVol=20, minTO=1.2, maxDD2=2.5, minScore=8.5,
       col=F, CPUnumber=def$CPUnumber, plotType="line", 
       combineMode="weighted", nameLength=15, plotEvery=def$plotEvery, 
       xMinVol=12, xMaxVol=19, xMinDD2=0.5, xMaxDD2=1.4, force=F) {

   # calculate how many parameters sets will be run
   searchForOptimalCombined(inputStrategyName=inputStrategyName, minF=minF, maxF=maxF, byF=byF, 
                            futureYears=futureYears, costs=costs, 
                            type=type, subtype=subtype, speed=speed, coeffEntropy=coeffEntropy,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, combineMode=combineMode, 
                            nameLength=nameLength, plotEvery=plotEvery, countOnly=T,
                            xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2, force=force) 
   
   print(paste0("strategy        |  TR   ", futureYears, " yrs: med, 5%| vol. alloc: avg, now|TO yrs | DD^2 | score") )
   print("----------------+-------+--------------+-------+-------------+-------+------+------")
   
   # actually calculating
   searchForOptimalCombined(inputStrategyName=inputStrategyName, minF=minF, maxF=maxF, byF=byF, 
                            futureYears=futureYears, costs=costs, 
                            type=type, subtype=subtype, speed=speed, coeffEntropy=coeffEntropy,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, combineMode=combineMode, 
                            nameLength=nameLength, plotEvery=plotEvery, countOnly=F,
                            xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2, force=force) 

   print("----------------+-------+--------------+-------+-------------+-------+------+------")
   showSummaryForStrategy(def$typicalBalanced, costs=costs, coeffEntropy=coeffEntropy, nameLength=nameLength)
}

## Calculating the balanced strategy directly, without the technical and value intermediaries
searchForOptimalBalanced2 <- function
(inputStrategyName = c(def$typicalSMA, def$typicalBoll1, def$typicalBoll2, def$typicalReversal, 
                       def$typicalCAPE1, def$typicalCAPE2, def$typicalDetrended), 
 minF = c(13.5, 15, 14, 16.5,  9.5, 11.5, 16.5), 
 maxF = c(14.5, 16, 15, 17.5, 10.5, 12.5, 17.5),  #16, 14, 14.5, 22, 15, 15, 12
 byF =  c(rep(.5, 6), .5), 
 futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
 type="search", subtype="balanced", coeffEntropy=def$coeffEntropyValue, 
 minTR=5, maxVol=20, minTO=1.2, maxDD2=2.5, minScore=11.7,
 col=F, CPUnumber=def$CPUnumber, plotType="dots", combineMode="all", 
 nameLength=41, plotEvery=def$plotEvery, 
 xMinVol=12, xMaxVol=19, xMinDD2=0.5, xMaxDD2=1.4, yMin=9, yMax=9.8, force=F) {
   
   # calculate how many parameters sets will be run
   searchForOptimalCombined(inputStrategyName=inputStrategyName, minF=minF, maxF=maxF, byF=byF, 
                            futureYears=futureYears, costs=costs, type=type, subtype=subtype, coeffEntropy=coeffEntropy, 
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, combineMode=combineMode, 
                            nameLength=nameLength, plotEvery=plotEvery, countOnly=T,
                            xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2, force=force) 
   
   print(paste0("strategy                                  |  TR   ", futureYears, 
                " yrs: med, 5%| vol. alloc: avg, now|TO yrs | DD^2 | score ") )
   dashes <- "------------------------------------------+-------+--------------+-------+-------------+-------+------+-------"
   print(dashes)
   
   # actually calculating
   searchForOptimalCombined(inputStrategyName=inputStrategyName, minF=minF, maxF=maxF, byF=byF, 
                            futureYears=futureYears, costs=costs, type=type, subtype=subtype, coeffEntropy=coeffEntropy, 
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, combineMode=combineMode, 
                            nameLength=nameLength, plotEvery=plotEvery, countOnly=F,
                            xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2, 
                            yMin=yMin, yMax=yMax, force=force)    
   print(dashes)
   #showSummaryForStrategy(def$typicalBalanced, costs=costs, coeffEntropy=25, nameLength=nameLength)
   #the balanced strategy from T and V has a different entropy and cannot be directly compared.
   showSummaryForStrategy(def$typicalBalanced1, costs=costs, coeffEntropy=coeffEntropy, nameLength=nameLength)
   showSummaryForStrategy(def$typicalBalanced2, costs=costs, coeffEntropy=coeffEntropy, nameLength=nameLength)
}


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
   def$coeffEntropy               <<- 2
   
   def$valueFractionCAPE1         <<- 10
   def$valueFractionCAPE2         <<- 20
   def$valueFractionDetrended1    <<- 35
   def$valueFractionDetrended2    <<- 35
   def$typicalValue               <<- paste0("value_", def$valueFractionCAPE1, "_", def$valueFractionCAPE2, "_", 
                                             def$valueFractionDetrended1, "_", def$valueFractionDetrended2)
   
   def$technicalFractionSMA1     <<- 33
   def$technicalFractionSMA2     <<- 15
   def$technicalFractionBoll     <<- 20
   def$technicalFractionReversal <<- 32
   def$typicalTechnical          <<- paste0("technical_", def$technicalFractionSMA1, "_", def$technicalFractionSMA2, "_", 
                                            def$technicalFractionBoll, "_", def$technicalFractionReversal)
   
   def$balancedFractionTechnical <<- 60
   def$balancedFractionValue     <<- 40
   def$balancedCombineMode       <<- "weighted"
   if (def$balancedCombineMode == "weighted")
      def$typicalBalanced        <<- paste0("balanced_", def$balancedFractionTechnical, "_", def$balancedFractionValue)
   else def$typicalBalanced      <<- paste0("balanced_", def$balancedCombineMode)
}


## Calculate the weighted average of the signals of up to 4 strategies
calcCombinedStrategySignal_weighted <- function(inputStrategyName1, inputStrategyName2, 
                                                inputStrategyName3, inputStrategyName4, 
                                                fraction1, fraction2, fraction3, fraction4, 
                                                strategyName, force=F) {
   
   sumCoeff <- fraction1 + fraction2 + fraction3 + fraction4
   if (abs(sumCoeff-1)>1e-6) stop(paste("Sum of coefficients must be 1, not", sumCoeff))
   
   requireColInSignal(inputStrategyName1)
   requireColInSignal(inputStrategyName2)
   if (!(inputStrategyName3 %in% colnames(signal)) & fraction3 != 0) 
      stop(paste0("signal$", inputStrategyName3, " does not exist."))
   if (!(inputStrategyName4 %in% colnames(signal)) & fraction4 != 0) 
      stop(paste0("signal$", inputStrategyName4, " does not exist."))

   if (!(strategyName %in% colnames(signal)) | force) { # if data do not exist yet or we force recalculation:   
      addNumColToSignal(strategyName)
      
   if(fraction1!=0) {
      signal[, strategyName] <<- fraction1*signal[, inputStrategyName1] 
      if(fraction2!=0) 
         signal[, strategyName] <<- signal[, strategyName] + fraction2*signal[, inputStrategyName2]
   } else 
      if(fraction2!=0) {
         signal[, strategyName] <<- signal[, strategyName] + fraction2*signal[, inputStrategyName2]
      }
   else error("One of fraction1 and fraction2 must be > 0.")
   if(fraction3!=0) signal[, strategyName] <<- signal[, strategyName] + fraction3*signal[, inputStrategyName3] 
   if(fraction4!=0) signal[, strategyName] <<- signal[, strategyName] + fraction4*signal[, inputStrategyName4] 
   
#       if(fraction4==0) {
#          if(fraction3==0) # weigthed average with only fractions 1 and 2
#             signal[, strategyName] <<- fraction1*signal[, inputStrategyName1] + fraction2*signal[, inputStrategyName2]
#          else # weigthed average with only the first three fractions  
#             signal[, strategyName] <<- fraction1*signal[, inputStrategyName1] + 
#             fraction2*signal[, inputStrategyName2] + fraction3*signal[, inputStrategyName3]
#       }
#       else # weigthed average with all four fractions
#          signal[, strategyName] <<- fraction1*signal[, inputStrategyName1] + fraction2*signal[, inputStrategyName2] + 
#          fraction3*signal[, inputStrategyName3] + fraction4*signal[, inputStrategyName4] 
   }
}


## Combine the signals of up to 4 strategies 
## by getting into the stock market if any of the strategies wants in.
calcCombinedStrategySignal_or <- function(inputStrategyName1, inputStrategyName2, 
                                          inputStrategyName3, inputStrategyName4, 
                                          strategyName, force=F) {
   
   if ( inputStrategyName1 != "" ) requireColInSignal(inputStrategyName1)
   if ( inputStrategyName2 != "" ) requireColInSignal(inputStrategyName2)
   if ( inputStrategyName3 != "" ) requireColInSignal(inputStrategyName3)
   if ( inputStrategyName4 != "" ) requireColInSignal(inputStrategyName4)
   
   if (!(strategyName %in% colnames(signal)) | force) { # if data do not exist yet or we force recalculation:   
      addNumColToSignal(strategyName)
      
      if(inputStrategyName4 == "") {
         if(inputStrategyName3 == "") # with only 2 input strategies
            for (i in 1:numData) 
               signal[i, strategyName] <<- max( signal[i, inputStrategyName1], signal[i, inputStrategyName2] )
         else # with only the first three input strategies  
            for (i in 1:numData) 
               signal[i, strategyName] <<- max( signal[i, inputStrategyName1], 
                                                signal[i, inputStrategyName2], signal[i, inputStrategyName3] )
      }
      else # with all four input strategies
         for (i in 1:numData) 
            signal[i, strategyName] <<- max( signal[i, inputStrategyName1], signal[i, inputStrategyName2], 
                                             signal[i, inputStrategyName3], signal[i, inputStrategyName4] )
      
      for (i in 1:numData)
         signal[i, strategyName] <<- max( min(signal[i, strategyName], def$signalMax), def$signalMin)
   }
}


## Combine the signals of up to 4 strategies 
## by getting into the stock market if all strategies want in.
calcCombinedStrategySignal_and <- function(inputStrategyName1, inputStrategyName2, 
                                           inputStrategyName3, inputStrategyName4, 
                                           strategyName, force=F) {   
   
   if ( inputStrategyName1 != "" ) requireColInSignal(inputStrategyName1)
   if ( inputStrategyName2 != "" ) requireColInSignal(inputStrategyName2)
   if ( inputStrategyName3 != "" ) requireColInSignal(inputStrategyName3)
   if ( inputStrategyName4 != "" ) requireColInSignal(inputStrategyName4)
   
   if (!(strategyName %in% colnames(signal)) | force) { # if data do not exist yet or we force recalculation:   
      addNumColToSignal(strategyName)
      
      if(inputStrategyName4 == "") {
         if(inputStrategyName3 == "") # with only 2 input strategies
            for (i in 1:numData) 
               signal[i, strategyName] <<- min( signal[i, inputStrategyName1], signal[i, inputStrategyName2] )
         else # with only the first three input strategies  
            for (i in 1:numData) 
               signal[i, strategyName] <<- min( signal[i, inputStrategyName1], 
                                                signal[i, inputStrategyName2], signal[i, inputStrategyName3] )
      }
      else # with all four input strategies
         for (i in 1:numData) 
            signal[i, strategyName] <<- min( signal[i, inputStrategyName1], signal[i, inputStrategyName2], 
                                             signal[i, inputStrategyName3], signal[i, inputStrategyName4] )
      
      for (i in 1:numData)
         signal[i, strategyName] <<- max( min(signal[i, strategyName], def$signalMax), def$signalMin)
   }
}
   
## Combine the signals of (exactly) 2 strategies dynamically.
## The output signal moves towards the input instead of being set to it
calcCombinedStrategySignal_dynamic <- function(inputStrategyName1, inputStrategyName2,
                                           strategyName, speed, force=F) {   
   
   requireColInSignal(inputStrategyName1)
   requireColInSignal(inputStrategyName2)
   
   if (!(strategyName %in% colnames(signal)) | force) { # if data do not exist yet or we force recalculation:      
      ## Find which strategy is slower and which faster
      if(stats$turnover[ which(stats$strategy == inputStrategyName1) ] > 
            stats$turnover[ which(stats$strategy == inputStrategyName2) ]) {
         slowStrat <- alloc[, inputStrategyName1]
         fastStrat <- alloc[, inputStrategyName2]      
      } else {
         slowStrat <- alloc[, inputStrategyName2]
         fastStrat <- alloc[, inputStrategyName1]      
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


## Combine the signals of up to 4 strategies
calcCombinedStrategySignal <- function
      (inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
       fraction1, fraction2, fraction3, fraction4, 
       strategyName, subtype, combineMode="weighted", speed="", force=F) {
   
   if (combineMode=="weighted")   
      calcCombinedStrategySignal_weighted(inputStrategyName1, inputStrategyName2, 
                                          inputStrategyName3, inputStrategyName4, 
                                          fraction1, fraction2, fraction3, fraction4, 
                                          strategyName, force)
   else if (combineMode=="or") 
      calcCombinedStrategySignal_or(inputStrategyName1, inputStrategyName2, 
                                    inputStrategyName3, inputStrategyName4, 
                                    strategyName=strategyName, force)
   else if (combineMode=="and") 
      calcCombinedStrategySignal_and(inputStrategyName1, inputStrategyName2, 
                                     inputStrategyName3, inputStrategyName4, 
                                     strategyName=strategyName, force)
   else if (combineMode=="dynamic") {
      if (inputStrategyName3=="" & inputStrategyName4=="")
         calcCombinedStrategySignal_dynamic(inputStrategyName1, inputStrategyName2, 
                                            strategyName=strategyName, speed=speed, force)
      else stop("Only two strategies can be used with combineMode==\'dynamic\'.")
   }
   else stop(combineMode, " is not a valid value for combineMode.")
}


combineStrategies <- function
      (inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
       fraction1=25, fraction2=25, fraction3=25, fraction4=25, 
       strategyName="", type="combined", subtype, combineMode="weighted", speed=0,
       futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
       coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {
   
   if(strategyName=="") {
      if (combineMode=="weighted") {
         if(fraction4==0) {
            if(fraction3==0) # name with only fractions 1 and 2
               strategyName <- paste0(subtype, "_", fraction1, "_", fraction2) 
            else # name with only the first three fractions
               strategyName <- paste0(subtype, "_", fraction1, "_", fraction2, "_", fraction3) 
         }
         else # name with all four fractions
            strategyName <- paste0(subtype, "_", fraction1, "_", fraction2, "_", fraction3, "_", fraction4)
      }
      else if (combineMode=="or") 
         strategyName <- paste0(subtype, "_or") 
      else if (combineMode=="and") 
         strategyName <- paste0(subtype, "_and")      
      else if (combineMode=="dynamic") 
         strategyName <- paste0(subtype, "_dyn", speed)      
   }
   
   if (!(strategyName %in% colnames(alloc)) | force) {   
      calcCombinedStrategySignal(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2, 
                                 inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                                 fraction1=fraction1/100, fraction2=fraction2/100, fraction3=fraction3/100, fraction4=fraction4/100, 
                                 strategyName=strategyName, subtype=subtype, combineMode=combineMode, speed=speed, force=force)
      calcAllocFromSignal(strategyName)
   }
   
   startIndex <- max( parameters$startIndex[ which(parameters$strategy == inputStrategyName1) ],
                      parameters$startIndex[ which(parameters$strategy == inputStrategyName2) ],
                      parameters$startIndex[ which(parameters$strategy == inputStrategyName3) ],
                      parameters$startIndex[ which(parameters$strategy == inputStrategyName4) ] )
   
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
      
      parameters$inputStrategyName1[index] <<- inputStrategyName1
      parameters$inputStrategyName2[index] <<- inputStrategyName2
      parameters$inputStrategyName3[index] <<- inputStrategyName3
      parameters$inputStrategyName4[index] <<- inputStrategyName4
      if (combineMode=="weighted") {
         parameters$fraction1[index] <<- fraction1
         parameters$fraction2[index] <<- fraction2
         parameters$fraction3[index] <<- fraction3
         parameters$fraction4[index] <<- fraction4         
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
   if (fraction1>0) entropy   <- entropy - fraction1/100 * log(fraction1/100)
   if (fraction2>0) entropy   <- entropy - fraction2/100 * log(fraction2/100)
   if (fraction3>0) entropy   <- entropy - fraction3/100 * log(fraction3/100)
   if (fraction4>0) entropy   <- entropy - fraction4/100 * log(fraction4/100)
   stats$entropy[indexStats] <<- entropy
}


searchForOptimalCombined <- function
      (inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
       minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
       minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4,
       futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
       type="search", subtype, speed=0, coeffEntropy=def$coeffEntropy,
       minTR=0, maxVol=20, maxDD2=5, minTO=0, minScore=0, 
       coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
       CPUnumber=def$CPUnumber, plotType="dots", 
       col, combineMode="weighted", force=F) {
  
   lastTimePlotted <- proc.time()
   
   if (combineMode=="or" | combineMode=="all" ) {
      combineStrategies(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                        strategyName=paste0(subtype,"_or"), type=type, subtype=subtype, 
                        costs=costs, combineMode="or", force=force)   
      showSummaryForStrategy(strategyName=paste0(subtype,"_or"), futureYears=futureYears, costs=costs, 
                             minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                             coeffEntropy=coeffEntropy, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=F)
   }
   
   if (combineMode=="and" | combineMode=="all" ) {
      combineStrategies(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                        strategyName=paste0(subtype,"_and"), type=type, subtype=subtype, 
                        costs=costs, combineMode="and", force=force)   
      showSummaryForStrategy(strategyName=paste0(subtype,"_and"), futureYears=futureYears, costs=costs, 
                             minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                             coeffEntropy=coeffEntropy, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=F)
   }

   if (combineMode=="dynamic" | combineMode=="all" ) {
      combineStrategies(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                        strategyName=paste0(subtype,"_dyn",speed), type=type, subtype=subtype, 
                        costs=costs, combineMode="dynamic", speed=speed, force=force)   
      showSummaryForStrategy(strategyName=paste0(subtype,"_dyn",speed), futureYears=futureYears, costs=costs, 
                             minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                             coeffEntropy=coeffEntropy, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=F)
   }
   
   if (combineMode=="weighted" | combineMode=="all" )
      for(f1 in seq(minF1, maxF1, by=byF1)) 
         for(f2 in seq(minF2, maxF2, by=byF2)) {
            for(f3 in seq(minF3, maxF3, by=byF3)) {
               f4 <- round(100 - f1 - f2 - f3, 2)
               if ((f4 >= minF4) & (f4 <= maxF4)) {
                  strategyName <- paste0(subtype)
                  if (maxF1 > 0) strategyName <- paste0(strategyName, "_", f1)
                  if (maxF2 > 0) strategyName <- paste0(strategyName, "_", f2)
                  if (maxF3 > 0) strategyName <- paste0(strategyName, "_", f3)
                  if (maxF4 > 0) strategyName <- paste0(strategyName, "_", f4)
                  
                  combineStrategies(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                    f1, f2, f3, f4, strategyName=strategyName, costs=costs,
                                    type=type, subtype=subtype, combineMode="weighted", force=force)
                  
                  showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                         minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                                         coeffEntropy=coeffEntropy, coeffTR=coeffTR, 
                                         coeffVol=coeffVol, coeffDD2=coeffDD2, force=F)
                  if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 5 ) { # we replot only if it's been a while
                     plotAllReturnsVsTwo(col=col, searchPlotType=plotType, costs=costs)
                     lastTimePlotted <- proc.time()
                  }
               }
            }
         }
   ## we replot only if we have not just plotted (literally) a second ago
   if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 1 ) 
      plotAllReturnsVsTwo(col=col, searchPlotType=plotType, costs=costs) 
}


searchForOptimalTechnical <- function
         (inputStrategyName1=def$typicalSMA1, inputStrategyName2=def$typicalSMA2,
          inputStrategyName3=def$typicalBoll, inputStrategyName4=def$typicalReversal, 
          minF1=28L, maxF1=40L, byF1=2L, minF2=8L, maxF2=20L, byF2=2L, 
          minF3=12L, maxF3=28L, byF3=2L, minF4=24L, maxF4=38L, 
          futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
          type="search", subtype="technical", coeffEntropy=def$coeffEntropy, 
          minTR=8, maxVol=16, maxDD2=1.2, minTO=1, minScore=16.2,
          col=F, CPUnumber=def$CPUnumber, plotType="dots", 
          combineMode="weighted", force=F) {

print(paste0("strategy              |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   searchForOptimalCombined(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2,
                            inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                            minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                            minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4, 
                            futureYears=futureYears, costs=costs, type=type, subtype=subtype,  coeffEntropy=coeffEntropy,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, combineMode=combineMode, force=force) 
   print("")
   showSummaryForStrategy(def$typicalTechnical, coeffEntropy=coeffEntropy, costs=costs)
}

searchForOptimalValue <- function
      (inputStrategyName1=def$typicalCAPE1, inputStrategyName2=def$typicalCAPE2, 
       inputStrategyName3=def$typicalDetrended1, inputStrategyName4=def$typicalDetrended2, 
       minF1= 6L, maxF1=16L, byF1=2L, minF2=16L, maxF2=24L, byF2=2L, 
       minF3=32L, maxF3=44L, byF3=2L, minF4=28L, maxF4=40L, 
       futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
       type="search", subtype="value", coeffEntropy=def$coeffEntropy, 
       minTR=0, maxVol=20, maxDD2=2, minTO=4, minScore=14.78,
       col=F, CPUnumber=def$CPUnumber, plotType="dots", combineMode="weighted", force=F) {
    
   print(paste0("strategy          |  TR   |", futureYears, " yrs: med, 5% | vol.  |alloc: avg, now|TO yrs| DD^2 | score") )
   searchForOptimalCombined(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2,
                            inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                            minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                            minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4, 
                            futureYears=futureYears, costs=costs, type=type, subtype=subtype, coeffEntropy=coeffEntropy, 
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, combineMode=combineMode, force=force) 
   print("")
   showSummaryForStrategy(def$typicalValue,  coeffEntropy=coeffEntropy, costs=costs)
}

searchForOptimalBalanced <- function
      (inputStrategyName1=def$typicalTechnical, inputStrategyName2=def$typicalValue, 
       inputStrategyName3="", inputStrategyName4="", 
       minF1=20L, maxF1=100L, byF1=1L, minF2=24L, maxF2=100L, byF2=1L, 
       minF3=0L, maxF3=0L, byF3=1L, minF4=0L, maxF4=0L, 
       futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
       type="search", subtype="balanced", speed=0, coeffEntropy=def$coeffEntropy, 
       minTR=5, maxVol=20, maxDD2=2.5, minTO=1.5, minScore=14.65,
       col=F, CPUnumber=def$CPUnumber, plotType="line", 
       combineMode="weighted", force=F) {
   totTime <- proc.time()
   print(paste0("strategy       |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score") )
   
   searchForOptimalCombined(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2,
                            inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                            minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                            minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4, 
                            futureYears=futureYears, costs=costs, 
                            type=type, subtype=subtype, speed=speed, coeffEntropy=coeffEntropy,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, combineMode=combineMode, force=force) 
   print("")
   showSummaryForStrategy(def$typicalBalanced, costs=costs, coeffEntropy=coeffEntropy)
   
   print( paste("time for searchForOptimalBalanced():", round(summary(proc.time())[[3]] - totTime[[3]] , 2), " s." ) )   
}


#default values of parameters:
setCombinedDefaultValues <- function() {
#    def$valueMaxVol     <<- 14.5
#    def$technicalMaxVol <<- 14.5
#    def$valueMaxDD2     <<- 2
#    def$technicalMaxDD2 <<- 1.8
#    def$valueMinTO      <<- 8
#    def$technicalMinTO  <<- 1.3
#    def$valueMinTR      <<- 6.5
#    def$technicalMinTR  <<- 6.5
   
   ## All strategies get at least 12%, so the combined strategy actually include all four contributions
   ## (this is to avoid idiosyncrasies: if what was the best strategy when fitting 
   ## is bad when testing, then other strategies can counterbalance it to an extent)
   ## It turns out that detrended1 is the best of the four
   def$valueFractionCAPE1         <<- 16
   def$valueFractionCAPE2         <<- 16
   def$valueFractionDetrended1    <<- 56
   def$valueFractionDetrended2    <<- 12
   def$typicalValue               <<- paste0("value_", def$valueFractionCAPE1, "_", def$valueFractionCAPE2, "_", 
                                             def$valueFractionDetrended1, "_", def$valueFractionDetrended2)
   
   def$technicalFractionSMA      <<- 16
   def$technicalFractionBoll     <<- 24
   def$technicalFractionReversal <<- 60
   def$typicalTechnical          <<- paste0("technical_", def$technicalFractionSMA, "_", def$technicalFractionBoll,
                                            "_", def$technicalFractionReversal)
   
   def$balancedFractionValue     <<- 67
   def$balancedFractionTechnical <<- 33
   def$typicalBalanced          <<- paste0("balanced_", def$balancedFractionValue, "_", def$balancedFractionTechnical)
}


## Calculating average alloc between 2 strategies, and corresponding results
calcCombinedStrategySignal <- function(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
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
      
      if(fraction4==0) {
         if(fraction3==0) # weigthed average with only fractions 1 and 2
            signal[, strategyName] <<- fraction1*signal[, inputStrategyName1] + fraction2*signal[, inputStrategyName2]
         else # weigthed average with only the first three fractions  
            signal[, strategyName] <<- fraction1*signal[, inputStrategyName1] + 
            fraction2*signal[, inputStrategyName2] + fraction3*signal[, inputStrategyName3]
      }
      else # weigthed average with all four fractions
         signal[, strategyName] <<- fraction1*signal[, inputStrategyName1] + fraction2*signal[, inputStrategyName2] + 
         fraction3*signal[, inputStrategyName3] + fraction4*signal[, inputStrategyName4] 
   }
}


combineStrategies <- function(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                              fraction1=25, fraction2=25, fraction3=25, fraction4="", 
                              strategyName="", type="combined", subtype, 
                              futureYears=def$futureYears, tradingCost=def$tradingCost, 
                              coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {
   
   if (!is.numeric(fraction1)) fraction1 <- 100 - fraction2 - fraction3 - fraction4
   if (!is.numeric(fraction2)) fraction2 <- 100 - fraction1 - fraction3 - fraction4
   if (!is.numeric(fraction3)) fraction3 <- 100 - fraction1 - fraction2 - fraction4
   if (!is.numeric(fraction4)) fraction4 <- 100 - fraction1 - fraction2 - fraction3
   sumCoeff <- fraction1 + fraction2 + fraction3 + fraction4
   if (abs(sumCoeff-100)>1e-6) stop(paste("Sum of coefficients must be 100, not", sumCoeff))
   
   if(strategyName=="") {
      if(fraction4==0) {
         if(fraction3==0) # name with only fractions 1 and 2
            strategyName <- paste0(subtype, "_", fraction1, "_", fraction2) #, "__", medianAlloc, "_", interQuartileAlloc)
         else # name with only the first three fractions
            strategyName <- paste0(subtype, "_", fraction1, "_", fraction2, "_", fraction3) #, "__", medianAlloc, "_", interQuartileAlloc)
      }
      else # name with all four fractions
         strategyName <- paste0(subtype, "_", fraction1, "_", fraction2, "_", fraction3, "_", fraction4) #, "__", medianAlloc, "_", interQuartileAlloc)
   }
   
   if (!(strategyName %in% colnames(alloc)) | force) {      
      calcCombinedStrategySignal(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2, 
                                 inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                                 fraction1=fraction1/100, fraction2=fraction2/100, fraction3=fraction3/100, fraction4=fraction4/100, 
                                 strategyName=strategyName, force=force)
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
      if ( !(strategyName %in% row.names(parameters) ) ) {
         #          print(nrow(parameters))
         parameters[nrow(parameters)+1, ] <<- NA
         #          print(nrow(parameters))
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index] <<- strategyName
      parameters$type[index] <<- type
      parameters$subtype[index] <<- subtype
      parameters$startIndex[index] <<- startIndex
      
      parameters$inputStrategyName1[index] <<- inputStrategyName1
      parameters$fraction1[index] <<- fraction1
      parameters$inputStrategyName2[index] <<- inputStrategyName2
      parameters$fraction2[index] <<- fraction2
      parameters$inputStrategyName3[index] <<- inputStrategyName3
      parameters$fraction3[index] <<- fraction3
      if (fraction4 > 0) {
         parameters$inputStrategyName4[index] <<- inputStrategyName4
         parameters$fraction4[index] <<- fraction4
      }
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, 
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
   #    calcTRnetOfTradingCost(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)      
}


searchForOptimalCombined <- function(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                     minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                                     minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4,
                                     futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                     type="search", subtype,
                                     minTR=0, maxVol=20, maxDD2=5, minTO=0, minScore=0, 
                                     coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
                                     CPUnumber=def$CPUnumber, plotType="dots", col, force=F) {
   
   if (CPUnumber > 1) 
      stop("Parallelization does not work.")
      
   lastTimePlotted <- proc.time()
   
   for(f1 in seq(minF1, maxF1, by=byF1)) 
      for(f2 in seq(minF2, maxF2, by=byF2)) {
         for(f3 in seq(minF3, maxF3, by=byF3)) {
            f4 <- round(100 - f1 - f2 - f3)
            if ((f4 >= minF4) & (f4 <= maxF4)) {
               strategyName <- paste0(subtype)
               if (maxF1 > 0) strategyName <- paste0(strategyName, "_", f1)
               if (maxF2 > 0) strategyName <- paste0(strategyName, "_", f2)
               if (maxF3 > 0) strategyName <- paste0(strategyName, "_", f3)
               if (maxF4 > 0) strategyName <- paste0(strategyName, "_", f4)
               
               combineStrategies(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                 f1, f2, f3, f4, 
                                 strategyName=strategyName, type=type, subtype=subtype, force=force)
               
               showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                      minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                                      coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=F)
               if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 5 ) { # we replot only if it's been a while
                  plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
                  lastTimePlotted <- proc.time()
               }
            }
         }
      }
   plotAllReturnsVsTwo(col=col, searchPlotType=plotType) 
}



searchForOptimalValue <- function(inputStrategyName1=def$typicalCAPE1, inputStrategyName2=def$typicalCAPE2, 
                                  inputStrategyName3=def$typicalDetrended1, inputStrategyName4=def$typicalDetrended2, 
                                  minF1=12L, maxF1=100L, byF1=8L, minF2=12L, maxF2=100L, byF2=8L, 
                                  minF3=12L, maxF3=100L, byF3=8L, minF4=12L, maxF4=100L, 
                                  futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                  type="search", subtype="value",
                                  minTR=0, maxVol=20, maxDD2=2, minTO=4, minScore=17,
                                  col=F, CPUnumber=def$CPUnumber, plotType="dots", force=F) {

   print("While you are waiting, here are the four strategies being used.")
   plotReturnAndAlloc(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4)
   
   print(paste0("strategy        |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score") )
   searchForOptimalCombined(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2,
                            inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                            minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                            minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4, 
                            futureYears=futureYears, tradingCost=tradingCost, type=type, subtype=subtype,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, force=force) 
   print("")
   showSummaryForStrategy(def$typicalValue)
}


searchForOptimalTechnical <- function(inputStrategyName1=def$typicalSMA, inputStrategyName2=def$typicalBoll, 
                                      inputStrategyName3=def$typicalReversal, inputStrategyName4="", 
                                      minF1=12L, maxF1=80L, byF1=4L, minF2=12L, maxF2=80L, byF2=4L, 
                                      minF3=12L, maxF3=80L, byF3=4L, minF4=0L, maxF4=0L, 
                                      futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                      type="search", subtype="technical",
                                      minTR=0, maxVol=20, maxDD2=2, minTO=1, minScore=17,
                                      col=F, CPUnumber=def$CPUnumber, plotType="dots", force=F) {
   
   print(paste0("strategy              |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   searchForOptimalCombined(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2,
                            inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                            minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                            minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4, 
                            futureYears=futureYears, tradingCost=tradingCost, type=type, subtype=subtype,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, force=force) 
   print("")
   showSummaryForStrategy(def$typicalTechnical)
}


searchForOptimalBalanced <- function(inputStrategyName1=def$typicalValue, inputStrategyName2=def$typicalTechnical, 
                                     inputStrategyName3="", inputStrategyName4="", 
                                     minF1=0L, maxF1=100L, byF1=4L, minF2=0L, maxF2=100L, byF2=4L, 
                                     minF3=0L, maxF3=0L, byF3=0L, minF4=0L, maxF4=0L, 
                                     futureYears=def$futureYears, tradingCost=def$tradingCost, type="search", subtype="balanced", 
                                     minTR=5, maxVol=20, maxDD2=2.5, minTO=3., minScore=17,
                                     col=F, CPUnumber=def$CPUnumber, plotType="line", force=F) {
   totTime <- proc.time()
   print(paste0("strategy         |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   
   searchForOptimalCombined(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2,
                            inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                            minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                            minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4, 
                            futureYears=futureYears, tradingCost=tradingCost, type=type, subtype=subtype,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore,
                            col=col, CPUnumber=CPUnumber, plotType=plotType, force=force) 
   print("")
   showSummaryForStrategy(def$typicalBalanced)
   
   print( paste("time for searchForOptimalBalanced():", round(summary(proc.time())[[3]] - totTime[[3]] , 2), " s." ) )   
}


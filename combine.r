#default values of parameters:
setCombinedDefaultValues <- function() {
   def$valueMaxVol     <<- 14.5
   def$technicalMaxVol <<- 14.5
   def$valueMaxDD2     <<- 2
   def$technicalMaxDD2 <<- 1.8
   def$valueMinTO      <<- 8
   def$technicalMinTO  <<- 1.3
   def$valueMinTR      <<- 6.5
   def$technicalMinTR  <<- 6.5
   
   def$valueFractionCAPE          <<- 20
   def$valueFractionDetrended1    <<- 30
   def$valueFractionDetrended2    <<- 50
   def$typicalValue               <<- paste0("value_", def$valueFractionCAPE, "_", 
                                             def$valueFractionDetrended1, "_", def$valueFractionDetrended2)
   
   def$technicalFractionSMA      <<- 30
   def$technicalFractionBoll     <<- 20
   def$technicalFractionMomentum <<- 20
   def$technicalFractionReversal <<- 30
   def$typicalTechnical          <<- paste0("technical_", def$technicalFractionSMA, "_", def$technicalFractionBoll, "_", 
                                            def$technicalFractionMomentum, "_", def$technicalFractionReversal)
   
   def$balancedFractionValue     <<- 50
   def$balancedFractionTechnical <<- 50
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
                                   strategyName="", type="combined", subtype, futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   
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
      #       print( row.names(parameters)[which(parameters$strategy == strategyName)] )
      #       row.names(parameters)[which(parameters$strategy == strategyName)] <<- strategyName
      #       print( row.names(parameters)[which(parameters$strategy == strategyName)] )
      
      parameters$strategy[index] <<- strategyName
      parameters$type[index] <<- type
      parameters$subtype[index] <<- subtype
      parameters$startIndex[index] <<- startIndex
      #       parameters$medianAlloc[index] <<-  medianAlloc
      #       parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
      
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
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
   #    calcTRnetOfTradingCost(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)      
}


searchForOptimalCombinedSerial <- function(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                           minF1, maxF1, byF1, minF2, maxF2, byF2, minF3, maxF3, byF3, minF4, maxF4, 
                                           futureYears, tradingCost, 
                                           type="search", subtype, minTR, maxVol, maxDD2, minTO, 
                                           col, plotType="dots", force=F) {
   
   lastTimePlotted <- proc.time()
   
   for(f1 in seq(minF1, maxF1, by=byF1)) 
      for(f2 in seq(minF2, maxF2, by=byF2)) {
         for(f3 in seq(minF3, maxF3, by=byF3)) {
            f4 <- round(100 - f1 - f2 - f3)
            if ((f4 >= minF4) & (f4 <= maxF4)) {
               #                for ( med in seq(minMed, maxMed, by=byMed) )       
               #                   for ( IQ in seq(minIQ, maxIQ, by=byIQ) ) {
               if (maxF4 > 0)
                  strategyName = paste0(subtype, "_", f1, "_", f2, "_", f3, "_", f4)
               else if (maxF3 > 0)
                  strategyName = paste0(subtype, "_", f1, "_", f2, "_", f3)
               else strategyName = paste0(subtype, "_", f1, "_", f2)
               #print(strategyName)
               combineStrategies(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                      f1, f2, f3, f4, 
                                      strategyName=strategyName, type=type, subtype=subtype, force=force)
               
               showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                      minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
               if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 5 ) { # we replot only if it's been a while
                  plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
                  lastTimePlotted <- proc.time()
               }
            }
         }
      }
   plotAllReturnsVsTwo(col=col, searchPlotType=plotType) 
}


# does not work
searchForOptimalCombinedParallel <- function(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                             minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                                             minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4,
                                             futureYears=def$futureYears, tradingCost=def$tradingCost, type="search", subtype,
                                             minTR=6, maxVol=14.5, maxDD2=2.2, minTO=0., plotType="dots", force=F) {
   
   wrapper <- function(IQ) {
      if (maxF3 > 0)
         strategyName = paste0(subtype, f1, "_", f2, "_", f3, "_", med, "_", IQ)
      else strategyName = paste0(subtype, f1, "_", f2, "_", med, "_", IQ)
      #print(strategyName)
      createCombinedStrategy(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                             f1, f2, f3, f4, medianAlloc=med, interQuartileAlloc=IQ, 
                             strategyName=strategyName, type=type, subtype=subtype, force=force)     
      #          showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
      #                                 minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
   }
   
   if (def$CPUnumber == 1)
      for(f1 in seq(minF1, maxF1, by=byF1)) {
         for(f2 in seq(minF2, maxF2, by=byF2)) 
            for(f3 in seq(minF3, maxF3, by=byF3)) {
               f4 <- round(100 - f1 - f2 - f3)
               if ((f4 >= minF4) & (f4 <= maxF4)) 
                  for ( med in seq(minMed, maxMed, by=byMed) )       
                     sfClusterApplyLB( seq(minIQ, maxIQ, by=byIQ) , wrapper )
            }
         plotAllReturnsVsFour(searchPlotType=plotType)
      }
   #    showSummaries(futureYears=futureYears, tradingCost=tradingCost, detailed=F, force=F)
}



searchForOptimalCombined <- function(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                     minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                                     minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4,
                                     futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                     type="search", subtype,
                                     minTR=6, maxVol=14.5, maxDD2=2.2, minTO=0., 
                                     CPUnumber=def$CPUnumber, plotType="dots", col, force=F) {
   
   if (CPUnumber > 1) {
      library(snowfall)
      sfInit( parallel=TRUE, cpus=CPUnumber )
      sfExportAll( )
      
      searchForOptimalCombinedParallel(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                       minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                                       minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4, 
                                       futureYears=futureYears, tradingCost=tradingCost, type=type, subtype=subtype,
                                       minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, 
                                       plotType=plotType, force=force) 
      sfStop()   
   } else
      searchForOptimalCombinedSerial(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                     minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                                     minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4, 
                                     futureYears=futureYears, tradingCost=tradingCost, type=type, subtype=subtype,
                                     minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, 
                                     col=col, plotType=plotType, force=force)   
}



searchForOptimalValue <- function(inputStrategyName1=def$typicalCAPE, inputStrategyName2=def$typicalDetrended1, 
                                  inputStrategyName3=def$typicalDetrended2, inputStrategyName4="", 
                                  minF1=4L, maxF1=100L, byF1=16L, minF2=0L, maxF2=100L, byF2=8L, 
                                  minF3=0L, maxF3=100L, byF3=8L, minF4=0L, maxF4=0L, 
                                  futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                  type="search", subtype="value",
                                  minTR=def$valueMinTR, maxVol=def$valueMaxVol, 
                                  maxDD2=def$valueMaxDD2, minTO=def$valueMinTO, 
                                  col=F, CPUnumber=def$CPUnumber, plotType="dots", force=F) {
   
   print(paste0("strategy     |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   searchForOptimalCombined(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2,
                            inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                            minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                            minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4, 
                            futureYears=futureYears, tradingCost=tradingCost, type=type, subtype=subtype,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, 
                            col=col, CPUnumber=CPUnumber, plotType=plotType, force=force) 
   print("")
   showSummaryForStrategy(def$typicalValue)
}


searchForOptimalTechnical <- function(inputStrategyName1=def$typicalSMA, inputStrategyName2=def$typicalBoll, 
                                      inputStrategyName3=def$typicalMomentum, inputStrategyName4=def$typicalReversal, 
                                      minF1=20L, maxF1=70L, byF1=4L, minF2=20L, maxF2=70L, byF2=4L, 
                                      minF3=20L, maxF3=70L, byF3=4L, minF4=20L, maxF4=70L, 
                                      futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                      type="search", subtype="technical",
                                      minTR=def$technicalMinTR, maxVol=def$technicalMaxVol, maxDD2=def$technicalMaxDD2, 
                                      minTO=def$technicalMinTO, col=F, 
                                      CPUnumber=def$CPUnumber, plotType="dots", force=F) {
   
   print(paste0("strategy              |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   searchForOptimalCombined(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2,
                            inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                            minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                            minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4, 
                            futureYears=futureYears, tradingCost=tradingCost, type=type, subtype=subtype,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, 
                            col=col, CPUnumber=CPUnumber, plotType=plotType, force=force) 
   print("")
   showSummaryForStrategy(def$typicalTechnical)
}


searchForOptimalBalanced <- function(inputStrategyName1=def$typicalValue, inputStrategyName2=def$typicalTechnical, 
                                     inputStrategyName3="", inputStrategyName4="", 
                                     minF1=0L, maxF1=100L, byF1=4L, minF2=0L, maxF2=100L, byF2=4L, 
                                     minF3=0L, maxF3=0L, byF3=0L, minF4=0L, maxF4=0L, 
                                     futureYears=def$futureYears, tradingCost=def$tradingCost, type="search", subtype="balanced", 
                                     minTR=def$valueMinTR+.5, maxVol=def$valueMaxVol, maxDD2=def$valueMaxDD2, 
                                     minTO=3.5, col=F, CPUnumber=def$CPUnumber, plotType="line", force=F) {
   totTime <- proc.time()
   print(paste0("strategy       |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   
   searchForOptimalCombined(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2,
                            inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                            minF1=minF1, maxF1=maxF1, byF1=byF1, minF2=minF2, maxF2=maxF2, byF2=byF2, 
                            minF3=minF3, maxF3=maxF3, byF3=byF3, minF4=minF4, maxF4=maxF4, 
                            futureYears=futureYears, tradingCost=tradingCost, type=type, subtype=subtype,
                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, 
                            col=col, CPUnumber=CPUnumber, plotType=plotType, force=force) 
   print("")
   showSummaryForStrategy(def$typicalBalanced)
   
   print( paste("time for searchForOptimalBalanced():", round(summary(proc.time())[[3]] - totTime[[3]] , 2), " s." ) )   
}


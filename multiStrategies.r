## Calculating average alloc between 2 strategies, and corresponding results
calcMultiStrategyNorm <- function(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                    fraction1=25, fraction2=25, fraction3=25, fraction4="", 
                                    strategyName, numNA, delta="", force=F) {
   
   
   requireColInNormalized(inputStrategyName1)
   requireColInNormalized(inputStrategyName2)
   requireColInNormalized(inputStrategyName3)
   if (!(inputStrategyName4 %in% colnames(normalized)) & fraction4 != 0) stop(paste0("normalized$", inputStrategyName4, " does not exist."))
   
   if (!(strategyName %in% colnames(normalized)) | force) { # if data do not exist yet or we force recalculation:   
      addNumColToNormalized(strategyName)
      
      if(fraction4==0)
         normalized[, strategyName] <<- fraction1*normalized[, inputStrategyName1] + fraction2*normalized[, inputStrategyName2] + 
         fraction3*normalized[, inputStrategyName3]
      else normalized[, strategyName] <<- fraction1*normalized[, inputStrategyName1] + fraction2*normalized[, inputStrategyName2] + 
         fraction3*alloc[, inputStrategyName3] + fraction4*alloc[, inputStrategyName4] 
   }
}

   
createMultiStrategy <- function(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                    fraction1=25, fraction2=25, fraction3=25, fraction4="", 
                                    strategyName="", subtype, delta="", futureYears=def$futureYears, force=F) {
   
   if (!is.numeric(fraction1)) fraction1 <- 100 - fraction2 - fraction3 - fraction4
   if (!is.numeric(fraction2)) fraction2 <- 100 - fraction1 - fraction3 - fraction4
   if (!is.numeric(fraction3)) fraction3 <- 100 - fraction1 - fraction2 - fraction4
   if (!is.numeric(fraction4)) fraction4 <- 100 - fraction1 - fraction2 - fraction3
   sumCoeff <- fraction1 + fraction2 + fraction3 + fraction4
   if (abs(sumCoeff-100)>1e-6) stop(paste("Sum of coefficients must be 100, not", sumCoeff))
   
   if(strategyName=="") strategyName <- paste0(subtype, fraction1, "_", fraction2, "_", fraction3, "_", fraction4)
   
   calcMultiStrategyNorm(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2, 
                           inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                           fraction1=fraction1, fraction2=fraction2, fraction3=fraction3, fraction4=fraction4, 
                           strategyName=strategyName, delta=delta, force=force)
   calcAllocFromNorm(strategyName)
   
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
      
      parameters$strategy[index] <<- strategyName
      parameters$type[index] <<- "multi"
      parameters$subtype[index] <<- subtype
      parameters$startIndex[index] <<- startIndex
      
      parameters$inputStrategyName1[index] <<- inputStrategyName1
      parameters$inputStrategyName2[index] <<- inputStrategyName2
      parameters$inputStrategyName3[index] <<- inputStrategyName3
      parameters$inputStrategyName4[index] <<- inputStrategyName4
      parameters$fraction1[index] <<- fraction1
      parameters$fraction2[index] <<- fraction2
      parameters$fraction3[index] <<- fraction3
      parameters$fraction4[index] <<- fraction4      
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
}
   
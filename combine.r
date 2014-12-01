
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

setCombinedDefaultValues <- function() {
   def$minScore             <<- 13
   
   def$technicalStrategies  <<- c(def$typicalBoll1, def$typicalBoll2, 
                                  def$typicalSMA1, def$typicalSMA2, 
                                  def$typicalReversal1)
   def$technicalScores      <<- c(16.56, 15.92, 16.35, 14.65, 15.17)  # 'none', riskAsCostTechnical=3.5%
   # def$technicalScores    <<- c(17.36, 17.38, 17.10, 15.05, 15.52)  # 'none', riskAsCostTechnical=0
   def$typicalTechnical     <<- "technical"
   def$minScoreTechnical    <<- def$minScore 
   
   def$valueStrategies      <<- c(def$typicalCAPE_hy1, def$typicalCAPE_hy2) #, def$typicalCAPE_NH, def$typicalDetrended1)
   def$valueScores          <<- c(13.63, 14.60)  # 'none', riskAsCost=0.5%
   # def$valueScores        <<- c(13.30, 14.26)  # 'none', riskAsCost=0
   def$typicalValue         <<- "value" 
   def$minScoreValue        <<- def$minScore 
   
   def$hybridStrategies     <<- c(def$typicalBoll_CAPE1, def$typicalBoll_CAPE2, def$typicalBoll_detrended1, 
                                  def$typicalSMA_CAPE1, def$typicalSMA_CAPE2)
   def$hybridScores         <<- c(17.45, 15.17, 16.52, 14.91, 14.13)  # 'none', riskAsCostTechnical=3.5%
   # def$hybridScores       <<- c(18.12, 15.83, 17.34, 15.19, 14.15)  # 'none', riskAsCostTechnical=0 
   def$typicalHybrid        <<- "hybrid"
   def$minScoreHybrid       <<- def$minScore 
   
   def$balancedStrategies   <<- c(def$typicalTechnical, def$typicalValue, def$typicalHybrid)
   def$balancedScores       <<- c(17.49, 14.09, 17.54)   # 'none', riskAsCost=0.5% / 3.5%
   # def$balancedScores     <<- c(17.61, 13.85, 17.74)   # 'none', riskAsCost=0
   def$typicalBalanced      <<- "balanced"
   def$minScoreBalanced     <<- def$minScore 
}


## Calculate the weighted average of the signals of several strategies
calcCombinedStrategySignal <- function(inputStrategyName, fraction, strategyName, force=F) {
   
   if ( length(inputStrategyName) != length(fraction) )
      stop("inputStrategyName and fraction must have the same length.")      
   numLoops <- length(inputStrategyName)
   
   if (abs(sum(fraction)-1) > 1e-6) stop(paste("Sum of coefficients must be 1, not", sumCoeff))
   
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


combineStrategies <- function(
      inputStrategyName, score, minScore, 
      futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost,
      coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
      strategyName="", type="combined", subtype, speed=0, force=F) {
   
   if ( length(inputStrategyName) != length(score) )
      stop("inputStrategyName and score must have the same length (resp. ",
           length(inputStrategyName), " and ", length(score), " right now).")         
   numLoops <- length(inputStrategyName)
   
   if(strategyName=="") strategyName <- subtype
   
   fraction <- numeric( length(score) )
   for ( i in 1:length(score) ) {
      if (score[i] < minScore) fraction[i] <- 0  # scores less than minScore are not used
      else fraction[i] <- score[i] - minScore
   }
   
   ## if they are all under minScore then the largest one gets 100% of the weight
   ##    (needed because a sum of fraction diffreent from 1 would later crash)
   if ( sum(fraction)==0 ) {
      for ( i in 1:length(score) ) 
         if ( score[i]== max(score) )
            fraction[i] <- 1 
      warning("All scores for ", strategyName, " are under minScore, the largest one gets 100% of the weight.")
   }
   
   total <- sum(fraction)
   fraction <- fraction/total # make the total 1
   
   if (!(strategyName %in% colnames(alloc)) | force) {   
      calcCombinedStrategySignal(inputStrategyName=inputStrategyName, fraction=fraction, 
                                 strategyName=strategyName, force=force)
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
   
   ## Filling 'parameters' with the values of coefficients
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
      
      parameters$fraction1[index] <<- fraction[1]
      parameters$fraction2[index] <<- fraction[2]
      if(numLoops >= 3) parameters$fraction3[index] <<- fraction[3]
      if(numLoops >= 4) parameters$fraction4[index] <<- fraction[4]         
      if(numLoops >= 5) parameters$fraction5[index] <<- fraction[5]
      if(numLoops >= 6) parameters$fraction6[index] <<- fraction[6]         
      if(numLoops >= 7) parameters$fraction7[index] <<- fraction[7]         
   }
   
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, costs=costs, 
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   
   indexStats      <- which(stats$strategy == strategyName)
   indexParameters <- which(parameters$strategy == strategyName)
   stats$type[indexStats]    <<- parameters$type[indexParameters]
   stats$subtype[indexStats] <<- parameters$subtype[indexParameters]
}


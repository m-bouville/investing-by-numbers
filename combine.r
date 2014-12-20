
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
   def$minScore           <<- 10
   
   def$technicalStrategies<<- c(typical$Boll1, typical$SMA1, typical$SMA2, typical$reversal1)
   def$technicalScores    <<- c(13.61, 13.35, 11.24, 12.65)  # 'testing', costs=2%
   # def$technicalScores  <<- c(12.59, 13.94, 12.50, 13.92)  # 'testing', costs=4%
   def$minScoreTechnical  <<- def$minScore 
   
   def$valueStrategies    <<- c(typical$CAPE_hy1, typical$CAPE_hy2, typical$CAPE_NH)
   def$valueScores        <<- c(12.72, 12.75, 10.63)  # 'testing', costs=2%
   def$minScoreValue      <<- def$minScore 
   
   def$hybridStrategies   <<- c(typical$Boll_CAPE1, typical$SMA_CAPE1, typical$SMA_CAPE2, 
                                typical$Boll_Boll1, typical$Boll_Boll2) #, typical$Boll_balanced1)
   def$hybridScores       <<- c(14.95, 12.14, 12.64, 15.34, 12.78)  # 'testing', costs=2%
   # def$hybridScores     <<- c(14.27, 13.70, 14.31, 15.26, 12.70)  # 'testing', costs=4%
   def$minScoreHybrid     <<- def$minScore 
   
   def$balancedStrategies <<- c(typical$technical, typical$value, typical$hybrid)
   def$balancedScores     <<- c(13.77, 13.05, 15.29)   # 'testing', costs=2%
   def$minScoreBalanced   <<- def$minScore 
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

   if(strategyName=="") {
      strategyName <- subtype
      for ( i in 1:length(fraction) ) 
         strategyName <- paste0(strategyName, "_", round(100*fraction[i]) )
      if(subtype=="technical")     typical$technical <<- strategyName
      else if(subtype=="value")    typical$value     <<- strategyName
      else if(subtype=="hybrid")   typical$hybrid    <<- strategyName
      else if(subtype=="balanced") typical$balanced  <<- strategyName
      
      # update default lists of strategies
      def$balancedStrategies <<- c(typical$technical, typical$value, typical$hybrid)
      typical$StratNames <<- c(stratName1="stocks", stratName2="bonds", 
                                  stratName3=typical$value, stratName4=typical$balanced)
      typical$StratNamesSubstrategies <<- c(stratName1=typical$technical, stratName2=typical$value, 
                                               stratName3=typical$hybrid, stratName4=typical$balanced)
   }  
   
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


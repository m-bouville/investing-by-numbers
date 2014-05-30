

#default values of parameters:
setReversalDefaultValues <- function() {
   def$reversalMedianAlloc <<- 95
   def$reversalInterQuartileAlloc <<- 15
   def$reversalAvgOver           <<- 12L
}


## calculating trend reversal (essentially a second derivative)
calcReversal <- function(inputDF, inputName, avgOver=def$reversalAvgOver, reversalName) {
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="normalized") input <- normalized[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")
   
   addNumColToDat(reversalName)
   dat[1:(2*avgOver), reversalName] <<- NA
   for(i in (2*avgOver+1):numData) 
      dat[i, reversalName] <<- input[i] * input[i-2*avgOver] / input[i-avgOver]^2 - 1
}





normalizeReversal <- function(inputDF, inputName, avgOver=def$reversalAvgOver, strategyName) {
   
   reversalName <- paste0("reversal_", inputName, "_", avgOver)
   if (!reversalName %in% colnames(dat)) 
      calcReversal(inputDF=inputDF, inputName=inputName, avgOver=avgOver, reversalName=reversalName)
   addNumColToNormalized(strategyName)
   
   bearish <- quantile(dat[, reversalName], 0.75, na.rm=T)[[1]] # backwards compared to others (high is good)
   bullish <- quantile(dat[, reversalName], 0.25, na.rm=T)[[1]]
   
   normalized[, strategyName] <<- 2 * (dat[, reversalName]-bullish) / (bearish-bullish) - 1
}

avg <- 12
offset <- 1
prefactor <- 1.
name <- paste0("reversal_TR_", avg, "_", offset, "_", prefactor)

createReversalStrategy("dat", "TR", avg, strategyName=name, force=T)

# calcTRnetOfTradingCost(name, force=T)
# plotReturnAndAlloc(name, startYear=1923, maxTR=2000)
showSummaryForStrategy(name)
plotAllReturnsVsFour(yMin=5.5, xMinVol=12.5)


createReversalStrategy <- function(inputDF, inputName, avgOver=def$reversalAvgOver, 
                                   medianAlloc=def$reversalMedianAlloc, interQuartileAlloc=def$reversalInterQuartileAlloc,
                                   strategyName="", futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   if (strategyName=="") 
      strategyName <- paste0("reversal_TR_", avg, "_", offset, "_", prefactor)  #, "_", medianAlloc, "_", interQuartileAlloc)
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      normalizeReversal(inputDF=inputDF, inputName=inputName, avgOver=avgOver, strategyName=strategyName)

      #       print( summary(normalized$temp) )
      #       
      #       print( mean( normalized[, strategyName] , na.rm=T) )
      normalized[, strategyName] <<- normalized[, strategyName] + offset - mean( normalized[, strategyName] , na.rm=T )
      
      requireColInNormalized(strategyName)
      addNumColToAlloc(strategyName)
      
      alloc[, strategyName] <<- NA
      alloc[2*avgOver+1, strategyName] <<- .5
      for (i in (2*avgOver+2):numData ) {
         alloc[i, strategyName] <<- alloc[i-1, strategyName] + prefactor * normalized[i, strategyName] 
         alloc[i, strategyName] <<- max( min(alloc[i, strategyName], 1), 0)
      }
      
      addNumColToTR(strategyName)  
      startIndex = max(avgOver, sum(is.na(alloc[ ,strategyName])))+1
      calcStrategyReturn(strategyName, startIndex)
   } 
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index]    <<- strategyName
      parameters$type[index]        <<- "reversal"
      parameters$subtype[index]     <<- inputName
      parameters$inputDF[index]     <<- inputDF
      parameters$inputName[index]   <<- inputName
      parameters$startIndex[index]  <<- 2*avgOver+1
#       parameters$medianAlloc[index] <<-  medianAlloc
#       parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
      parameters$avgOver[index]     <<-  avgOver
      
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
   #    calcTRnetOfTradingCost(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)      
}

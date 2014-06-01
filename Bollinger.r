#default values of parameters:
setBollDefaultValues <- function() {
   def$BollInputDF          <<- "dat"
   def$BollInputName        <<- "TR"
   def$BollAvgOver          <<- 18L
   def$BollMedianAlloc      <<- 99
   def$BollInterQuartileAlloc <<- 25
   def$typicalBoll          <<- paste0("Boll_", def$BollInputName, "_", def$BollAvgOver, "_", 
                                       def$BollMedianAlloc, "_", def$BollInterQuartileAlloc)
}

normalizeBoll <- function(inputDF, inputName, avgOver=def$BollAvgOver, strategyName="", force=F) {

   BollBandsName <- paste0(inputName, avgOver)
   avgName <- paste0("avg_", BollBandsName)
   SDname <- paste0("SD_", BollBandsName)
   
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="normalized") input <- normalized[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")
   
   #    time1 <- proc.time()
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
   #    print( c( "Bollinger - calc-avg-&SD time:", round(summary(proc.time())[[1]] - time1[[1]] , 2) ) )
   
   #    time1 <- proc.time()    
   if ( !(strategyName %in% colnames(normalized)) | force) {# if data do not exist yet or we force recalculation:
      addNumColToNormalized(strategyName)
      normalized[, strategyName] <<- (input[] - dat[, avgName]) / dat[, SDname]
      
      bearish <- quantile(normalized[, strategyName], 0.75, na.rm=T)[[1]]
      bullish <- quantile(normalized[, strategyName], 0.25, na.rm=T)[[1]]
      normalized[, strategyName] <<- 2 * (normalized[, strategyName]-bullish) / (bearish-bullish) - 1
   }
   #    print( c( "Bollinger - compare-to-bands time:", round(summary(proc.time())[[1]] - time1[[1]] , 2) ) )
}


createBollStrategy <- function(inputDF, inputName, avgOver=def$BollAvgOver, 
                               medianAlloc=def$BollMedianAlloc, interQuartileAlloc=def$BollInterQuartileAlloc,
                               strategyName="", type="", futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {

   if(strategyName=="")  
      strategyName <- paste0("Boll_", inputName, "_", avgOver, "_", medianAlloc, "_", interQuartileAlloc)
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      #       time0 <- proc.time()
      normalizeBoll(inputDF=inputDF, inputName=inputName, avgOver=avgOver, strategyName=strategyName, force=force)
      #       print( c( "Bollinger - normalizeBoll() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )
      
      #       time0 <- proc.time()
      calcAllocFromNorm(strategyName, medianAlloc=medianAlloc, interQuartileAlloc=interQuartileAlloc)
      #       print( c( "Bollinger - calcAllocFromNorm() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )
      
      
      #       time0 <- proc.time()
      addNumColToTR(strategyName)
      startIndex <- sum(is.na(alloc[, strategyName])) + 1
      calcStrategyReturn(strategyName, startIndex)
      #       print( c( "Bollinger - calcStrategyReturn() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )
   }
   
#    time0 <- proc.time()
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
      parameters$medianAlloc[index] <<-  medianAlloc
      parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
      parameters$avgOver[index]    <<-  avgOver
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
#    calcTRnetOfTradingCost(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)      
# print( c( "Bollinger - stats time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
}


searchForOptimalBoll <- function(inputDF="dat", inputName="TR", minAvgOver=21L, maxAvgOver=21L, byAvgOver=3L, 
                       minMed=50, maxMed=95, byMed=10, minIQ=10, maxIQ=50, byIQ=10, 
                       futureYears=def$futureYears, tradingCost=def$tradingCost, 
                       minTR=def$technicalMinTR, maxVol=def$technicalMaxVol, maxDD2=def$technicalMaxDD2, 
                       minTO=def$technicalMinTO, force=F) {
   
   lastTimePlotted <- proc.time()
   print(paste0("strategy         |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )

   for ( avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver) ) {
      for ( med in seq(minMed, maxMed, by=byMed) ) {    
         for ( IQ in seq(minIQ, maxIQ, by=byIQ) ) {
            strategyName <- paste0("Boll_", inputName, "_", avgOver, "_", med, "_", IQ)
            
            createBollStrategy(inputDF, inputName, avgOver=avgOver, type="search",
                               medianAlloc=med, interQuartileAlloc=IQ,
                               strategyName=strategyName, futureYears=futureYears, force=force)
            
            showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                   minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
         }
         if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 5 ) { # we replot only if it's been a while
            plotAllReturnsVsFour()
            lastTimePlotted <- proc.time()
         }
      }
   }
   print("")
   showSummaryForStrategy(def$typicalBoll)
}


#default values of parameters:
setBollDefaultValues <- function() {
   def$BollMedianAlloc   <<- 95
   def$BollInterQuartileAlloc <<- 20
   def$BollAvgOver    <<- 21L
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
                               strategyName="", futureYears=def$FutureYears, force=F) {

   if(strategyName=="")  
      strategyName <- paste0("Boll_", inputName, "_", avgOver, "_", medianAlloc, "_", interQuartileAlloc)

   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
#       time0 <- proc.time()
      normalizeBoll(inputDF=inputDF, inputName=inputName, avgOver=avgOver, strategyName=strategyName, force=force)
#       print( c( "Bollinger - normalizeBoll() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )

#       time0 <- proc.time()
      calcAllocFromNorm(strategyName, medianAlloc=medianAlloc, interQuartileAlloc=interQuartileAlloc)
#       print( c( "Bollinger - calcAllocFromNorm() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )
      
      addNumColToTR(strategyName)  
      
#       time0 <- proc.time()
      calcStrategyReturn(strategyName, avgOver+1)
#       print( c( "Bollinger - calcStrategyReturn() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )
   }
   
#    time0 <- proc.time()
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$type[index] <<- "Bollinger"
      parameters$startIndex[index] <<- avgOver+1
      parameters$inputDF[index]   <<- inputDF
      parameters$inputName[index] <<- inputName
      parameters$medianAlloc[index] <<-  medianAlloc
      parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
      parameters$avgOver[index]    <<-  avgOver
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
# print( c( "Bollinger - stats time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
}


searchForOptimalBoll <- function(inputDF="dat", inputName="TR", minAvgOver=21L, maxAvgOver=21L, byAvgOver=12L, 
                       minMed=90, maxMed=95, byMed=5, minIQ=10, maxIQ=30, byIQ=5, 
                       futureYears=def$futureYears, tradingCost=def$tradingCost, 
                       minTR=6.7, maxVol=14.7, maxDD2=1.8, minTO=1.2, force=F) {
   
   print(paste0("strategy         |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   for ( avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver) ) {
      for ( med in seq(minMed, maxMed, by=byMed) )       
         for ( IQ in seq(minIQ, maxIQ, by=byIQ) ) {
            strategyName <- paste0("Boll_", inputName, "_", avgOver, "_", med, "_", IQ)
            
            createBollStrategy(inputDF, inputName, avgOver=avgOver, 
                               medianAlloc=med, interQuartileAlloc=IQ,
                               strategyName=strategyName, futureYears=futureYears, force=force)
            
            showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                   minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
         }
      plotReturnVsBothBadWithLine()
   }
   #    showSummaries(futureYears=futureYears, tradingCost=tradingCost, detailed=F, force=F)
}


#default values of parameters:
setMomentumDefaultValues <- function() {
   def$momentumInputDF           <<- "dat"
   def$momentumInputName         <<- "TR"
   def$momentumAvgOver           <<- 12L
   def$momentumMedianAlloc       <<- 95
   def$momentumInterQuartileAlloc<<- 15
   def$typicalMomentum           <<- paste0("momentum_", def$momentumInputName, "_", def$momentumAvgOver, "_", 
                                            def$momentumMedianAlloc, "_", def$momentumInterQuartileAlloc)
}


## calculating momentum
calcMomentum <- function(inputDF, inputName, avgOver=def$momentumAvgOver, momentumName) {
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="normalized") input <- normalized[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")
   
   addNumColToDat(momentumName)
   for(i in 1:avgOver) { dat[i, momentumName] <<- NA }
   for(i in (avgOver+1):numData) 
      dat[i, momentumName] <<- input[i] / input[i-avgOver] - 1
}


## Normalize momentum
normalizeMomentum <- function(inputDF, inputName, avgOver=def$momentumAvgOver, strategyName) {

   momentumName <- paste0("momentum_", inputName, "_", avgOver)
   if (!momentumName %in% colnames(dat)) 
      calcMomentum(inputDF=inputDF, inputName=inputName, avgOver=avgOver, momentumName=momentumName)
   addNumColToNormalized(strategyName)
   
   bearish <- quantile(dat[, momentumName], 0.75, na.rm=T)[[1]] # backwards compared to others (high is good)
   bullish <- quantile(dat[, momentumName], 0.25, na.rm=T)[[1]]

   normalized[, strategyName] <<- 2 * (dat[, momentumName]-bullish) / (bearish-bullish) - 1
}


createMomentumStrategy <- function(inputDF, inputName, avgOver=def$momentumAvgOver, 
                                   medianAlloc=def$momentumMedianAlloc, interQuartileAlloc=def$momentumInterQuartileAlloc,
                                   strategyName="", futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   
   if (strategyName=="") 
      strategyName <- paste0("momentum_", inputName, "_", avgOver, "_", medianAlloc, "_", interQuartileAlloc)
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      normalizeMomentum(inputDF=inputDF, inputName=inputName, avgOver=avgOver, strategyName=strategyName)
      calcAllocFromNorm(strategyName, medianAlloc=medianAlloc, interQuartileAlloc=interQuartileAlloc)
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
      parameters$type[index]        <<- "momentum"
      parameters$subtype[index]     <<- inputName
      parameters$inputDF[index]     <<- inputDF
      parameters$inputName[index]   <<- inputName
      parameters$startIndex[index]  <<- avgOver+1
      parameters$medianAlloc[index] <<-  medianAlloc
      parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
      parameters$avgOver[index]     <<-  avgOver
      
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
#    calcTRnetOfTradingCost(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)      
}

searchForOptimalMomentum <-function(inputDF="dat", inputName="TR", 
                           minAvgOver=12L, maxAvgOver=12L, byAvgOver=3L, 
                           minMed=90, maxMed=99, byMed=2, minIQ=2, maxIQ=20, byIQ=2, 
                           futureYears=def$futureYears, tradingCost=def$tradingCost, 
                           minTR=def$technicalMinTR, maxVol=def$technicalMaxVol, maxDD2=def$technicalMaxDD2, 
                           minTO=def$technicalMinTO, force=F) {
   
   print(paste0("strategy             |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   for ( avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver) ) 
      for ( med in seq(minMed, maxMed, by=byMed) ) {
         for ( IQ in seq(minIQ, maxIQ, by=byIQ) ) {
            strategyName <- paste0("momentum_", inputName, "_", avgOver    , "_", med, "_", IQ)
            
            createMomentumStrategy(inputDF=inputDF, inputName=inputName, avgOver    =avgOver    , 
                                   medianAlloc=med, interQuartileAlloc=IQ, strategyName=strategyName, force=force)                  
            showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                   minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
         }
         plotAllReturnsVsFour()
      }
   print("")
   showSummaryForStrategy(def$typicalMomentum)
}


# No longer works (based on obsolete data structure)
plotMomentum <- function(avgOver=def$momentumAvgOver, futureYears=def$futureYears, startYear=1885L) {
   futureReturnName <- paste0("futureReturn", futureYears)
   if (!futureReturnName %in% colnames(dat)) calcFutureReturn(futureYears)
   strategyName <- paste0("momentum", avgOver)
   if (!strategyName %in% colnames(dat)) calcMomentum(avgOver)
   
   par(mar=c(2.5, 4, 1.5, 1.5))
   par(mfrow = c(2, 1))
   temp <- numeric(numData)
   
   temp <- dat[, strategyName]
   
   plot(dat$date, temp, type="l", xlim=c(dat$date[(startYear-1871)*12], dat$date[numData]), xlab="SMA ratio", 
        ylab=paste0(," - ", 1+round(m,2)), ylim=c(-.5,.5))
   plot(temp, dat[, futureReturnName], xlab="momentum", ylab="future return", xlim=c(-.5,.5))
   mod <- lm( dat[, futureReturnName] ~ temp)
   abline(mod)   
}

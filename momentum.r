#default values of parameters:
setMomentumDefaultValues <- function() {
   def$momentumInputDF   <<- "dat"
   def$momentumInputName <<- "TR"
   def$momentumAvgOver   <<-  12L
   def$momentumBearish   <<- -25L
   def$momentumBullish   <<- -25L
   def$typicalMomentum   <<- paste0("momentum_", def$momentumInputName, "_", def$momentumAvgOver, "__", 
                                    -def$momentumBearish, "_", def$momentumBullish)
}


## calculating momentum
calcMomentum <- function(inputDF, inputName, avgOver=def$momentumAvgOver, momentumName) {
   if (inputDF=="dat")            input <- dat[, inputName]
   else if (inputDF=="signal")    input <- signal[, inputName]
   else if (inputDF=="alloc")     input <- alloc[, inputName]
   else if (inputDF=="TR")        input <- TR[, inputName]
   else if (inputDF=="next30yrs") input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")
   
   addNumColToDat(momentumName)
   for(i in 1:avgOver) { dat[i, momentumName] <<- NA }
   for(i in (avgOver+1):numData) 
      #dat[i, momentumName] <<- input[i] - input[i-avgOver]
      #dat[i, momentumName] <<- input[i] / input[i-avgOver] - 1
      dat[i, momentumName] <<- log( input[i] / input[i-avgOver] )
      ## this is the slope from a linear fit:
      #dat[i, momentumName] <<- regression( (i-avgOver):i, input[(i-avgOver):i] )[[2]]
      ## this is the slope from an exponential fit:
      #dat[i, momentumName] <<- exp( regression( (i-avgOver):i, log(input[(i-avgOver):i]) )[[2]] ) - 1

   ## there is (surprisingly) little difference between all these versions
}


calcMomentumSignal <- function(inputDF=def$momentumInputDF, inputName=def$momentumInputName, 
                               avgOver=def$momentumAvgOver, 
                               bearish=def$BollBearish, bullish=def$BollBullish, 
                               signalMin=def$signalMin, signalMax=def$signalMax,
                               strategyName) {

   momentumName <- paste0("momentum_", inputName, "_", avgOver)
   if (!momentumName %in% colnames(dat)) 
      calcMomentum(inputDF=inputDF, inputName=inputName, avgOver=avgOver, momentumName=momentumName)
   addNumColToSignal(strategyName)
   
   bearish=bearish/100
   bullish=bullish/100

   calcSignalForStrategy(strategyName, input=dat[, momentumName], bearish=bearish, bullish=bullish,
                         signalMin=signalMin, signalMax=signalMax, startIndex=avgOver+1 ) 
}


createMomentumStrategy <- function(inputDF=def$momentumInputDF, inputName=def$momentumInputName, 
                                   avgOver=def$momentumAvgOver, 
                                   bearish=def$momentumBearish, bullish=def$momentumBullish, 
                                   signalMin=def$signalMin, signalMax=def$signalMax,
                                   strategyName="", type="", futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                   coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {
   if (strategyName=="") 
      strategyName <- paste0("momentum_", inputName, "_", avgOver, "__", -bearish, "_", bullish)
   if (bearish==bullish) bullish = bearish + 1e-3 # bear==bull creates problems
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      calcMomentumSignal(inputDF=inputDF, inputName=inputName, avgOver=avgOver, 
                         bearish=bearish, bullish=bullish, 
                         signalMin=signalMin, signalMax=signalMax, strategyName=strategyName)
      calcAllocFromSignal(strategyName)
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
      
      parameters$strategy[index]   <<- strategyName
      if (type=="search") {
         parameters$type[index]    <<- "search"
         parameters$subtype[index] <<- "momentum"        
      } else {
         parameters$type[index]    <<- "momentum"
         parameters$subtype[index] <<- inputName
      }
      parameters$inputDF[index]    <<- inputDF
      parameters$inputName[index]  <<- inputName
      parameters$startIndex[index] <<- avgOver+1
      parameters$bearish[index]    <<- bearish
      parameters$bullish[index]    <<- bullish  
      parameters$avgOver[index]    <<-  avgOver
      
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, 
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
}

searchForOptimalMomentum <-function(inputDF="dat", inputName="TR", 
                           minAvgOver=9L, maxAvgOver=15L, byAvgOver=3L, 
                           minBear=-60L, maxBear= 0L, byBear=10L, 
                           minDelta=0, maxDelta=50, byDelta=10, 
                           futureYears=def$futureYears, tradingCost=def$tradingCost, type="search", 
                           minTR=0, maxVol=20, maxDD2=5, minTO=1, minScore=13,
                           col=F, plotType="symbols", force=F) {
   
   lastTimePlotted <- proc.time()
   print(paste0("strategy              |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score") )

   for ( avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver) ) 
      for ( bear in seq(minBear, maxBear, by=byBear) ) {     
         for ( delta in seq(minDelta, maxDelta, by=byDelta) ) {
            bull = bear + delta
            strategyName <- paste0("momentum_", inputName, "_", avgOver, "__", -bear, "_", bull)
            
            createMomentumStrategy(inputDF=inputDF, inputName=inputName, avgOver=avgOver, type="search",
                                   bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                                   strategyName=strategyName, force=force)                  
            showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                   minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, force=F)
         }
         if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 5 ) { # we replot only if it's been a while
            plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
            lastTimePlotted <- proc.time()
         }
      }
   print("")
   showSummaryForStrategy(def$typicalMomentum)
plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
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

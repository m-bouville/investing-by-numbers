#default values of parameters:
setDetrendedDefaultValues <- function() {
   def$detrendedInputDF           <<- "dat"
   def$detrendedInputName         <<- "TR"
   
   ## detrended strategy in stocks 90% of the time, just dodging the worst bubbles
   def$detrendedAvgOver1           <<- 33L
   def$detrendedBearish1           <<- 30
   def$detrendedBullish1           <<- 30
   def$typicalDetrended1           <<- paste0("detrended_", def$detrendedInputName, "_avg", def$detrendedAvgOver1, "__", 
                                             def$detrendedBearish1, "_", def$detrendedBullish1)
   
   ## detrended strategy with lower stock allocation (and vol and DD)
   def$detrendedAvgOver2           <<- 24L
   def$detrendedBearish2           <<- 40
   def$detrendedBullish2           <<-  0
   def$typicalDetrended2           <<- paste0("detrended_", def$detrendedInputName, "_avg", def$detrendedAvgOver2, "__", 
                                              def$detrendedBearish2, "_", def$detrendedBullish2)
}

calcDetrended <- function(inputDF, inputName, detrendedName="") {
   if (inputDF=="dat")             logInput <- log( dat[, inputName] )
   else if (inputDF=="signal")     logInput <- log( signal[, inputName] )
   else if (inputDF=="alloc")      logInput <- log( alloc[, inputName] )
   else if (inputDF=="TR")         logInput <- log( TR[, inputName] )
   else if (inputDF=="next30yrs")  logInput <- log( next30yrs[, inputName] )
   else stop("data frame ", inputDF, " not recognized")
   
   fitPara <- regression(TR$numericDate[], logInput[])
   a <- fitPara[[1]]
   b <- fitPara[[2]]

   if (detrendedName=="") detrendedName <- paste0("detrended_", inputName)

   addNumColToDat(detrendedName)
   dat[, detrendedName] <<- logInput[] - (a + b * TR$numericDate[])
}


## Average detrended over 'avgOver' months
calcAvgDetrended <- function(detrendedName, avgOver=def$detrendedAvgOver) {

   requireColInDat(detrendedName)
   avgDetrendedName <- paste0(detrendedName, "_avg", avgOver)
   addNumColToDat(avgDetrendedName)
   for(i in 1:(avgOver-1)) dat[i, avgDetrendedName] <<- NA # not enough data to calculate average
   for(i in avgOver:numData) dat[i, avgDetrendedName] <<- mean(dat[(i-avgOver+1):i, detrendedName])  
}


CalcDetrendedSignal <- function(inputDF=def$detrendedInputDF, inputName=def$detrendedInputName,
                                bearish=def$detrendedBearish, bullish=def$detrendedBullish, 
                                signalMin=def$signalMin, signalMax=def$signalMax, strategyName, avgOver) {
   
   detrendedName <- paste0("detrended_", inputName)
#   if (!detrendedName %in% colnames(dat)) 
   {
      calcDetrended(inputDF=inputDF, inputName=inputName, detrendedName=detrendedName)
      if( is.numeric(avgOver) ) {
         if( !paste0(detrendedName,"_avg",avgOver) %in% colnames(dat) ) 
            calcAvgDetrended(detrendedName, avgOver)
          detrendedName <- paste0(detrendedName, "_avg", avgOver)
      }
   }

bearish=bearish/100
bullish=bullish/100

calcSignalForStrategy(strategyName, input=dat[, detrendedName], bearish=bearish, bullish=bullish,
                      signalMin=signalMin, signalMax=signalMax, startIndex=avgOver ) 
}


createDetrendedStrategy <- function(inputDF=def$detrendedInputDF, inputName=def$detrendedInputName, 
                                    avgOver=def$detrendedAvgOver, 
                                    bearish=def$detrendedBearish, bullish=def$detrendedBullish, 
                                    signalMin=def$signalMin, signalMax=def$signalMax,
                                    strategyName="",  type="detrended",
                                    futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                    coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {   
   if (strategyName=="") {
      if( is.numeric(avgOver) )
         strategyName <- paste0("detrended_", inputName, "_avg", avgOver, "__", bearish, "_", bullish)
      else strategyName <- paste0("detrended_", inputName, "__", bearish, "_", bullish)
   }
   if (bullish == bearish) bullish = bearish - 1e-3 # bearish=bullish creates problems
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      CalcDetrendedSignal(inputDF=inputDF, inputName=inputName, bearish=bearish, bullish=bullish, 
                          signalMin=signalMin, signalMax=signalMax, strategyName=strategyName, avgOver=avgOver)
      calcAllocFromSignal(strategyName)
      addNumColToTR(strategyName)  
      startIndex = avgOver # max(months, sum(is.na(alloc[ ,strategyName])))+1
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
         parameters$type[index]        <<- "search"
         parameters$subtype[index]     <<- "detrended" 
      } else {
         parameters$type[index]        <<- "detrended"
         parameters$subtype[index]     <<- inputName
      }
      parameters$inputDF[index]    <<- inputDF
      parameters$inputName[index]  <<- inputName
      parameters$startIndex[index] <<- startIndex
      parameters$avgOver[index]    <<- avgOver
      parameters$bearish[index]    <<- bearish
      parameters$bullish[index]    <<- bullish      
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, 
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
#    calcTRnetOfTradingCost(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)      
}


searchForOptimalDetrended <- function(inputDF=def$detrendedInputDF, inputName=def$detrendedInputName, 
                                      minAvgOver=24, maxAvgOver=42, byAvgOver=6, 
                                      minBear=20, maxBear=50, byBear=10, 
                                      minBull=-40, maxBull=20, byBull=10, 
                                      futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                      minTR=0, maxVol=20, maxDD2=5, minTO=5, minScore=16, 
                                      coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, col=F, 
                                      plotType="symbols", CPUnumber=def$CPUnumber, force=F) {
   
   print(paste0("strategy                  |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score") )
   detrendedName <- paste0("detrended_", inputName)
   calcDetrended(inputDF=inputDF, inputName=inputName, detrendedName) 
   
   for (avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver)) {
      calcAvgDetrended(detrendedName, avgOver=avgOver)
      for ( bear in seq(minBear, maxBear, by=byBear) ) {      
         for ( bull in seq(minBull, maxBull, by=byBull) ) {
            if (bull < bear + 1e-3 ) {
               strategyName <- paste0(detrendedName, "_avg", avgOver, "__", bear, "_", bull)
               if (bull == bear) bull = bear - 1e-3 # bear=bull creates problems
               
               createDetrendedStrategy(inputDF=inputDF, inputName=inputName, avgOver=avgOver, strategyName=strategyName, 
                                       bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                                       type="search", futureYears=futureYears, force=force)
               
               showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                      minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, 
                                      minScore=minScore, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=F)
            }
         }
         plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
      }
   }
   print("")
   showSummaryForStrategy(def$typicalDetrended1, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2)
   showSummaryForStrategy(def$typicalDetrended2, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2)
   plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
}


searchForTwoOptimalDetrended <-function(plotType="symbols", force=F) {
   
   searchForOptimalDetrended(minAvgOver=24, maxAvgOver=42, byAvgOver=3, 
                             minBear=20, maxBear=40, byBear=5, 
                             minBull=20, maxBull=35, byBull=5,
                             coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, minScore=17, 
                             plotType=plotType, force=force)
   
   ## Using a greater penalty for volatility and drawdowns
   ## (and using parameters more likely to work under such conditions)
   searchForOptimalDetrended(minAvgOver=18, maxAvgOver=30, byAvgOver=3, 
                             minBear=20,  maxBear=55, byBear=10, 
                             minBull=-10, maxBull=30, byBull=10,
                             coeffVol=3*def$coeffVol, coeffDD2=3*def$coeffDD2, 
                             maxVol=17, minScore=14.5, 
                             plotType=plotType, force=force)
   
}



############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
##       SMA.r generates a strategy       ##
##        based on the crossing of        ##
##      simple moving averages (SMAs)     ##
##                                        ##
############################################


#default values of parameters:
setSMAdefaultValues <- function() {
   def$SMAinputDF    <<- "dat"
   def$SMAinputName  <<- "TR"

   # lower volatility and lower DD (but lower retrurn too)
   def$SMA1        <<- 14L
   def$SMA2        <<-  1L
   def$SMAbearish  <<- 12L
   def$SMAbullish  <<- 12L
   def$typicalSMA  <<- paste0("SMA_", def$SMA1, "_", def$SMA2, "__", def$SMAbearish, "_", def$SMAbullish)
}

## calculating simple moving average (SMA)
calcSMA <- function(inputDF, inputName, avgOver, SMAname) {
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="signal")     input <- signal[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")

   addNumColToDat(SMAname)
   dat[1:(avgOver-1), SMAname] <<- NA
   for(i in avgOver:numData) {
      dat[i, SMAname] <<- mean(input[(i-avgOver+1):i], na.rm=F)
   }
}


calcSMAsignal <- function(SMAname1, SMAname2,
                          bearish=def$BollBearish, bullish=def$BollBullish, 
                          signalMin=def$signalMin, signalMax=def$signalMax,
                          strategyName, startIndex) {
   requireColInDat(SMAname1)
   requireColInDat(SMAname2)
   addNumColToSignal(strategyName)
   
   SMAratio <- dat[, SMAname1] / dat[, SMAname2] - 1
   
   calcSignalForStrategy(strategyName, input=SMAratio, bearish=bearish, bullish=bullish,
                         signalMin=signalMin, signalMax=signalMax, startIndex=startIndex ) 
}


createSMAstrategy <- function(inputDF="dat", inputName="TR", SMA1=def$SMA1, SMA2=def$SMA2, 
                              bearish=def$detrendedBearish, bullish=def$detrendedBullish, 
                              signalMin=def$signalMin, signalMax=def$signalMax,
                              strategyName="", type="", futureYears=def$futureYears, costs=def$tradingCost, 
                              coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {
   
   if (strategyName=="") 
      strategyName <- paste0("SMA_", SMA1, "_", SMA2, "__", bearish, "_", bullish)
   if (bullish == bearish) 
      bullish = bearish - 1e-3 # bearish=bullish creates problems
   bearish <- bearish/1000
   bullish <- bullish/1000
   
   SMAname1 <- paste0("SMA_", inputName, "_", SMA1)
   if (!(SMAname1 %in% colnames(dat)) | force)
      calcSMA(inputDF, inputName, SMA1, SMAname1)      
   SMAname2 <- paste0("SMA_", inputName, "_", SMA2)

   if (!(SMAname2 %in% colnames(dat)) | force)
      calcSMA(inputDF, inputName, SMA2, SMAname2)      
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      calcSMAsignal(SMAname1, SMAname2, bearish=bearish, bullish=bullish, 
                    signalMin=signalMin, signalMax=signalMax, 
                    strategyName=strategyName, startIndex=max(SMA1,SMA2)+1)
      calcAllocFromSignal(strategyName)
#       calcSMAallocation(SMA1, SMA2, offset, ratioLow, ratioHigh, allocLow, allocHigh, strategyName=strategyName)
      addNumColToTR(strategyName)
      calcStrategyReturn(strategyName, max(SMA1,SMA2)+1)
   }
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index] <<- strategyName
      if (type=="search") {
         parameters$type[index]        <<- "search"
         parameters$subtype[index]     <<- "SMA"        
      } else {
         parameters$type[index]        <<- "SMA"
         parameters$subtype[index]     <<- inputName
      }
      parameters$startIndex[index] <<- max(SMA1,SMA2)+1
      parameters$inputDF[index]    <<- inputDF
      parameters$inputName[index]  <<- inputName
      parameters$bearish[index]    <<- bearish
      parameters$bullish[index]    <<- bullish      
      
       parameters$name1[index] <<- "SMA1"
      parameters$value1[index] <<-  SMA1
       parameters$name2[index] <<- "SMA2"
      parameters$value2[index] <<-  SMA2
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, costs=costs,
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
}


searchForOptimalSMA <- function(inputDF="dat", inputName="TR", 
                                minSMA1= 13L, maxSMA1= 15L, bySMA1= 1L,
                                minSMA2=  1L, maxSMA2=  2L, bySMA2= 1L, 
                                minBear= 11,  maxBear= 14,  byBear= 0.5, 
                                minDelta= 0,  maxDelta= 3,  byDelta=0.5, 
                                futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
                                minTR=0, maxVol=20, maxDD2=2, minTO=0.7, minScore=8.45, 
                                type="search", col=F, plotType="symbols", 
                                nameLength=22, plotEvery=def$plotEvery, force=F) {
   
   lastTimePlotted <- proc.time()
   dashes <- displaySummaryHeader(futureYears=futureYears, nameLength=nameLength)
   
   for ( SMA1 in seq(minSMA1, maxSMA1, by=bySMA1) ) 
      for ( SMA2 in seq(minSMA2, maxSMA2, by=bySMA2) )       
         for ( bear in seq(minBear, maxBear, by=byBear) ) {     
            for ( delta in seq(minDelta, maxDelta, by=byDelta) ) {
               bull = bear - delta               
               
               strategyName <- paste0("SMA_", SMA1, "_", SMA2, "__", bear, "_", bull)
               if (delta==0) bull = bear - 1e-3 # bear=bull creates problems
               
               createSMAstrategy(inputDF=inputDF, inputName=inputName, SMA1=SMA1, SMA2=SMA2,
                                 bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                                 strategyName=strategyName, force=force)                  
               showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                      minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                                      nameLength=nameLength, force=F)
            }
            if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > plotEvery ) { # we replot only if it's been a while
               plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
               lastTimePlotted <- proc.time()
            }
         }
   print(dashes)
   showSummaryForStrategy(def$typicalSMA, nameLength=nameLength, costs=costs)
   plotAllReturnsVsTwo(col=col, costs=costs, searchPlotType=plotType)
}



searchForTwoOptimalSMA <-function(plotType="symbols", force=F) {
   print("SMA1")
   searchForOptimalSMA(minSMA1= 9L, maxSMA1=11L, bySMA1= 1L,
                       minSMA2= 4L, maxSMA2= 6L, bySMA2= 1L, 
                       minBear=18,  maxBear=20,  byBear= 0.5, 
                       minDelta=0,  maxDelta=2,  byDelta=0.5, minScore=8.737)
   print("")
   
   print("SMA2")
   searchForOptimalSMA(minSMA1= 13L, maxSMA1= 15L, bySMA1= 1L,
                       minSMA2=  1L, maxSMA2=  2L, bySMA2= 1L, 
                       minBear= 11,  maxBear= 14,  byBear= 0.5, 
                       minDelta= 0,  maxDelta= 3,  byDelta=0.5, minScore=8.44)
}

## OBSOLETE
plotSMA <- function(SMA1=def$SMA1, SMA2=def$SMA2, futureYears=def$FutureYears, startYear=1885) {
   futureReturnName <- paste0("future", futureYears)
#    if (!futureReturnName %in% colnames(dat)) calcStocksFutureReturn(futureYears)
#    SMAname1 <- paste0("SMA", SMA1)
#    SMAname2 <- paste0("SMA", SMA2)
#    
#    par(mar=c(2.5, 4, 1.5, 1.5))
#    par(mfrow = c(2, 1))
#    temp <- numeric(numData)
#    
#    temp <- dat[, SMAname1] / dat[, SMAname2] - 1
#    
#    plot(dat$date, temp, type="l", xlim=c(dat$date[(startYear-1871)*12], dat$date[numData]), xlab="SMA ratio", 
#         ylab=paste0(SMAname1," / ",SMAname2," - ", 1+round(m,2)), ylim=c(-.5,.5))
#    plot(temp, dat[, futureReturnName], xlab="SMA ratio", ylab="future return", xlim=c(-.5,.5))
#    mod <- lm( dat[, futureReturnName] ~ temp)
#    abline(mod)
}

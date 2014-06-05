#default values of parameters:
setSMAdefaultValues <- function() {
   def$SMAinputDF          <<- "dat"
   def$SMAinputName        <<- "TR"
   def$SMA1                <<- 12L
   def$SMA2                <<- 1L
   def$SMAbearish          <<- 35
   def$SMAbullish          <<- 25
   #    def$SMAmedianAlloc      <<- 90
#    def$SMAinterQuartileAlloc <<- 50
   def$typicalSMA        <<- paste0("SMA_", def$SMAinputName, def$SMA1, "_", def$SMA2, "__", 
                                    def$SMAbearish, "_", def$SMAbullish)
}

## calculating simple moving average (SMA)
calcSMA <- function(inputDF, inputName, avgOver, SMAname) {
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="signal") input <- signal[, inputName]
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

#    bearish <- quantile(temp, 0.25, na.rm=T)[[1]]
#    bullish <- quantile(temp, 0.75, na.rm=T)[[1]]
#    
#    if (interQuartileAlloc==100) interQuartileAlloc <- 100-1e-3
#    
#    b <- tan(pi*(medianAlloc/100-.5))
#    tan2A <- tan(pi*interQuartileAlloc/100)
#    a <- sqrt(1/tan2A^2 + 1 + b^2) - 1/tan2A

# print(summary(SMAratio))
   
calcSignalForStrategy(strategyName, input=SMAratio, bearish=bearish, bullish=bullish,
                      signalMin=signalMin, signalMax=signalMax, startIndex=startIndex ) 
# signal[, strategyName] <<- a * ( 2 * (temp-bullish) / (bearish-bullish) - 1 ) + b
}


createSMAstrategy <- function(inputDF="dat", inputName="TR", SMA1=def$SMA1, SMA2=def$SMA2, 
                              bearish=def$detrendedBearish, bullish=def$detrendedBullish, 
                              signalMin=def$signalMin, signalMax=def$signalMax,
                              strategyName="", type="", futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   
   if (strategyName=="") 
      strategyName <- paste0("SMA_", inputName, SMA1, "_", SMA2, "__", bearish, "_", bullish)
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
      
#       parameters$medianAlloc[index] <<-  medianAlloc
#       parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
      
       parameters$name1[index] <<- "SMA1"
      parameters$value1[index] <<-  SMA1
       parameters$name2[index] <<- "SMA2"
      parameters$value2[index] <<-  SMA2
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
#    calcTRnetOfTradingCost(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)      
}


searchForOptimalSMA <-function(inputDF="dat", inputName="TR", 
                      minSMA1=12L, maxSMA1=12L, bySMA1=3L, minSMA2=1L, maxSMA2=1L, bySMA2=1L, 
                      minBear=0, maxBear=60, byBear=5, 
                      minBull=10, maxBull=50, byBull=5, 
                      futureYears=def$futureYears, tradingCost=def$tradingCost, type="search", 
                      minTR=def$technicalMinTR, maxVol=def$technicalMaxVol, maxDD2=def$technicalMaxDD2, 
                      minTO=def$technicalMinTO, col=F, plotType="symbols", force=F) {
   
   lastTimePlotted <- proc.time()
   print(paste0("strategy           |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )

   for ( SMA1 in seq(minSMA1, maxSMA1, by=bySMA1) ) 
      for ( SMA2 in seq(minSMA2, maxSMA2, by=bySMA2) )       
         for ( bear in seq(minBear, maxBear, by=byBear) ) {     
            for ( bull in seq(minBull, maxBull, by=byBull) ) {
               if (bear > bull + 1e-3 ) 
               {
                  strategyName <- paste0("SMA_", inputName, SMA1, "_", SMA2, "__", bear, "_", bull)
                   
                  createSMAstrategy(inputDF=inputDF, inputName=inputName, SMA1=SMA1, SMA2=SMA2,
                                    bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                                    strategyName=strategyName, force=force)                  
                  showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                         minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
               }
            }
            if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 5 ) { # we replot only if it's been a while
               plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
               lastTimePlotted <- proc.time()
            }
         }
   print("")
   showSummaryForStrategy(def$typicalSMA)
   plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
}

# 
# plotSMA <- function(SMA1=def$SMA1, SMA2=def$SMA2, futureYears=def$FutureYears, startYear=1885) {
#    futureReturnName <- paste0("future", futureYears)
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
# }

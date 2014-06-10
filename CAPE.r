

############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
##      CAPE.r generates a strategy       ##
##    based on the cyclically adjusted    ##
##     price-to-earnings ratio (CAPE)     ##
##                                        ##
############################################



#default values of parameters
setCAPEdefaultValues <- function() {
   def$CAPEyears    <<- 10
   def$CAPEcheat    <<- 2
   
   ## CAPE strategy without hysteresis
   def$CAPEavgOver1 <<- 35
   def$CAPEbearish1 <<- 21
   def$CAPEbullish1 <<- 21
   def$typicalCAPE1 <<- paste0("CAPE", def$CAPEyears, "avg", def$CAPEavgOver1, "__", 
                                def$CAPEbearish1, "_", def$CAPEbullish1)
   
   ## CAPE strategy with hysteresis
   def$CAPEavgOver2 <<- 33
   def$hystLoopWidthMidpoint2 <<- 19
   def$hystLoopWidth2 <<- 7
   def$slope2 <<- 2.2
   def$typicalCAPE2 <<- paste0("CAPE", def$CAPEyears, "avg", def$CAPEavgOver2, "__hyst_", 
                                def$hystLoopWidthMidpoint2, "_", def$hystLoopWidth2, "_", def$slope2)
   
   def$initialOffset <<- (def$CAPEyears-def$CAPEcheat)*12 + max(def$CAPEavgOver1, def$CAPEavgOver2)
   def$CAPEstrategies <<- c(def$typicalCAPE1, def$typicalCAPE2, def$typicalCAPE1, def$typicalCAPE2)
}


## calculating CAPE
calcCAPE <- function(years=def$CAPEyears, cheat=def$CAPEcheat) {
  CAPEname <- paste0("CAPE", years)
  addNumColToDat(CAPEname)
  months <- 12*years
  for(i in 1:(months-12*cheat)) { dat[i, CAPEname] <<- NA }
  for(i in (months-12*cheat+1):numData) 
    dat[i, CAPEname] <<- dat$price[i] / mean(dat$earnings[1:(i-1)], na.rm=T)
  for(i in (months+1):numData) 
     dat[i, CAPEname] <<- dat$price[i] / mean(dat$earnings[(i-months):(i-1)], na.rm=T)
}


## Average CAPE over 'avgOver' months
calcAvgCAPE <- function(years=def$CAPEyears, cheat=def$CAPEcheat, avgOver=def$CAPEavgOver) {
   CAPEname <- paste0("CAPE", years)
   if (!(CAPEname %in% colnames(dat))) 
      calcCAPE(years=years, cheat=cheat)
   if(avgOver>0) {
      avgCAPEname <- paste0(CAPEname,"avg",avgOver)
      addNumColToDat(avgCAPEname)
      #   message(paste0("NB: dat$", avgCAPEname, " has average of ", CAPEname, "over ", avgOver, " *months*."))
      for(i in 1:(avgOver-1)) dat[i, avgCAPEname] <<- NA # not enough data to calculate average
      for(i in avgOver:numData) dat[i, avgCAPEname] <<- mean(dat[(i-avgOver+1):i, CAPEname])
   }
}


## Calculate CAPE signal, to be used to calculate allocation
calcCAPEsignal <- function(CAPEname, bearish=def$CAPEbearish, bullish=def$CAPEbullish, 
                           signalMin=def$signalMin, signalMax=def$signalMax, 
                           startIndex=def$startIndex, strategyName="") {
   
   requireColInDat(CAPEname)   
   if(strategyName=="") strategyName <- paste0(CAPEname, "__", bearish, "_", bullish)
   calcSignalForStrategy(strategyName, input=dat[, CAPEname], bearish=bearish, bullish=bullish,
                         signalMin=signalMin, signalMax=signalMax, startIndex=startIndex ) 
}


## Calculate CAPE signal based on an hysteresis loop
## There can be no call to calcSignalForStrategy() because this is not a state function: 
## the signal depends on the direction (CAPE increasing or decreasing), not just on the value of the CAPE
calcCAPEsignalWithHysteresis <- function(CAPEname, hystLoopWidthMidpoint=def$hystLoopWidthMidpoint2, 
                                         hystLoopWidth=def$hystLoopWidth2, slope=def$slope2,
                                         signalMin=def$signalMin, signalMax=def$signalMax, 
                                         startIndex=def$startIndex, strategyName="") {
   bullish <- hystLoopWidthMidpoint - hystLoopWidth/2 + (0.5-signalMin)*slope
   bearish <- hystLoopWidthMidpoint + hystLoopWidth/2 - (signalMax-0.5)*slope
   
#    print(c(bearish, bullish))
   if (bearish < bullish - slope*(signalMax-signalMin) )
      stop("bearish (", bearish, ") cannot be smaller than bullish-slope*(signalMax-signalMin) (", 
           bullish-slope*(signalMax-signalMin), ").")
   
   requireColInDat(CAPEname)
   if(strategyName=="") 
      strategyName <- paste0(CAPEname, "__hyst_", hystLoopWidthMidpoint, "_", hystLoopWidth, "_", slope)

   addNumColToSignal(strategyName)
   CAPEinput <- dat[, CAPEname]
   
   dateRange <- startIndex:numData
   if( sum(is.na(CAPEinput[dateRange])) > 0) # there should be no NA after startIndex
      stop("Input contains NA after startIndex (", startIndex, ").")   
   processedCAPE <- numeric(numData)
      
   ## Initializing CAPEgoingUp
   if ( CAPEinput[startIndex] <= (bullish+bearish)/2 ) { # if CAPE is low, 
      #       moving <- T                                   # we consider that we are going up
      processedCAPE[startIndex] <- signalMax                     # we consider that we should be in stocks
   } else {
      #       moving <- T
      processedCAPE[startIndex] <- signalMin
   }
#    processedCAPE[startIndex] <- 0.5
   
   for (i in (startIndex+1):numData) {
      if( CAPEinput[i]<bearish & processedCAPE[i-1]>signalMax-0.01 )
         processedCAPE[i] <- processedCAPE[i-1]
      else if( CAPEinput[i]>bullish & processedCAPE[i-1]<signalMin+0.01 ) 
         processedCAPE[i] <- processedCAPE[i-1]
      else {
         processedCAPE[i] <- processedCAPE[i-1] - (CAPEinput[i]-CAPEinput[i-1]) * slope
         if (processedCAPE[i] <= signalMin) processedCAPE[i] <- signalMin
         if (processedCAPE[i] >= signalMax) processedCAPE[i] <- signalMax
      }
   }
   
#    for (i in (startIndex+1):numData) {
#       if( CAPEinput[i]>bearish & CAPEgoingUp ) {
#          processedCAPE[i] <- processedCAPE[i-1] - (CAPEinput[i]-CAPEinput[i-1]) * slope
#          if (processedCAPE[i] <= signalMin) { # if we went down below signalMin
#             processedCAPE[i] <- signalMin     # we saturate processedCAPE at signalMin
#             CAPEgoingUp <- F                  # we get ready for the CAPE to go down
#          }         
#       } else if( CAPEinput[i]<bullish & !CAPEgoingUp ) {
#          processedCAPE[i] <- processedCAPE[i-1] - (CAPEinput[i]-CAPEinput[i-1]) * slope         
#          if (processedCAPE[i] >= signalMax) { # if we went up abobe signalMax
#             processedCAPE[i] <- signalMax     # we saturate processedCAPE at signalMax
#             CAPEgoingUp <- T                  # we get ready for the CAPE to go up
#          }
#       } else processedCAPE[i] <- processedCAPE[i-1]   
#    }
   #    print(tail(processedCAPE))

#    plot(dat$numericDate[dateRange], processedCAPE[dateRange], type="l")
#    par(new=T)
#    plot(dat$numericDate[dateRange], CAPEinput[dateRange], type="l", col="blue")
#    par(new=F)
   
#        plot(CAPEinput[dateRange], processedCAPE[dateRange], type="l", xlim=c(5,30))

   signal[dateRange, strategyName] <<- processedCAPE[dateRange]
   
#    isZero <- tan ( pi * ( -signalMin / (signalMax - signalMin) - 1/2 ) ) 
#    isOne <- tan ( pi * ( (1-signalMin) / (signalMax - signalMin) - 1/2 ) )
#    a <- (isOne-isZero) / (bullish-bearish)
#    b <- isOne - a * bullish
#    signal[1:(startIndex-1), strategyName] <<- NA  
#    signal[dateRange, strategyName] <<- ( atan( a * processedCAPE[dateRange] + b ) / pi + .5 ) * 
#       (signalMax - signalMin) + signalMin   
}


createCAPEstrategy <- function(years=def$CAPEyears, cheat=def$CAPEcheat, avgOver=0, 
                               hysteresis=F, 
                               bearish=def$CAPEbearish1, bullish=def$CAPEbullish1, 
                               hystLoopWidthMidpoint=def$hystLoopWidthMidpoint2, 
                               hystLoopWidth=def$hystLoopWidth2, slope=def$slope2,
                               signalMin=def$signalMin, signalMax=def$signalMax,
                               strategyName="", type="CAPE", 
                               futureYears=def$futureYears, costs=def$tradingCost, 
                               coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {

   if (avgOver>0)
      CAPEname <- paste0("CAPE", years, "avg", avgOver)
   else CAPEname <- paste0("CAPE", years)
   
   if (!(CAPEname %in% colnames(dat)))
      calcAvgCAPE(years=years, cheat=cheat, avgOver=avgOver)
   startIndex <- (years-cheat)*12 + avgOver + 1
   
   if(hysteresis) {
      if(strategyName=="") 
         strategyName <- paste0(CAPEname, "__hyst_", hystLoopWidthMidpoint, "_", hystLoopWidth, "_", slope)
   } else {
      if(strategyName=="") strategyName <- paste0(CAPEname, "__", bearish, "_", bullish)
      if (bearish==bullish) bullish = bearish - 1e-3 # bear==bull creates problems
   }
   
   ## if data do not exist yet or we force recalculation:
   if (!(strategyName %in% colnames(TR)) | !(strategyName %in% colnames(alloc)) | force) { 
      if(hysteresis)          
         calcCAPEsignalWithHysteresis(CAPEname, hystLoopWidth=hystLoopWidth, 
                                      hystLoopWidthMidpoint=hystLoopWidthMidpoint, slope=slope,
                                      signalMin=signalMin, signalMax=signalMax,
                                      startIndex=startIndex, strategyName=strategyName)
      else
         calcCAPEsignal(CAPEname, bearish=bearish, bullish=bullish, signalMin=signalMin, signalMax=signalMax,
                        startIndex=startIndex, strategyName=strategyName)
      calcAllocFromSignal(strategyName)
      addNumColToTR(strategyName)
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
         parameters$subtype[index] <<- "CAPE"        
      } else {
         parameters$type[index]    <<- "CAPE"
      }
      parameters$startIndex[index] <<- startIndex
      parameters$avgOver[index]    <<- avgOver
      if (hysteresis) {
         parameters$name1[index]   <<- "hystLoopWidthMidpoint"
         parameters$value1[index]  <<-  hystLoopWidthMidpoint
         parameters$name2[index]   <<- "hystLoopWidthMidpoint"
         parameters$value2[index]  <<-  hystLoopWidthMidpoint
         parameters$name3[index]   <<- "slope"
         parameters$value3[index]  <<-  slope
      } else {
         parameters$bearish[index]    <<- bearish
         parameters$bullish[index]    <<- bullish
      }      
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, costs=costs,
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
}


searchForOptimalCAPE <-function(minYears=10, maxYears=10, byYears=0, cheat=2, 
                                minAvgOver=12L, maxAvgOver=36L, byAvgOver=12L, 
                                minBear=16L, maxBear=24L,  byBear=2L, 
                                minDelta=0L, maxDelta=5L, byDelta=1L, 
                                futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
                                minTR=0, maxVol=20, maxDD2=5, minTO=5, minScore=13,
                                coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
                                CPUnumber=def$CPUnumber, col=F, plotType="symbols", force=F) {
   
   lastTimePlotted <- proc.time()
   print(paste0("strategy           |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )

   for (years in seq(minYears, maxYears, by=byYears)) {
         calcCAPE(years=years, cheat=cheat)
         for (avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver)) {
            calcAvgCAPE(years=years, cheat=cheat, avgOver=avgOver)
            for ( bear in seq(minBear, maxBear, by=byBear) ) {      
               for ( delta in seq(minDelta, maxDelta, by=byDelta) ) {
                  bull = bear - delta
                  strategyName <- paste0("CAPE", years, "avg", avgOver, "__", bear, "_", bull)
                  
                  createCAPEstrategy(years=years, cheat=cheat, avgOver=avgOver, strategyName=strategyName, 
                                     bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                                     hysteresis=F, type="search", futureYears=futureYears, force=force)
                  showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                         minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, 
                                         minScore=minScore, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=F)
               }
               if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 5 ) { # we replot only if it's been a while
                  plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
                  lastTimePlotted <- proc.time()
               }
            }
         }
      }
   print("")
   showSummaryForStrategy(def$typicalCAPE1, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2)
   showSummaryForStrategy(def$typicalCAPE2, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2)
   plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
}

searchForOptimalCAPEwithHysteresis <-function(minYears=10, maxYears=10, byYears=0, cheat=2, 
                                              minAvgOver=24L, maxAvgOver=36L, byAvgOver=6L, 
                                              minMid=12L,   maxMid=22L,  byMid=2L, 
                                              minWidth=0.5, maxWidth=4.5,byWidth=1,
                                              minSlope=0.2, maxSlope=1,  bySlope=0.2, 
                                              futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
                                              minTR=0, maxVol=20, maxDD2=5, minTO=5, minScore=13,
                                              coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
                                              CPUnumber=def$CPUnumber, col=F, plotType="symbols", force=F) {
   
   lastTimePlotted <- proc.time()
   print(paste0("strategy                  |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   
   for (years in seq(minYears, maxYears, by=byYears)) {
      calcCAPE(years=years, cheat=cheat)
      for (avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver)) {
         calcAvgCAPE(years=years, cheat=cheat, avgOver=avgOver)
         if (avgOver>0)
            CAPEname <- paste0("CAPE", years, "avg", avgOver)
         else CAPEname <- paste0("CAPE", years)
         
         for ( mid in seq(minMid, maxMid, by=byMid) )   
            for ( width in seq(minWidth, maxWidth, by=byWidth) ) {      
               for ( slope in seq(minSlope, maxSlope, by=bySlope) ) {
                  strategyName <- paste0(CAPEname, "__hyst_", mid, "_", width, "_", slope)
                  
                  createCAPEstrategy(years=years, cheat=cheat, avgOver=avgOver, strategyName=strategyName, 
                                     hystLoopWidthMidpoint=mid, hystLoopWidth=width, slope=slope,
                                     signalMin=def$signalMin, signalMax=def$signalMax,
                                     hysteresis=T, type="search", futureYears=futureYears, force=force)
                  showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                         minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, 
                                         minScore=minScore, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=F)
               }
               if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 5 ) { # we replot only if it's been a while
                  plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
                  lastTimePlotted <- proc.time()
               }
            }
      }
   }
   print("")
   showSummaryForStrategy(def$typicalCAPE1, costs=costs, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2)
   showSummaryForStrategy(def$typicalCAPE2, costs=costs, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2)
   plotAllReturnsVsTwo(col=col, searchPlotType=plotType, costs=costs)
}

searchForTwoOptimalCAPE <-function(plotType="symbols", force=F) {
   
   ## What turns out to work best:
   ## a small value of delta, which means a strategy that is either all in or all out;
   ## a high value of the threshold (21), which leads to a high average stock allocation.
   searchForOptimalCAPE(minAvgOver=24L, maxAvgOver=48L, byAvgOver=3L, 
                        minBear=18L, maxBear=24L, byBear=1L, 
                        minDelta=0, maxDelta=2, byDelta=1, 
                        coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, minScore=14.5, 
                        plotType=plotType, force=force)
   
   ## Using a large penalty for drawdowns to get parameters corresponding to 
   ## the outgrowth (see figure) of DD2 = 1 and net return = 7.4%
   searchForOptimalCAPE(minAvgOver=30L, maxAvgOver=36L, byAvgOver=2L, 
                        minBear=15L, maxBear=20L, byBear=1L, 
                        minDelta=0, maxDelta=2, byDelta=1, 
                        coeffVol=def$coeffVol, coeffDD2=10*def$coeffDD2, 
                        maxDD2=1.1, minTR=6.8, minScore=15, 
                        plotType=plotType, force=force)   
}


plotFutureReturnVsCAPE <- function(CAPEname1=paste0("CAPE", def$CAPEyears),
                                   CAPEname2=paste0("CAPE", def$CAPEyears, "avg", def$CAPEavgOver1),
                                   #CAPEname2=paste0("CAPE", def$CAPEyears, "avg", def$CAPEavgOver2),
                                   col1="blue", col2="red", showFit=T, complete=T,
                                   futureYears=def$futureYears, 
                                   minCAPE=1.5, maxCAPE=44, minTR="", maxTR="", 
                                   pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                   pngName=paste0("figures/return_over_next_", futureYears, "_years_vs_CAPE.png") ) {
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
   
   para <- parametrizeFutureReturnPlot("", yLabel="", futureYears, minTR, maxTR) 
   yLabel <- paste("stock", para[[1]]); minTR <- as.numeric(para[[2]]); maxTR <- as.numeric(para[[3]])
         
   if (futureYears==10) 
      returnVect <- 100 * next10yrs[, "stocks"]
   else if (futureYears==20) 
      returnVect <- 100 * next20yrs[, "stocks"]
   else if (futureYears==30) 
      returnVect <- 100 * next30yrs[, "stocks"]
   
   dateRange <- 1:numData
   if ( complete ) {
      ## adjust dateRange based on complete data
      i <- 1
      while ( ( CAPEname1 != "" && is.na(dat[i, CAPEname2]) ) | 
              ( CAPEname2 != "" && is.na(dat[i, CAPEname2]) ) |
                is.na(returnVect[i]) ) 
         i <- i+1
      j <- numData
      while ( ( CAPEname1 != "" && is.na(dat[j, CAPEname2]) ) | 
              ( CAPEname2 != "" && is.na(dat[j, CAPEname2]) ) |
                is.na(returnVect[j]) ) 
         j <- j-1
      dateRange <- i:j
   }
   
   par(mar=c(4.2, 4.2, 1.5, 1.5))
   xRange <- c(minCAPE, maxCAPE)
   yRange <- c(minTR, maxTR)
   
   if( CAPEname1 != "" ) {   
      plot(dat[dateRange, CAPEname1], returnVect[dateRange], col=col1, xlim=xRange, ylim=yRange, xlab="CAPE", ylab=yLabel)
      if (showFit) {
         fit1 <- lm( returnVect[dateRange] ~ dat[dateRange, CAPEname1], na.action=na.omit)
         abline(fit1, col=col1)
      }
      par(new=T)
   }
   if( CAPEname2 != "" ) {
      plot(dat[dateRange, CAPEname2], returnVect[dateRange], col=col2, xlim=xRange, ylim=yRange, xlab="", ylab="")
      if (showFit) {
         fit2 <- lm( returnVect[dateRange] ~ dat[dateRange, CAPEname2], na.action=na.omit)
         abline(fit2, col=col2)
      }
   }
   
   legend( "topright", c(CAPEname1, CAPEname2), bty="n", col=c(col1, col2), pch=1 )
   par(new=F)
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }   
}




## OBSOLETE
searchForOptimalMetaCAPE <-function(stratName1=def$CAPEstrategies[[1]], stratName2=def$CAPEstrategies[[2]], 
                                    stratName3=def$CAPEstrategies[[3]], stratName4="",
                            minF1=20, maxF1=80, dF1=20, minF2=minF1, maxF2=maxF1, dF2=dF1, minF3=minF1, maxF3=maxF1, 
                            futureYears=def$FutureYears, tradingCost=def$TradingCost, cutoffScore=17, force=F) {

#    for(f1 in seq(minF1, maxF1, by=dF1)) 
#       for(f2 in seq(minF2, maxF2, by=dF2)) {
#          f4 <- 0   
#          f3 <- round(100 - f1 - f2)
#          #name = paste0("metaCAPE", "_", f1, "_", f2, "_", f3, "_",  f4)
#          displayName <- paste0(f1, "_", f2, "_", f3)
#          strategyName <- paste0("metaCAPE", displayName)
#          #print(name)
#          if ((f3 > minF3 - 1e-6) & (f3 < maxF3 + 1e-6)) {
#             calcMultiStrategyReturn(name1=stratName1, name2=stratName2, name3=stratName3, name4=stratName4, 
#                                     f1/100, f2/100, f3/100, f4/100, strategyName=strategyName, 10*12, delta="", force=force)
#             showSummaryStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
#                                 cutoffScore=cutoffScore, displayName=displayName, force=force)
#          }
#       }
#    showSummaryStrategy("CAPE10avg24",  futureYears=futureYears, tradingCost=tradingCost, displayName="CAPE10  ", force=F)
#    showSummaryStrategy("balanced40_25_10_25", futureYears=futureYears, tradingCost=tradingCost, displayName="balanced", force=F)
}


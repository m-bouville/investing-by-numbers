

############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
##    inflation.r generates a strategy    ##
##            based on inflation          ##
##                                        ##
############################################


#default values of parameters:
setInflationDefaultValues <- function() {
   def$inflationAvgOver <<- 10L
   def$inflationBearish <<- -1.4
   def$inflationBullish <<- -1.4
   def$typicalInflation <<- paste0("inflation_", def$inflationAvgOver, "__", 
                                   def$inflationBearish, "_", def$inflationBullish)   
}


addInflationToFutureReturns <- function() {   
   months   <- def$futureYears*12
   newRange <- 1:(numData-months)
   oldRange <- (months+1):numData
   NArange  <- (numData-months+1):numData
   
   inflationBuff <- numeric(numData)
   inflationBuff [newRange] <- dat$inflation15yr[oldRange]
   inflationBuff [NArange] <- NA
   
   # derivatives
   derivative5yrInflation5yrRescaledBuff <- numeric(numData)
   derivative5yrInflation5yrRescaledBuff[newRange] <- 
      0.08 - dat$derivative5yrInflation5yr[oldRange] * 2/3
   derivative5yrInflation5yrRescaledBuff[NArange] <- NA
   
   derivative7.5yrInflation7.5yrRescaledBuff <- numeric(numData)
   derivative7.5yrInflation7.5yrRescaledBuff[newRange] <- 
      0.09 - dat$derivative7.5yrInflation7.5yr[oldRange]
   derivative7.5yrInflation7.5yrRescaledBuff[NArange] <- NA
   
   derivative10yrInflation10yrRescaledBuff <- numeric(numData)
   derivative10yrInflation10yrRescaledBuff[newRange] <- 
      0.09 - dat$derivative10yrInflation10yr[oldRange]
   derivative10yrInflation10yrRescaledBuff[NArange] <- NA
   
   #    inflationDerivative15yrRescaledBuff <- numeric(numData)
   #    inflationDerivative15yrRescaledBuff[newRange] <- 
   #       0.09 - dat$inflationDerivative10yr[oldRange]
   #    inflationDerivative15yrRescaledBuff[NArange] <- NA
   
   # 2nd derivatives
#    inflation2ndDerivative5yrRescaledBuff <- numeric(numData)
#    inflation2ndDerivative5yrRescaledBuff[newRange] <- 
#       0.09 - dat$inflation2ndDerivative5yr[oldRange] * 2/3
#    inflation2ndDerivative5yrRescaledBuff[NArange] <- NA
# 
#    inflation2ndDerivative7.5yrRescaledBuff <- numeric(numData)
#    inflation2ndDerivative7.5yrRescaledBuff[newRange] <- 
#       0.09 - dat$inflation2ndDerivative7.5yr[oldRange] * 2/3
#    inflation2ndDerivative7.5yrRescaledBuff[NArange] <- NA
# 
#    inflation2ndDerivative10yrRescaledBuff <- numeric(numData)
#    inflation2ndDerivative10yrRescaledBuff[newRange] <- 
#       0.09 - dat$inflation2ndDerivative10yr[oldRange] * 2/3
#    inflation2ndDerivative10yrRescaledBuff[NArange] <- NA
   
   #    inflation2ndDerivative15yrRescaledBuff <- numeric(numData)
   #    inflation2ndDerivative15yrRescaledBuff[newRange] <- 
   #       0.09 - dat$inflation2ndDerivative15yr[oldRange] * 2/3
   #    inflation2ndDerivative15yrRescaledBuff[NArange] <- NA
   
   if ( (def$futureYears==10) ) {   
      next10yrs$inflation <<- inflationBuff      
      # derivatives
      next10yrs$derivative5yrInflation5yrRescaled     <<- derivative5yrInflation5yrRescaledBuff
      next10yrs$derivative7.5yrInflation7.5yrRescaled <<- derivative7.5yrInflation7.5yrRescaledBuff
      next10yrs$derivative10yrInflation10yrRescaled   <<- derivative10yrInflation10yrRescaledBuff
      #       next10yrs$inflationDerivative15yrRescaled  <<- inflationDerivative15yrRescaledBuff 

      # 2nd derivatives
#       next10yrs$inflation2ndDerivative5yrRescaled   <<- inflation2ndDerivative5yrRescaledBuff 
#       next10yrs$inflation2ndDerivative7.5yrRescaled <<- inflation2ndDerivative7.5yrRescaledBuff 
#       next10yrs$inflation2ndDerivative10yrRescaled  <<- inflation2ndDerivative10yrRescaledBuff 
      #       next10yrs$inflation2ndDerivative15yrRescaled  <<- inflation2ndDerivative15yrRescaledBuff 
   } 
   else if ( (def$futureYears==15) ) {
      next15yrs$inflation <<- inflationBuff
      # derivatives
      next15yrs$derivative5yrInflation5yrRescaled     <<- derivative5yrInflation5yrRescaledBuff
      next15yrs$derivative7.5yrInflation7.5yrRescaled <<- derivative7.5yrInflation7.5yrRescaledBuff
      next15yrs$derivative10yrInflation10yrRescaled   <<- derivative10yrInflation10yrRescaledBuff
      #       next15yrs$inflationDerivative15yrRescaled  <<- inflationDerivative15yrRescaledBuff

      # 2nd derivatives
#       next15yrs$inflation2ndDerivative5yrRescaled   <<- inflation2ndDerivative5yrRescaledBuff 
#       next15yrs$inflation2ndDerivative7.5yrRescaled <<- inflation2ndDerivative7.5yrRescaledBuff 
#       next15yrs$inflation2ndDerivative10yrRescaled  <<- inflation2ndDerivative10yrRescaledBuff 
#       next15yrs$inflation2ndDerivative15yrRescaled  <<- inflation2ndDerivative15yrRescaledBuff 
   }      
}


calcInflationDerivativeAndAddToDat <- function(inflationYears, derivativeYears) {
   
   inflationName <- paste0("inflation",inflationYears,"yr")
   if ( (!inflationName %in% colnames(dat)) || (!inflationName %in% colnames(TR)) ) {
      print(paste("Calculating inflation over", inflationYears, "years.") )
      calcInflationAndAddToDat(inflationYears)
   }
   
   inflationMonths  <- inflationYears *12
   derivativeMonths <- derivativeYears*12
   months <- inflationMonths+derivativeMonths
   
   derivativeBuff <- numeric(numData)
   derivativeBuff[1:months] <- NA
   
   for ( index in months:numData ) {
      dateRange <- (index-derivativeMonths+1):index
      fitPara <- regression(dat$numericDate[dateRange], dat[dateRange, inflationName])
      derivativeBuff[index] <- fitPara[[2]]
   }
   
   # adding to 'dat'
   derivativeName <- paste0("derivative", derivativeYears, "yrInflation", inflationYears,"yr")
   addNumColToDat(derivativeName)
   dat[derivativeName] <<- derivativeBuff
   
   # dummy for plotReturnSideways()
   TR[derivativeName] <<- numeric(numData)
}

addInflationDerivativesToDat <- function() {
   
   calcInflationDerivativeAndAddToDat(inflationYears= 5, derivativeYears= 5)
   calcInflationDerivativeAndAddToDat(inflationYears= 5, derivativeYears=10)
   calcInflationDerivativeAndAddToDat(inflationYears=10, derivativeYears= 5)
   calcInflationDerivativeAndAddToDat(inflationYears=10, derivativeYears=10)   
   
   
#    addNumColToDat("derivative5yrInflation5yr")
#    months <- 2*12*5
#    dat$derivative5yrInflation5yr[1:months]   <<- NA
#    dat$derivative5yrInflation5yr[(months+1):numData] <<- 
#       dat$inflation5yr[(months+1):numData] - dat$inflation5yr[((months+1):numData)-12*5]
      
   ## 2nd derivatives
#    addNumColToDat("inflation2ndDerivative5yr")
#    dat$inflation2ndDerivative5yr[1:(12*20)]   <<- NA
#    dat$inflation2ndDerivative5yr[(12*20+1):numData] <<- 
#       dat$inflationDerivative5yr[(12*20+1):numData] - 
#       dat$inflationDerivative5yr[((12*20+1):numData)-12*10]
}


calcInflationAndAddToDat <- function(inflationYears) {
   
   months <- 12*inflationYears   
   inflationBuff <- numeric(numData)
   inflationBuff[1:months] <- NA
   
   for ( index in months:numData ) {
      dateRange <- (index-months+1):index
      fitPara <- regression(dat$numericDate[dateRange], log(dat$CPI[dateRange]))
      inflationBuff[index] <- fitPara[[2]]
   }
   
   # adding to 'dat'
   inflationName <- paste0("inflation",inflationYears,"yr")
   addNumColToDat(inflationName)
   dat[inflationName] <<- inflationBuff
   
   # dummy for plotReturnSideways()
   TR[inflationName] <<- rep(NA, numData)
}

addInflationToDat <- function() {
   
   calcInflationAndAddToDat( 1)
   calcInflationAndAddToDat( 2)
   calcInflationAndAddToDat( 5)
   calcInflationAndAddToDat( 7)
   calcInflationAndAddToDat(10)

   TR$CPI          <<- dat$CPI
      
#    addNumColToDat("inflation2yr")
#    dat$inflation2yr[1:(12*2)]   <<- NA
#    dat$inflation2yr[(12*2 +1):numData]  <<- ( dat$CPI[    (12*2+1):numData] / 
#                                                  dat$CPI[  ((12*2+1):numData)-12*2]  ) ^ (1/2)  - 1
      
#    addInflationDerivativesToDat()
#   addInflationToFutureReturns()   
}


CalcInflationSignal <- function(avgOver=def$inflationAvgOver, 
                                bearish=def$inflationBearish, bullish=def$inflationBullish, 
                                signalMin=def$signalMin, signalMax=def$signalMax, strategyName) {
   
   inflationName <- paste0("inflation", avgOver, "yr")
   requireColInDat(inflationName)
   
   bearish=bearish/100
   bullish=bullish/100
   
   calcSignalForStrategy(strategyName, input=dat[, inflationName], bearish=bearish, bullish=bullish,
                         signalMin=signalMin, signalMax=signalMax, startIndex=avgOver*12+1 ) 
}


createInflationStrategy <- function(avgOver=def$inflationAvgOver, 
                                    bearish=def$inflationBearish, bullish=def$inflationBullish, 
                                    signalMin=def$signalMin, signalMax=def$signalMax,
                                    strategyName="",  type="inflation",
                                    futureYears=def$futureYears, costs=def$tradingCost, 
                                    coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {   
   if (strategyName=="") 
       strategyName <- paste0("inflation_", avgOver, "__", bearish, "_", bullish)

   if (bullish == bearish) bullish = bearish + 1e-3 # bearish=bullish creates problems
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      CalcInflationSignal(avgOver=avgOver, bearish=bearish, bullish=bullish, 
                          signalMin=signalMin, signalMax=signalMax, strategyName=strategyName)
      calcAllocFromSignal(strategyName)
      addNumColToTR(strategyName)  
      startIndex = avgOver*12+1
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
         parameters$type[index]     <<- "search"
         parameters$subtype[index]  <<- "inflation" 
         
      } else parameters$type[index] <<- "inflation" 
      parameters$startIndex[index] <<- startIndex
      parameters$avgOver[index]    <<- avgOver
      parameters$bearish[index]    <<- bearish
      parameters$bullish[index]    <<- bullish      
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, costs=costs,
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
}


searchForOptimalInflation <- function(minAvgOver=5, maxAvgOver=15, byAvgOver=5, 
                                      minBear=-2, maxBear=0, byBear=0.5, 
                                      minDelta=0, maxDelta=2, byDelta=0.5, 
                                      futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
                                      minTR=0, maxVol=20, maxDD2=5, minTO=4, minScore=9, 
                                      coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, col=F, 
                                      plotType="symbols", CPUnumber=def$CPUnumber, force=F) {
   
   lastTimePlotted <- proc.time()
   print(paste0("strategy               |  TR  |", futureYears, 
                " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score") )
   
   for (avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver)) {
      inflationName <- paste0("inflation", avgOver, "yr")
#       if ( !(inflationName %in% colnames(dat)) ) 
         {
         addNumColToDat(inflationName)
         dat[1:(12*avgOver), inflationName] <<- NA         
         dat[(12*avgOver+1):numData, inflationName] <<- ( dat$CPI[(12*avgOver+1):numData] / 
                                                         dat$CPI[((12*avgOver+1):numData)-(12*avgOver)] ) ^ (1/avgOver) - 1
      }
      
      for ( bear in seq(minBear, maxBear, by=byBear) ) {      
         for ( delta in seq(minDelta, maxDelta, by=byDelta) ) {
            bull = bear + delta
            strategyName <- paste0("inflation_", avgOver, "__", bear, "_", bull)
            if (delta==0) bull = bear + 1e-3 # bear=bull creates problems
            
            createInflationStrategy(avgOver=avgOver, strategyName=strategyName, 
                                    bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                                    type="search", futureYears=futureYears, force=force)
            
            showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                   minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, 
                                   minScore=minScore, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=F)            
         }
         if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > 10 ) { # we replot only if it's been a while
            plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
            lastTimePlotted <- proc.time()
         }
      }
   }
   
   print("")
   showSummaryForStrategy(def$typicalInflation, costs=costs, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2)
   plotAllReturnsVsTwo(col=col, costs=costs, searchPlotType=plotType)
}


## Plots with inflation
plotFutureReturnVsInflation <- function(avgOver=def$inflationAvgOver, futureYears=def$futureYears) {
   library(ggplot2)
#    qplot(100*dat$inflation10yr, 100*next10yrs$stocks) + 
#       geom_smooth()
#    
#    cutpoints <- quantile(dat$inflation10yr, seq(0, 1, length = 4), na.rm = TRUE)
#    dat$inflation10yrFactor <- cut(dat$inflation10yr, cutpoints)
#    qplot(dat$CAPE10, 100*next10yrs$stocks, color=dat$inflation10yrFactor) + 
#       geom_smooth(method = "lm")
   
   if ( "splitting" %in% colnames(dat) ) {
      if ( (def$futureYears==10) ) {
         dumbDF <- data.frame(numericDate=dat$numericDate, 
                           inflation10yr=100*dat$inflation10yr, 
                           next10yrsStocks=100*next10yrs$stocks, 
                           next10yrsBonds=100*next10yrs$bonds, 
                           next10yrsDifference=100*(next10yrs$stocks-next10yrs$bonds), 
                           CAPE10=dat$CAPE10,
                           splitting=dat$splitting)
         cutpoints <- quantile(dumbDF$inflation10yr, seq(0, 1, length = 4), na.rm = T)
         dumbDF$inflation10yrFactor <- cut(dumbDF$inflation10yr, cutpoints)
         qplot(CAPE10, 
               next10yrsDifference, 
               data=dumbDF[def$startIndex:(numData-12*futureYears), ], 
               color=inflation10yrFactor
               #             , facets= . ~ splitting
         ) +
            geom_smooth(method = "lm")
      } else if ( (def$futureYears==15) ) {
         dumbDF <- data.frame(numericDate=dat$numericDate, 
                              inflation15yr=100*dat$inflation15yr, 
                              next15yrsStocks=100*next15yrs$stocks, 
                              next15yrsBonds=100*next15yrs$bonds, 
                              next15yrsDifference=100*(next15yrs$stocks-next15yrs$bonds), 
                              CAPE10=dat$CAPE10,
                              splitting=dat$splitting)
         cutpoints <- quantile(dumbDF$inflation15yr, seq(0, 1, length = 4), na.rm = T)
         dumbDF$inflation15yrFactor <- cut(dumbDF$inflation15yr, cutpoints)
         qplot(CAPE10, 
               next15yrsDifference, 
               data=dumbDF[def$startIndex:(numData-12*futureYears), ], 
               color=inflation15yrFactor
               #             , facets= . ~ splitting
         ) +
            geom_smooth(method = "lm")
      }
   }
}

plotInflationDerivatives <- function(startYear=1900) {
   par(mfrow = c(2, 1))   
   #   plot(dat$numericDate, 100*dat$inflationDerivative5yr,   type="l", ylim=c(-8,10), xlim=c(startYear,2015) )
   plot(dat$numericDate, 100*dat$inflationDerivative7.5yr, 
        type="l", ylim=c(-8,10), xlim=c(startYear,2015), ylab="inflationDerivative7.5yr" )
   plot(dat$numericDate, 100*dat$inflationDerivative10yr, 
        type="l", ylim=c(-8,10), xlim=c(startYear,2015), ylab="inflationDerivative10yr" )
   par(mfrow = c(1, 1))  
}

plotInflation2ndDerivatives <- function(startYear=1900) {
   par(mfrow = c(3, 1))   
   plot(dat$numericDate, 100*dat$inflation2ndDerivative5yr,   
        type="l", ylim=c(-10,15), xlim=c(startYear,2015), ylab="inflation2ndDerivative5yr" )
   plot(dat$numericDate, 100*dat$inflation2ndDerivative7.5yr, 
        type="l", ylim=c(-10,15), xlim=c(startYear,2015), ylab="inflation2ndDerivative7.5yr" )
   plot(dat$numericDate, 100*dat$inflation2ndDerivative10yr,  
        type="l", ylim=c(-10,15), xlim=c(startYear,2015), ylab="inflation2ndDerivative10yr" )
   par(mfrow = c(1, 1))  
}

plotInflation10yr <- function(startYear=1900) {
   par(mfrow = c(3, 1))   
   plot(dat$numericDate, 100*dat$inflation10yr, 
        type="l", ylim=c(-3,9), xlim=c(startYear,2014), ylab="inflation10yr" )
   plot(dat$numericDate, 100*dat$inflationDerivative10yr, 
        type="l", ylim=c(-8,8), xlim=c(startYear,2014), ylab="inflationDerivative10yr" )
   plot(dat$numericDate, 100*dat$inflation2ndDerivative10yr,  
        type="l", ylim=c(-10,15), xlim=c(startYear,2014), ylab="inflation2ndDerivative10yr" )
   par(mfrow = c(1, 1))  
}

plotInflationDerivativeAndFutureReturn <- function
      (stratName3="derivative10yrInflation10yrRescaled", 
       stratName4="derivative7.5yrInflation7.5yrRescaled", startYear=1895, endYear=2000) {
   plotFutureReturn(startYear=startYear, endYear=endYear, legendPlacement="topleft", stratName2="stocks", 
                    # stratName3="stocks", col3=def$colConstantAlloc, 
                    stratName3=stratName3, col3="black", 
                    stratName4=stratName4, col4=def$colInflation
                    # stratName4="inflation2ndDerivative10yrRescaled", col4=def$colInflation
   )
}
   
plotInflationDerivativeAndFutureReturn2 <- function(inflationName="inflation2ndDerivative10yr", startYear=1895) {
      par(mfrow = c(2, 1))   
      plot(dat$numericDate, -100*dat[[inflationName]], 
           type="l", xlim=c(startYear+15,2013), ylab=inflationName )
      plotFutureReturn(startYear=startYear, endYear=1998, legendPlacement="topright", stratName2="stocks", 
                       stratName3="stocks", col3=def$colConstantAlloc, stratName4="bonds", col4=def$colBonds)
      par(mfrow = c(1, 1))  
}

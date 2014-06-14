

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
         plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
      }
   }
   
   print("")
   showSummaryForStrategy(def$typicalInflation, costs=costs, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2)
   plotAllReturnsVsTwo(col=col, costs=costs, searchPlotType=plotType)
}

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
   }
}

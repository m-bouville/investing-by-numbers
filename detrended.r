
############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
##         detrended.r subtracts          ##
##          the long-term trend           ##
##        from the current price          ##
##                                        ##
############################################



#default values of parameters:
setDetrendedDefaultValues <- function() {
   def$detrendedInputDF     <<- "dat"
   def$detrendedInputName   <<- "TR"
   
   ## detrended strategy in stocks 90% of the time, just dodging the worst bubbles
   def$detrendedAvgOver1    <<- 40L
   def$detrendedBearish1    <<- 29.4
   def$detrendedBullish1    <<- 29.4
   def$typicalDetrended1    <<- paste0("detrended_", def$detrendedInputName, "_avg", def$detrendedAvgOver1, "__", 
                                             def$detrendedBearish1, "_", def$detrendedBullish1)
   
   ## detrended strategy with lower stock allocation (and vol and DD)
   def$detrendedAvgOver2    <<- 23L
   def$detrendedBearish2    <<- 22.4
   def$detrendedBullish2    <<- 22.2
   def$typicalDetrended2    <<- paste0("detrended_", def$detrendedInputName, "_avg", def$detrendedAvgOver2, "__", 
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
                                    futureYears=def$futureYears, costs=def$tradingCost, 
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
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, costs=costs,
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
}


searchForOptimalDetrended <- function(inputDF=def$detrendedInputDF, inputName=def$detrendedInputName, 
                                      minAvgOver=20L, maxAvgOver=42L, byAvgOver=2L, 
                                      minBear=22, maxBear=32, byBear=1, 
                                      minDelta=0, maxDelta=1.5, byDelta=0.5, 
                                      futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
                                      minTR=0, maxVol=20, maxDD2=5, minTO=4, minScore=14.2, 
                                      coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, col=F, 
                                      plotType="symbols", CPUnumber=def$CPUnumber, 
                                      nameLength=28, plotEvery=def$plotEvery, force=F) {
   
   lastTimePlotted <- proc.time()
   print(paste0("strategy                     |  TR   ", futureYears, 
                " yrs: med, 5%| vol. alloc: avg, now|TO yrs| DD^2 | score") )
   print("-----------------------------+-------+--------------+-------+-------------+------+------+------")

   detrendedName <- paste0("detrended_", inputName)
   calcDetrended(inputDF=inputDF, inputName=inputName, detrendedName) 
   
   for (avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver)) {
      calcAvgDetrended(detrendedName, avgOver=avgOver)
      for ( bear in seq(minBear, maxBear, by=byBear) ) {      
         for ( delta in seq(minDelta, maxDelta, by=byDelta) ) {
            bull = bear - delta
            strategyName <- paste0(detrendedName, "_avg", avgOver, "__", bear, "_", bull)
            if (delta==0) bull = bear - 1e-3 # bear=bull creates problems
            
            createDetrendedStrategy(inputDF=inputDF, inputName=inputName, avgOver=avgOver, strategyName=strategyName, 
                                    bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                                    type="search", futureYears=futureYears, force=force)
            
            showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                   minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, 
                                   minScore=minScore, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, 
                                   nameLength=nameLength, force=F)
            
         }
         if ( (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > plotEvery ) { # we replot only if it's been a while
            plotAllReturnsVsTwo(col=col, searchPlotType=plotType)
            lastTimePlotted <- proc.time()
         }
      }
   }
   print("")
   showSummaryForStrategy(def$typicalDetrended1, costs=costs, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, nameLength=nameLength)
   showSummaryForStrategy(def$typicalDetrended2, costs=costs, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, nameLength=nameLength)
   plotAllReturnsVsTwo(col=col, costs=costs, searchPlotType=plotType)
}


## There are two optima fairly far apart in parameter space, with little of interest in between
searchForTwoOptimalDetrended <-function(plotType="symbols", force=F) {   
   searchForOptimalDetrended(minAvgOver=39L, maxAvgOver=41L, byAvgOver=1L, 
                             minBear=29.0,   maxBear=29.8,   byBear=0.2, 
                             minDelta=0,     maxDelta=0.4,   byDelta=0.2, 
                             minScore=14.55,
                             plotType=plotType, force=force)
   print("")
   searchForOptimalDetrended(minAvgOver=22L, maxAvgOver=24L, byAvgOver=1L, 
                             minBear=22.,    maxBear=23.,    byBear=0.2, 
                             minDelta=0,     maxDelta=0.6,   byDelta=0.2, 
                             minScore=14.52,
                             plotType=plotType, force=force)   
}


plotFutureReturnVsDetrended <- function(futureYears=def$futureYears) {
   library(ggplot2)   
   if ( "splitting" %in% colnames(dat) ) {
      dumbDF <- data.frame(numericDate=dat$numericDate, 
                           detrended=100*detrended_TR, 
                           next10yrsStocks=100*next10yrs$stocks, 
                           next10yrsBonds=100*next10yrs$bonds, 
                           next10yrsDifference=100*(next10yrs$stocks-next10yrs$bonds), 
                           CAPE10=dat$CAPE10,
                           splitting=dat$splitting)
#       cutpoints <- quantile(dumbDF$inflation10yr, seq(0, 1, length = 4), na.rm = T)
#       dumbDF$inflation10yrFactor <- cut(dumbDF$inflation10yr, cutpoints)
      qplot(detrended, 
            next10yrsDifference, 
            data=dumbDF[def$startIndex:(numData-12*futureYears), ]
#             , color=inflation10yrFactor
            #             , facets= . ~ splitting
      ) +
         geom_smooth(method = "lm")
   } else {
      dumbDF <- data.frame(numericDate=dat$numericDate, 
                           detrended=dat$detrended_TR, 
                           detrended1Alloc=100*alloc[, def$typicalDetrended1], 
                           detrended2Alloc=100*alloc[, def$typicalDetrended2], 
                           CAPE10=dat$CAPE10, 
                           CAPE1Alloc=100*alloc[, def$typicalCAPE1], 
                           CAPE2Alloc=100*alloc[, def$typicalCAPE2],
                           next10yrsStocks=100*next10yrs$stocks, 
                           next10yrsBonds=100*next10yrs$bonds, 
                           next10yrsDifference=100*(next10yrs$stocks-next10yrs$bonds))
      dumbDF <- dumbDF[def$startIndex:(numData-12*futureYears), ]
      
      CAPE10quartiles <- quantile(dumbDF$CAPE10, c(.25, .75, 1), na.rm = T)
      dumbDF$normalizedCAPE10 <- (dumbDF$CAPE10-CAPE10quartiles[1]) /
         (CAPE10quartiles[2]-CAPE10quartiles[1])
      detrendedQuartiles <- quantile(dumbDF$detrended, c(.25, .75, 1), na.rm = T)
      dumbDF$normalizedDetrended <- (dumbDF$detrended-detrendedQuartiles[1]) /
         (detrendedQuartiles[2]-detrendedQuartiles[1])
      
      #       CAPE10cutpoints <- quantile(dumbDF$normalizedCAPE10, seq(0, 1, length = 3), na.rm = T)
      dumbDF$CAPEranges <- cut(dumbDF$normalizedCAPE10, c(-Inf, 0, 1, Inf) )
      #       detrendedCutpoints <- quantile(dumbDF$normalizedDetrended, seq(0, 1, length = 3), na.rm = T)
      dumbDF$detrendedRanges <- cut(dumbDF$normalizedDetrended, c(-Inf, 0, 1, Inf) )
      
      dumbDF$normalizedDifference <- dumbDF$normalizedDetrended - dumbDF$normalizedCAPE10
      differenceCutpoints <- quantile(dumbDF$normalizedDifference, seq(0, 1, length = 6), na.rm = T)
      dumbDF$differenceRanges <- cut(dumbDF$normalizedDifference, differenceCutpoints )
      #       dumbDF$combinedRanges <- cut(dumbDF$CAPE10, cutpoints )
      
      p <- qplot(normalizedDetrended, 
                 next10yrsDifference, 
                 data=dumbDF 
                 , color= differenceRanges
      ) + geom_smooth(method = "lm")      
      print(p)
      
      CL <- levels(dumbDF$CAPEranges)
      DL <- levels(dumbDF$detrendedRanges)
      print("   CAPE   detrended   fraction,      ")
      for (i in 1:length(CL))
         for (j in 1:length(DL)) {
           criterion <- ( dumbDF$CAPEranges==CL[i] & dumbDF$detrendedRanges==DL[j] )
           avg <- mean(dumbDF$next10yrsDifference, na.rm=T)
           #             print( summary( dumbDF$CAPEranges==CL[i] & dumbDF$detrendedRanges==DL[j] ) )
           if( sum(criterion) > 0 )
            print(c(CL[i], DL[j], 
                    round(sum(criterion) / length(dumbDF$CAPEranges), 2),
                    round(mean(dumbDF$next10yrsDifference[ criterion ], na.rm=T) - avg, 1) ) )
         }
            
   }
}

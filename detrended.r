
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
      
   
   def$detrendedYears1      <<- 14L    # optimized with costs = 2%
   def$detrendedCheat1      <<-  6L 
   def$detrendedAvgOver1    <<- 69L 
   def$detrendedBearish1    <<- 10
   def$detrendedBullish1    <<- 10 
   def$typicalDetrended1    <<- paste0("detrended_", def$detrendedYears1, "_", def$detrendedAvgOver1, "_", 
                                       def$detrendedBearish1, "_", def$detrendedBullish1)
   
   def$detrendedYears2      <<- 13L    # optimized with costs = 2%
   def$detrendedCheat2      <<-  6L 
   def$detrendedAvgOver2    <<- 30L 
   def$detrendedBearish2    <<- -5
   def$detrendedBullish2    <<- -5
   def$typicalDetrended2    <<- paste0("detrended_", def$detrendedYears2, "_", def$detrendedAvgOver2, "_", 
                                       def$detrendedBearish2, "_", def$detrendedBullish2)
   
#    ## detrended strategy with lower stock allocation (and vol and DD)
#    def$detrendedAvgOver1    <<- 23L    # optimized with costs = 2%
#    def$detrendedBearish1    <<- 22.6
#    def$detrendedBullish1    <<- 22.4 
#    def$typicalDetrended1    <<- paste0("detrended_", def$detrendedAvgOver1, "_", 
#                                        def$detrendedBearish1, "_", def$detrendedBullish1)
#    
#    ## detrended strategy in stocks 90% of the time, just dodging the worst bubbles
#    def$detrendedAvgOver2    <<- 40L    # optimized with costs = 2%
#    def$detrendedBearish2    <<- 29.4
#    def$detrendedBullish2    <<- 29.4
#    def$typicalDetrended2    <<- paste0("detrended_", def$detrendedAvgOver2, "_", 
#                                        def$detrendedBearish2, "_", def$detrendedBullish2)
}

calcDetrended <- function(inputDF, inputName, years, cheat=0, detrendedName="") {
   
   if (inputDF=="dat")             logInput <- log( dat[, inputName] )
   else if (inputDF=="signal")     logInput <- log( signal[, inputName] )
   else if (inputDF=="alloc")      logInput <- log( alloc[, inputName] )
   else if (inputDF=="TR")         logInput <- log( TR[, inputName] )
   else if (inputDF=="next30yrs")  logInput <- log( next30yrs[, inputName] )
   else stop("data frame ", inputDF, " not recognized")
   
   if (detrendedName=="") {
      if (inputName=="TR") detrendedName <- paste0("detrended")
      else detrendedName <- paste0("detrended_", inputName)
   }
   
   addNumColToDat(detrendedName)
   
   for(i in 1:(12*(years-cheat)) )
      dat[i, detrendedName] <<- NA
   for(i in (12*(years-cheat)+1):numData) {
      startIndex <- max(1, i-12*years+1) # we do it over 'years' years, unless that's a negative index
      fitPara <- regression(TR$numericDate[startIndex:i], logInput[startIndex:i])
      a <- fitPara[[1]];  b <- fitPara[[2]]      
      dat[i, detrendedName] <<- logInput[i] - (a + b * TR$numericDate[i])
   }
}


## Average detrended over 'avgOver' months
calcAvgDetrended <- function(detrendedName, avgOver=def$detrendedAvgOver) {

   requireColInDat(detrendedName)
   avgDetrendedName <- paste0(detrendedName, "_avg", avgOver)
   addNumColToDat(avgDetrendedName)
   for(i in 1:(avgOver-1))   dat[i, avgDetrendedName] <<- NA # not enough data to calculate average
   for(i in avgOver:numData) dat[i, avgDetrendedName] <<- mean(dat[(i-avgOver+1):i, detrendedName])  
}


CalcDetrendedSignal <- function(inputDF=def$detrendedInputDF, inputName=def$detrendedInputName,
                                years, cheat,
                                bearish=def$detrendedBearish, bullish=def$detrendedBullish, 
                                signalMin=def$signalMin, signalMax=def$signalMax, strategyName, avgOver) {
   
   if (inputName=="TR")
      detrendedName <- paste0("detrended_", years)
   else detrendedName <- paste0("detrended_", years, "_", inputName)
   
   if (!detrendedName %in% colnames(dat)) 
      calcDetrended(inputDF=inputDF, inputName=inputName, detrendedName=detrendedName, years=years, cheat=cheat)

   if( is.numeric(avgOver) ) {
      if( ! paste0(detrendedName, "_avg", avgOver) %in% colnames(dat) ) 
         calcAvgDetrended(detrendedName, avgOver)
      detrendedName <- paste0(detrendedName, "_avg", avgOver)
   }
   
   calcSignalForStrategy(strategyName, input=dat[, detrendedName], bearish=bearish/100, bullish=bullish/100,
                         signalMin=signalMin, signalMax=signalMax, startIndex=avgOver+12*(years-cheat)+1 ) 
}


createDetrendedStrategy <- function(inputDF=def$detrendedInputDF, inputName=def$detrendedInputName, 
         years, cheat, avgOver, bearish, bullish, 
         signalMin=def$signalMin, signalMax=def$signalMax,
         strategyName="",  type="detrended",
         futureYears=def$futureYears, costs=def$tradingCost, 
         coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {   
   if (strategyName=="") 
      strategyName <- paste0("detrended_", years, "_", avgOver, "_", bearish, "_", bullish)
   
   if (bullish == bearish) bullish = bearish - 1e-3 # bearish=bullish creates problems
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      CalcDetrendedSignal(inputDF=inputDF, inputName=inputName, bearish=bearish, bullish=bullish, 
                          years=years, cheat=cheat,
                          signalMin=signalMin, signalMax=signalMax, strategyName=strategyName, avgOver=avgOver)
      calcAllocFromSignal(strategyName)
      addNumColToTR(strategyName)  
      startIndex = avgOver + 12*(years-cheat)+1 # max(months, sum(is.na(alloc[ ,strategyName])))+1
      calcStrategyReturn(strategyName, startIndex)
   }
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index]   <<- strategyName
      if (type=="training") {
         parameters$type[index]        <<- "training"
         parameters$subtype[index]     <<- "detrended" 
      } else {
         parameters$type[index]        <<- "detrended"
         parameters$subtype[index]     <<- inputName
      }
      parameters$inputDF[index]    <<- inputDF
      parameters$inputName[index]  <<- inputName
      parameters$startIndex[index] <<- startIndex
      parameters$years[index]      <<- years
      parameters$cheat[index]      <<- cheat
      parameters$avgOver[index]    <<- avgOver
      parameters$bearish[index]    <<- bearish
      parameters$bullish[index]    <<- bullish      
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, costs=costs,
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
}


calcOptimalDetrended <- function(inputDF, inputName, 
         minYears, maxYears, byYears, cheat, minAvgOver, maxAvgOver, byAvgOver, 
         minBear, maxBear, byBear, minDelta, maxDelta, byDelta, 
         futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore, 
         coeffTR, coeffVol, coeffDD2, countOnly,
         col, plotType, xMinVol, xMaxVol, xMinDD2, xMaxDD2,
         CPUnumber, nameLength, plotEvery, force) {
   
   counterTot <- 0; counterNew <- 0
   lastTimePlotted <- proc.time()

   # creating ranges that allow to sample the parameter space broadly initially
   rangeYears   <- createRange(minYears,   maxYears,   byYears)
   rangeAvgOver <- createRange(minAvgOver, maxAvgOver, byAvgOver)
   rangeBear    <- createRange(minBear,    maxBear,    byBear)
   rangeDelta   <- createRange(minDelta,   maxDelta,   byDelta)   
   
   for (years in rangeYears) {
      if (inputName=="TR") detrendedName <- paste0("detrended_", years)
         else detrendedName <- paste0("detrended_", years, "_avg", inputName)    
      if (!countOnly && (!detrendedName %in% colnames(dat) || force) )
         calcDetrended(inputDF=inputDF, inputName=inputName, years=years, cheat=cheat, detrendedName=detrendedName) 
      
      for (avgOver in rangeAvgOver) {
         if (!countOnly && (!paste0(detrendedName, "_", avgOver) %in% colnames(dat) || force) ) 
            calcAvgDetrended(detrendedName, avgOver=avgOver)
         for (bear in rangeBear) {      
            for (delta in rangeDelta) {
               bull = bear - delta
               strategyName <- paste0(detrendedName, "_", avgOver, "_", bear, "_", bull)
               if (delta==0) bull = bear - 1e-3 # bear=bull creates problems
               
               counterTot <- counterTot + 1 
               if(countOnly) {
                  if ( !(strategyName %in% colnames(TR)) | !(strategyName %in% colnames(alloc)) )
                     counterNew <- counterNew + 1                  
               } else {
                  createDetrendedStrategy(inputDF=inputDF, inputName=inputName, years=years, cheat=cheat,
                                          avgOver=avgOver, strategyName=strategyName, 
                                          bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                                          type="training", futureYears=futureYears, force=force)
                  
                  showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                         minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, 
                                         minScore=minScore, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, 
                                         nameLength=nameLength, force=F)            
               }
            }
            if ( !countOnly && (summary(proc.time())[[1]] - lastTimePlotted[[1]] ) > plotEvery ) { 
               # we replot only if it's been a while
               plotAllReturnsVsTwo(col=col, trainingPlotType=plotType,
                                   xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2)
               lastTimePlotted <- proc.time()
            }
         }
      }
   }
   if(countOnly)
      print (paste0("Running ", counterTot, " parameter sets (", counterNew, " new)"))
}

searchForOptimalDetrended <- function(inputDF=def$detrendedInputDF, inputName=def$detrendedInputName, 
         minYears=  40L, maxYears=   48L,  byYears=   4L, cheat=8L, 
         minAvgOver=12L, maxAvgOver=108L,  byAvgOver=12L,
         minBear=  -16,  maxBear=    54,   byBear=    8,
         minDelta=   0,  maxDelta=    0,   byDelta=   1,
         futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, 
         minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=2, minScore=12, 
         coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, col=F, 
         plotType="symbols", CPUnumber=def$CPUnumber, 
         xMinVol=16.5, xMaxVol=24.5, xMinDD2=3, xMaxDD2=11.5,
         referenceStrategies=c(def$typicalDetrended1, def$typicalDetrended2), 
         nameLength=25, plotEvery=def$plotEvery, force=F) {
 
   if (dataSplit != "training") 
      warning("Doing training in '", dataSplit, "' mode.", immediate.=T)
   
   cleanUpStrategies()
   
   # calculate how many parameters sets will be run   
   calcOptimalDetrended(inputDF, inputName, minYears, maxYears, byYears, cheat, 
                        minAvgOver, maxAvgOver, byAvgOver, 
                        minBear, maxBear, byBear, minDelta, maxDelta, byDelta, 
                        futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore, 
                        coeffTR, coeffVol, coeffDD2, countOnly=T,
                        col, plotType, xMinVol, xMaxVol, xMinDD2, xMaxDD2,
                        CPUnumber, nameLength, plotEvery, force)   
   
   dashes <- displaySummaryHeader(futureYears=futureYears, nameLength=nameLength)
   
   # actually calculating
   calcOptimalDetrended(inputDF, inputName, minYears, maxYears, byYears, cheat, 
                        minAvgOver, maxAvgOver, byAvgOver, 
                        minBear, maxBear, byBear, minDelta, maxDelta, byDelta, 
                        futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore, 
                        coeffTR, coeffVol, coeffDD2, countOnly=F,
                        col, plotType, xMinVol, xMaxVol, xMinDD2, xMaxDD2,
                        CPUnumber, nameLength, plotEvery, force)

   print(dashes)
   if( length(referenceStrategies) > 0 )
      for ( i in 1:length(referenceStrategies) )
         showSummaryForStrategy(referenceStrategies[i], nameLength=nameLength, 
                                coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, costs=costs)
   plotAllReturnsVsTwo(col=col, costs=costs, trainingPlotType=plotType, 
                       xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2)
}


## These are two optimal (and fairly far apart in parameter space), with little of interest in between
searchForTwoOptimalDetrended <-function(minScore1=15, minScore2=14.4, do1=T, do2=T,
                                        plotType="symbols", force=F) {   
   if(do1) {
      print("Detrended 1")
      searchForOptimalDetrended(minAvgOver=22L,  maxAvgOver=24L,  byAvgOver=1L, 
                                minBear=   22.,  maxBear=   23.,  byBear=   0.2, 
                                minDelta=   0,   maxDelta=  0.8,  byDelta=  0.2, 
                                referenceStrategies=def$typicalDetrended1,
                                minScore=   minScore1,      plotType=plotType, force=force)   
   }
   if(do1 && do2) { # needed only if we do both
      print("")
      print("*******************************************************************************************")
      print("")
   }
   if(do2) {
      print("Detrended 2")
      searchForOptimalDetrended(minAvgOver=39L,  maxAvgOver=41L,  byAvgOver=1L, 
                                minBear=   29.0, maxBear=   30,   byBear=   0.2, 
                                minDelta=   0,   maxDelta=   0.6, byDelta=  0.2, 
                                referenceStrategies=def$typicalDetrended2,
                                minScore=   minScore2,   plotType=plotType, force=force)
   }
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

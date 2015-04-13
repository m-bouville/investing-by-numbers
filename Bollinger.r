

############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
##    Bollinger.r generates a strategy    ##
##        based on Bollinger bands        ##
##                                        ##
############################################



#default values of parameters:
setBollDefaultValues <- function() {
   def$BollInputDF     <<- "dat"
   def$BollInputName   <<- "TR"
   
   def$BollAvgOver1    <<-  14L    #  14L  # with costs=2%, w/o yoyo prevention
   def$BollBearish1    <<- -33.5   # -31.5
   def$BollBullish1    <<- -33.5   # -31.5 
   def$BollYoyoOffset1 <<-   3L    #   1L
   def$BollYoyoPenalty1<<-   1     #   0
   typical$Boll1       <<- nameBollStrategy(def$BollInputName, def$BollAvgOver1,    def$BollBearish1, 
                                            def$BollBullish1,  def$BollYoyoOffset1, def$BollYoyoPenalty1)
   
   def$BollAvgOver2    <<-  10L    # 10L   # with costs=2%, w/o yoyo prevention
   def$BollBearish2    <<-  46.5   # 46.5
   def$BollBullish2    <<-  46.5   # 46.5 
   def$BollYoyoOffset2 <<-   3L    #  1L
   def$BollYoyoPenalty2<<-   1     #  0
   typical$Boll2       <<- nameBollStrategy(def$BollInputName, def$BollAvgOver2,    def$BollBearish2, 
                                            def$BollBullish2,  def$BollYoyoOffset2, def$BollYoyoPenalty2)
}

nameBollStrategy <- function(inputName="TR", avgOver, bearish, bullish, yoyoOffset=1L, yoyoPenalty=0) { 
   strategyName <- paste0("Boll_", avgOver, "_", bearish, "_", bullish)
   if (yoyoOffset>=2 && yoyoPenalty>0)
      strategyName <- paste0(strategyName, "_", yoyoOffset, "_", yoyoPenalty)
   if (inputName!="TR")
      strategyName <- paste0(strategyName, "__", inputName)
   return(strategyName)
}


calcBollSignal <- function(inputDF=def$BollInputDF, inputName=def$BollInputName, allocSource="",
                           avgOver=def$BollAvgOver, bearish=def$BollBearish, bullish=def$BollBullish, 
                           signalMin=def$signalMin, signalMax=def$signalMax, yoyoOffset=1, yoyoPenalty=0,
                           strategyName="", force=F) {

   BollBandsName <- paste0(inputName, avgOver)
   avgName <- paste0("avg_", BollBandsName)
   SDname <- paste0("SD_", BollBandsName)
   
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="signal")     input <- signal[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")
   
   if (allocSource == "") {
      if (inputName=="TR")
         allocSource <- "stocks"             # When in market we are in stocks.
      else if (inputDF=="TR") {
         allocSource <- alloc[, inputName]   # When in market we are in 'inputName'.
         # print(paste0("Using alloc[, '", inputName, "'] when in market.") )
      } else {
         allocSource <- "stocks"
         warning("Using stocks when in market (didn't know what else to do with inputDF=", 
                 inputDF, " and inputName=", inputName, ").")
      }
   } else if (allocSource != "stocks")
      allocSource <- alloc[, allocSource]    
   
   if ( !(avgName %in% colnames(dat)) | !(SDname %in% colnames(dat)) | force) {
      # if data do not exist yet or we force recalculation:
      addNumColToDat(avgName)
      addNumColToDat(SDname)
      dat[1:(avgOver-1), avgName] <<- NA # not enough data to calculate average or SD
      dat[1:(avgOver-1), SDname]  <<- NA 
      for(i in avgOver:numData) {
         dat[i, avgName] <<- mean(input[(i-avgOver+1):i])  
         dat[i, SDname]  <<-   sd(input[(i-avgOver+1):i])
      }
   }      

   if ( !(strategyName %in% colnames(signal)) | force) {
      # if data do not exist yet or we force recalculation:
      rawSignal <- numeric(numData)
      rawSignal <- (input - dat[, avgName]) / dat[, SDname]

      startIndex <- sum(is.na(rawSignal)) + 1
      calcSignalForStrategy(strategyName, input=rawSignal, allocSource=allocSource,
                            bearish=bearish, bullish=bullish, 
                            yoyoOffset=yoyoOffset, yoyoPenalty=yoyoPenalty,
                            signalMin=signalMin, signalMax=signalMax, startIndex=startIndex ) 
   }
}

createBollStrategy <- function(inputDF=def$BollInputDF, inputName=def$BollInputName, allocSource="",
                               avgOver=def$BollAvgOver, bearish=def$BollBearish, bullish=def$BollBullish, 
                               signalMin=def$signalMin, signalMax=def$signalMax, 
                               yoyoOffset=1, yoyoPenalty=0,
                               strategyName="", type="", subtype="", futureYears=def$futureYears, costs=def$tradingCost, 
                               coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {
   
   if(strategyName=="") 
      strategyName <- nameBollStrategy(inputName, avgOver, bearish, bullish, yoyoOffset, yoyoPenalty)
      
   if (bullish == bearish) bullish <- bearish + 1e-3 # bearish==bullish creates problems
   bearish <- bearish/100
   bullish <- bullish/100
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      calcBollSignal(inputDF=inputDF, inputName=inputName, avgOver=avgOver, 
                     bearish=bearish, bullish=bullish, allocSource=allocSource,
                     signalMin=signalMin, signalMax=signalMax, yoyoOffset=yoyoOffset, yoyoPenalty=yoyoPenalty,
                     strategyName=strategyName, force=force)

      calcAllocFromSignal(strategyName)

      addNumColToTR(strategyName)
      startIndex <- sum(is.na(alloc[, strategyName])) + 1
      calcStrategyReturn(strategyName, startIndex)
   }
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      if(inputName=="TR")
         if (type=="training") {
            parameters$type[index]    <<- "training"
            parameters$subtype[index] <<- "Bollinger"        
         } else {
            parameters$type[index]    <<- "Bollinger"
            parameters$subtype[index] <<- inputName
         }
      else if(substr(inputName, 1, 4)=="CAPE")
         if (type=="training") {
            parameters$type[index]    <<- "training"
            parameters$subtype[index] <<- paste0("Boll_CAPE")
         } else {
            parameters$type[index]    <<- paste0("Boll_CAPE")
            parameters$subtype[index] <<- inputName
         }      
      else if(substr(inputName, 1, 9)=="detrended")
         if (type=="training") {
            parameters$type[index]    <<- "training"
            parameters$subtype[index] <<- paste0("Boll_detrended")
         } else {
            parameters$type[index]    <<- paste0("Boll_detrended")
            parameters$subtype[index] <<- inputName
         }
      else if(substr(inputName, 1, 4)=="Boll")
         if (type=="training") {
            parameters$type[index]    <<- "training"
            parameters$subtype[index] <<- paste0("Boll_Boll")
         } else {
            parameters$type[index]    <<- paste0("Boll_Boll")
            parameters$subtype[index] <<- inputName
         }
      else if(substr(inputName, 1, 8)=="balanced")
         if (type=="training") {
            parameters$type[index]    <<- "training"
            parameters$subtype[index] <<- paste0("Boll_balanced")
         } else {
            parameters$type[index]    <<- paste0("Boll_balanced")
            parameters$subtype[index] <<- inputName
         }
      if    (type != "") parameters$type[index]    <<- type
      if (subtype != "") parameters$subtype[index] <<- subtype
      
      parameters$startIndex[index] <<- startIndex
      parameters$inputDF[index]    <<- inputDF
      parameters$inputName[index]  <<- inputName
      parameters$bearish[index]    <<- bearish
      parameters$bullish[index]    <<- bullish  
      parameters$avgOver[index]    <<- avgOver
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, costs=costs,
                             coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
}

calcOptimalBoll <- function(inputDF, inputName, allocSource, minAvgOver, maxAvgOver, byAvgOver, 
                            minBear, maxBear, byBear, minDelta, maxDelta, byDelta,  
                            minYoyoOffset, maxYoyoOffset, byYoyoOffset, minYoyoPenalty, maxYoyoPenalty, byYoyoPenalty,
                            futureYears, costs, type,
                            minTR, maxVol, maxDD2, minTO, minScore, coeffTR, coeffVol, coeffDD2,
                            xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly,
                            col, plotType, nameLength, plotEvery, force) {
   
   counterTot <- 0; counterNew <- 0
   lastTimePlotted <- proc.time()
   
   # creating ranges that allow to sample the parameter space broadly initially
   rangeAvgOver    <- createRange(minAvgOver,    maxAvgOver,    byAvgOver)
   rangeBear       <- createRange(minBear,       maxBear,       byBear)
   rangeYoyoOffset <- createRange(minYoyoOffset, maxYoyoOffset, byYoyoOffset)
   rangeDelta      <- createRange(minDelta,      maxDelta,      byDelta)   
   
   for (avgOver in rangeAvgOver) 
      for (bear in rangeBear) 
         for (yoyoOffset in rangeYoyoOffset) {
            if (yoyoOffset>1) # if yoyoOffset==1 there is no yoyo penalization: no need for a whole range
               rangeYoyoPenalty <- createRange(minYoyoPenalty, maxYoyoPenalty, byYoyoPenalty)
            else rangeYoyoPenalty <- minYoyoPenalty 
            
            for (yoyoPenalty in rangeYoyoPenalty) {
               if (yoyoOffset==1 || yoyoPenalty==0) {# if no yoyo prevention because of _either_ parameter
                   yoyoOffset<-1                     #   then no yoyo prevention because of _both_ parameter.
                   yoyoPenalty<-0                    # This prevents several equivalent parameter couples
               }                                     #   (e.g. doing both offset=1, penalty=1 and offset=2, penalty=0)
               
               for (delta in rangeDelta) {
                  bull = bear + delta
                  strategyName <- nameBollStrategy(inputName, avgOver, bear, bull, yoyoOffset, yoyoPenalty)
                  
                  counterTot <- counterTot + 1 
                  if(countOnly) {
                     if ( force || !(strategyName %in% colnames(TR)) || !(strategyName %in% colnames(alloc)) )
                        counterNew <- counterNew + 1
                  } else {
                     createBollStrategy(inputDF, inputName, allocSource=allocSource, avgOver=avgOver, type=type,
                                        bearish=bear, bullish=bull, signalMin=def$signalMin, signalMax=def$signalMax,
                                        strategyName=strategyName, futureYears=futureYears, 
                                        yoyoOffset=yoyoOffset, yoyoPenalty=yoyoPenalty,
                                        force=force)
                     
                     showSummaryForStrategy(strategyName, futureYears=futureYears, costs=costs, 
                                            minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, minScore=minScore, 
                                            nameLength=nameLength, force=F) 
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
      return( c(counterTot, counterNew) )
}

searchForOptimalBoll <- function(inputDF="dat",   inputName="TR",  allocSource="",
         minAvgOver=    4L, maxAvgOver=   40L, byAvgOver=    2L, 
         minBear=    -150,  maxBear=      40,  byBear=       5, 
         minDelta=      0,  maxDelta=      0,  byDelta=      5,    # 0 is generally close to the optimum
         minYoyoOffset= 1L, maxYoyoOffset= 5L, byYoyoOffset= 2L, 
         minYoyoPenalty=1,  maxYoyoPenalty=1,  byYoyoPenalty=0.4,  # 1 generally works best
         futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
         minTR=-Inf, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.7, minScore=8,
         coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
         xMinVol=12.5, xMaxVol=16.5, xMinDD2=3, xMaxDD2=11,
         type="training", col=F, plotType="symbols", 
         nameLength=24, plotEvery=def$plotEvery, countOnly=F, showHeading=T,
         referenceStrategies=c(typical$Boll1, typical$Boll2), force=F) {
 
   if (dataSplit != "training") 
      warning("Doing training in '", dataSplit, "' mode.", immediate.=T)
   if (costs < 1/100) 
      warning("costs = ", costs*100, "%.", immediate.=T)
      
   # calculate how many parameters sets will be run
   count <- calcOptimalBoll(inputDF, inputName, allocSource, 
                            minAvgOver=    minAvgOver,    maxAvgOver=    maxAvgOver,    byAvgOver=    byAvgOver,
                            minBear   =    minBear,       maxBear   =    maxBear,       byBear   =    byBear,
                            minDelta  =    minDelta,      maxDelta  =    maxDelta,      byDelta  =    byDelta,
                            minYoyoOffset= minYoyoOffset, maxYoyoOffset= maxYoyoOffset, byYoyoOffset= byYoyoOffset, 
                            minYoyoPenalty=minYoyoPenalty,maxYoyoPenalty=maxYoyoPenalty,byYoyoPenalty=byYoyoPenalty,
                            futureYears, costs, type, 
                            minTR, maxVol, maxDD2, minTO, minScore, coeffTR, coeffVol, coeffDD2,
                            xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly=T,
                            col, plotType, nameLength, plotEvery, force)
   
   if (!countOnly) {# actually calculating
      if(showHeading) {
         print (paste0("Running ", count[1], " parameter sets (", count[2], " new)"))      
         if(showHeading) dashes <- displaySummaryHeader(futureYears=futureYears, nameLength=nameLength)
      }
#       cleanUpStrategies()
      
      calcOptimalBoll(inputDF, inputName, allocSource,  
                      minAvgOver=    minAvgOver,    maxAvgOver=    maxAvgOver,    byAvgOver=    byAvgOver,
                      minBear   =    minBear,       maxBear   =    maxBear,       byBear   =    byBear,
                      minDelta  =    minDelta,      maxDelta  =    maxDelta,      byDelta  =    byDelta,
                      minYoyoOffset= minYoyoOffset, maxYoyoOffset= maxYoyoOffset, byYoyoOffset= byYoyoOffset, 
                      minYoyoPenalty=minYoyoPenalty,maxYoyoPenalty=maxYoyoPenalty,byYoyoPenalty=byYoyoPenalty,
                      futureYears, costs, type, 
                      minTR, maxVol, maxDD2, minTO, minScore, coeffTR, coeffVol, coeffDD2,
                      xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly=F,
                      col, plotType, nameLength, plotEvery, force)
      
      if(showHeading) {
         print(dashes)
         if( length(referenceStrategies) > 0 )
            for ( i in 1:length(referenceStrategies) )
               showSummaryForStrategy(referenceStrategies[i], nameLength=nameLength, costs=costs)
         plotAllReturnsVsTwo(col=col, trainingPlotType=plotType,
                             xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2)
      }
   } else return (count)
}


searchForTwoOptimalBoll <- function( minScore1=15.5, minScore2=14.4, do1=T, do2=T,
         minAvgOver1  = 13L, maxAvgOver1  = 15L, byAvgOver1   = 1L, 
         minBear1     =-36,  maxBear1     =-30,  byBear1      = 0.5, 
         minYoyoOffset1= 1L, maxYoyoOffset1= 6L, byYoyoOffset1= 1L, 
         minYoyoPenalty1=1,  maxYoyoPenalty1=1,  byYoyoPenalty1=0.2,  # 1 generally works best
         minDelta1    =  0,  maxDelta1    =  0,  byDelta1     = 0.5,  # 0 is generally close to the optimum
         
         minAvgOver2  =  9L, maxAvgOver2  = 11L, byAvgOver2   = 1L,
         minBear2     = 45,  maxBear2     = 50,  byBear2      = 0.5,
         minYoyoOffset2= 1L, maxYoyoOffset2= 4L, byYoyoOffset2= 1L, 
         minYoyoPenalty2=1,  maxYoyoPenalty2=1,  byYoyoPenalty2=0.2,  # 1 generally works best
         minDelta2    =  0,  maxDelta2    =  0,  byDelta2     = 0.5,  # 0 is generally close to the optimum
         costs=def$tradingCost+def$riskAsCostTechnical,
         plotType="symbols", force=F) {
   message("costs = ", costs*100, "%")
   cleanUpStrategies()
   
   if(do1) {   
      print("Bollinger 1...")
      createBollStrategy(inputDF=def$BollInputDF, inputName=def$BollInputName, avgOver=def$BollAvgOver1, 
                         bearish=def$BollBearish1, bullish=def$BollBullish1, 
                         yoyoOffset=def$BollYoyoOffset1, yoyoPenalty=def$BollYoyoPenalty1, allocSource="stocks",
                         costs=costs, force=force)      
      searchForOptimalBoll(minAvgOver   = minAvgOver1,    maxAvgOver   = maxAvgOver1,    byAvgOver   = byAvgOver1,
                           minBear      = minBear1,       maxBear      = maxBear1,       byBear      = byBear1,
                           minYoyoOffset= minYoyoOffset1, maxYoyoOffset= maxYoyoOffset1, byYoyoOffset= byYoyoOffset1, 
                           minYoyoPenalty=minYoyoPenalty1,maxYoyoPenalty=maxYoyoPenalty1,byYoyoPenalty=byYoyoPenalty1,
                           minDelta     = minDelta1,      maxDelta     = maxDelta1,      byDelta     = byDelta1,
                           referenceStrategies=typical$Boll1, costs=costs, 
                           minScore=minScore1, force=force)
   }
   if(do1 && do2) { # separator needed only if we do both
      print("")
      print("*******************************************************************************************")
      print("")
   }
   if(do2) {
      print("Bollinger 2...")
      createBollStrategy(inputDF=def$BollInputDF, inputName=def$BollInputName, avgOver=def$BollAvgOver2, 
                         bearish=def$BollBearish2, bullish=def$BollBullish2, 
                         yoyoOffset=def$BollYoyoOffset2, yoyoPenalty=def$BollYoyoPenalty2, allocSource="stocks",
                         costs=costs, force=force)      
      searchForOptimalBoll(minAvgOver   = minAvgOver2,    maxAvgOver   = maxAvgOver2,    byAvgOver   = byAvgOver2,
                           minBear      = minBear2,       maxBear      = maxBear2,       byBear      = byBear2,
                           minYoyoOffset= minYoyoOffset2, maxYoyoOffset= maxYoyoOffset2, byYoyoOffset= byYoyoOffset2, 
                           minYoyoPenalty=minYoyoPenalty2,maxYoyoPenalty=maxYoyoPenalty2,byYoyoPenalty=byYoyoPenalty2,
                           minDelta     = minDelta2,      maxDelta     = maxDelta2,      byDelta     = byDelta2,
                           referenceStrategies=typical$Boll2, costs=costs, 
                           minScore=minScore2, force=force)
   }
}


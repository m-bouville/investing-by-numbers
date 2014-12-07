

############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
##     hybrid.r generates strategies      ##
##      by applying technical tools       ##
##    to value input (CAPE, detrended)    ##
##                                        ##
############################################



#default values of parameters:
setHybridDefaultValues <- function() {
   ## Boll(CAPE)
   {
      def$Boll_CAPEinputDF    <<- "dat"
      def$Boll_CAPEyears1     <<-   15
      def$Boll_CAPEcheat1     <<-    8   # given how big 'years' is, we need a bigger 'cheat' than usual
      def$Boll_CAPEavgOver1   <<-   18L
      def$Boll_CAPEbearish1   <<- -148
      def$Boll_CAPEbullish1   <<- -148
      def$Boll_CAPEinputName1 <<- paste0("CAPE", def$Boll_CAPEyears1)
      def$typicalBoll_CAPE1   <<- paste0("Boll_", def$Boll_CAPEavgOver1, "_",
                                         def$Boll_CAPEbearish1, "_", def$Boll_CAPEbullish1,
                                         "__", def$Boll_CAPEinputName1)
      
      def$Boll_CAPEyears2     <<-   5 
      def$Boll_CAPEcheat2     <<-   0    # given how small 'years' is, we need no 'cheat' at all
      def$Boll_CAPEavgOver2   <<-  23L
      def$Boll_CAPEbearish2   <<- -85
      def$Boll_CAPEbullish2   <<- -85
      def$Boll_CAPEinputName2 <<- paste0("CAPE", def$Boll_CAPEyears2)
      def$typicalBoll_CAPE2   <<- paste0("Boll_", def$Boll_CAPEavgOver2, "_",
                                         def$Boll_CAPEbearish2, "_", def$Boll_CAPEbullish2,
                                         "__", def$Boll_CAPEinputName2)
   }
   
   ## Boll(detrended)
   {
      def$Boll_detrendedInputDF    <<- "dat"
      def$Boll_detrendedInputName  <<- "detrended_TR"
      def$Boll_detrendedAvgOver1   <<-   19L
      def$Boll_detrendedBearish1   <<- -143
      def$Boll_detrendedBullish1   <<- -143
      def$typicalBoll_detrended1   <<- paste0("Boll_", def$Boll_detrendedAvgOver1, "_",
                                              def$Boll_detrendedBearish1, "_", def$Boll_detrendedBullish1,
                                              "__", def$Boll_detrendedInputName)
   }

   ## SMA(CAPE)
   {
      # SMA(CAPE) 1
      def$SMA_CAPEinputDF    <<- "dat"
      def$SMA_CAPEyears1     <<-   10
      def$SMA_CAPEcheat1     <<- def$CAPEcheat
      def$SMA_CAPEinputName1 <<- paste0("CAPE", def$SMA_CAPEyears1)
      
      def$SMA_CAPE_SMA1_1    <<- 12L
      def$SMA_CAPE_SMA2_1    <<-  5L
      def$SMA_CAPEbearish1   <<- 42
      def$SMA_CAPEbullish1   <<- 41
      def$typicalSMA_CAPE1   <<- paste0("SMA_", def$SMA_CAPE_SMA1_1, "_", def$SMA_CAPE_SMA2_1, "_", 
                                        def$SMA_CAPEbearish1, "_", def$SMA_CAPEbullish1,
                                        "__", def$SMA_CAPEinputName1)
      
      # SMA(CAPE) 2
      def$SMA_CAPEinputDF    <<- "dat"
      def$SMA_CAPEyears2     <<-  7    # 6
      def$SMA_CAPEcheat2     <<-  0
      def$SMA_CAPEinputName2 <<- paste0("CAPE", def$SMA_CAPEyears2)
      
      def$SMA_CAPE_SMA1_2    <<- 15L 
      def$SMA_CAPE_SMA2_2    <<-  5L
      def$SMA_CAPEbearish2   <<- 89   # 93
      def$SMA_CAPEbullish2   <<- 89   # 93
      def$typicalSMA_CAPE2   <<- paste0("SMA_", def$SMA_CAPE_SMA1_2, "_", def$SMA_CAPE_SMA2_2, "_", 
                                        def$SMA_CAPEbearish2, "_", def$SMA_CAPEbullish2,
                                        "__", def$SMA_CAPEinputName2)
   }
   
   ## reversal(CAPE)
   {
      def$reversal_CAPEinputDF      <<- "dat"
      # reversal(CAPE) 1
      def$reversal_CAPEyears1       <<- 10
      def$reversal_CAPEcheat1       <<- def$CAPEcheat
      def$reversal_CAPEinputName1   <<- paste0("CAPE", def$reversal_CAPEyears1)
      
      def$reversal_CAPEavgOver1     <<- 14L
      def$reversal_CAPEreturnToMean1<<- 44
      def$reversal_CAPEbearish1     <<-  9
      def$reversal_CAPEbullish1     <<-  9 
      def$typicalReversal_CAPE1     <<- paste0(
         "reversal_", def$reversal_CAPEavgOver1, "_", 
         def$reversal_CAPEreturnToMean1, "_", def$reversal_CAPEbearish1, "_", def$reversal_CAPEbullish1,
         "__", def$reversal_CAPEinputName1)
      
      # reversal(CAPE) 2
      def$reversal_CAPEyears2       <<-  6
      def$reversal_CAPEcheat2       <<-  0
      def$reversal_CAPEinputName2   <<- paste0("CAPE", def$reversal_CAPEyears2)
      
      def$reversal_CAPEavgOver2     <<- 12L
      def$reversal_CAPEreturnToMean2<<- 21.5
      def$reversal_CAPEbearish2     <<-  5
      def$reversal_CAPEbullish2     <<-  5 
      def$typicalReversal_CAPE2     <<- paste0(
         "reversal_", def$reversal_CAPEavgOver2, "_", 
         def$reversal_CAPEreturnToMean2, "_", def$reversal_CAPEbearish2, "_", def$reversal_CAPEbullish2,
         "__", def$reversal_CAPEinputName2)
   }

   ## Boll(Boll)
   {
      def$Boll_BollInputDF      <<- "TR"
      # Boll(Boll) 1
      def$Boll_BollAllocSource1 <<- "stocks"
      def$Boll_BollAvgOver1_1   <<-   25L
      def$Boll_BollBearish1_1   <<- -145
      def$Boll_BollBullish1_1   <<- -145
      def$Boll_BollInputName1   <<- paste0("Boll_", def$Boll_BollAvgOver1_1, "_", 
                                          def$Boll_BollBearish1_1, "_", def$Boll_BollBullish1_1)      
      def$Boll_BollAvgOver2_1   <<-   16L
      def$Boll_BollBearish2_1   <<-  -32
      def$Boll_BollBullish2_1   <<-  -32
      def$typicalBoll_Boll1     <<- paste0("Boll_", def$Boll_BollAvgOver2_1, "_",
                                           def$Boll_BollBearish2_1, "_", def$Boll_BollBullish2_1,
                                           "__", def$Boll_BollInputName1)

      # Boll(Boll) 2
      def$Boll_BollAllocSource2 <<- "stocks"
      def$Boll_BollAvgOver1_2   <<-   13L
      def$Boll_BollBearish1_2   <<- -105
      def$Boll_BollBullish1_2   <<- -105
      def$Boll_BollInputName2   <<- paste0("Boll_", def$Boll_BollAvgOver1_2, "_", 
                                          def$Boll_BollBearish1_2, "_", def$Boll_BollBullish1_2) 
      
      def$Boll_BollAvgOver2_2   <<-   15L
      def$Boll_BollBearish2_2   <<-  -25
      def$Boll_BollBullish2_2   <<-  -24
      def$typicalBoll_Boll2     <<- paste0("Boll_", def$Boll_BollAvgOver2_2, "_",
                                         def$Boll_BollBearish2_2, "_", def$Boll_BollBullish2_2,
                                         "__", def$Boll_BollInputName2)
   }
}

   
searchForOptimalBoll_CAPE <- function(
      inputDF="dat", cheat=def$CAPEcheat,
      minCAPEyears=  14L, maxCAPEyears=  16L,  byCAPEyears=   1L,
      minCAPEavgOver= 0L, maxCAPEavgOver= 0L,  byCAPEavgOver= 0L,
      minAvgOver=    14L, maxAvgOver=    22L,  byAvgOver=     2L,
      minBear=     -170,  maxBear=     -130,   byBear=        5, 
      minDelta=       0,  maxDelta=       0,   byDelta=       1,
      futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
      minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.6, minScore=14.8,
      coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
      xMinVol=15, xMaxVol=19.5, xMinDD2=7, xMaxDD2=11.5,
      type="search", col=F, plotType="symbols", 
      nameLength=30, plotEvery=def$plotEvery, countOnly=F,
      referenceStrategies=c(def$typicalBoll_CAPE1, def$typicalBoll_CAPE2), force=F) {
   
   if (dataSplit != "search") 
      warning("Doing a search for parameters in '", dataSplit, "' mode.", immediate.=T)
   if (costs < 1/100) 
      warning("costs = ", costs*100, "%.", immediate.=T)
   
   cleanUpStrategies()
   
   if (minCAPEyears==maxCAPEyears) numCAPEyears <- 1
      else numCAPEyears <- (maxCAPEyears-minCAPEyears)/byCAPEyears + 1
   if (minCAPEavgOver==maxCAPEavgOver) numCAPEavgOver <- 1
      else numCAPEavgOver <- (maxCAPEavgOver-minCAPEavgOver)/byCAPEavgOver + 1
   if (!countOnly) print(paste0("there will be ", numCAPEyears * numCAPEavgOver, " CAPE inputs.") )
   
   # creating ranges that allow to sample the parameter space broadly initially
   rangeCAPEyears   <- createRange(minCAPEyears,   maxCAPEyears,   byCAPEyears)
   rangeCAPEavgOver <- createRange(minCAPEavgOver, maxCAPEavgOver, byCAPEavgOver)
 
   counterTot <- 0; counterNew <- 0
   
   for (CAPEyears in rangeCAPEyears) {
      for (CAPEavgOver in rangeCAPEavgOver) {
         if (CAPEavgOver>0)
            CAPEname <- paste0("CAPE", CAPEyears, "avg", CAPEavgOver)
         else CAPEname <- paste0("CAPE", CAPEyears)
         
         if (!(CAPEname %in% colnames(dat)))
            calcAvgCAPE(years=CAPEyears, cheat=cheat, avgOver=CAPEavgOver)
 
         if (countOnly) {
            count <- searchForOptimalBoll(
                     inputDF, inputName=CAPEname,
                     minAvgOver, maxAvgOver, byAvgOver, minBear,  maxBear,  byBear, 
                     minDelta,  maxDelta,  byDelta, futureYears, costs, 
                     minTR, maxVol, maxDD2, minTO, minScore,
                     coeffTR, coeffVol, coeffDD2, 
                     xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly=T,
                     type, col, plotType, nameLength, plotEvery, 
                     referenceStrategies, force) 
            counterTot <- counterTot + count[1]; counterNew <- counterNew + count[2]
         } else {
            print( paste0("Starting the search for an optimal Boll(", CAPEname, ")..." ) )
            searchForOptimalBoll(
                     inputDF, inputName=CAPEname,
                     minAvgOver, maxAvgOver, byAvgOver, minBear,  maxBear,  byBear, 
                     minDelta,  maxDelta,  byDelta, futureYears, costs, 
                     minTR, maxVol, maxDD2, minTO, minScore,
                     coeffTR, coeffVol, coeffDD2, 
                     xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly=F,
                     type, col, plotType, nameLength, plotEvery, 
                     referenceStrategies, force) 
            print("")            
         }
      }
   }
   if (countOnly) print (paste0("There will be ", numCAPEyears * numCAPEavgOver, " CAPE inputs",
                                " for a total of ", counterTot, " parameter sets (", counterNew, " new)"))
}


searchForTwoOptimalBoll_CAPE <- function(minScore1=15.8, minScore2=14.1,
                                         plotType="symbols", countOnly=F, force=F) {
   cleanUpStrategies()
   print("*** Boll(CAPE) 1...")
   searchForOptimalBoll_CAPE( cheat=def$Boll_CAPEcheat1,
                              minCAPEyears=  13L, maxCAPEyears= 16L,  byCAPEyears=  1L,
                              minCAPEavgOver= 0L, maxCAPEavgOver=6L,  byCAPEavgOver=3L,
                              minAvgOver=    16L, maxAvgOver=   19L,  byAvgOver=    1L,
                              minBear=     -156,  maxBear=    -140,   byBear=       1, 
                              minDelta=       0,  maxDelta=      1,   byDelta=      0.5,
                              plotType=plotType, force=force, countOnly=countOnly, 
                              referenceStrategies=def$typicalBoll_CAPE1,
                              maxVol=17.5, minScore=minScore1)
   print("")
   print("*********************************************************************")
   print("")
   print("*** Boll(CAPE) 2...")
   searchForOptimalBoll_CAPE( cheat=def$Boll_CAPEcheat2,
                              minCAPEyears=  3L, maxCAPEyears=   8L, byCAPEyears=  1L, 
                              minCAPEavgOver=0L, maxCAPEavgOver= 6L, byCAPEavgOver=6L,
                              minAvgOver=   20L, maxAvgOver=    25L, byAvgOver=    1L,
                              minBear=    -100,  maxBear=      -75,  byBear=       4, 
                              minDelta=      0,  maxDelta=       1,  byDelta=      0.5, 
                              plotType=plotType, force=force, countOnly=countOnly, 
                              maxVol=15.5, maxDD2=6.5, minScore=minScore2,
                              referenceStrategies=def$typicalBoll_CAPE2)
}


searchForOptimalSMA_CAPE <- function(
         inputDF="dat", cheat=def$CAPEcheat,
         minCAPEyears= 7L, maxCAPEyears= 7L, byCAPEyears=1L, 
         minCAPEavgOver= 0L, maxCAPEavgOver= 0L,  byCAPEavgOver= 0L,
         minSMA1= 14L, maxSMA1= 15L, bySMA1= 1L,
         minSMA2=  5L, maxSMA2=  5L, bySMA2= 1L, 
         minBear= 78,  maxBear= 92,  byBear= 1, 
         minDelta= 0,  maxDelta= 3,  byDelta=1, 
         futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
         minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.7, minScore=13.6, 
         xMinVol=14, xMaxVol=18, xMinDD2=5.5, xMaxDD2=9.5, countOnly=F,
         type="search", col=F, plotType="symbols", nameLength=25, plotEvery=def$plotEvery, 
         referenceStrategies=c(def$typicalSMA_CAPE1,def$typicalSMA_CAPE2), force=F) {
   
   if (dataSplit != "search") 
      warning("Doing a search for parameters in '", dataSplit, "' mode.", immediate.=T)
   if (costs < 1/100) 
      warning("costs = ", costs*100, "%.", immediate.=T)
   
   cleanUpStrategies()
   
   if (minCAPEyears==maxCAPEyears) numCAPEyears <- 1
   else numCAPEyears <- (maxCAPEyears-minCAPEyears)/byCAPEyears + 1
   if (minCAPEavgOver==maxCAPEavgOver) numCAPEavgOver <- 1
   else numCAPEavgOver <- (maxCAPEavgOver-minCAPEavgOver)/byCAPEavgOver + 1
   
   print(paste0("there will be ", numCAPEyears * numCAPEavgOver, " CAPE inputs.") )
   
   # creating ranges that allow to sample the parameter space broadly initially
   rangeCAPEyears   <- createRange(minCAPEyears,   maxCAPEyears,   byCAPEyears)
   rangeCAPEavgOver <- createRange(minCAPEavgOver, maxCAPEavgOver, byCAPEavgOver)
   
   counterTot <- 0; counterNew <- 0
   
   for (CAPEyears in rangeCAPEyears) 
      for (CAPEavgOver in rangeCAPEavgOver) {
         if (CAPEavgOver>0)
            CAPEname <- paste0("CAPE", CAPEyears, "avg", CAPEavgOver)
         else CAPEname <- paste0("CAPE", CAPEyears)         
         
         print( paste0("Starting the search for an optimal SMA(", CAPEname, ")..." ) )
         if (!(CAPEname %in% colnames(dat))) 
            calcCAPE(years=CAPEyears, cheat=cheat)   
         
         if (countOnly) {
         count <- searchForOptimalSMA(inputDF, inputName=CAPEname, minSMA1, maxSMA1, bySMA1, minSMA2, maxSMA2, bySMA2, 
                             minBear, maxBear, byBear,  minDelta, maxDelta, byDelta, 
                             futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore, 
                             xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2,
                             type, col, plotType, nameLength, plotEvery, countOnly=T,
                             referenceStrategies=referenceStrategies, force)
         
         counterTot <- counterTot + count[1]; counterNew <- counterNew + count[2]
          } else {
            searchForOptimalSMA(inputDF, inputName=CAPEname, minSMA1, maxSMA1, bySMA1, minSMA2, maxSMA2, bySMA2, 
                                         minBear, maxBear, byBear,  minDelta, maxDelta, byDelta, 
                                         futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore, 
                                         xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2,
                                         type, col, plotType, nameLength, plotEvery, countOnly=F,
                                         referenceStrategies=referenceStrategies, force)
            print("")            
         }
      }
   if (countOnly) print (paste0("Running ", counterTot, " parameter sets (", counterNew, " new)"))
}
   

searchForOptimalReversal_CAPE <- function(
      inputDF="dat", cheat=def$CAPEcheat,
      minCAPEyears=6L, maxCAPEyears= 6L, byCAPEyears=1L, 
      minAvgOver= 11L, maxAvgOver=  13L, byAvgOver=  1L,
      minRTM=     21,  maxRTM=      23,  byRTM=      0.5,
      minBear=     4,  maxBear=      6,  byBear=     0.5,
      minDelta=    0,  maxDelta=     1,  byDelta=    0.5, 
      futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
      minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.7, minScore=14.5,
      xMinVol=12, xMaxVol=15, xMinDD2=3.5, xMaxDD2=6,
      col=F, plotType="symbols", nameLength=30, plotEvery=def$plotEvery, 
      referenceStrategies=c(def$typicalReversal_CAPE1,def$typicalReversal_CAPE2), force=F) {

   if (dataSplit != "search") 
      warning("Doing a search for parameters in '", dataSplit, "' mode.", immediate.=T)
   if (costs < 1/100) 
      warning("costs = ", costs*100, "%.", immediate.=T)
      
   for (CAPEyears in seq(minCAPEyears, maxCAPEyears, by=byCAPEyears)) {
      CAPEname <- paste0("CAPE",CAPEyears)
      print( paste0("Starting the search for an optimal reversal(", CAPEname, ")..." ) )
      if (!(CAPEname %in% colnames(dat))) 
         calcCAPE(years=CAPEyears, cheat=cheat)   
      
      searchForOptimalReversal(inputDF, inputName=CAPEname, minAvgOver, maxAvgOver, byAvgOver, 
                               minRTM, maxRTM, byRTM, minBear, maxBear, byBear, minDelta, maxDelta, byDelta, 
                               futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore,
                               xMinVol, xMaxVol, xMinDD2, xMaxDD2, col, plotType, nameLength, plotEvery, 
                               referenceStrategies, force)      
      print("")
   }
}



## Bollinger (with parameters 2) is applied to a Bollinger strategy (with parameters 1): Boll2(Boll1)
searchForOptimalBoll_Boll <- function(
         minAvgOver2=  16L, maxAvgOver2= 20L, byAvgOver2= 4L,
         minBear2=    -64,  maxBear2=   -32,  byBear2=   32, 
         minDelta2=     0,  maxDelta2=    0,  byDelta2=   1,
         minAvgOver1=  12L, maxAvgOver1= 28L, byAvgOver1= 4L,
         minBear1=   -224,  maxBear1=   -80,  byBear1=   32,
         minDelta1=     0,  maxDelta1=    0,  byDelta1=   1,
         futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
         minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.6, minScore=12.5,
         coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
         xMinVol=14.5, xMaxVol=17.5, xMinDD2=5.5, xMaxDD2=9.,
         type="search", col=F, plotType="symbols", 
         referenceStrategies=c(def$typicalBoll_Boll), 
         nameLength=37, plotEvery=def$plotEvery, countOnly=F, force=F) {
   
   if (dataSplit != "search") 
      warning("Doing a search for parameters in '", dataSplit, "' mode.", immediate.=T)
   if (costs < 1/100) 
      warning("costs = ", costs*100, "%.", immediate.=T)
   
   cleanUpStrategies()

   if (!countOnly)
      searchForOptimalBoll_Boll(minAvgOver2, maxAvgOver2, byAvgOver2, minBear2, maxBear2, byBear2, 
                                minDelta2,  maxDelta2,  byDelta2, minAvgOver1, maxAvgOver1, byAvgOver1, minBear1, maxBear1, byBear1, 
                                minDelta1,  maxDelta1,  byDelta1, countOnly=T )   
   
   if (minAvgOver1==maxAvgOver1) numAvgOver1 <- 1
      else numAvgOver1 <- (maxAvgOver1-minAvgOver1)%/%byAvgOver1 + 1
   if (minBear1==maxBear1) numBear1 <- 1
      else numBear1 <- (maxBear1-minBear1)%/%byBear1 + 1
   if (minDelta1==maxDelta1) numDelta1 <- 1
      else numDelta1 <- (maxDelta1-minDelta1)%/%byDelta1 + 1
   if (!countOnly) print(paste0("there will be ", numAvgOver1*numBear1*numDelta1, " Bollinger input strategies.") )

   # creating ranges that allow to sample the parameter space broadly initially
   rangeAvgOver1 <- createRange(minAvgOver1, maxAvgOver1, byAvgOver1)
   rangeBear1    <- createRange(minBear1,    maxBear1,    byBear1)
   rangeDelta1   <- createRange(minDelta1,   maxDelta1,   byDelta1)   
   
   counterTot <- 0; counterNew <- 0
   
   if (!countOnly) dashes <- displaySummaryHeader(futureYears=futureYears, nameLength=nameLength)
   
   for (avgOver1 in rangeAvgOver1) 
      for (bear1 in rangeBear1) 
         for (delta1 in rangeDelta1) {
            bull1 = bear1 + delta1               
            inputName <- paste0("Boll_", avgOver1, "_", bear1, "_", bull1) 
            # print(inputName)
            if (countOnly) {
               count <- searchForOptimalBoll(inputDF="TR", inputName=inputName,
                                             minAvgOver2, maxAvgOver2, byAvgOver2, minBear2, maxBear2, byBear2, 
                                             minDelta2,  maxDelta2,  byDelta2, futureYears, costs, 
                                             minTR, maxVol, maxDD2, minTO, minScore,
                                             coeffTR, coeffVol, coeffDD2, 
                                             xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly=T, showHeading=F,
                                             type, col, plotType, nameLength, plotEvery, 
                                             referenceStrategies, force) 
               counterTot <- counterTot + count[1]; counterNew <- counterNew + count[2]
            } else {
               # print( paste0("Starting the search for an optimal Boll(", inputName, ")..." ) )
               
               if ( !(inputName %in% colnames(alloc)) ) # creating input strategy if need be
                  createBollStrategy(inputDF="dat", inputName="TR", avgOver=avgOver1, type="Bollinger",
                                     bearish=bear1, bullish=bull1, signalMin=def$signalMin, signalMax=def$signalMax,
                                     strategyName=inputName, futureYears=futureYears, force=force)
               
               searchForOptimalBoll(inputDF="TR", inputName=inputName,
                                    minAvgOver2, maxAvgOver2, byAvgOver2, minBear2, maxBear2, byBear2, 
                                    minDelta2,  maxDelta2, byDelta2, futureYears, costs, 
                                    minTR, maxVol, maxDD2, minTO, minScore,
                                    coeffTR, coeffVol, coeffDD2, 
                                    xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly=F, showHeading=F,
                                    type, col, plotType, nameLength, plotEvery, 
                                    referenceStrategies, force) 
               # print("")            
            }
         }
   if (countOnly) print (paste0("There will be a total of ", counterTot, " parameter sets (", counterNew, " new)"))
   else {
      print(dashes)
      if( length(referenceStrategies) >0 )
         for ( i in 1:length(referenceStrategies) )
            showSummaryForStrategy(referenceStrategies[i], nameLength=nameLength, costs=costs)
   }
}


searchForTwoOptimalBoll_Boll <- function(minScore1=16, minScore2=15.4, do1=T, do2=T,
                                         plotType="symbols", countOnly=F, force=F) {
   if(do1) {
      print("*** Boll(Boll) 1...")
      searchForOptimalBoll_Boll( minAvgOver2=  16L, maxAvgOver2= 16L, byAvgOver2= 1L,
                                 minBear2=    -32,  maxBear2=   -32,  byBear2=    1, 
                                 minDelta2=     0,  maxDelta2=    2,  byDelta2=   0.5,
                                 minAvgOver1=  25L, maxAvgOver1= 25L, byAvgOver1= 1L,
                                 minBear1=   -146,  maxBear1=  -144,  byBear1=    1,
                                 minDelta1=     0,  maxDelta1=    2,  byDelta1=   0.5,
                                 plotType=plotType, force=force, countOnly=countOnly, 
                                 referenceStrategies=def$typicalBoll_Boll1,
                                 maxVol=def$maxVol, minScore=minScore1)
   }
   if(do1 && do2) { # needed only if we do both
      print("")
      print("*******************************************************************************************")
      print("")
   }
   if(do2) {
      print("*** Boll(Boll) 2...")
      searchForOptimalBoll_Boll( minAvgOver2=  15L, maxAvgOver2= 15L, byAvgOver2= 1L,
                                 minBear2=    -27,  maxBear2=   -23,  byBear2=    1, 
                                 minDelta2=     0,  maxDelta2=    3,  byDelta2=   1,
                                 minAvgOver1=  13L, maxAvgOver1= 13L, byAvgOver1= 1L,
                                 minBear1=   -106,  maxBear1=  -104,  byBear1=    1,
                                 minDelta1=     0,  maxDelta1=    1,  byDelta1=   1,
                                 plotType=plotType, force=force, countOnly=countOnly, 
                                 maxVol=def$maxVol, maxDD2=7.1, minScore=minScore2,
                                 referenceStrategies=def$typicalBoll_Boll2)
   }
}

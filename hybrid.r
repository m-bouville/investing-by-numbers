

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
   def$Boll_CAPEinputDF    <<- "dat"
   def$Boll_CAPEyears1     <<-   15
   def$Boll_CAPEcheat1     <<-    8   # given how big 'years' is, we need a bigger 'cheat' than usual
   def$Boll_CAPEavgOver1   <<-   18L
   def$Boll_CAPEbearish1   <<- -148
   def$Boll_CAPEbullish1   <<- -148
   def$Boll_CAPEinputName1 <<- paste0("CAPE", def$Boll_CAPEyears1)
   def$typicalBoll_CAPE1   <<- paste0("Boll_", def$Boll_CAPEinputName1, "_", def$Boll_CAPEavgOver1, "_",
                                      def$Boll_CAPEbearish1, "_", def$Boll_CAPEbullish1)
   
   def$Boll_CAPEyears2     <<-   5 
   def$Boll_CAPEcheat2     <<-   0    # given how small 'years' is, we need no 'cheat' at all
   def$Boll_CAPEavgOver2   <<-  23L
   def$Boll_CAPEbearish2   <<- -85
   def$Boll_CAPEbullish2   <<- -85
   def$Boll_CAPEinputName2 <<- paste0("CAPE", def$Boll_CAPEyears2)
   def$typicalBoll_CAPE2   <<- paste0("Boll_", def$Boll_CAPEinputName2, "_", def$Boll_CAPEavgOver2, "_",
                                      def$Boll_CAPEbearish2, "_", def$Boll_CAPEbullish2)
   
   ## Boll(detrended)
   def$Boll_detrendedInputDF    <<- "dat"
   def$Boll_detrendedInputName  <<- "detrended_TR"
   def$Boll_detrendedAvgOver1   <<-   19L
   def$Boll_detrendedBearish1   <<- -143
   def$Boll_detrendedBullish1   <<- -143
   def$typicalBoll_detrended1   <<- paste0("Boll_", def$Boll_detrendedInputName, "_", def$Boll_detrendedAvgOver1, "_",
                                      def$Boll_detrendedBearish1, "_", def$Boll_detrendedBullish1)

   ## SMA(CAPE) 1 
   def$SMA_CAPEinputDF    <<- "dat"
   def$SMA_CAPEyears1     <<-   10
   def$SMA_CAPEcheat1     <<- def$CAPEcheat
   def$SMA_CAPEinputName1 <<- paste0("CAPE", def$SMA_CAPEyears1)

   def$SMA_CAPE_SMA1_1    <<- 12L
   def$SMA_CAPE_SMA2_1    <<-  5L
   def$SMA_CAPEbearish1   <<- 42
   def$SMA_CAPEbullish1   <<- 41
   def$typicalSMA_CAPE1   <<- paste0("SMA_", def$SMA_CAPEinputName1, "_", def$SMA_CAPE_SMA1_1, "_", 
                                     def$SMA_CAPE_SMA2_1, "__", def$SMA_CAPEbearish1, "_", def$SMA_CAPEbullish1)

   ## SMA(CAPE) 2
   def$SMA_CAPEinputDF    <<- "dat"
   def$SMA_CAPEyears2     <<-  7    # 6
   def$SMA_CAPEcheat2     <<-  0
   def$SMA_CAPEinputName2 <<- paste0("CAPE", def$SMA_CAPEyears2)
   
   def$SMA_CAPE_SMA1_2    <<- 15L 
   def$SMA_CAPE_SMA2_2    <<-  5L
   def$SMA_CAPEbearish2   <<- 89   # 93
   def$SMA_CAPEbullish2   <<- 89   # 93
   def$typicalSMA_CAPE2   <<- paste0("SMA_", def$SMA_CAPEinputName2, "_", def$SMA_CAPE_SMA1_2, "_", 
                                     def$SMA_CAPE_SMA2_2, "__", def$SMA_CAPEbearish2, "_", def$SMA_CAPEbullish2)
   
   ## reversal(CAPE) 1
   def$reversal_CAPEinputDF      <<- "dat"
   def$reversal_CAPEyears1       <<- 10
   def$reversal_CAPEcheat1       <<- def$CAPEcheat
   def$reversal_CAPEinputName1   <<- paste0("CAPE", def$reversal_CAPEyears1)
   
   def$reversal_CAPEavgOver1     <<- 14L
   def$reversal_CAPEreturnToMean1<<- 44
   def$reversal_CAPEbearish1     <<-  9
   def$reversal_CAPEbullish1     <<-  9 
   def$typicalReversal_CAPE1     <<- paste0(
         "reversal_",  def$reversal_CAPEinputName1, "_", def$reversal_CAPEavgOver1, "_", 
         def$reversal_CAPEreturnToMean1, "_", def$reversal_CAPEbearish1, "_", def$reversal_CAPEbullish1)   

   ## reversal(CAPE) 2
   def$reversal_CAPEyears2       <<-  6
   def$reversal_CAPEcheat2       <<-  0
   def$reversal_CAPEinputName2   <<- paste0("CAPE", def$reversal_CAPEyears2)
   
   def$reversal_CAPEavgOver2     <<- 12L
   def$reversal_CAPEreturnToMean2<<- 21.5
   def$reversal_CAPEbearish2     <<-  5
   def$reversal_CAPEbullish2     <<-  5 
   def$typicalReversal_CAPE2     <<- paste0(
      "reversal_",  def$reversal_CAPEinputName2, "_", def$reversal_CAPEavgOver2, "_", 
      def$reversal_CAPEreturnToMean2, "_", def$reversal_CAPEbearish2, "_", def$reversal_CAPEbullish2)   
}

   
searchForOptimalBoll_CAPE <- function(
      inputDF="dat", cheat=def$CAPEcheat,
      minCAPEyears= 8L, maxCAPEyears=17L,  byCAPEyears=1L,
      minAvgOver=  16L, maxAvgOver=  20L,  byAvgOver=  1L,
      minBear=   -154,  maxBear=   -134,   byBear=     1, 
      minDelta=     0,  maxDelta=     1,   byDelta=    1,
      futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
      minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.6, minScore=13.5,
      coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
      xMinVol=14.5, xMaxVol=18.5, xMinDD2=5, xMaxDD2=9.5,
      type="search", col=F, plotType="symbols", 
      nameLength=28, plotEvery=def$plotEvery, 
      referenceStrategies=c(def$typicalBoll_CAPE1, def$typicalBoll_CAPE2), force=F) {
   
   cleanUpStrategies()

   for (CAPEyears in seq(minCAPEyears, maxCAPEyears, by=byCAPEyears)) {
      CAPEname <- paste0("CAPE",CAPEyears)
      print( paste0("Starting the search for an optimal Boll(", CAPEname, ")..." ) )
      if (!(CAPEname %in% colnames(dat))) 
         calcCAPE(years=CAPEyears, cheat=cheat)
      
      searchForOptimalBoll(inputDF, inputName=CAPEname,
                           minAvgOver, maxAvgOver, byAvgOver, minBear,  maxBear,  byBear, 
                           minDelta,  maxDelta,  byDelta, futureYears, costs, 
                           minTR, maxVol, maxDD2, minTO, minScore,
                           coeffTR, coeffVol, coeffDD2, 
                           xMinVol, xMaxVol, xMinDD2, xMaxDD2,
                           type, col, plotType, nameLength, plotEvery, 
                           referenceStrategies, force)
      print("")
      print("")
   }  
}


searchForTwoOptimalBoll_CAPE <- function(plotType="symbols", force=F) {
   cleanUpStrategies()
   print("*** Boll(CAPE) 1...")
   searchForOptimalBoll_CAPE( minCAPEyears=14L, maxCAPEyears=18L,  byCAPEyears=1L,
                              minAvgOver=  16L, maxAvgOver=  20L,  byAvgOver=  1L,
                              minBear=   -156,  maxBear=   -140,   byBear=     1, 
                              minDelta=     0,  maxDelta=     2,   byDelta=    1,
                              plotType=plotType, force=force, referenceStrategies=def$typicalBoll_CAPE1,
                              maxVol=17.5, minScore=13.5)
   print("")
   print("*********************************************************************")
   print("")
   print("*** Boll(CAPE) 2...")
   searchForOptimalBoll_CAPE( minCAPEyears= 3L, maxCAPEyears= 8L, byCAPEyears=1L, 
                              minAvgOver=  20L, maxAvgOver=  25L, byAvgOver=  1L,
                              minBear=   -100,  maxBear=    -75,  byBear=     1, 
                              minDelta=     0,  maxDelta=     0,  byDelta=    1, 
                              plotType=plotType, force=force, maxVol=15.5, referenceStrategies=def$typicalBoll_CAPE2,
                              maxDD2=6.5, minScore=14.1)
}


searchForOptimalSMA_CAPE <- function(
      inputDF="dat", cheat=def$CAPEcheat,
      minCAPEyears= 7L, maxCAPEyears= 7L, byCAPEyears=1L, 
      minSMA1= 14L, maxSMA1= 15L, bySMA1= 1L,
      minSMA2=  5L, maxSMA2=  5L, bySMA2= 1L, 
      minBear= 78,  maxBear= 92,  byBear= 1, 
      minDelta= 0,  maxDelta= 3,  byDelta=1, 
      futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
      minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.7, minScore=13.6, 
      xMinVol=14, xMaxVol=18, xMinDD2=5.5, xMaxDD2=9.5,
      type="search", col=F, plotType="symbols", nameLength=25, plotEvery=def$plotEvery, 
      referenceStrategies=c(def$typicalSMA_CAPE1,def$typicalSMA_CAPE2), force=F) {
   
   cleanUpStrategies()

   for (CAPEyears in seq(minCAPEyears, maxCAPEyears, by=byCAPEyears)) {
      CAPEname <- paste0("CAPE",CAPEyears)
      print( paste0("Starting the search for an optimal SMA(", CAPEname, ")..." ) )
      if (!(CAPEname %in% colnames(dat))) 
         calcCAPE(years=CAPEyears, cheat=cheat)   
   
      searchForOptimalSMA(inputDF, inputName=CAPEname, minSMA1, maxSMA1, bySMA1, minSMA2, maxSMA2, bySMA2, 
                          minBear, maxBear, byBear,  minDelta, maxDelta, byDelta, 
                          futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore, 
                          xMinVol=xMinVol, xMaxVol=xMaxVol, xMinDD2=xMinDD2, xMaxDD2=xMaxDD2,
                          type, col, plotType, nameLength, plotEvery, 
                          referenceStrategies=referenceStrategies, force)
      print("")
   }
}
   

searchForOptimalReversal_CAPE <- function(
      inputDF="dat", cheat=def$CAPEcheat,
      minCAPEyears=6L, maxCAPEyears= 6L, byCAPEyears=1L, 
      minAvgOver= 11L, maxAvgOver=  13L, byAvgOver=  1L,
      minRTM=     21,  maxRTM=      22,  byRTM=      0.5,
      minBear=     4,  maxBear=      6,  byBear=     0.5,
      minDelta=    0,  maxDelta=     1,  byDelta=    0.5, 
      futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
      minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.7, minScore=13,
      xMinVol=12, xMaxVol=15, xMinDD2=4, xMaxDD2=6,
      col=F, plotType="symbols", nameLength=28, plotEvery=def$plotEvery, 
      referenceStrategies=c(def$typicalReversal_CAPE1,def$typicalReversal_CAPE2), force=F) {

   cleanUpStrategies()
   
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



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
   def$Boll_CAPEinputDF1   <<- "dat"
   def$Boll_CAPEyears1     <<-   10 
   def$Boll_CAPEavgOver1   <<-   18L
   def$Boll_CAPEbearish1   <<- -141
   def$Boll_CAPEbullish1   <<- -141
   def$Boll_CAPEinputName1 <<- paste0("CAPE", def$Boll_CAPEyears1)
   def$typicalBoll_CAPE1   <<- paste0("Boll_", def$Boll_CAPEinputName1, "_", def$Boll_CAPEavgOver1, "_",
                                      def$Boll_CAPEbearish1, "_", def$Boll_CAPEbullish1)
   
   def$Boll_CAPEinputDF2   <<- "dat"
   def$Boll_CAPEyears2     <<-   5 
   def$Boll_CAPEavgOver2   <<-   23L
   def$Boll_CAPEbearish2   <<-  -85
   def$Boll_CAPEbullish2   <<-  -85
   def$Boll_CAPEinputName2 <<- paste0("CAPE", def$Boll_CAPEyears2)
   def$typicalBoll_CAPE2   <<- paste0("Boll_", def$Boll_CAPEinputName2, "_", def$Boll_CAPEavgOver2, "_",
                                      def$Boll_CAPEbearish2, "_", def$Boll_CAPEbullish2)
   
   def$typicalBoll_detrended<<- "Boll_detrended"

   def$typicalSMA_CAPE     <<- def$typicalBoll_CAPE1
}

   
searchForOptimalBoll_CAPE <- function(inputDF="dat", cheat=def$CAPEcheat,
                                      minCAPEyears= 9L, maxCAPEyears=11L, byCAPEyears=1L, 
                                      minAvgOver=  14L, maxAvgOver=  22L, byAvgOver=  2L, 
                                      minBear=   -156,  maxBear=   -128,  byBear=     4, 
                                      minDelta=     0,  maxDelta=     4,  byDelta=    2,  
                                      futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
                                      minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.6, minScore=7.6,
                                      coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
                                      xMinVol=14, xMaxVol=18, xMinDD2=5.5, xMaxDD2=9.5,
                                      type="search", col=F, plotType="symbols", 
                                      nameLength=26, plotEvery=def$plotEvery, referenceStrategies=def$typicalBoll_CAPE1, force=F) {
   
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
   }  
}


searchForTwoOptimalBoll_CAPE <- function(plotType="symbols", force=F) {
   cleanUpStrategies()
   print("* Boll(CAPE) 1...")
   searchForOptimalBoll_CAPE( minCAPEyears= 5L, maxCAPEyears= 5L, byCAPEyears=1L, 
                              minAvgOver=  23L, maxAvgOver=  23L, byAvgOver=  1L,
                              minBear=    -87,  maxBear=    -83,  byBear=     1, 
                              minDelta=     0,  maxDelta=     4,  byDelta=    1, 
                              plotType=plotType, force=force, maxVol=15, minScore=8.15)
   print("")
   print("* Boll(CAPE) 2...")
   searchForOptimalBoll_CAPE( minCAPEyears= 8L, maxCAPEyears= 8L, byCAPEyears=1L, 
                              minAvgOver=  21L, maxAvgOver=  23L, byAvgOver=  1L,
                              minBear=    -94,  maxBear=    -88,  byBear=     1, 
                              minDelta=     0,  maxDelta=     4,  byDelta=    1, 
                              plotType=plotType, force=force, maxVol=15.5, minScore=8.05)
}


searchForOptimalSMA_CAPE <- function(
      inputDF="dat", inputName="CAPE10", 
      minSMA1=  2L, maxSMA1= 40L, bySMA1= 8L,
      minSMA2=  1L, maxSMA2= 10L, bySMA2= 2L, 
      minBear=  4,  maxBear= 24,  byBear= 4, 
      minDelta= 0,  maxDelta= 4,  byDelta=2, 
      futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
      minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.7, minScore=8., 
      type="search", col=F, plotType="symbols", 
      nameLength=22, plotEvery=def$plotEvery, force=F) {
   
   searchForOptimalSMA(inputDF, inputName, minSMA1, maxSMA1, bySMA1, minSMA2, maxSMA2, bySMA2, 
         minBear, maxBear, byBear,  minDelta, maxDelta, byDelta, 
         futureYears, costs, minTR, maxVol, maxDD2, minTO, minScore, 
         type, col, plotType, nameLength, plotEvery, referenceStrategies=def$typicalSMA_CAPE, force)
}
   

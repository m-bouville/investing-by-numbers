

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
      def$Boll_CAPEinputDF     <<- "dat"
      def$Boll_CAPEyears1      <<-   15
      def$Boll_CAPEcheat1      <<-    8   # given how big 'years' is, we need a bigger 'cheat' than usual
      def$Boll_CAPEavgOver1    <<-   18L
      def$Boll_CAPEbearish1    <<- -148
      def$Boll_CAPEbullish1    <<- -148
      def$Boll_CAPEyoyoOffset1 <<-   1L   # optimization with yoyo prevention enabled did not do better
      def$Boll_CAPEyoyoPenalty1<<-   0 
      def$Boll_CAPEinputName1  <<- paste0("CAPE", def$Boll_CAPEyears1)
      typical$Boll_CAPE1       <<- nameBollStrategy(def$Boll_CAPEinputName1,  def$Boll_CAPEavgOver1,
                                                    def$Boll_CAPEbearish1,    def$Boll_CAPEbullish1,  
                                                    def$Boll_CAPEyoyoOffset1, def$Boll_CAPEyoyoPenalty1)      
      
      def$Boll_CAPEyears2      <<-   5 
      def$Boll_CAPEcheat2      <<-   0    # given how small 'years' is, we need no 'cheat' at all
      def$Boll_CAPEavgOver2    <<-  23L
      def$Boll_CAPEbearish2    <<- -85
      def$Boll_CAPEbullish2    <<- -85
      def$Boll_CAPEyoyoOffset2 <<-   1L   # optimization with yoyo prevention enabled did not do better
      def$Boll_CAPEyoyoPenalty2<<-   0
      def$Boll_CAPEinputName2  <<- paste0("CAPE", def$Boll_CAPEyears2)
      typical$Boll_CAPE2       <<- nameBollStrategy(def$Boll_CAPEinputName2,  def$Boll_CAPEavgOver2,
                                                    def$Boll_CAPEbearish2,    def$Boll_CAPEbullish2,  
                                                    def$Boll_CAPEyoyoOffset2, def$Boll_CAPEyoyoPenalty2)      
   }
   
   ## Boll(detrended)
   {
      def$Boll_detrendedInputDF    <<- "dat"
      def$Boll_detrendedInputName  <<- "detrended_TR"
      def$Boll_detrendedAvgOver1   <<-   19L
      def$Boll_detrendedBearish1   <<- -143
      def$Boll_detrendedBullish1   <<- -143
      typical$Boll_detrended1   <<- paste0("Boll_", def$Boll_detrendedAvgOver1, "_",
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
      typical$SMA_CAPE1   <<- paste0("SMA_", def$SMA_CAPE_SMA1_1, "_", def$SMA_CAPE_SMA2_1, "_", 
                                        def$SMA_CAPEbearish1, "_", def$SMA_CAPEbullish1,
                                        "__", def$SMA_CAPEinputName1)
      
      # SMA(CAPE) 2 (costs=4%)
      def$SMA_CAPEyears2     <<-  5
      def$SMA_CAPEcheat2     <<-  0 
      def$SMA_CAPEinputName2 <<- paste0("CAPE", def$SMA_CAPEyears2)
      
      def$SMA_CAPE_SMA1_2    <<- 16L
      def$SMA_CAPE_SMA2_2    <<-  3L
      def$SMA_CAPEbearish2   <<- 64
      def$SMA_CAPEbullish2   <<- 64
      typical$SMA_CAPE2   <<- paste0("SMA_", def$SMA_CAPE_SMA1_2, "_", def$SMA_CAPE_SMA2_2, "_", 
                                        def$SMA_CAPEbearish2, "_", def$SMA_CAPEbullish2,
                                        "__", def$SMA_CAPEinputName2)
      
      # SMA(CAPE) 3
      #       def$SMA_CAPEinputDF    <<- "dat"
      #       def$SMA_CAPEyears2     <<-   7
      #       def$SMA_CAPEcheat2     <<-  0    # 0
      #       def$SMA_CAPEinputName2 <<- paste0("CAPE", def$SMA_CAPEyears2)
      #       
      #       def$SMA_CAPE_SMA1_2    <<- 15L # (costs = 2%)
      #       def$SMA_CAPE_SMA2_2    <<-    5L
      #       def$SMA_CAPEbearish2   <<-  89
      #       def$SMA_CAPEbullish2   <<-  89
      #       typical$SMA_CAPE2   <<- paste0("SMA_", def$SMA_CAPE_SMA1_2, "_", def$SMA_CAPE_SMA2_2, "_", 
      #                                         def$SMA_CAPEbearish2, "_", def$SMA_CAPEbullish2,
      #                                         "__", def$SMA_CAPEinputName2)
      
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
      typical$reversal_CAPE1     <<- paste0(
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
      typical$reversal_CAPE2     <<- paste0(
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
      def$Boll_BollYoyoOffset1  <<-   1L  # optimization with yoyo prevention enabled did not do better
      def$Boll_BollYoyoPenalty1 <<-   0
      typical$Boll_Boll1        <<- nameBollStrategy(def$Boll_BollInputName1,  def$Boll_BollAvgOver2_1,
                                                     def$Boll_BollBearish2_1,  def$Boll_BollBullish2_1,
                                                     def$Boll_BollYoyoOffset1, def$Boll_BollYoyoPenalty1)

      # Boll(Boll) 2
      def$Boll_BollAllocSource2 <<- "stocks"
      def$Boll_BollAvgOver1_2   <<-   13L   #   13L
      def$Boll_BollBearish1_2   <<- -105    # -105
      def$Boll_BollBullish1_2   <<- -105    # -105
      def$Boll_BollInputName2   <<- paste0("Boll_", def$Boll_BollAvgOver1_2, "_", 
                                          def$Boll_BollBearish1_2, "_", def$Boll_BollBullish1_2) 
      
      def$Boll_BollAvgOver2_2   <<-   15L   #   15L
      def$Boll_BollBearish2_2   <<-  -24    #  -25
      def$Boll_BollBullish2_2   <<-  -24    #  -24
      def$Boll_BollYoyoOffset2  <<-    3L   #    1L
      def$Boll_BollYoyoPenalty2 <<-    1    #    0
      typical$Boll_Boll2        <<- nameBollStrategy(def$Boll_BollInputName2,  def$Boll_BollAvgOver2_2,
                                                     def$Boll_BollBearish2_2,  def$Boll_BollBullish2_2,
                                                     def$Boll_BollYoyoOffset2, def$Boll_BollYoyoPenalty2)
   }

   ## Boll(balanced)
   {
      def$Boll_balancedInputDF      <<- "TR"   
      #       def$Boll_balancedInputName    <<- typical$balanced
      def$Boll_balancedAvgOver1     <<-   30L  #   30L
      def$Boll_balancedBearish1     <<- -200   # -190
      def$Boll_balancedBullish1     <<- -200   # -190  
      def$Boll_balancedYoyoOffset1  <<-    2L  #    1L
      def$Boll_balancedYoyoPenalty1 <<-    1   #    0
      #       typical$Boll_balanced1        <<- nameBollStrategy(def$Boll_balancedInputName,  def$Boll_balancedAvgOver1,
      #                                                      def$Boll_balancedBearish1,  def$Boll_balancedBullish1,
      #                                                      def$Boll_balancedYoyoOffset1, def$Boll_balancedYoyoPenalty1)
      
      # typical$Boll_balanced1  is defined after balanced is created
   }
   
}

   
searchForOptimalBoll_CAPE <- function(
      inputDF="dat", cheat=def$CAPEcheat,
      minCAPEyears=  14L, maxCAPEyears=  16L,  byCAPEyears=   1L,
      minCAPEavgOver= 0L, maxCAPEavgOver= 0L,  byCAPEavgOver= 1L,  # 0 generally works best
      minAvgOver=    14L, maxAvgOver=    22L,  byAvgOver=     2L,
      minBear=     -170,  maxBear=     -130,   byBear=        5, 
      minYoyoOffset=  1L, maxYoyoOffset=  5L,  byYoyoOffset=  2L, 
      minYoyoPenalty= 1,  maxYoyoPenalty= 1,   byYoyoPenalty= 0.4, # 1 generally works best
      minDelta=       0,  maxDelta=       0,   byDelta=       1,   # 0 is generally close to the optimum
      futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
      minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.6, minScore=14.8,
      coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
      xMinVol=14, xMaxVol=18., xMinDD2=6, xMaxDD2=10.5,
      type="training", col=F, plotType="symbols", 
      nameLength=34, plotEvery=def$plotEvery, countOnly=F, 
      referenceStrategies=c(typical$Boll_CAPE1, typical$Boll_CAPE2), force=F) {
   
   if (dataSplit != "training") 
      warning("Doing training in '", dataSplit, "' mode.", immediate.=T)
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
                     inputDF, inputName=CAPEname, allocSource="stocks",
                     minAvgOver, maxAvgOver, byAvgOver, minBear,  maxBear,  byBear, 
                     minDelta,  maxDelta,  byDelta, 
                     minYoyoOffset, maxYoyoOffset, byYoyoOffset, minYoyoPenalty, maxYoyoPenalty, byYoyoPenalty,
                     futureYears, costs, 
                     minTR, maxVol, maxDD2, minTO, minScore,
                     coeffTR, coeffVol, coeffDD2, 
                     xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly=T, showHeading=F,
                     type, col, plotType, nameLength, plotEvery, 
                     referenceStrategies, force) 
            counterTot <- counterTot + count[1]; counterNew <- counterNew + count[2]
         } else {
            print( paste0("Starting the search for an optimal Boll(", CAPEname, ")..." ) )
            searchForOptimalBoll(
                     inputDF, inputName=CAPEname, allocSource="stocks",
                     minAvgOver, maxAvgOver, byAvgOver, minBear,  maxBear,  byBear, 
                     minDelta,  maxDelta,  byDelta, 
                     minYoyoOffset, maxYoyoOffset, byYoyoOffset, minYoyoPenalty, maxYoyoPenalty, byYoyoPenalty,
                     futureYears, costs, 
                     minTR, maxVol, maxDD2, minTO, minScore,
                     coeffTR, coeffVol, coeffDD2, 
                     xMinVol, xMaxVol, xMinDD2, xMaxDD2, countOnly=F, showHeading=T,
                     type, col, plotType, nameLength, plotEvery, 
                     referenceStrategies, force) 
            print("")            
         }
      }
   }
   if (countOnly) print (paste0("There will be ", numCAPEyears * numCAPEavgOver, " CAPE inputs",
                                " for a total of ", counterTot, " parameter sets (", counterNew, " new)"))
}


searchForTwoOptimalBoll_CAPE <- function(minScore1=15.7, minScore2=15.7, do1=T, do2=T,
         minCAPEyears1=  14L, maxCAPEyears1= 16L, byCAPEyears1=  1L,
         minCAPEavgOver1= 0L, maxCAPEavgOver1=0L, byCAPEavgOver1=6L,  # 0 generally works best
         minAvgOver1=    17L, maxAvgOver1=   19L, byAvgOver1=    1L,
         minBear1=     -150,  maxBear1=    -146,  byBear1=       1, 
         minYoyoOffset1=  1L, maxYoyoOffset1= 5L, byYoyoOffset1= 1L, 
         minYoyoPenalty1= 1,  maxYoyoPenalty1=1,  byYoyoPenalty1=0.2, # 1 generally works best
         minDelta1=       0,  maxDelta1=      0,  byDelta1=      1,   # 0 is generally close to the optimum
         
         minCAPEyears2=  4L, maxCAPEyears2=   6L, byCAPEyears2=  1L, 
         minCAPEavgOver2=0L, maxCAPEavgOver2= 0L, byCAPEavgOver2=6L,  # 0 generally works best
         minAvgOver2=   21L, maxAvgOver2=    25L, byAvgOver2=    1L,
         minBear2=     -90,  maxBear2=      -80,  byBear2=       2, 
         minYoyoOffset2= 1L, maxYoyoOffset2=  5L, byYoyoOffset2= 1L, 
         minYoyoPenalty2=1,  maxYoyoPenalty2= 1,  byYoyoPenalty2=0.4, # 1 generally works best
         minDelta2=      0,  maxDelta2=       0,  byDelta2=      1,   # 0 is generally close to the optimum
         plotType="symbols", countOnly=F, force=F) {
   if(do1) {
      print("*** Boll(CAPE) 1...")
      if ((typical$Boll_CAPE1 %in% colnames(TR))) {
         calcCAPE(years=def$Boll_CAPEyears1, cheat=def$Boll_CAPEcheat1)
         createBollStrategy(inputDF=def$Boll_CAPEinputDF, inputName=def$Boll_CAPEinputName1, 
                            avgOver=def$Boll_CAPEavgOver1,
                            bearish=def$Boll_CAPEbearish1, bullish=def$Boll_CAPEbullish1, 
                            yoyoOffset=def$Boll_CAPEyoyoOffset1, yoyoPenalty=def$Boll_CAPEyoyoPenalty1,
                            strategyName=typical$Boll_CAPE1, allocSource="stocks", force=force)
      }   
   
      searchForOptimalBoll_CAPE( cheat=def$Boll_CAPEcheat1,
            minCAPEyears=  minCAPEyears1,   maxCAPEyears=  maxCAPEyears1,   byCAPEyears=  byCAPEyears1,
            minCAPEavgOver=minCAPEavgOver1, maxCAPEavgOver=maxCAPEavgOver1, byCAPEavgOver=byCAPEavgOver1,
            minAvgOver=    minAvgOver1,     maxAvgOver=    maxAvgOver1,     byAvgOver=    byAvgOver1,
            minBear   =    minBear1,        maxBear   =    maxBear1,        byBear   =    byBear1,
            minYoyoOffset= minYoyoOffset1,  maxYoyoOffset= maxYoyoOffset1,  byYoyoOffset= byYoyoOffset1, 
            minYoyoPenalty=minYoyoPenalty1, maxYoyoPenalty=maxYoyoPenalty1, byYoyoPenalty=byYoyoPenalty1,
            minDelta  =    minDelta1,       maxDelta  =    maxDelta1,       byDelta  =    byDelta1,
            plotType=plotType, force=force, countOnly=countOnly, 
            referenceStrategies=typical$Boll_CAPE1, maxVol=17.5, minScore=minScore1)
   }
   if(do1 && do2) { # needed only if we do both
      print("")
      print("*******************************************************************************************")
      print("")
   }
   if(do2) {
      print("*** Boll(CAPE) 2...")

      if ((typical$Boll_CAPE1 %in% colnames(TR))) {
         calcCAPE(years=def$Boll_CAPEyears2, cheat=def$Boll_CAPEcheat2)
         createBollStrategy(inputDF=def$Boll_CAPEinputDF,  inputName=def$Boll_CAPEinputName2, avgOver=def$Boll_CAPEavgOver2,
                            bearish=def$Boll_CAPEbearish2, bullish=def$Boll_CAPEbullish2, 
                            yoyoOffset=def$Boll_CAPEyoyoOffset2, yoyoPenalty=def$Boll_CAPEyoyoPenalty2,
                            strategyName=typical$Boll_CAPE2, allocSource="stocks", force=force)
      }
      
      searchForOptimalBoll_CAPE( cheat=def$Boll_CAPEcheat2,
            minCAPEyears=  minCAPEyears2,   maxCAPEyears=  maxCAPEyears2,   byCAPEyears=  byCAPEyears2,
            minCAPEavgOver=minCAPEavgOver2, maxCAPEavgOver=maxCAPEavgOver2, byCAPEavgOver=byCAPEavgOver2,
            minAvgOver=    minAvgOver2,     maxAvgOver=    maxAvgOver2,     byAvgOver=    byAvgOver2,
            minBear   =    minBear2,        maxBear   =    maxBear2,        byBear   =    byBear2,
            minYoyoOffset= minYoyoOffset2,  maxYoyoOffset= maxYoyoOffset2,  byYoyoOffset= byYoyoOffset2, 
            minYoyoPenalty=minYoyoPenalty2, maxYoyoPenalty=maxYoyoPenalty2, byYoyoPenalty=byYoyoPenalty2,
            minDelta  =    minDelta2,       maxDelta  =    maxDelta2,       byDelta  =    byDelta2,
            plotType=plotType, force=force, countOnly=countOnly, 
            maxVol=15.5, maxDD2=6.5, minScore=minScore2,
            referenceStrategies=typical$Boll_CAPE2)
   }
}


searchForOptimalSMA_CAPE <- function(
         inputDF="dat", cheat=def$CAPEcheat,
         minCAPEyears=   4L, maxCAPEyears=  10L, byCAPEyears=   2L, 
         minCAPEavgOver= 0L, maxCAPEavgOver= 0L, byCAPEavgOver= 0L,
         minSMA1=       12L, maxSMA1=       20L, bySMA1=        2L,
         minSMA2=        4L, maxSMA2=        8L, bySMA2=        2L, 
         minBear=       36,  maxBear=       96,  byBear=        4, 
         minDelta=       0,  maxDelta=       0,  byDelta=       1,  # 0 generally works best
         futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
         minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.7, minScore=13.6, 
         xMinVol=15, xMaxVol=18, xMinDD2=7.5, xMaxDD2=10.5, countOnly=F,
         type="training", col=F, plotType="symbols", nameLength=25, plotEvery=def$plotEvery, 
         referenceStrategies=c(typical$SMA_CAPE1,typical$SMA_CAPE2), force=F) {
   
   if (dataSplit != "training") 
      warning("Doing training in '", dataSplit, "' mode.", immediate.=T)
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
         minDelta=    0,  maxDelta=     0,  byDelta=    0.5,  # 0 generally works best
         futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
         minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.7, minScore=14.5,
         xMinVol=12, xMaxVol=15, xMinDD2=3.5, xMaxDD2=6,
         col=F, plotType="symbols", nameLength=30, plotEvery=def$plotEvery, 
         referenceStrategies=c(typical$reversal_CAPE1,typical$reversal_CAPE2), force=F) {

   if (dataSplit != "training") 
      warning("Doing training in '", dataSplit, "' mode.", immediate.=T)
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
         minAvgOver2=   16L,maxAvgOver2=   20L, byAvgOver2=    4L,
         minBear2=     -64, maxBear2=     -32,  byBear2=      32, 
         minYoyoOffset2= 1L,maxYoyoOffset2= 5L, byYoyoOffset2= 2L, 
         minYoyoPenalty2=1, maxYoyoPenalty2=1,  byYoyoPenalty2=0.4, # 1 generally works best
         minDelta2=      0, maxDelta2=      0,  byDelta2=      1,   # 0 is generally close to the optimum
         minAvgOver1=   12L,maxAvgOver1=   28L, byAvgOver1=    4L,
         minBear1=    -224, maxBear1=     -80,  byBear1=      32,
         minDelta1=      0, maxDelta1=      0,  byDelta1=      1,   # 0 is generally close to the optimum
         futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCostTechnical, 
         minTR=0, maxVol=def$maxVol, maxDD2=def$maxDD2, minTO=0.6, minScore=12.5,
         coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, 
         xMinVol=14, xMaxVol=16.5, xMinDD2=6., xMaxDD2=8.,
         type="training", col=F, plotType="symbols", 
         referenceStrategies=c(typical$Boll_Boll), 
         nameLength=42, plotEvery=def$plotEvery, countOnly=F, force=F) {
   
   if (dataSplit != "training") 
      warning("Doing training in '", dataSplit, "' mode.", immediate.=T)
   if (costs < 1/100) 
      warning("costs = ", costs*100, "%.", immediate.=T)
   
   cleanUpStrategies()

   if (!countOnly)
      searchForOptimalBoll_Boll(
            minAvgOver2=    minAvgOver2,    maxAvgOver2=    maxAvgOver2,    byAvgOver2=    byAvgOver2,
            minBear2   =    minBear2,       maxBear2   =    maxBear2,       byBear2   =    byBear2,
            minDelta2  =    minDelta2,      maxDelta2  =    maxDelta2,      byDelta2  =    byDelta2,
            minYoyoOffset2= minYoyoOffset2, maxYoyoOffset2= maxYoyoOffset2, byYoyoOffset2= byYoyoOffset2, 
            minYoyoPenalty2=minYoyoPenalty2,maxYoyoPenalty2=maxYoyoPenalty2,byYoyoPenalty2=byYoyoPenalty2,
            minAvgOver1=    minAvgOver1,    maxAvgOver1=    maxAvgOver1,    byAvgOver1=    byAvgOver1,
            minBear1   =    minBear1,       maxBear1   =    maxBear1,       byBear1   =    byBear1,
            minDelta1  =    minDelta1,      maxDelta1  =    maxDelta1,      byDelta1  =    byDelta1,
            countOnly=T )   
   
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
               count <- searchForOptimalBoll(inputDF="TR", inputName=inputName, allocSource="stocks",
                     minAvgOver=    minAvgOver2,    maxAvgOver=    maxAvgOver2,    byAvgOver=    byAvgOver2,
                     minBear   =    minBear2,       maxBear   =    maxBear2,       byBear   =    byBear2,
                     minDelta  =    minDelta2,      maxDelta  =    maxDelta2,      byDelta  =    byDelta2,
                     minYoyoOffset= minYoyoOffset2, maxYoyoOffset= maxYoyoOffset2, byYoyoOffset= byYoyoOffset2, 
                     minYoyoPenalty=minYoyoPenalty2,maxYoyoPenalty=maxYoyoPenalty2,byYoyoPenalty=byYoyoPenalty2,
                     futureYears=futureYears, costs=costs, 
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
               
               searchForOptimalBoll(inputDF="TR", inputName=inputName, allocSource="stocks",
                     minAvgOver=    minAvgOver2,    maxAvgOver=    maxAvgOver2,    byAvgOver=    byAvgOver2,
                     minBear   =    minBear2,       maxBear   =    maxBear2,       byBear   =    byBear2,
                     minDelta  =    minDelta2,      maxDelta  =    maxDelta2,      byDelta  =    byDelta2,
                     minYoyoOffset= minYoyoOffset2, maxYoyoOffset= maxYoyoOffset2, byYoyoOffset= byYoyoOffset2, 
                     minYoyoPenalty=minYoyoPenalty2,maxYoyoPenalty=maxYoyoPenalty2,byYoyoPenalty=byYoyoPenalty2,
                     futureYears=futureYears, costs=costs, 
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
      searchForOptimalBoll_Boll( 
            minAvgOver2=   15L, maxAvgOver2=   17L, byAvgOver2=    1L,
            minBear2=     -33,  maxBear2=     -31,  byBear2=       1, 
            minYoyoOffset2= 1L, maxYoyoOffset2= 5L, byYoyoOffset2= 2L, 
            minYoyoPenalty2=1,  maxYoyoPenalty2=1,  byYoyoPenalty2=0.4, # 1 generally works best
            minDelta2=      0,  maxDelta2=      0,  byDelta2=      0.5, # 0 is generally close to the optimum
            minAvgOver1=   24L, maxAvgOver1=   26L, byAvgOver1=    1L,
            minBear1=    -147,  maxBear1=    -142,  byBear1=       1,
            minDelta1=      0,  maxDelta1=      0,  byDelta1=      0.5, # 0 is generally close to the optimum
            plotType=plotType, force=force, countOnly=countOnly, 
            referenceStrategies=typical$Boll_Boll1,
            maxVol=def$maxVol, minScore=minScore1)
   }
   if(do1 && do2) { # needed only if we do both
      print("")
      print("*******************************************************************************************")
      print("")
   }
   if(do2) {
      print("*** Boll(Boll) 2...")
      searchForOptimalBoll_Boll( 
            minAvgOver2=   14L, maxAvgOver2=   16L, byAvgOver2=    1L,
            minBear2=     -26,  maxBear2=     -23,  byBear2=       1, 
            minYoyoOffset2= 2L, maxYoyoOffset2= 5L, byYoyoOffset2= 1L, 
            minYoyoPenalty2=1,  maxYoyoPenalty2=1,  byYoyoPenalty2=0.4, # 1 generally works best
            minDelta2=      0,  maxDelta2=      1,  byDelta2=      1,   # 0 is generally close to the optimum
            minAvgOver1=   12L, maxAvgOver1=   14L, byAvgOver1=    1L,
            minBear1=    -107,  maxBear1=    -104,  byBear1=       1,
            minDelta1=      0,  maxDelta1=      1,  byDelta1=      1,   # 0 is generally close to the optimum
            plotType=plotType, force=force, countOnly=countOnly, 
            maxVol=def$maxVol, maxDD2=7.1, minScore=minScore2,
            referenceStrategies=typical$Boll_Boll2)
   }
}

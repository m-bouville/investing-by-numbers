
############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
##         loading.r loads data           ##
##       and initializes everything       ##
##                                        ##
############################################



setDefaultValues <- function(dataSplit, futureYears, 
                             tradingCost, riskAsCost, riskAsCostTechnical, force=F) {
   if (!exists("def") | force) def <<- list()
   
   if(dataSplit=="search") 
      message("Time range: \'SEARCH\' phase")
   else if(dataSplit=="testing") 
      message("Time range: \'TESTING\' phase")      
   else if(dataSplit=="none") 
      message("Time range:  \'NONE\' phase")      
   else stop("dataSplit can only be one of 'none', 'search' or 'testing', not ", dataSplit)
   
   def$futureYears   <<- futureYears    # default value for the number of years over which future returns are calculated
   message("default futureYears: ", def$futureYears, " years")
   
   def$tradingCost   <<- tradingCost    # default value for the trading costs
   message("default tradingCost:         ", def$tradingCost*100, "% / per year of turnover")

   # default value for the trading costs
   if ( round((tradingCost+riskAsCost)*100,2) %in% c(0.5, 1, 2, 3, 4, 6, 8, 10) ) {
   def$riskAsCost    <<- riskAsCost 
   message("default riskAsCost:          ", def$riskAsCost*100, "% / per year of turnover")
   } else stop("The sum of tradingCost and riskAsCost can only be one of 0.5%, 1%, 2%, 3% or 4%, not ", 
               (tradingCost+riskAsCost)*100, "%." )

   # default value for the trading costs for technical strategies
   #    with a high value it is less common to sell to buy back the next month
   if ( round((tradingCost+riskAsCostTechnical)*100,2) %in% c(0.5, 1, 2, 3, 4, 6, 8, 10) ) {
      def$riskAsCostTechnical     <<- riskAsCostTechnical 
      message("default riskAsCostTechnical: ", def$riskAsCostTechnical*100, "% / per year of turnover")
   } else stop("The sum of tradingCost and riskAsCostTechnical can only be one of 0.5%, 1%, 2%, 3% or 4%, not ", 
               (tradingCost+riskAsCostTechnical)*100, "%." )
   
   def$dataStartYear <<- 1871
   def$startIndex    <<- 105  # a good guess
   def$plotStartYear <<- 1882
   
   
   def$CPUnumber     <<-  1 # Parallelization does not work
   def$plotEvery     <<- 20 # replot every n seconds when searchnig for parameters
   def$nameLength    <<- 20 # width of strategy name in summaries
   
   ## So-called DD2 is the sum over drawdowns DD_i of (DD_i)^DDpower 
   ## (historically called DD2 because DDpower was set to 2)
   def$DDpower       <<- 1.5
   
   def$signalMin     <<- -0.2 # lowest possible value (asymptote) for signal
   def$signalMax     <<- 1 - def$signalMin
   
   #### coeffTR, coeffVol and coeffDD2 are coefficients to calculate the score
   ## coefficients of total return 
   ##    coefficients for median and 5% returns over next n years are both equal to (1-coeffTR)/2.
   def$coeffTR       <<- 0.4

   ## how many extra percentage points of return justify one extra percentage point of volatility.
   ## 0.05 means that we are indifferent if the volatility increases by 1%, 
   ## provided that the return increases by 0.05%
   def$coeffVol      <<- 0.05

   ## how many extra percentage points of return justify one extra unit of DD2.
   ## 1/8 means that we are indifferent if DD2 increases by 0.8, 
   ## provided that the return increases by at least 0.1%
   def$coeffDD2      <<- 3.5
   
   setPlottingDefaultValues()
   setCAPEdefaultValues()
   setDetrendedDefaultValues()
   setBollDefaultValues()
   setSMAdefaultValues()
   setReversalDefaultValues()
   setHybridDefaultValues()
   setCombinedDefaultValues()
   
   def$typicalStrategies <<- c(def$typicalBalanced, def$typicalTechnical, def$typicalValue, "stocks")
}

createStatsDF <- function(futureYears=def$futureYears) {
   stats <<- data.frame(strategy = character(), 
                        type = character(), # type: constant allocation, CAPE, SMA, mixed, etc.
                        subtype = character(), # especially for combinedstrategy
                        TR = numeric(),  # average real total return (exponential regression)
                        netTR0.5 = numeric(),  # average real total return net of 0.5% of trading costs
                        netTR1 = numeric(),  # average real total return net of 1% of costs (trading + risk)
                        netTR2 = numeric(),  # etc. 
                        netTR3 = numeric(),  # etc. 
                        netTR4 = numeric(),  
                        netTR6 = numeric(),  
                        netTR8 = numeric(),  
                        netTR10= numeric(),  
                        volatility = numeric(), 
                        avgStockAlloc = numeric(), # average allocation to stocks
                        latestStockAlloc = numeric(), # allocation to stocks as of the last date of the data
                        turnover = numeric(), # turnover of the portfolio (in years)
                        invTurnover = numeric(), # 1 / turnover
                        DD2 = numeric(), # sum of the squares of the drawdowns
                        score = numeric(), 
                        entropy = numeric(), 
                        stringsAsFactors=F)
   stats[, paste0("median", futureYears)] <<- numeric()
   stats[, paste0("five", futureYears)] <<- numeric()
}

createParametersDF <- function() {
   parameters <<- data.frame(strategy = character(), 
                             type = character(), # type: constant allocation, CAPE, SMA, mixed, etc.
                             subtype = character(), # especially for Bollinger and mixed

                             startIndex = numeric(), # the first index that is not NA
                             inputDF = character(),
                             inputName = character(),
                             avgOver = numeric(),
                             bearish = numeric(),
                             bullish = numeric(),

                             name1 = character(), # other parameters used to create the strategy
                             value1 = numeric(), # values of these parameters
                             name2 = character(), 
                             value2 = numeric(), 
                             name3 = character(), 
                             value3 = numeric(), 
                             
                             inputStrategyName1 = character(), # for combined strategies: name of strategy used as input
                             fraction1 = numeric(), # for combined strategies: fraction (between 0 and 100)
                             inputStrategyName2 = character(),
                             fraction2 = numeric(), 
                             inputStrategyName3 = character(),
                             fraction3 = numeric(),
                             inputStrategyName4 = character(),
                             fraction4 = numeric(),
                             inputStrategyName5 = character(),
                             fraction5 = numeric(),
                             stringsAsFactors=F)
}


splitData <- function(dataSplit, force) {
   if (dataSplit == "search") {
      numData <<- numData %/% 2
      dat <<- dat[1:numData, ] # we keep only the first half
      def$plotEndYear <<- round( (numData-1)/12 + def$dataStartYear )
      
      def$maxTR  <<-400
      def$yTRmin <<-  7.8
      def$yTRmax <<- 10.2
      def$minVol <<- 12.5
      def$maxVol <<- 20.5
      def$minDD2 <<-  3
      def$maxDD2 <<- 11.5
      #def$coeffDD2 <<- def$coeffDD2 * 2 # DD2 is half as big with half as many years, hence the rescaling
 
      DD <<- DD[1:26, ]
      numDD <<- dim(DD)[[1]]
      
      if (!force)
         warning("When switching to \'search\' from a complete data set or from \'testing\', 
              it is recommended to run \'start\' with \'force=T\'.")
   } else if (dataSplit == "testing") {
      ## we subtract 10 years to start in 1932, so that the first strategies can be got around 1942
      startIndex <- ( numData %/% 24 - 10) * 12 + 1 
      dat <<- dat[startIndex:numData, ] # we keep only the second half
      numData <<- numData - startIndex + 1
      def$dataStartYear  <<- (startIndex-1)/12 + def$dataStartYear 
      def$plotStartYear  <<- 1942
      
      def$maxTR  <<-600
      def$yTRmin <<-  5.5
      def$yTRmax <<-  9.5
      def$minVol <<- 11
      def$maxVol <<- 15.5
      def$minDD2 <<-  2.5
      def$maxDD2 <<-  8.
      #def$coeffDD2 <<- def$coeffDD2 * 2 # DD2 is half as big with half as many years, hence the rescaling

      DD <<- DD[28:numDD, ]
      numDD <<- dim(DD)[[1]]
      
      if (!force)
         warning("When switching to \'testing\' from a complete data set or from \'search\', 
              it is recommended to run \'start\' with \'force=T\'.")
   } else if (dataSplit != "none") 
      warning(dataSplit, " is not a valid value for \'dataSplit\': choose one of \'none\', \'search\' or \'testing\'.")
}

addFutureReturnsToDat <- function(force=F) {
   if ( (def$futureYears==5) & (!exists("next5yrs") | force) ) 
      next5yrs  <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   else if ( (def$futureYears==10) & (!exists("next10yrs") | force) ) 
      next10yrs <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   else if ( (def$futureYears==15) & (!exists("next15yrs") | force) ) 
      next15yrs <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   else if ( (def$futureYears==20) & (!exists("next20yrs") | force) ) 
      next20yrs <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   else if ( (def$futureYears==30) & (!exists("next30yrs") | force) ) 
      next30yrs <<- data.frame(date = dat$date, numericDate = dat$numericDate)
}

addConstAllocToDat <- function(smoothConstantAlloc, force=F) {
   time0 <- proc.time()
   #message("Creating constant-allocation stock-bond strategies.") 
   if(smoothConstantAlloc)
      constAllocList <- seq(100, 0, by=-5)
   else 
      constAllocList <- c(100, 80, 60, 45, 30, 0)
   invisible ( lapply( constAllocList, function(alloc) createConstAllocStrategy(
      alloc, futureYears=def$futureYears, force=force) ) )
   #    message( "Time spent creating constant-allocation stock-bond strategies: ", 
   #              round(summary(proc.time())[[1]] - time0[[1]] , 1), " s." )
}


## Loading data from xls file
## the xls file has *nominal* values, the "dat" data frame has *real* values
loadData <- function(extrapolateDividends=T, downloadAndCheckAllFiles=T, lastMonthSP500="") {  
   if(!file.exists("./data/ie_data.xls")) # download file if not already locally available
      download.file("http://www.econ.yale.edu/~shiller/./data/ie_data.xls", "./data/ie_data.xls", mode = "wb")
   else if(downloadAndCheckAllFiles) # We force checking whether the local file is up to date
      checkXlsFileIsUpToDate()
   
   suppressMessages( library(XLConnect) ) # to handle xls file
   wk <- loadWorkbook("./data/ie_data.xls") 
   rawDat <- readWorksheet(wk, sheet="Data", startRow=8)
   
   numData <<- dim(rawDat)[1]-1 # number of rows (exclude row of comments)
   if (dataSplit!="search") { # if search the the message are irrelevant
      message(paste0("According to the xls file, \'", rawDat$P[numData+1], "\'")) # displaying information given in xls file
      message(paste0("According to the xls file, \'", rawDat$CPI[numData+1], "\'"))
   }
   rawDat <- rawDat[-(numData+1), ] # removing the row containing the information just displayed
   if (lastMonthSP500=="") {
      # removing the current month, since it is not data for the end of month
      rawDat <- rawDat[-numData, ] 
      numData <<- numData-1
   }
   else if (dataSplit!="search") { # if last-month S&P 500 is of relevance
      rawDat$P[numData] <- lastMonthSP500
      if (rawDat$P[numData-1] == lastMonthSP500)
         warning( "The value of lastMonthSP500 (", lastMonthSP500, ") is equal to the S&P 500 value for ", rawDat$Date[numData-1], immediate.=T )
   }
      
   # if the latest dividends are missing (if it is too early for the data to be available), 
   # ... either we extrapolate them or we remove the rows
   if ( is.na(rawDat$D[numData]) ) 
      if (extrapolateDividends) { 
         lastIndex <- numData # lastIndex will be the last index where dividends are valid         
         while( is.na(rawDat$D[lastIndex]) ) 
            lastIndex <- lastIndex-1         
         fitPara <- regression( (lastIndex-12):lastIndex, rawDat$D[(lastIndex-12):lastIndex] )
         a <- fitPara[[1]] # a and b are the parameters of a linear fit
         b <- fitPara[[2]] # ... they are used to extrapolate linearly the dividends
         rawDat$D[ (lastIndex+1) : numData ] <- a + b*( (lastIndex+1) : numData) # extrapolate dividend from past year
      } else # remove rows where dividend is NA (cannot calculate total return)
         while( is.na(rawDat$D[numData] ) ) {
            rawDat <- rawDat[-numData, ]
            numData <<- numData-1      
         }   
   
   # data are for the last (business) day of each month, 28th is close enough
   dat <<- data.frame(date       = ISOdate( floor(rawDat$Date), round((rawDat$Date%%1)*100,0)+1, 1, hour=0, min=0, sec=0 ) - 1,
                      numericDate= as.numeric(rawDat$Fraction),
                      CPI        = as.numeric(rawDat$CPI), # reference for inflation
                      dividend   = as.numeric(rawDat$D), # loads nominal dividend (real dividend will be calculated below)
                      price      = as.numeric(rawDat$P), # loads nominal S&P price (real price will be calculated below)
                      earnings   = as.numeric(rawDat$E), # loads nominal earnings (real earnings will be calculated below)
                      TR         = numeric(numData),
                      bonds      = numeric(numData)
   )
   
   dat$numericDate <<- dat$numericDate+0.49/12 # data are actually for end of month, not middle
   refCPI          <<- dat$CPI[numData] # reference for inflation
   dat$price       <<- dat$price    / dat$CPI * refCPI # calculating real price
   dat$dividend    <<- dat$dividend / dat$CPI * refCPI # calculating real dividend
   dat$earnings    <<- dat$earnings / dat$CPI * refCPI # calculating real earnings
   
   dat$TR[1] <<- 1
   for(i in 2:numData)
      dat$TR[i] <<- dat$TR[i-1] * dat$price[i]/dat$price[i-1] * (1 + dat$dividend[i]/dat$price[i]/12)
   
   dat$bonds <<- read.csv("./data/bonds.csv", header=T)[1:numData, 1]
   # message("Real bond prices were imported from an Excel calculation.")
   if (lastMonthSP500!="" && dataSplit!="search" ) {
      dat$bonds[numData] <<- dat$bonds[numData-1]
      message( "dat$bonds[", numData, "] set to the value of dat$bonds[", numData-1, "], i.e. ", dat$bonds[numData-1] )
   }
      
   while(is.na(dat$bonds[numData])) {
      warning(paste0("removing last row of data (for ", dat$date[numData], ") because bond data are missing."))
      dat <<- dat[-numData, ]
      numData <<- numData - 1
   }
   # message("Shiller's xls file has *nominal* values, the \'dat\' data frame has *real* values.")
}

checkXlsFileIsUpToDate <- function(fileName="./data/ie_data.xls") {
   if(!file.exists(fileName)) 
      stop(fileName, " is not on the local disk.")
   
   download.file("http://www.econ.yale.edu/~shiller/data/ie_data.xls", "ie_data-remote.xls", mode = "wb")
   
   if ( file.info(fileName)$size == file.info("ie_data-remote.xls")$size ) 
      print(paste("The file", fileName, "is up to date.") )
   else { # The size of the local file is different from Shiller's file 
      file.copy("ie_data-remote.xls", fileName, overwrite=T)
      print(paste("The file", fileName, "has been updated.") )   
   }
   file.remove("ie_data-remote.xls")
}

## for some columns in 'parameters' and 'stats', there are only a handful of possible values
makeStringsFactors <- function() {
   allTypes <- c("constantAlloc", "gold", "UKhousePrice", "inflation", 
                 "CAPE", "detrended", "Bollinger", "SMA", "reversal", "combined", 
                 "Boll_CAPE", "Boll_detrended", "SMA_CAPE", "SMA_detrended", "reversal_CAPE", "reversal_detrended" )
   allSubtypes <- c(allTypes, "balanced", "technical", "value", "hybrid", "TR")
   allTypes <- c(allTypes, "search") # "search" can only be a type, not a subtype
   
   parameters$type    <<- factor(parameters$type, levels=allTypes) 
   parameters$subtype <<- factor(parameters$subtype, levels=allSubtypes)
   parameters$inputDF <<- factor(parameters$inputDF, 
                                    levels = c( "dat", "signal", "alloc", "TR", "next5yrs", "next10yrs", "next15yrs", "next20yrs", "next30yrs" ) )
   
   stats$type         <<- factor(stats$type, levels=allTypes) 
   stats$subtype      <<- factor(stats$subtype, levels=allSubtypes)
   
   ## To handle searches:
#    levels(parameters$subtype) <<- c(levels(parameters$type), levels(parameters$subtype))
#    levels(parameters$type) <<- c(levels(parameters$type), "search")   
#    levels(stats$type)      <<- c(levels(stats$type), levels(parameters$type))
#    levels(stats$subtype)   <<- c(levels(stats$subtype), levels(parameters$subtype))
}

# Generating typical strategies
createTypicalStrategies <- function(extrapolateDividends=T, costs=def$tradingCost, 
                                    coeffTR=def$coeffTR, coeffVol=def$coeffVol, coeffDD2=def$coeffDD2, force=F) {
   #message("Creating entries for the typical strategies")
   
   ## Technical strategies
   createBollStrategy(inputDF=def$BollInputDF, inputName=def$BollInputName, avgOver=def$BollAvgOver1, 
                      bearish=def$BollBearish1, bullish=def$BollBullish1, 
                      signalMin=def$signalMin, signalMax=def$signalMax,
                      futureYears=def$futureYears, costs=costs, 
                      coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force) 
   createBollStrategy(inputDF=def$BollInputDF, inputName=def$BollInputName, avgOver=def$BollAvgOver2, 
                      bearish=def$BollBearish2, bullish=def$BollBullish2, 
                      signalMin=def$signalMin, signalMax=def$signalMax,
                      futureYears=def$futureYears, costs=costs, 
                      coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)   
   
   createSMAstrategy(inputDF=def$SMAinputDF, inputName=def$SMAinputName, SMA1=def$SMA1_1, SMA2=def$SMA2_1, 
                     bearish=def$SMAbearish1, bullish=def$SMAbullish1, 
                     signalMin=def$signalMin, signalMax=def$signalMax,
                     futureYears=def$futureYears, costs=costs, 
                     coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   createSMAstrategy(inputDF=def$SMAinputDF, inputName=def$SMAinputName, SMA1=def$SMA1_2, SMA2=def$SMA2_2, 
                     bearish=def$SMAbearish2, bullish=def$SMAbullish2, 
                     signalMin=def$signalMin, signalMax=def$signalMax,
                     futureYears=def$futureYears, costs=costs, 
                     coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   
   createReversalStrategy(inputDF=def$reversalInputDF, inputName=def$reversalInputName, 
                          avgOver=def$reversalAvgOver1, returnToMean=def$reversalReturnToMean1, 
                          bearish=def$reversalBearish1, bullish=def$reversalBullish1, 
                          signalMin=def$signalMin, signalMax=def$signalMax,
                          futureYears=def$futureYears, costs=costs, 
                          coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force) 
   #    createReversalStrategy(inputDF=def$reversalInputDF, inputName=def$reversalInputName, 
   #                           avgOver=def$reversalAvgOver2, returnToMean=def$reversalReturnToMean2, 
   #                           bearish=def$reversalBearish2, bullish=def$reversalBullish2, 
   #                           signalMin=def$signalMin, signalMax=def$signalMax,
   #                           futureYears=def$futureYears, costs=costs, 
   #                           coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force) 

   
   ## Value strategies
   createCAPEstrategy(years=def$CAPEyears_hy1, cheat=def$CAPEcheat1, avgOver=def$CAPEavgOver_hy1, 
                      hysteresis=T, hystLoopWidthMidpoint=def$hystLoopWidthMidpoint1,
                      hystLoopWidth=def$hystLoopWidth1, slope=def$slope1,
                      signalMin=def$signalMin, signalMax=def$signalMax,
                      futureYears=def$futureYears, costs=costs, 
                      coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   createCAPEstrategy(years=def$CAPEyears_hy2, cheat=def$CAPEcheat2, avgOver=def$CAPEavgOver_hy2, 
                      hysteresis=T, hystLoopWidthMidpoint=def$hystLoopWidthMidpoint2,
                      hystLoopWidth=def$hystLoopWidth2, slope=def$slope2,
                      signalMin=def$signalMin, signalMax=def$signalMax,
                      futureYears=def$futureYears, costs=costs, 
                      coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   #    createCAPEstrategy(years=def$CAPEyears_NH, cheat=def$CAPEcheat, avgOver=def$CAPEavgOver_NH, 
   #                       hysteresis=F, bearish=def$CAPEbearish, bullish=def$CAPEbullish, 
   #                       signalMin=def$signalMin, signalMax=def$signalMax,
   #                       futureYears=def$futureYears, costs=costs, 
   #                       coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)  
   #    
   #    createDetrendedStrategy(inputDF=def$detrendedInputDF, inputName=def$detrendedInputName, 
   #                            avgOver=def$detrendedAvgOver1, 
   #                            bearish=def$detrendedBearish1, bullish=def$detrendedBullish1, 
   #                            signalMin=def$signalMin, signalMax=def$signalMax,
   #                            futureYears=def$futureYears, costs=costs, 
   #                            coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   #    createDetrendedStrategy(inputDF=def$detrendedInputDF, inputName=def$detrendedInputName, 
   #                            avgOver=def$detrendedAvgOver2, 
   #                            bearish=def$detrendedBearish2, bullish=def$detrendedBullish2, 
   #                            signalMin=def$signalMin, signalMax=def$signalMax,
   #                            futureYears=def$futureYears, costs=costs, 
   #                            coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   
   ## Hybrid strategies
   if (!(def$Boll_CAPEinputName1 %in% colnames(dat))) 
      calcCAPE(years=def$Boll_CAPEyears1, cheat=def$Boll_CAPEcheat1)
   createBollStrategy(inputDF=def$Boll_CAPEinputDF, inputName=def$Boll_CAPEinputName1, avgOver=def$Boll_CAPEavgOver1,
                      bearish=def$Boll_CAPEbearish1, bullish=def$Boll_CAPEbullish1, strategyName="",
                      signalMin=def$signalMin, signalMax=def$signalMax,
                      futureYears=def$futureYears, costs=costs, 
                      coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)

   if (!(def$Boll_CAPEinputName2 %in% colnames(dat))) 
      calcCAPE(years=def$Boll_CAPEyears2, cheat=def$Boll_CAPEcheat2)
   createBollStrategy(inputDF=def$Boll_CAPEinputDF, inputName=def$Boll_CAPEinputName2, avgOver=def$Boll_CAPEavgOver2,
                      bearish=def$Boll_CAPEbearish2, bullish=def$Boll_CAPEbullish2, strategyName="",
                      signalMin=def$signalMin, signalMax=def$signalMax,
                      futureYears=def$futureYears, costs=costs, 
                      coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force) 

   if (!(def$Boll_detrendedInputName %in% colnames(dat))) 
      calcDetrended(def$Boll_detrendedInputDF, "TR")
   createBollStrategy(inputDF=def$Boll_detrendedInputDF, inputName=def$Boll_detrendedInputName, 
                      avgOver=def$Boll_detrendedAvgOver1, bearish=def$Boll_detrendedBearish1, 
                      bullish=def$Boll_detrendedBullish1, strategyName="",
                      signalMin=def$signalMin, signalMax=def$signalMax,
                      futureYears=def$futureYears, costs=costs, 
                      coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)   

   if (!(def$SMA_CAPEinputName1 %in% colnames(dat))) calcCAPE(years=def$SMA_CAPEyears1, cheat=def$SMA_CAPEcheat1)
   createSMAstrategy(inputDF=def$SMA_CAPEinputDF, inputName=def$SMA_CAPEinputName1, SMA1=def$SMA_CAPE_SMA1_1, 
                     SMA2=def$SMA_CAPE_SMA2_1, bearish=def$SMA_CAPEbearish1, bullish=def$SMA_CAPEbullish1, 
                     signalMin=def$signalMin, signalMax=def$signalMax,
                     futureYears=def$futureYears, costs=costs, 
                     coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   if (!(def$SMA_CAPEinputName2 %in% colnames(dat))) calcCAPE(years=def$SMA_CAPEyears2, cheat=def$SMA_CAPEcheat2)
   createSMAstrategy(inputDF=def$SMA_CAPEinputDF, inputName=def$SMA_CAPEinputName2, SMA1=def$SMA_CAPE_SMA1_2, 
                     SMA2=def$SMA_CAPE_SMA2_2, bearish=def$SMA_CAPEbearish2, bullish=def$SMA_CAPEbullish2, 
                     signalMin=def$signalMin, signalMax=def$signalMax,
                     futureYears=def$futureYears, costs=costs, 
                     coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)

   #    if (!(def$reversal_CAPEinputName1 %in% colnames(dat))) calcCAPE(years=def$reversal_CAPEyears1, cheat=def$reversal_CAPEcheat1)
   #    createReversalStrategy(inputDF=def$reversal_CAPEinputDF, inputName=def$reversal_CAPEinputName1, 
   #                           avgOver=def$reversal_CAPEavgOver1, returnToMean=def$reversal_CAPEreturnToMean1, 
   #                           bearish=def$reversal_CAPEbearish1, bullish=def$reversal_CAPEbullish1, 
   #                           signalMin=def$signalMin, signalMax=def$signalMax,
   #                           futureYears=def$futureYears, costs=costs, 
   #                           coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force) 
   #    if (!(def$reversal_CAPEinputName2 %in% colnames(dat))) calcCAPE(years=def$reversal_CAPEyears2, cheat=def$reversal_CAPEcheat2)
   #    createReversalStrategy(inputDF=def$reversal_CAPEinputDF, inputName=def$reversal_CAPEinputName2, 
   #                           avgOver=def$reversal_CAPEavgOver2, returnToMean=def$reversal_CAPEreturnToMean2, 
   #                           bearish=def$reversal_CAPEbearish2, bullish=def$reversal_CAPEbullish2, 
   #                           signalMin=def$signalMin, signalMax=def$signalMax,
   #                           futureYears=def$futureYears, costs=costs, 
   #                           coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force) 
   
   ## Combined strategies

   combineStrategies(inputStrategyName=def$technicalStrategies, score=def$technicalScores, subtype="technical", 
                     minScore=def$minScoreTechnical,
                     costs=costs, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)

   combineStrategies(inputStrategyName=def$valueStrategies, score=def$valueScores, subtype="value", 
                     minScore=def$minScoreValue, 
                     costs=costs, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   
   combineStrategies(inputStrategyName=def$hybridStrategies, score=def$hybridScores, subtype="hybrid", 
                     minScore=def$minScoreHybrid,
                     costs=costs, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force)
   
   combineStrategies(inputStrategyName=def$balancedStrategies, score=def$balancedScores, subtype="balanced", 
                     minScore=def$minScoreBalanced, 
                     costs=costs, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force) 
   
   def$startIndex    <<- max(parameters$startIndex)
   

#    combineStrategies(inputStrategyName=c(def$technicalStrategies, def$valueStrategies, def$hybridStrategies), 
#                      score=c(def$technicalScores, def$valueScores, def$hybridScores), subtype="balanced", 
#                      minScore=def$minScore, maxScore=def$maxScore, strategyName="balanced2",
#                      costs=costs, coeffTR=coeffTR, coeffVol=coeffVol, coeffDD2=coeffDD2, force=force) 

}

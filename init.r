


# Loading and preparing data
start <- function(extrapolateDividends=T, # whether to extrapolate missing recent dividends (or remove incomplete months)
                  smoothConstantAlloc=F, # calculates more constant-allocation portfolios, to get smoother curves in plots
                  downloadAndCheckAllFiles=F, # downloads data files even if they exist locally, to check whether they are up to date
                  otherAssetClasses=F, # loads gold and UK house prices
                  newcomer=F, # displays some information on the code
                  force=F) {
   
   if(!file.exists("utils.r")) stop("Use \'setwd()\' to change the working directory to that containing the data.")

   source("utils.r")      # general functions (i.e. those not fitting in another file)
   source("DD.r")         # drawdowns
   source("plotting.r")   # various functions that generate plots
   
   # Strategies:
   source("CAPE.r")
   source("detrended.r")
   source("Bollinger.r")
   source("SMA.r")
   source("momentum.r")
   source("reversal.r")
   source("combine.r")
   
   totTime <- proc.time()
   
   setDefaultValues(force=force)
   
   if (!exists("dat") | downloadAndCheckAllFiles) { # if data frame does not exist (or if we want to force the loading), we load the xls file
      message("Starting to load the data from the xls file.")
      message("Then we will also load a list of drawdowns, of gold prices and of UK house prices.")
      message("After that, we will create the basic data structures and calculate some basic strategies.")
      message()
      #if (!constAlloc) message("If you want stock-bond constant allocations to be created and their statistics to be calculated, run \'start(constAlloc=T)\'.")
      loadData( downloadAndCheckAllFiles=downloadAndCheckAllFiles )
   } 
   
   if (!exists("signal")| force) signal <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   if (!exists("alloc") | force) alloc  <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   if (!exists("TR")    | force) TR     <<- data.frame(date = dat$date, numericDate = dat$numericDate, stocks = dat$TR)

   if ( def$tradingCost==0.02 & (!exists("netTR2") | force) )
      netTR2 <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   else if ( def$tradingCost==0.04 & (!exists("netTR4") | force) )
      netTR4 <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   
   if ( (def$futureYears==10) & (!exists("next10yrs") | force) )
      next10yrs <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   else if ( (def$futureYears==20) & (!exists("next20yrs") | force) )
      next20yrs <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   else if ( (def$futureYears==30) & (!exists("next30yrs") | force) )
      next30yrs <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   
   if (!exists("DD") | force) loadDDlist(force=force) # loading the dates of major drawdowns
   if (!exists("stats") | force)  createStatsDF()   
   if (!exists("parameters") | force) createParametersDF()
   
   addNumColToDat("TRmonthly")
   addNumColToDat("bondsMonthly")
   addNumColToDat("monthlyDifference")
   for(i in 2:numData) {
      dat$TRmonthly[i] <<- dat$TR[i] / dat$TR[i-1] 
      dat$bondsMonthly[i] <<- dat$bonds[i] / dat$bonds[i-1] 
      dat$monthlyDifference[i] <<- dat$TRmonthly[i] - dat$bondsMonthly[i]
   }
   
   time0 <- proc.time()
   message("Creating constant-allocation stock-bond strategies.") 
   if(smoothConstantAlloc)
      constAllocList <- seq(100, 0, by=-5)
   else {
      if (def$tradingCost==0.02)
         constAllocList <- c(100, 95, 90, 85, 0)
      else if (def$tradingCost==0.04)
         constAllocList <- c(100, 90, 80, 70, 60, 0)
   }
      invisible ( lapply( constAllocList, function(alloc) createConstAllocStrategy(
      alloc, futureYears=def$futureYears, tradingCost=def$tradingCost, force=force) ) )
   print( c( "constant allocation time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   if(otherAssetClasses) {
      source("otherAssetClasses.r")# gold and UK housing
      
      if (!"gold" %in% colnames(dat) | !"gold" %in% stats$strategy | force) {
         loadGoldData()
         createGoldStrategy(futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)
         message("Real gold prices were obtained from a local csv file.")
      }
      
      if (!"UKhousePrice" %in% colnames(dat) | !"UKhousePrice" %in% stats$strategy | downloadAndCheckAllFiles) {
         loadUKhousePriceData(downloadAndCheckAllFiles=downloadAndCheckAllFiles)
         createUKhousePriceStrategy(futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)
         message("Real UK house prices were obtained from Nationwide; they are in pounds, and based on UK inflation.")
      }
   }
   
   if(!downloadAndCheckAllFiles) {
      print( Sys.time() )
   }
   
   if(newcomer) showForNewcomer()
   
   createTypicalStrategies(force=force)
   
   showSummaries()
   
   makeStringsFactors()
   
#    print(proc.time() - totTime)
   print( paste0( "This took: ", 
                 round(summary(proc.time())[[3]] - totTime[[3]] , 0), " s, with about " , 
                 round(summary(proc.time())[[1]] - totTime[[1]] , 0), " s for calculations and " ,
                 round(summary(proc.time())[[3]]-summary(proc.time())[[1]] + totTime[[1]]-totTime[[3]] , 0), 
                 " s to download files." ) )
}


setDefaultValues <- function(force=F) {
   if (!exists("def") | force) def <<- list()
   
   def$futureYears       <<- 20L    # default value for the number of years over which future returns are calculated
   message("default futureYears: ", def$futureYears)
   def$tradingCost       <<- 2/100 # default value for the trading costs
   message("default tradingCost: ", def$tradingCost*100, "% / per year of turnover")
#    def$interQuartileAlloc<<- 90
#    message("default interQuartileAlloc: ", def$interQuartileAlloc, "%")
   
   def$startIndex        <<- round(10.5*12+1)
   def$startYear         <<- (def$startIndex-1)/12+1871
   
   def$CPUnumber         <<- 1 # Parallelization does not work
   
   def$signalMin <<- -0.2
   def$signalMax <<-  1.2
   
   setPlottingDefaultValues()
   setCAPEdefaultValues()
   setDetrendedDefaultValues()
   setBollDefaultValues()
   setSMAdefaultValues()
   setMomentumDefaultValues()
   setReversalDefaultValues()
   setCombinedDefaultValues()
   
   def$typicalStrategies <<- c(def$typicalBalanced, def$typicalTechnical, def$typicalValue, "stocks")
}

createStatsDF <- function() {
   stats <<- data.frame(strategy = character(), 
                        type = character(), # type: constant allocation, CAPE, SMA, mixed, etc.
                        subtype = character(), # especially for combinedstrategy
                        TR = numeric(),  # average real total return (exponential regression)
                        netTR2 = numeric(),  # average real total return net of 2% of trading costs
                        netTR4 = numeric(),  # average real total return net of 4% of trading costs
                        volatility = numeric(), 
                        avgStockAlloc = numeric(), # average allocation to stocks
                        latestStockAlloc = numeric(), # allocation to stocks as of the last date of the data
                        turnover = numeric(), # turnover of the portfolio (in years)
                        invTurnover = numeric(), # 1 / turnover
                        DD2 = numeric(), # sum of the squares of the drawdowns
                        score = numeric(), 
                        stringsAsFactors=F)
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
                             # medianAlloc = numeric(),
                             # interQuartileAlloc = numeric(),

                             name1 = character(), # other parameters used to create the strategy
                             value1 = numeric(), # values of these parameters
                             name2 = character(), 
                             value2 = numeric(), 
                             
                             inputStrategyName1 = character(), # for combined strategies: name of strategy used as input
                             fraction1 = numeric(), # for combined strategies: fraction (between 0 and 100)
                             inputStrategyName2 = character(),
                             fraction2 = numeric(), 
                             inputStrategyName3 = character(),
                             fraction3 = numeric(),
                             stringsAsFactors=F)
}


# Loading data from xls file
loadData <- function(extrapolateDividends=T, downloadAndCheckAllFiles=F) {  # the xls file has *nominal* values, the "dat" data frame has *real* values
   library(XLConnect) # to handle xls file
   if(!file.exists("data/ie_data.xls")) # download file if not already locally available
      download.file("http://www.econ.yale.edu/~shiller/data/ie_data.xls", "data/ie_data.xls", mode = "wb")
   else if(downloadAndCheckAllFiles) # We check whether the local file is up to date (if we have time)
      checkXlsFileIsUpToDate()
   
   wk <- loadWorkbook("data/ie_data.xls") 
   rawDat <- readWorksheet(wk, sheet="Data", startRow=8)
   
   numData <<- dim(rawDat)[1]-2 # number of rows
   message(paste0("According to the xls file, \'", rawDat$P[numData+2], "\'")) # displaying information given in xls file
   message(paste0("According to the xls file, \'", rawDat$CPI[numData+2], "\'"))
#   message(paste0("According to the xls file, \'", rawDat$Rate.GS10[numData+2], "\'"))
   rawDat <- rawDat[-(numData+2), ] # removing the row containing the information just displayed
   rawDat <- rawDat[-(numData+1), ] # removing the current month, as it is not data for the end of month
   
   if (extrapolateDividends) { # do we extrapolate the missing dividends (too soon for data to be available) or do we remove the rows
      lastIndex <- numData # last index where dividends are valid
      while( is.na(rawDat$D[lastIndex]) )  lastIndex <- lastIndex-1
      
      fitPara <- regression( (lastIndex-12):lastIndex, rawDat$D[(lastIndex-12):lastIndex] )
      a <- fitPara[[1]]
      b <- fitPara[[2]]
      rawDat$D[ (lastIndex+1) : numData ] <- a + b*( (lastIndex+1) : numData) # extrapolate dividend from past year
   } else # remove rows where dividend is NA (cannot calculate total return)
      while( is.na(rawDat$D[numData] ) ) { # remove rows where dividend is NA (cannot calculate total return)
         rawDat <- rawDat[-numData, ]
         numData <<- numData-1      
      }
   
   
   # data are for the last (business) day of each month, 28th is close enough
   dat <<- data.frame(date       = ISOdate( floor(rawDat$Date), round((rawDat$Date%%1)*100,0), 28 ),
                      numericDate= as.numeric(rawDat$Fraction),
                      CPI        = as.numeric(rawDat$CPI), # reference for inflation
                      dividend   = as.numeric(rawDat$D), # loads nominal dividend (real dividend will be calculated below)
                      price      = as.numeric(rawDat$P), # loads nominal S&P price (real price will be calculated below)
                      earnings   = as.numeric(rawDat$E), # loads nominal earnings (real earnings will be calculated below)
                      TR= numeric(numData),
                      bonds      = numeric(numData)
   )
   
   refCPI       <<- dat$CPI[numData] # reference for inflation
   dat$price    <<- dat$price    / dat$CPI * refCPI # calculating real price
   dat$dividend <<- dat$dividend / dat$CPI * refCPI # calculating real dividend
   dat$earnings <<- dat$earnings / dat$CPI * refCPI # calculating real earnings
   
   dat$TR[1] <<- 1
   for(i in 2:numData)
      dat$TR[i] <<- dat$TR[i-1] * dat$price[i]/dat$price[i-1] * (1 + dat$dividend[i]/dat$price[i]/12)
   
   dat$bonds <<- read.csv("data/bonds.csv", header=T)[1:numData, 1]
   message("Real bond prices were imported from an Excel calculation.")
   
   message("Shiller's xls file has *nominal* values, the \'dat\' data frame has *real* values.")
}

checkXlsFileIsUpToDate <- function(fileName="data/ie_data.xls") {
   if(!file.exists(fileName)) 
      stop(fileName, " is not on the local disk.")
   
   library(XLConnect) # to handle xls file
   wk <- loadWorkbook(fileName) # this is the local file
   localVersion <- readWorksheet(wk, sheet="Data", startRow=8)
   
   lastLine <- dim(localVersion)[1] 
   localMessage1 <- as.character(localVersion$P[lastLine])
   localMessage2 <- as.character(localVersion$CPI[lastLine])
   localMessage3 <- as.character(localVersion$Rate.GS10[lastLine])
   
   download.file("http://www.econ.yale.edu/~shiller/data/ie_data.xls", "ie_data-remote.xls", mode = "wb")
   library(XLConnect) # to handle xls file
   wk <- loadWorkbook("ie_data-remote.xls")  # this is the local file
   remoteVersion <- readWorksheet(wk, sheet="Data", startRow=8)
   file.remove("ie_data-remote.xls") 
   
   lastLine <- dim(remoteVersion)[1] 
   remoteMessage1 <- as.character(remoteVersion$P[lastLine])
   remoteMessage2 <- as.character(remoteVersion$CPI[lastLine])
   remoteMessage3 <- as.character(remoteVersion$Rate.GS10[lastLine])
   
   if (localMessage1 == remoteMessage1 & localMessage2 == remoteMessage2 & localMessage3 == remoteMessage3)
      print(paste("The file", fileName, "is up to date.") )
   else {
      if (localMessage1 != remoteMessage1)
         print(paste0("On the remote xls file: ", remoteMessage1, "whereas on the local file: ", localMessage1))
      if (localMessage2 != remoteMessage2)
         print(paste0("On the remote xls file: ", remoteMessage2, "whereas on the local file: ", localMessage2))
      if (localMessage3 != remoteMessage3)
         print(paste0("On the remote xls file: ", remoteMessage3, "whereas on the local file: ", localMessage3))
   }
}

## for some columns in 'parameters' and 'stats', there are only a handful of possible values
makeStringsFactors <- function() {
   parameters$type      <<- as.factor(parameters$type) 
   parameters$subtype   <<- as.factor(parameters$subtype)
   parameters$inputDF   <<- as.factor(parameters$inputDF)
   levels(parameters$inputDF) <<- c( "dat", "signal", "alloc", "TR", "next30yrs" ) 
   
   stats$type           <<- as.factor(stats$type) 
   stats$subtype        <<- as.factor(stats$subtype)

   ## To handle searches:
   levels(parameters$subtype) <<- c(levels(parameters$type), levels(parameters$subtype))
   levels(parameters$type) <<- c(levels(parameters$type), "search")   
   levels(stats$type)      <<- c(levels(stats$type), levels(parameters$type))
   levels(stats$subtype)   <<- c(levels(stats$subtype), levels(parameters$subtype))
}

# Generating typical strategies
createTypicalStrategies <- function(extrapolateDividends=T, force=F) {
   message("Creating entries for the typical strategies")

   time0 <- proc.time()
   createCAPEstrategy(years=def$CAPEyears, cheat=def$CAPEcheat, avgOver=def$CAPEavgOver, 
                      bearish=def$CAPEbearish, bullish=def$CAPEbullish, 
                      signalMin=def$signalMin, signalMax=def$signalMax,
                      futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)
   print( c( "CAPE time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )

   time0 <- proc.time()
   createDetrendedStrategy(inputDF=def$detrendedInputDF, inputName=def$detrendedInputName, 
                           avgOver=def$detrendedAvgOver1, 
                           bearish=def$detrendedBearish1, bullish=def$detrendedBullish1, 
                           signalMin=def$signalMin, signalMax=def$signalMax,
                           futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)
   print( c( "detrended1 time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )

   time0 <- proc.time()
   createDetrendedStrategy(inputDF=def$detrendedInputDF, inputName=def$detrendedInputName, 
                           avgOver=def$detrendedAvgOver2, 
                           bearish=def$detrendedBearish2, bullish=def$detrendedBullish2, 
                           signalMin=def$signalMin, signalMax=def$signalMax,
                           futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)
   print( c( "detrended2 time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   time0 <- proc.time()
   createBollStrategy(inputDF=def$BollInputDF, inputName=def$BollInputName, avgOver=def$BollAvgOver, 
                      bearish=def$BollBearish, bullish=def$BollBullish, 
                      signalMin=def$signalMin, signalMax=def$signalMax,
                      futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)   
   print( c( "Bollinger time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   time0 <- proc.time()
   createSMAstrategy(inputDF=def$SMAinputDF, inputName=def$SMAinputName, SMA1=def$SMA1, SMA2=def$SMA2, 
                     bearish=def$SMAbearish, bullish=def$SMAbullish, 
                     signalMin=def$signalMin, signalMax=def$signalMax,
                     futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)   
   print( c( "SMA time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   time0 <- proc.time()
   createMomentumStrategy(def$momentumInputDF, def$momentumInputName, def$momentumAvgOver, 
                          bearish=def$momentumBearish, bullish=def$momentumBullish, 
                          signalMin=def$signalMin, signalMax=def$signalMax,
                          futureYears=def$futureYears, tradingCost=def$tradingCost, force=force) 
   print( c( "momentum time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   time0 <- proc.time()
   createReversalStrategy(inputDF=def$reversalInputDF, inputName=def$reversalInputName, 
                          avgOver=def$reversalAvgOver, returnToMean=def$reversalReturnToMean, 
                          bearish=def$reversalBearish, bullish=def$reversalBullish, 
                          signalMin=def$signalMin, signalMax=def$signalMax,
                          futureYears=def$futureYears, tradingCost=def$tradingCost, force=force) 
   print( c( "reversal time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
       
   time0 <- proc.time()
   combineStrategies(inputStrategyName1=def$typicalCAPE, inputStrategyName2=def$typicalDetrended1, 
                     inputStrategyName3=def$typicalDetrended2, "",
                     def$valueFractionCAPE, def$valueFractionDetrended1, def$valueFractionDetrended2, 0, 
                     type="combined", subtype="value", tradingCost=def$tradingCost, force=force)
   print( c( "value time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   time0 <- proc.time()
   combineStrategies(def$typicalSMA, def$typicalBoll, def$typicalMomentum, def$typicalReversal, 
                     def$technicalFractionSMA, def$technicalFractionBoll, 
                     def$technicalFractionMomentum, def$technicalFractionReversal, 
                     type="combined", subtype="technical", 
                     tradingCost=def$tradingCost, force=force)
   print( c( "technical time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   time0 <- proc.time()
   combineStrategies(def$typicalValue, def$typicalTechnical, "", "",
                     def$balancedFractionValue, def$balancedFractionTechnical, 0, 0, 
                     type="combined", subtype="balanced", 
                     tradingCost=def$tradingCost, force=force)
   print( c( "balanced time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )   
   
}

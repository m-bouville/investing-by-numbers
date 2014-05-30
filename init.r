


# Loading and preparing data
start <- function(extrapolateDividends=T, newbie=F, LoadAndCheckAll=F, force=F) {
   
   if(!file.exists("utils.r")) stop("Use \'setwd()\' to change the working directory to that containing the data.")
   
   totTime <- proc.time()
   
   setDefaultValues(force=force)
   
   if (!exists("dat") | LoadAndCheckAll) { # if data frame does not exist (or if we want to force the loading), we load the xls file
      message("Starting to load the data from the xls file.")
      message("Then we will also load a list of drawdowns and gold prices.")
      message("After that, we will create the basic data structures.")
      message()
      #if (!constAlloc) message("If you want stock-bond constant allocations to be created and their statistics to be calculated, run \'start(constAlloc=T)\'.")
      loadData( LoadAndCheckAll=LoadAndCheckAll )
   } 
   
   if (!exists("normalized")| force) normalized<<- data.frame(date = dat$date, numericDate = dat$numericDate)
   if (!exists("alloc")     | force) alloc     <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   if (!exists("TR")        | force) TR        <<- data.frame(date = dat$date, numericDate = dat$numericDate, stocks = dat$TR)

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
   invisible ( 
      lapply( c(100, 90, 80, 0), #seq(0, 100, by=5), 
             function(alloc) createConstAllocStrategy(
                alloc, futureYears=def$futureYears, tradingCost=def$tradingCost, force=force) )
   )
   print( c( "constant allocation time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   if (!"gold" %in% colnames(dat) | !"gold" %in% stats$strategy | force) {
      loadGoldData()
      createGoldStrategy(futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)
      message("Real gold prices were obtained from a local csv file.")
   }
   
   if (!"UKhousePrice" %in% colnames(dat) | !"UKhousePrice" %in% stats$strategy | LoadAndCheckAll) {
      loadUKhousePriceData(LoadAndCheckAll=LoadAndCheckAll)
      createUKhousePriceStrategy(futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)
      message("Real UK house prices were obtained from Nationwide; they are in pounds, and based on UK inflation.")
   }
   
   if(newbie) showFornewbie()
   
   createTypicalStrategies(force=force)
   
   plotAllReturnsVsFour()
   # plotReturnAndAlloc()
   
   showSummaries()
   
   print(proc.time() - totTime)
}


setDefaultValues <- function(force=F) {
   if (!exists("def") | force) def <<- list()
   def$futureYears       <<- 10L    # default value for the number of years over which future returns are calculated
   def$tradingCost       <<- 2/100 # default value for the trading costs
   def$startIndex        <<- round(10.5*12+1)
   def$startYear         <<- (def$startIndex-1)/12+1871
   
   def$typicalCAPE       <<- "CAPE10_2avg30_90_98"
   def$typicalDetrended  <<- "detrendedTRavg30_90_97"
   
   def$typicalSMA        <<- "SMA_TR12_1_90_50"
   def$typicalBoll       <<- "Boll_TR_21_95_20"
   def$typicalMomentum   <<- "momentum_TR_12_95_15"
   
   def$typicalValue      <<- "value70_30_85_98"
   def$typicalTechnical  <<- "technical50_25_25_95_60"
   def$typicalBalanced   <<- "balanced75_25_98_70"
   def$typicalStrategies <<- c(def$typicalTechnical, def$typicalValue, def$typicalBalanced, "stocks")
   
   def$CPUnumber         <<- 1 # Parallelization does not work
      
   setPlottingDefaultValues()
   setCAPEdefaultValues()
   def$detrendedAvgOver <<- 12
   setBollDefaultValues()
   setMomentumDefaultValues()
   setSMAdefaultValues()
   setMultidefaultValues()
}

createStatsDF <- function() {
   stats <<- data.frame(strategy = character(), 
                        type = character(), # type: constant allocation, CAPE, SMA, mixed, etc.
                        subtype = character(), # especially for multistrategy
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
                             medianAlloc = numeric(),
                             interQuartileAlloc = numeric(),

                             name1 = character(), # other parameters used to create the strategy
                             value1 = numeric(), # values of these parameters
                             name2 = character(), 
                             value2 = numeric(), 
                             
                             inputStrategyName1 = character(), # for multi strategies: name of strategy used as input
                             fraction1 = numeric(), # for multi strategies: fraction (between 0 and 100)
                             inputStrategyName2 = character(),
                             fraction2 = numeric(), 
                             inputStrategyName3 = character(),
                             fraction3 = numeric(),
                             stringsAsFactors=F)
}


# Loading data from xls file
loadData <- function(extrapolateDividends=T, LoadAndCheckAll=F) {  # the xls file has *nominal* values, the "dat" data frame has *real* values
   library(XLConnect) # to handle xls file
   if(!file.exists("ie_data.xls")) # download file if not already locally available
      download.file("http://www.econ.yale.edu/~shiller/data/ie_data.xls", "ie_data.xls", mode = "wb")
   else if(LoadAndCheckAll) # We check whether the local file is up to date (if we have time)
      checkXlsFileIsUpToDate()
   
   wk <- loadWorkbook("ie_data.xls") 
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
   
   dat$bonds <<- read.csv("bonds.csv", header=T)[1:numData, 1]
   message("Real bond prices were imported from an Excel calculation.")
   
   message("Shiller's xls file has *nominal* values, the \'dat\' data frame has *real* values.")
}

checkXlsFileIsUpToDate <- function() {
   if(!file.exists("ie_data.xls")) 
      return("ie_data.xls is not on the local disk.")
   
   wk <- loadWorkbook("ie_data.xls") # this is the local file
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
   
   if (localMessage1 != remoteMessage1)
      print(paste0("On the remote xls file: ", remoteMessage1, "whereas on the local file: ", localMessage1))
   if (localMessage2 != remoteMessage2)
      print(paste0("On the remote xls file: ", remoteMessage2, "whereas on the local file: ", localMessage2))
   if (localMessage3 != remoteMessage3)
      print(paste0("On the remote xls file: ", remoteMessage3, "whereas on the local file: ", localMessage3))
}


# Generating typical strategies
createTypicalStrategies <- function(extrapolateDividends=T, force=F) {
   message("Creating entries for the typical strategies")

   time0 <- proc.time()
   createCAPEstrategy(years=10, cheat=2, avgOver=30, medianAlloc=90, 
                      interQuartileAlloc=98, futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)
   print( c( "CAPE time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )

   time0 <- proc.time()
   createDetrendedStrategy(inputDF="dat", inputName="TR", avgOver=30, 
                           medianAlloc=90, interQuartileAlloc=97, 
                           futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)
   print( c( "detrended time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   time0 <- proc.time()
   createBollStrategy("dat", "TR", avgOver=21,  medianAlloc=95, interQuartileAlloc=20, 
                      futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)   
   print( c( "Bollinger time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   time0 <- proc.time()
   createSMAstrategy("dat", "TR", SMA1=12, SMA2=1, 
                     medianAlloc=90, interQuartileAlloc=50, 
                     futureYears=def$futureYears, tradingCost=def$tradingCost, force=force)   
   print( c( "SMA time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   time0 <- proc.time()
   createMomentumStrategy("dat", "TR", 12, medianAlloc=95, interQuartileAlloc=15, 
                          futureYears=def$futureYears, tradingCost=def$tradingCost, force=force) 
   print( c( "momentum time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   time0 <- proc.time()
   createMultiStrategy(inputStrategyName1=def$typicalCAPE, inputStrategyName2=def$typicalDetrended, "", "",
                       70, 30, 0, 0, medianAlloc=85, interQuartileAlloc=98,
                       strategyName="value70_30_85_98", subtype="value", 
                       tradingCost=def$tradingCost, force=force)
   print( c( "value time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
   
   time0 <- proc.time()
   createMultiStrategy(def$typicalSMA, def$typicalBoll, def$typicalMomentum, "", 50, 25, 25, 0, 
                       medianAlloc=95, interQuartileAlloc=60,
                       strategyName="technical50_25_25_95_60", subtype="technical", 
                       tradingCost=def$tradingCost, force=force)
   print( c( "technical time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )

   time0 <- proc.time()
   createMultiStrategy(def$typicalValue, def$typicalTechnical, "", "",
                       75, 25, 0, 0, medianAlloc=98, interQuartileAlloc=70,
                       strategyName="balanced75_25_98_70", subtype="balanced", 
                       tradingCost=def$tradingCost, force=force)
   print( c( "balanced time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )   
}

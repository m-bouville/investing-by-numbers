


# Loading and preparing data
start <- function(thorough=F, force=F) {
   
   if(!file.exists("utils.r")) stop("Use \'setwd()\' to change the working directory to that containing the data.")
   
   totTime <- proc.time()
   
   defFutureYears <<- 30L    # default value for the number of years over which future returns are calculated
   defTradingCost <<- 4/100 # default value for the trading costs
   
   if (!exists("dat") | force) { # if data frame does not exist (or if we want to force the loading), we load the xls file
      message("Starting to load the data from the xls file.")
      message("Then we will also load a list of drawdowns and gold prices.")
      message("After that, we will create the basic data structures.")
      message()
      if (!thorough) message("If you want some strategies to be created and their statistics to be calculated, run \'start(thorough=T)\'.")
      loadData(2, thorough=thorough)
   } 
   
   if (!exists("strategy") | force) 
      strategy <<- data.frame(date = dat$date, numericDate = dat$numericDate, stocksTR = dat$totalReturn)
#   calcStocksFutureReturn(defFutureYears)
   calcCAPE(10, cheat=2)
   lapply(c(1, 12), calcSMA)
   
   if (!exists("DD") | force) 
      loadDDlist(force=force) # loading the dates of major drawdowns
   
   if (!exists("stats") | force) 
      stats <<- data.frame(strategy = character(), 
                           type = character(), # type: constant allocation, CAPE, SMA, mixed, etc.
                           TR = numeric(),  # average real total return (exponential regression)
                           volatility = numeric(), 
                           avgStockAlloc = numeric(), # average allocation to stocks
                           latestStockAlloc = numeric(), # allocation to stocks as of the last date of the data
                           turnover = numeric(), # turnover of the portfolio (in years)
                           DD2 = numeric(), # sum of the squares of the drawdowns
                           score = numeric(), 
                           stringsAsFactors=F)
   
   if (!exists("parameters") | force) 
      parameters <<- data.frame(strategy = character(), 
                                type = character(), # type: constant allocation, CAPE, SMA, mixed, etc.
                                subtype = character(), # especially for Bollinger and mixed
                                allocLow = numeric(),
                                allocHigh = numeric(),
                                offset = numeric(),
                                inputStrategyName1 = character(), # for multi strategies: name of strategy used as input
                                inputStrategyName2 = character(),
                                inputStrategyName3 = character(),
                                inputStrategyName4 = character(),
                                fraction1 = numeric(), # for multi strategies: fraction (between 0 and 100)
                                fraction2 = numeric(), 
                                fraction3 = numeric(),
                                fraction4 = numeric(), 
                                name1 = character(), # other parameters used to create the strategy
                                value1 = numeric(), # values of these parameters
                                name2 = character(), 
                                value2 = numeric(), 
                                name3 = character(), 
                                value3 = numeric(), 
                                name4 = character(), 
                                value4 = numeric(), 
                                stringsAsFactors=F)
   
   if (!"gold" %in% colnames(dat) | !"gold" %in% stats$strategy | force) {
      loadGoldData()
      createGoldStrategy( futureYears=defFutureYears, tradingCost=defTradingCost, force=force)
      message("Real gold prices were obtained from a local csv calculation.")
   }   
   
   if (thorough) {
      message("Creating constant-allocation stock-bond strategies.") 
      step <- -10
   }
   else step <- -100 # 0 to 100 by steps of 100 means we do only 0 and 100, i.e. bonds and stocks
   
   invisible ( 
      lapply(seq(100, 0, by=step), 
             function(alloc) createConstAllocStrategy(
                alloc, futureYears=defFutureYears, tradingCost=defTradingCost, force=force) )
   )     
   createTypicalStrategies()
   print(proc.time() - totTime)
}


# Loading gold data from local csv file
loadGoldData <- function() {
   if (!"gold" %in% colnames(dat) ) 
      addNumColToDat("gold")
   dat$gold <<- NA
   nominalGold <- read.csv("gold.csv", header=T)[, 1]
   
   index1968 <- (1968-1871)*12+1
   lastGoldIndex <- length(nominalGold)
   lastDatIndex <- numData
   
   if ( lastGoldIndex + index1968 - 1 > lastDatIndex )  # if there are too many months of data for gold
      lastGoldIndex <- lastDatIndex - index1968 + 1
   else if (lastGoldIndex + index1968 - 1 < lastDatIndex)  # if there are too few months of data for gold
      lastDatIndex <- lastGoldIndex + index1968 - 1
   
   dat$gold[ index1968:lastDatIndex ] <<- nominalGold[1:lastGoldIndex]
   dat$gold <<- dat$gold  / dat$CPI * refCPI # calculating real gold prices
}

checkXlsFileIsUpToDate <- function() {
   if(!file.exists("ie_data.xls")) 
      return("ie_data.xls is not on the local disk.")
   
   wk <- loadWorkbook("ie_data.xls") # this is the local file
   rawDat <- readWorksheet(wk, sheet="Data", startRow=8)
   
   lastLine <- dim(rawDat)[1] 
   localMessage1 <- as.character(rawDat$P[lastLine])
   localMessage2 <- as.character(rawDat$CPI[lastLine])
   localMessage3 <- as.character(rawDat$Rate.GS10[lastLine])
   
   download.file("http://www.econ.yale.edu/~shiller/data/ie_data.xls", "ie_data-remote.xls", mode = "wb")
   library(XLConnect) # to handle xls file
   wk <- loadWorkbook("ie_data-remote.xls")  # this is the local file
   rawDat <- readWorksheet(wk, sheet="Data", startRow=8)
   file.remove("ie_data-remote.xls") 
   
   lastLine <- dim(rawDat)[1] 
   remoteMessage1 <- as.character(rawDat$P[lastLine])
   remoteMessage2 <- as.character(rawDat$CPI[lastLine])
   remoteMessage3 <- as.character(rawDat$Rate.GS10[lastLine])
   
   if (localMessage1 != remoteMessage1)
      print(paste0("On the remote xls file: ", remoteMessage1, "whereas on the local file: ", localMessage1))
   if (localMessage2 != remoteMessage2)
      print(paste0("On the remote xls file: ", remoteMessage2, "whereas on the local file: ", localMessage2))
   if (localMessage3 != remoteMessage3)
      print(paste0("On the remote xls file: ", remoteMessage3, "whereas on the local file: ", localMessage3))
}

# Loading data from xls file
loadData <- function(rowsRemoved = 0L, thorough=F) {  # the xls file has *nominal* values, the "dat" data frame has *real* values
   library(XLConnect) # to handle xls file
   if(!file.exists("ie_data.xls")) # download file if not already locally available
      download.file("http://www.econ.yale.edu/~shiller/data/ie_data.xls", "ie_data.xls", mode = "wb")
   else if(thorough)
      checkXlsFileIsUpToDate()
   
   wk <- loadWorkbook("ie_data.xls") 
   rawDat <- readWorksheet(wk, sheet="Data", startRow=8)
   
   numData <<- dim(rawDat)[1]-1 # number of rows (minus last row, which contains info, not data)
   message(paste0("According to the xls file, \'", rawDat$P[numData+1], "\'")) # displaying information given in xls file
   message(paste0("According to the xls file, \'", rawDat$CPI[numData+1], "\'"))
   message(paste0("According to the xls file, \'", rawDat$Rate.GS10[numData+1], "\'"))
   numData <<- numData - rowsRemoved # last few rows, full of NA are removed
   rawDat <- rawDat[1:numData, ] # removing last row(s)
   
   # data are for the last (business) day of each month, 28th is close enough
   dat <<- data.frame(date       = ISOdate( floor(rawDat$Date), round((rawDat$Date%%1)*100,0), 28 ),
                      numericDate= as.numeric(rawDat$Fraction),
                      CPI        = as.numeric(rawDat$CPI), # reference for inflation
                      dividend   = as.numeric(rawDat$D), # loads nominal dividend (real dividend will be calculated below)
                      price      = as.numeric(rawDat$P), # loads nominal S&P price (real price will be calculated below)
                      earnings   = as.numeric(rawDat$E), # loads nominal earnings (real earnings will be calculated below)
                      totalReturn= numeric(numData),
                      bonds      = numeric(numData)
   )
   
   refCPI       <<- dat$CPI[numData] # reference for inflation
   dat$price    <<- dat$price    / dat$CPI * refCPI # calculating real price
   dat$dividend <<- dat$dividend / dat$CPI * refCPI # calculating real dividend
   dat$earnings <<- dat$earnings / dat$CPI * refCPI # calculating real earnings
   
   dat$totalReturn[1] <<- 1
   for(i in 2:numData)
      dat$totalReturn[i] <<- dat$totalReturn[i-1] * dat$price[i]/dat$price[i-1] * (1 + dat$dividend[i]/dat$price[i]/12)
   
   dat$bonds <<- read.csv("bonds.csv", header=T)[1:numData, 1]
   message("Real bond prices were imported from an Excel calculation.")
   
   message("Shiller's xls file has *nominal* values, the \'dat\' data frame has *real* values.")
}

# Generating typical strategies
createTypicalStrategies <- function(force=F) {
   message("Creating entries for the typical strategies")
   calcCAPE(years=defCAPEyears, cheat=defCAPEcheat)
   calcAvgCAPE(CAPEname="CAPE10", avgOver=defCAPEavgOver)
   createCAPEstrategy(CAPEname="CAPE10avg24", offset=defInitialOffset, 
                      CAPElow=defCAPElow, CAPEhigh=defCAPEhigh, 
                      allocLow=defCAPEallocLow, allocHigh=defCAPEallocHigh, 
                      futureYears=defFutureYears, force=force)
   
   createBollTRstrategy(avgOver=21, factorLow=0.6, factorHigh=-0.5, futureYears=defFutureYears, force=force)
   
   createSMAstrategy(SMA1=12, SMA2=1, offset="mean", ratioLow=5, ratioHigh=5.5, allocLow=1, allocHigh=0, 
                     futureYears=defFutureYears, force=force)
   
   createMomentumStrategy(12, offset="mean", momentumLow=15, momentumHigh=25, allocLow=1, allocHigh=0, 
                          futureYears=defFutureYears, force=force) 
   
   createMultiStrategy("SMA12_1_5_5.5", "BollTR21_0.6_-0.5", "momentum12_15_25", "", 60, 20, 20, 0, 
                       strategyName="technical60_20_20", 12, delta="", subtype="technical", force=force)
   calcSMAofStrategy("technical60_20_20", 4, strategyName="technical_SMA4", force=force)
   createMultiStrategy("technical60_20_20", "technical_SMA4", "CAPE10avg24_14.6_16.7", "CAPE10avg24_14.6_16.7Unbound", 
                       40, 25, 10, 25, strategyName="balanced40_25_10_25", defInitialOffset, delta="", subtype="balanced", force=force)
}

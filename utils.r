addNumColToDat <- function(colName) {
   if (!colName %in% colnames(dat)) dat[, colName] <<- numeric(numData)
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


## calculating future annualized return of strategies
calcStrategyFutureReturn <- function(stratName, futureYears = numeric(), force=F) {
   futureReturnName <- paste0(stratName, "Future", futureYears)
   TRname <- paste0(stratName, "TR")
   if (!futureReturnName %in% colnames(strategy) | force) {
      strategy[, futureReturnName] <<- numeric(numData)
      months <- 12*futureYears
      for(i in 1:(numData-months)) 
         strategy[i, futureReturnName] <<- (strategy[i+months, TRname] / strategy[i, TRname]) ^ (1/futureYears) - 1
      for(i in (numData-months+1):numData) 
         strategy[i, futureReturnName] <<- NA
   }
}

## calculating future annualized return of stocks
calcStocksFutureReturn <- function(years = numeric()) {
   futureReturnName <- paste0("future", years)
   addNumColToDat(futureReturnName)
   months <- 12*years
   for(i in 1:(numData-months))
      dat[i, futureReturnName] <<- (dat$totalReturn[i+months] / dat$totalReturn[i]) ^ (1/years) - 1
   for(i in (numData-months+1):numData) { dat[i, futureReturnName] <<- NA }
}


## Calculating real returns of constant allocation
calcTRconstAlloc <- function(stockAllocation = 70L, strategyName="", force=F) { # parameter is stock allocation in %
   if(stockAllocation<0 | stockAllocation>100) stop("Stock allocation must be between 0 and 100 (percents).")
   #   if(stockAllocation != floor(stockAllocation)) stop("Stock allocation must be an integer.")
   
   if (strategyName == "") {
      if (stockAllocation == 100) { strategyName <- "stocks" }
      else if (stockAllocation == 0) { strategyName <- "bonds" }
      else { strategyName <- paste0("constantAlloc", stockAllocation, "_", 100-stockAllocation) }
   } 
   
   TRconstAllocName <- paste0(strategyName, "TR")
   if (!TRconstAllocName %in% colnames(strategy) | force) {
      if (!TRconstAllocName %in% colnames(strategy)) strategy[, TRconstAllocName] <<- numeric(numData)
      
      stockAllocation <- stockAllocation / 100
      
      strategy[1, TRconstAllocName] <<- 1
      for(i in 2:numData) 
         strategy[i, TRconstAllocName] <<- strategy[i-1, TRconstAllocName] * ( 
            stockAllocation * dat$totalReturn[i] / dat$totalReturn[i-1] + 
               (1-stockAllocation) * dat$bonds[i] / dat$bonds[i-1]  )
   }
}

   
createGoldStrategyEntry <- function(strategyName="", futureYears=defFutureYears, tradingCost=defTradingCost, force=F) {
   if (strategyName == "") strategyName <- "gold" 
   TRgoldName <- paste0(strategyName, "TR")
   if (!TRgoldName %in% colnames(strategy)) strategy[, TRgoldName] <<- numeric(numData)
   
   index1968 <- (1968-1871)*12+1
   strategy[, TRgoldName] <<- NA
   strategy[index1968, TRgoldName] <<- 1
   for(i in (index1968+1):numData) 
      strategy[i, TRgoldName] <<- strategy[i-1, TRgoldName] * dat$gold[i] / dat$gold[i-1] 
   
#    if ( !(strategyName %in% parameters$strategy) ) {
#       parameters$type[index] <<- "gold"
#    }
   
   if ( !(strategyName %in% stats$strategy) ) {
      index <- nrow(stats)+1 # row where the info will be added
      stats[index, ] <<- NA
      stats$strategy[index] <<- strategyName
      stats$avgStockAlloc[index] <<- NA
      stats$latestStockAlloc[index] <<- NA
      stats$turnover[index] <<- Inf
   }
   calcStatisticsForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
}


createConstAllocStrategyEntry <- function(stockAllocation = 70L, strategyName="", futureYears=defFutureYears, tradingCost=defTradingCost, force=F) { # parameter is stock allocation in %

   if(stockAllocation<0 | stockAllocation>100) stop("Stock allocation must be between 0 and 100 (percents).")
   #   if(stockAllocation != floor(stockAllocation)) stop("Stock allocation must be an integer.")
   
   if (strategyName == "") {
      if (stockAllocation == 100) { strategyName <- "stocks" }
      else if (stockAllocation == 0) { strategyName <- "bonds" }
      else { strategyName <- paste0("constantAlloc", stockAllocation, "_", 100-stockAllocation) }
   } 

   TRconstAllocName <- calcTRconstAlloc(stockAllocation=stockAllocation, strategyName=strategyName, force=force) 
   
#    if ( !(strategyName %in% parameters$strategy) ) {
#       parameters$type[index] <<- "constantAlloc"
#    }
   
   if ( !(strategyName %in% stats$strategy) ) {
      index <- nrow(stats)+1 # row where the info will be added
      stats[index, ] <<- NA
      stats$strategy[index] <<- strategyName
      stats$avgStockAlloc[index] <<- stockAllocation/100
      stats$latestStockAlloc[index] <<- stockAllocation/100
      stats$turnover[index] <<- Inf
   }
   
   calcStatisticsForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
}
   
calcStrategyReturn <- function(allocName, TRname, numNA) {
   for(i in 1:numNA) strategy[i, TRname] <<- NA
   strategy[numNA+1, TRname] <<- 1
   for(i in (numNA+2):numData) 
      strategy[i, TRname] <<- strategy[i-1, TRname] * ( 
         strategy[i-1, allocName] * dat$totalReturn[i] / dat$totalReturn[i-1] + 
            (1-strategy[i-1, allocName]) * dat$bonds[i] / dat$bonds[i-1] )
}

## Calculating average alloc between 2 strategies, and corresponding results
calcMultiStrategyReturn <- function(name1, name2, name3, name4, 
                                    fraction1=.25, fraction2=.25, fraction3=.25, fraction4="", 
                                    outputName="multi", numNA, delta="", force=T) {

   allocName1 <- paste0(name1, "Alloc")
   allocName2 <- paste0(name2, "Alloc")
   allocName3 <- paste0(name3, "Alloc")
   allocName4 <- paste0(name4, "Alloc")

   UBallocName<- paste0(outputName, "UnboundAlloc")
   allocName  <- paste0(outputName, "Alloc")
   TRname     <- paste0(outputName, "TR")
   
   if (!is.numeric(fraction1)) fraction1 <- 1 - fraction2 - fraction3 - fraction4
   if (!is.numeric(fraction2)) fraction1 <- 1 - fraction1 - fraction3 - fraction4
   if (!is.numeric(fraction3)) fraction1 <- 1 - fraction1 - fraction2 - fraction4
   if (!is.numeric(fraction4)) fraction1 <- 1 - fraction1 - fraction2 - fraction3
   sumCoeff <- fraction1 + fraction2 + fraction3 + fraction4
   if (abs(sumCoeff-1)>1e-6) stop(paste("Sum of coefficients must be 1, not", sumCoeff))
   
   if (!(allocName1 %in% colnames(strategy))) stop(paste0("strategy$", allocName1, " does not exist."))
   if (!(allocName2 %in% colnames(strategy))) stop(paste0("strategy$", allocName2, " does not exist."))
   if (!(allocName3 %in% colnames(strategy))) stop(paste0("strategy$", allocName3, " does not exist."))
   if (!(allocName4 %in% colnames(strategy)) & fraction4 != 0) stop(paste0("strategy$", allocName4, " does not exist."))
   
   if (!(TRname %in% colnames(strategy)) | force) { # if data do not exist yet or we force recalculation:   
      if (!(allocName %in% colnames(strategy))) strategy[, allocName] <<- numeric(numData)
      if (!(UBallocName %in% colnames(strategy))) strategy[, UBallocName] <<- numeric(numData)
      if (!(TRname %in% colnames(strategy))) {strategy[, TRname] <<- numeric(numData)}

      if(fraction4==0)
         strategy[, UBallocName] <<- fraction1*strategy[, allocName1] + fraction2*strategy[, allocName2] + fraction3*strategy[, allocName3]
         else strategy[, UBallocName] <<- fraction1*strategy[, allocName1] + fraction2*strategy[, allocName2] + fraction3*strategy[, allocName3] + fraction4*strategy[, allocName4] 
      for(i in 1:numData) strategy[i, allocName] <<- max(min(strategy[i, UBallocName], 1), 0)
      
      if(is.numeric(delta)) 
         for(i in (numNA+2):numData) 
            if (abs(strategy[i, allocName] - strategy[i-1, allocName]) < delta) 
               strategy[i, allocName] <<- strategy[i-1, allocName] 
      
      calcStrategyReturn(allocName, TRname, numNA)
   }
}


calcSMAofStrategy <- function(name, avgOver=3L, outputName="", force=F) {
   if (outputName=="") outputName <- paste0(name, "_SMA", avgOver)
   inputAllocName <- paste0(name, "Alloc")
   outputAllocName <- paste0(outputName, "Alloc")
   outputTRname <- paste0(outputName, "TR")
   if (!(inputAllocName %in% colnames(strategy)))  stop(paste0("strategy$", inputAllocName, " does not exist."))
   
   if (!(outputAllocName %in% colnames(strategy)) | !(outputTRname %in% colnames(strategy)) | force) { # if data do not exist yet or we force recalculation:   
      if (!(outputAllocName %in% colnames(strategy))) strategy[, outputAllocName] <<- numeric(numData)
      if (!(outputTRname %in% colnames(strategy))) strategy[, outputTRname] <<- numeric(numData)
      
      strategy[1:(avgOver-1), outputAllocName] <<- NA
      for (i in avgOver:numData) 
         strategy[i, outputAllocName] <<- mean(strategy[(i-avgOver+1):i, inputAllocName], na.rm=F)
      calcStrategyReturn(outputAllocName, outputTRname, sum(is.na(strategy[, outputAllocName])))
   }
}

regression <- function(x, y) { # y = a + b x
   b <- cov(x,y) / var(x)
   a <- mean(y) - b * mean(x)
   return( c(a, b) )
}

# prepareStrategy <- function(strategyName, futureYears=defFutureYears, force=F) {
#    if (!(paste0(strategyName, "Future", futureYears) %in% colnames(strategy)) | force) 
#       calcStrategyFutureReturn(strategyName, futureYears, force=force)
#    if (!(strategyName %in% colnames(DD)) | force)
#       CalcAllDrawdowns(strategyName, force=force)
# }

calcAllForStrategy <- function(strategyName, futureYears=defFutureYears, tradingCost=defTradingCost, force=F) {

   totTime <- proc.time()
   
   allocName <- paste0(strategyName, "Alloc")
   TRname <- paste0(strategyName, "TR")
   futureReturnName <- paste0(strategyName, "Future", futureYears)
   
   if (!(TRname %in% colnames(strategy) ) |
          !(strategyName %in% stats$strategy) | 
          !(futureReturnName %in% colnames(strategy) ) |
          !(strategyName %in% colnames(DD)) | 
          force) { # if data do not exist yet or we force recalculation:   
      
      if ( !(TRname %in% colnames(strategy)) ) stop(paste0("strategy$", TRname, " n'existe pas."))
      
      if ( !(futureReturnName %in% colnames(strategy)) | force)
         calcStrategyFutureReturn(strategyName, futureYears=futureYears, force=force)
      
#       DDtime <- proc.time()
#       if ( !(strategyName %in% colnames(DD)) | force)
#          CalcAllDrawdowns(strategyName, force=force)      
#       print( "DD time:" )
#       print( proc.time() - DDtime )
## calcStatisticsForStrategy calculates DD

      if ( !(strategyName %in% stats$strategy) | force ) 
         calcStatisticsForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   }
   print("overall time:")
   print(proc.time() - totTime)
   
}


calcStatisticsForStrategy <- function(strategyName, futureYears=defFutureYears, tradingCost=defTradingCost, force=F) {
   
   if ( !(strategyName %in% stats$strategy) ) {
      stats[nrow(stats)+1, ] <<- NA
      stats$strategy[nrow(stats)] <<- strategyName
#      warning("The entry for ", strategyName, " in \'stats\' was not created at the time of allocation generation, and will thus be incomplete.")
   }
   index <- which(stats$strategy == strategyName)
   if(length(index) > 1) 
      stop("There are ", length(index), " entries for ", strategyName, " in stats$strategy.")
#   print( c( index, stats$strategy[index], stats$score[index], is.na(stats$score[index]), force ) )
   
   if ( is.na(stats$score[index]) | force) { 
      # if data do not exist (we use 'score' to test this as it requires a lot of other data) yet or we force recalculation:   
      
      if (!(paste0(strategyName, "Future", futureYears) %in% colnames(strategy)) | force) 
         calcStrategyFutureReturn(strategyName, futureYears, force=force)
      if (!(strategyName %in% colnames(DD)) | force)
         CalcAllDrawdowns(strategyName, force=force)

      allocName <- paste0(strategyName, "Alloc")
      TRname <- paste0(strategyName, "TR")
      medianName <- paste0("median", futureYears)
      fiveName <- paste0("five", futureYears)
      dateRange <- (defInitialOffset+2):numData
      
      fit <- numeric(numData)
      fitPara <- regression(strategy$numericDate[dateRange], log(strategy[dateRange, TRname]))
      a <- fitPara[[1]]
      b <- fitPara[[2]]
      fit[dateRange] <- log(strategy[dateRange, TRname]) - (a + b * strategy$numericDate[dateRange])
      fit2 <- numeric(numData)
      fit2[dateRange] <- fit[dateRange] - fit[dateRange-12]
      
      if ( (allocName %in% colnames(strategy)) ) {# this means we are not dealing with constant allocation (e.g. stocks)
         turnover <- numeric(numData)
         for(i in dateRange) 
            turnover[i] <- abs(strategy[i, allocName] - strategy[i-1, allocName])
         stats$turnover[index] <<- 1/12/mean(turnover[dateRange], na.rm=F)
         #       TOcost <- tradingCost / TO
         #       invTO <- 1/TO     
         stats$avgStockAlloc[index]    <<- mean(strategy[dateRange, allocName], na.rm=T)
         stats$latestStockAlloc[index] <<- strategy[numData, allocName]     
      }
      
      stats$TR[index]         <<- exp(b)-1 # -TOcost
      stats$volatility[index] <<- sd(fit2[dateRange], na.rm=T)
      stats[index, medianName]<<- median(strategy[, paste0(strategyName, "Future", futureYears)], na.rm=T) # -TOcost
      stats[index, fiveName]  <<- quantile(strategy[, paste0(strategyName, "Future", futureYears)], .05, na.rm=T) # -TOcost
      stats$DD2[index]        <<- sum(DD[, strategyName]^2)
      stats$score[index]      <<- 100*stats$TR[index] - 100*stats$volatility[index]/5 + 100*stats[index, medianName] + 
         100*stats[index, fiveName] - stats$DD2[index] - 1/stats$turnover[index] # + 1.5*tradingCost*100
   }
}

showSummaryForStrategy <- function(strategyName, displayName="", futureYears=defFutureYears, tradingCost=defTradingCost, 
                                refReturn=0, refMedian=0, refFive=0, cutoffScore="", force=F) {
   
   if ( !(strategyName %in% stats$strategy) | force) 
      calcStatisticsForStrategy(strategyName, years=years, tradingCost=tradingCost, force=force) 
   index <- which(stats$strategy == strategyName)
   medianName <- paste0("median", futureYears)
   fiveName <- paste0("five", futureYears)
   if(displayName=="") 
      displayName <- strategyName
   
   TO <- stats$turnover[index] 
   TOcost <- tradingCost / TO
   
   avgAlloc <- 100*stats$avgStockAlloc[index]
   latestAlloc <- 100*stats$latestStockAlloc[index]    

   ret  <- 100*(stats$TR[index] - TOcost) - refReturn
   vol  <- 100*stats$volatility[index]
   med  <- 100*(stats[index, medianName] - TOcost) - refMedian
   five <- 100*(stats[index, fiveName] - TOcost) - refFive
   DD2  <- stats$DD2[index]
   score<- stats$score[index] - 3*100*TOcost + 1.5*tradingCost*100
                             
   if (round(ret,1)%%1 == 0) retPad = "  "
      else retPad = ""
   if (round(vol,1)%%1 == 0) volPad = "  "
      else volPad = ""
   if (round(med,1)%%1 == 0) medPad = "  "
      else medPad = ""
   if (round(five,1)%%1 == 0) fivePad = "  "
      else fivePad = ""
   if ( is.na(latestAlloc) ) latestAllocPad = " "
      else if (latestAlloc < 1e-6) latestAllocPad = "  "
      else if (latestAlloc == 100) latestAllocPad = ""
      else latestAllocPad = " "
   if ((10*round(DD2,2))%%1 == 0) DD2Pad = " "
      else DD2Pad = ""
     
   if(!is.numeric(cutoffScore) | score >= cutoffScore)
      print(paste0(displayName, ": TR: ", round(ret,1), retPad, "% ", 
                   "(", futureYears, " yrs, med: ", round(med,1), medPad, "%, 5%: ", round(five,1), fivePad, "%), ",
                   "vol.: ", round(vol,1), volPad, "%, ",
                   "avg. stock: ", round(avgAlloc), "% ",
                   "(now: ", round(latestAlloc), latestAllocPad, "%), ",
                   "turnover: ", round(TO, 1), " yrs, ",
                   "DD^2: ", round(DD2,2), ", ", DD2Pad,
                   "score: ", round(score,1) ) )
}

showSummaries <- function(futureYears=defFutureYears, tradingCost=defTradingCost, detailed=T, force=F) {
   # force pertains only to showSummaryForStrategy, not to calc...StrategyReturn (these are all set to F)

   print(paste0("* Statistics of the strategies (trading costs = ", round(100*tradingCost,2), "% per year of turnover):"))
   showSummaryForStrategy("stocks", displayName="stocks   ", futureYears=futureYears, tradingCost=0, force=force)
   #       calcTRconstAlloc(70)
   #       strategy$alloc70_30TR <<- dat$TR70stock
   showSummaryForStrategy("alloc70_30", displayName="70 - 30  ", futureYears=futureYears, tradingCost=0, force=F)
   
   if(detailed) {
# calcTRconstAlloc(0)
# strategy$bondsTR <<- dat$TR0stock
# showSummaryForStrategy("bonds", displayName="bonds    ", futureYears=futureYears, tradingCost=0, force=force)
#showSummaryConstAlloc("bonds", displayName="bonds    ")
   }  
   calcAvgCAPE(CAPEname="CAPE10", avgOver=24)
     showSummaryForStrategy("CAPE10avg24", displayName="CAPE10   ", futureYears=futureYears, tradingCost=tradingCost, force=force)

   calcCAPEstrategyReturn(CAPEname="CAPE10avg24", offset=defInitialOffset, CAPElow=14.6, CAPEhigh=16.7, allocLow=1, allocHigh=0, force = F)
   calcBollTRstrategyReturn(avgOver=21, factorLow=0.6, factorHigh=-0.5, "BollTR21_0.6_-0.5", force = F)
   calcSMAstrategyReturn(SMA1=12, SMA2=1, offset="mean", ratioLow=.05, ratioHigh=.055, allocLow=1, allocHigh=0, force = F)
   calcMomentumStrategyReturn(12, offset="mean", momentumLow=.15, momentumHigh=.25, allocLow=1, allocHigh=0, 
                              outputName="momentum12_15_25", force = F) 
   if(detailed) {
      showSummaryForStrategy("BollTR21_0.6_-0.5", displayName="Bollinger", futureYears=futureYears, tradingCost=tradingCost, force=force)
      showSummaryForStrategy("SMA12_1", displayName="SMA 12-1 ", futureYears=futureYears, tradingCost=tradingCost, force=force)   
      showSummaryForStrategy("momentum12_15_25", displayName="momentum ", futureYears=futureYears, tradingCost=tradingCost, force=force)
   }
   
     calcMultiStrategyReturn("SMA12_1", "BollTR21_0.6_-0.5", "momentum12_15_25", "", 0.6, 0.2, 0.2, 0, 
                           outputName="hiFreq60_20_20", 12, delta="", force=F)
     showSummaryForStrategy("hiFreq60_20_20", displayName="hi freq. ", futureYears=futureYears, tradingCost=tradingCost, force=force)

   calcSMAofStrategy("hiFreq60_20_20", 4, outputName="hiFreq_SMA4", force=F)
   calcMultiStrategyReturn("hiFreq60_20_20", "hiFreq_SMA4", "CAPE10avg24", "CAPE10avg24Unbound", .4, .25, .1, .25,
                           outputName="balanced40_25_10_25", defInitialOffset, delta="", force=F)
     showSummaryForStrategy("balanced40_25_10_25", displayName="balanced ", futureYears=futureYears, tradingCost=tradingCost, force=force)
   
   print("")
}
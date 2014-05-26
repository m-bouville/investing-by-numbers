addNumColToDat <- function(colName) {
   if (!colName %in% colnames(dat)) dat[, colName] <<- numeric(numData)
}


futureDFelement <- function(strategyName, futureYears, index) {
  return ( eval(parse(text=paste0("future",futureYears,"[",i,",\"",strategyName,"\"]") ) ) )
}

calcNext20YrsReturn <- function(strategyName, force=F) {
   if (!strategyName %in% colnames(next20yrs) | force) {
      next20yrs[, strategyName] <<- numeric(numData)
      months <- 12*20
      exponent <- 1/20

      next20yrs[1:(numData-months), strategyName] <<- 
         (TR[1:(numData-months)+months, strategyName] / TR[1:(numData-months), strategyName]) ^ exponent - 1
      next20yrs[(numData-months+1):numData, strategyName] <<- NA
   }
   median20 <- median(next20yrs[, strategyName], na.rm=T)
   five20 <- quantile(next20yrs[, strategyName], .05, na.rm=T)
   return( c(median=median20, five=five20) )
}


calcNext30YrsReturn <- function(strategyName, force=F) {
   if (!strategyName %in% colnames(next30yrs) | force) {
      next30yrs[, strategyName] <<- numeric(numData)
      months <- 12*30
      exponent <- 1/30

      next30yrs[1:(numData-months), strategyName] <<- 
         (TR[1:(numData-months)+months, strategyName] / TR[1:(numData-months), strategyName]) ^ exponent - 1
      next30yrs[(numData-months+1):numData, strategyName] <<- NA
   }
   median30 <- median(next30yrs[, strategyName], na.rm=T)
   five30 <- quantile(next30yrs[, strategyName], .05, na.rm=T)
   return( c(median=median30, five=five30) )
}


## calculating future annualized return of strategies
calcStrategyFutureReturn <- function(strategyName, futureYears = numeric(), force=F) {
#    if (futureYears==10)
#       median_five <- calcNext10YrsReturn(strategyName, force)
   if (futureYears==20)
      median_five <- calcNext20YrsReturn(strategyName, force)
   else if (futureYears==30)
      median_five <- calcNext30YrsReturn(strategyName, force)
   return(median_five)
}

   
createGoldStrategy <- function(strategyName="", futureYears=defFutureYears, tradingCost=defTradingCost, force=F) {
   if (strategyName == "") strategyName <- "gold" 
   if (!strategyName %in% colnames(TR)) TR[, strategyName] <<- numeric(numData)
   
   index1968 <- (1968-1871)*12+1
   TR[, strategyName] <<- NA
   TR[index1968, strategyName] <<- 1
   for(i in (index1968+1):numData) 
      TR[i, strategyName] <<- TR[i-1, strategyName] * dat$gold[i] / dat$gold[i-1] 
   
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
   stats$type[which(stats$strategy == strategyName)] <<- "gold"

#    calcStatisticsForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
## since gold data start in 1968 gold statistics cannot be relevantly compared with other assets or strategies
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
   
   if (!strategyName %in% colnames(TR) | force) {
      if (!strategyName %in% colnames(TR)) TR[, strategyName] <<- numeric(numData)
      
      stockAllocation <- stockAllocation / 100
      
      TR[1, strategyName] <<- 1
      for(i in 2:numData) 
         TR[i, strategyName] <<- TR[i-1, strategyName] * ( 
            stockAllocation * dat$totalReturn[i] / dat$totalReturn[i-1] + 
               (1-stockAllocation) * dat$bonds[i] / dat$bonds[i-1]  )
   }
}


createConstAllocStrategy <- function(stockAllocation = 70L, strategyName="", 
                                     futureYears=defFutureYears, tradingCost=defTradingCost, force=F) { # parameter is stock allocation in %

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
   stats$type[which(stats$strategy == strategyName)] <<- "constantAlloc"
}
   
calcStrategyReturn <- function(strategyName, numNA) {
   for(i in 1:numNA) TR[i, strategyName] <<- NA
   TR[numNA+1, strategyName] <<- 1
   for(i in (numNA+2):numData) 
      TR[i, strategyName] <<- TR[i-1, strategyName] * ( 
         alloc[i-1, strategyName] * dat$totalReturn[i] / dat$totalReturn[i-1] + 
            (1-alloc[i-1, strategyName]) * dat$bonds[i] / dat$bonds[i-1] )
}


calcSMAofStrategy <- function(inputStrategyName, avgOver=3L, strategyName="", force=F) {
   if (strategyName=="") strategyName <- paste0(name, "_SMA", avgOver)

   if (!(inputStrategyName %in% colnames(alloc)))  stop(paste0("alloc$", inputStrategyName, " does not exist."))
   
   if (!(strategyName %in% colnames(TR)) | !(strategyName %in% colnames(alloc)) | force) { # if data do not exist yet or we force recalculation:   
      if (!(strategyName %in% colnames(alloc))) alloc[, strategyName] <<- numeric(numData)
      if (!(strategyName %in% colnames(TR))) TR[, strategyName] <<- numeric(numData)
      
      alloc[1:(avgOver-1), strategyName] <<- NA
      for (i in avgOver:numData) 
         alloc[i, strategyName] <<- mean( alloc[(i-avgOver+1):i, inputStrategyName], na.rm=F )
      calcStrategyReturn(strategyName, sum(is.na( alloc[, strategyName])))
   }
   warning("Strategy ", strategyName, ", created by calcSMAofStrategy(), has no entry in either \'parameters\' or \'stats\'.")
}

regression <- function(x, y) { # y = a + b x
   b <- cov(x,y) / var(x)
   a <- mean(y) - b * mean(x)
   return( c(a, b) )
}


calcAllForStrategy <- function(strategyName, futureYears=defFutureYears, tradingCost=defTradingCost, force=F) {

#   totTime <- proc.time()
   
   
   if (!(strategyName %in% colnames(TR) ) |
          !(strategyName %in% stats$strategy) | 
          !(futureReturnName %in% colnames(strategy) ) |
          !(strategyName %in% colnames(DD)) | 
          force) { # if data do not exist yet or we force recalculation:   
      
      if ( !(strategyName %in% colnames(TR)) ) stop(paste0("strategy$", TRname, " n'existe pas."))
      
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
#    print("overall time:")
#    print(proc.time() - totTime)
   
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
      
      median_five <- calcStrategyFutureReturn(strategyName, futureYears, force=force)
      #print(median_five)
      
      if (!(strategyName %in% colnames(DD)) | force) {
         #          time0 <- proc.time()
         CalcAllDrawdowns(strategyName, force=force)
         #          print("DD:")
         #          print(proc.time()-time0)
      }

      medianName <- paste0("median", futureYears)
      fiveName <- paste0("five", futureYears)
      dateRange <- (defInitialOffset+2):numData
      
      fit <- numeric(numData)
      fitPara <- regression(TR$numericDate[dateRange], log(TR[dateRange, strategyName]))
      a <- fitPara[[1]]
      b <- fitPara[[2]]
      fit[dateRange] <- log(TR[dateRange, strategyName]) - (a + b * TR$numericDate[dateRange])
      fit2 <- numeric(numData)
      fit2[dateRange] <- fit[dateRange] - fit[dateRange-12]
      
      if ( (strategyName %in% colnames(alloc)) ) {# this means we are not dealing with constant allocation (e.g. stocks)
         turnover <- numeric(numData)
         for(i in dateRange) 
            turnover[i] <- abs(alloc[i, strategyName] - alloc[i-1, strategyName])
         stats$turnover[index] <<- 1/12/mean(turnover[dateRange], na.rm=F)
         #       TOcost <- tradingCost / TO
         #       invTO <- 1/TO     
         stats$avgStockAlloc[index]    <<- mean(alloc[dateRange, strategyName], na.rm=T)
         stats$latestStockAlloc[index] <<- alloc[numData, strategyName]     
      }
      
      stats$TR[index]         <<- exp(b)-1 # -TOcost
      stats$volatility[index] <<- sd(fit2[dateRange], na.rm=T)
      stats[index, medianName]<<- median_five[[1]] # -TOcost
      stats[index, fiveName]  <<- median_five[[2]] # -TOcost
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
   if(displayName=="") displayName <- strategyName
   
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
                              strategyName="momentum12_15_25", force = F) 
   if(detailed) {
      showSummaryForStrategy("BollTR21_0.6_-0.5", displayName="Bollinger", futureYears=futureYears, tradingCost=tradingCost, force=force)
      showSummaryForStrategy("SMA12_1", displayName="SMA 12-1 ", futureYears=futureYears, tradingCost=tradingCost, force=force)   
      showSummaryForStrategy("momentum12_15_25", displayName="momentum ", futureYears=futureYears, tradingCost=tradingCost, force=force)
   }
   
     calcMultiStrategyReturn("SMA12_1", "BollTR21_0.6_-0.5", "momentum12_15_25", "", 0.6, 0.2, 0.2, 0, 
                           strategyName="hiFreq60_20_20", 12, delta="", force=F)
     showSummaryForStrategy("hiFreq60_20_20", displayName="hi freq. ", futureYears=futureYears, tradingCost=tradingCost, force=force)

   calcSMAofStrategy("hiFreq60_20_20", 4, strategyName="hiFreq_SMA4", force=F)
   calcMultiStrategyReturn("hiFreq60_20_20", "hiFreq_SMA4", "CAPE10avg24", "CAPE10avg24Unbound", .4, .25, .1, .25,
                           strategyName="balanced40_25_10_25", defInitialOffset, delta="", force=F)
     showSummaryForStrategy("balanced40_25_10_25", displayName="balanced ", futureYears=futureYears, tradingCost=tradingCost, force=force)
   
   print("")
}
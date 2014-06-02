# Add numeric column to DF if column does not already exist
addNumColToDat <- function(colName) {
   if (!colName %in% colnames(dat)) dat[, colName] <<- numeric(numData)
}
addNumColToSignal <- function(colName) {
   if (!colName %in% colnames(signal)) signal[, colName] <<- numeric(numData)
}
addNumColToAlloc <- function(colName) {
   if (!colName %in% colnames(alloc)) alloc[, colName] <<- numeric(numData)
}
addNumColToTR <- function(colName) {
   if (!colName %in% colnames(TR)) TR[, colName] <<- numeric(numData)
}

# Stop if column does not exist in DF
requireColInDat <- function(colName) {
   if (!colName %in% colnames(dat)) stop(paste0("dat$", colName, " does not exist."))
}
requireColInSignal <- function(colName) {
   if (!colName %in% colnames(signal)) stop(paste0("signal$", colName, " does not exist."))
}
requireColInAlloc <- function(colName) {
   if (!colName %in% colnames(alloc)) stop(paste0("alloc$", colName, " does not exist."))
}
requireColInTR <- function(colName) {
   if (!colName %in% colnames(TR)) stop(paste0("TR$", colName, " does not exist."))
}


calcNext10YrsReturn <- function(strategyName, force=F) {
   if (!strategyName %in% colnames(next10yrs) | force) {
      next10yrs[, strategyName] <<- numeric(numData)
      months <- 12*10
      exponent <- 1/10
      
      next10yrs[1:(numData-months), strategyName] <<- 
         (TR[1:(numData-months)+months, strategyName] / TR[1:(numData-months), strategyName]) ^ exponent - 1
      next10yrs[(numData-months+1):numData, strategyName] <<- NA
   }
   median10 <- median(next10yrs[, strategyName], na.rm=T)
   five10 <- quantile(next10yrs[, strategyName], .05, na.rm=T)[[1]]
   return( c(median=median10, five=five10) )
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
   five20 <- quantile(next20yrs[, strategyName], .05, na.rm=T)[[1]]
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
   five30 <- quantile(next30yrs[, strategyName], .05, na.rm=T)[[1]]
   return( c(median=median30, five=five30) )
}

## calculating future annualized return of strategies
calcStrategyFutureReturn <- function(strategyName, futureYears = numeric(), force=F) {
#    if (futureYears==10)
#       median_five <- calcNext10YrsReturn(strategyName, force)
   if (futureYears==10)
      median_five <- calcNext10YrsReturn(strategyName, force)
   else if (futureYears==20)
      median_five <- calcNext20YrsReturn(strategyName, force)
   else if (futureYears==30)
      median_five <- calcNext30YrsReturn(strategyName, force)
   else stop("No data frame \'calcNext", futureYears, "YrsReturn\' exists.")
   return(median_five)
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
            stockAllocation * dat$TR[i] / dat$TR[i-1] + 
               (1-stockAllocation) * dat$bonds[i] / dat$bonds[i-1]  )
   }
   calcTRnetOfTradingCost(strategyName, futureYears=futureYears, force=force)       
}


createConstAllocStrategy <- function(stockAllocation = 70L, strategyName="", 
                                     futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) { # parameter is stock allocation in %

   if(stockAllocation<0 | stockAllocation>100) stop("Stock allocation must be between 0 and 100 (percents).")
   #   if(stockAllocation != floor(stockAllocation)) stop("Stock allocation must be an integer.")
   
   if (strategyName == "") {
      if (stockAllocation == 100) { strategyName <- "stocks" }
      else if (stockAllocation == 0) { strategyName <- "bonds" }
      else { strategyName <- paste0("constantAlloc", stockAllocation, "_", 100-stockAllocation) }
   } 

   TRconstAllocName <- calcTRconstAlloc(stockAllocation=stockAllocation, strategyName=strategyName, force=force) 
   
   if ( !(strategyName %in% stats$strategy) ) {
      index <- nrow(stats)+1 # row where the info will be added
      stats[index, ] <<- NA
      stats$strategy[index] <<- strategyName
      stats$type[index] <<- "constantAlloc"
      stats$avgStockAlloc[index] <<- stockAllocation/100
      stats$latestStockAlloc[index] <<- stockAllocation/100
      stats$turnover[index] <<- Inf
   }   
   calcStatisticsForStrategy(strategyName, futureYears=futureYears, force=force)
}


calcStrategyReturn <- function(strategyName, startIndex) {
   TR[1:(startIndex-1), strategyName] <<- NA
   TR[startIndex, strategyName] <<- 1
   for(i in (startIndex+1):numData) 
      TR[i, strategyName] <<- TR[i-1, strategyName] * ( 
         alloc[i-1, strategyName] * dat$monthlyDifference[i] + dat$bondsMonthly[i] ) # alloc * (stocks-bonds) + bonds = alloc * stocks + (1-alloc) * bonds
}


## Calculating allocation from signal
calcAllocFromSignal <- function(strategyName) {

   requireColInSignal(strategyName)
   addNumColToAlloc(strategyName)
   alloc[, strategyName] <<- atan( signal[, strategyName] ) /pi + .5 
}


calcSMAofStrategy <- function(inputStrategyName, avgOver=3L, futureYears=def$futureYears,
                              medianAlloc=def$medianAlloc, interQuartileAlloc=def$interQuartileAlloc, 
                              strategyName="", force=F) {
   if (strategyName=="") strategyName <- paste0(name, "_SMA", avgOver)

   if (!(inputStrategyName %in% colnames(alloc)))  stop(paste0("alloc$", inputStrategyName, " does not exist."))
   
   if (!(strategyName %in% colnames(TR)) | !(strategyName %in% colnames(alloc)) | force) { # if data do not exist yet or we force recalculation:   
      if (!(strategyName %in% colnames(alloc))) alloc[, strategyName] <<- numeric(numData)
      if (!(strategyName %in% colnames(TR))) TR[, strategyName] <<- numeric(numData)
      
      signal[1:(avgOver-1), strategyName] <<- NA
      for (i in avgOver:numData) 
         signal[i, strategyName] <<- mean( signal[(i-avgOver+1):i, inputStrategyName], na.rm=F )
      calcAllocFromSignal(strategyName, medianAlloc, interQuartileAlloc)
      startIndex <- sum(is.na(alloc[, strategyName]))+1
      calcStrategyReturn( strategyName, startIndex )
   }
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      inputIndex <- which(parameters$strategy == inputStrategyName)
      
      parameters$strategy[index] <<- strategyName
      parameters$type[index] <<- parameters$type[inputIndex]
      parameters$subtype[index] <<- parameters$subtype[inputIndex]
      parameters$startIndex[index] <<- startIndex
      
      parameters$inputStrategyName1[index] <<- inputStrategyName
      parameters$medianAlloc[index] <<-  medianAlloc
      parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
      parameters$name1[index] <<-  "SMA of strategy"
      parameters$name2[index] <<- "avgOver"
      parameters$value2[index] <<-  avgOver     
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
   stats$subtype[which(stats$strategy == strategyName)] <<- parameters$subtype[which(parameters$strategy == strategyName)]
   
#    warning("Strategy ", strategyName, ", created by calcSMAofStrategy(), has no entry in either \'parameters\' or \'stats\'.")
}

## Modern portfolio theory (Markowitz)
# MPT <- function(x, y) { 
#    a <- var(x)+2*cov(x,y) / (var(x)+var(y)-2*cov(x,y)
#    return( a )
# }

regression <- function(x, y) { # y = a + b x
   b <- cov(x,y) / var(x)
   a <- mean(y) - b * mean(x)
   return( c(a, b) )
}


calcTRnetOfTradingCost <- function(strategyName, futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   requireColInTR(strategyName)
   index <- which(stats$strategy == strategyName)
   
   cost <- tradingCost/stats$turnover[index]/12
    
   if ( !(strategyName %in% colnames(alloc)) ) {# this means we ARE dealing with constant allocation (e.g. stocks)
      if (tradingCost == 0.02)
         netTR2[, strategyName] <<- TR[, strategyName] # no trading, no trading cost
      else if(tradingCost == 0.04)
         netTR4[, strategyName] <<- TR[, strategyName] 
      else stop("No data frame \'netTR", round(tradingCost*100), "\' exists.")
   } else {
      if (tradingCost == 0.02) {
         if (!(strategyName %in% colnames(netTR2)) | force) {
            startIndex <- parameters$startIndex[which(parameters$strategy == strategyName)]
            netTR2[1 : (startIndex-1), strategyName] <<- NA
            netTR2[startIndex, strategyName] <<- 1
            for(i in (startIndex+1):numData) netTR2[i, strategyName] <<- netTR2[i-1, strategyName] * 
               ( TR[i, strategyName] / TR[i-1, strategyName] - cost )
         }
      } else if(tradingCost == 0.04) {
         if (!(strategyName %in% colnames(netTR4)) | force) {
            startIndex <- parameters$startIndex[which(parameters$strategy == strategyName)]
            netTR4[1 : (startIndex-1), strategyName] <<- NA
            netTR4[startIndex, strategyName] <<- 1
            for(i in (startIndex+1):numData) netTR4[i, strategyName] <<- netTR4[i-1, strategyName] * 
               ( TR[i, strategyName] / TR[i-1, strategyName] - cost )
         }
      } else stop("No data frame \'netTR", round(tradingCost*100), "\' exists.")
   }
}


# not used
calcTurnoverAndTRnetOfTradingCost <- function(strategyName, futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   #       time1 <- proc.time()      
   if ( (strategyName %in% colnames(alloc)) ) {# this means we are not dealing with constant allocation (e.g. stocks)
      dateRange <- def$startIndex:(numData-1)
      index <- which(stats$strategy == strategyName)

      turnover <- numeric(numData)
      turnover[1:def$startIndex] <- NA
      turnover[dateRange+1] <- abs(alloc[dateRange+1, strategyName] - alloc[dateRange, strategyName])
      stats$turnover[index] <<- 1/12/mean(turnover[dateRange+1], na.rm=F)
      
      if (tradingCost == 0.02) {
         netTR2[def$startIndex, strategyName] <<- 1
         for(i in dateRange+1)
            netTR2[i, strategyName] <<-  netTR2[i-1, strategyName] * 
            ( TR[i, strategyName] / TR[i-1, strategyName] - tradingCost*turnover[i] )
         stats$netTR2[index] <<- exp( regression(netTR2$numericDate[dateRange+1], log(netTR2[dateRange+1, strategyName]))[[2]] ) - 1
      }
      else if(tradingCost == 0.04)  {
         netTR4[def$startIndex, strategyName] <<- 1
         for(i in (def$startIndex+1) : numData )
            netTR4[i, strategyName] <<-  netTR4[i-1, strategyName] * 
            ( TR[i, strategyName] / TR[i-1, strategyName] - tradingCost*turnover[i] )
         stats$netTR4[index] <<- exp( regression(netTR4$numericDate[dateRange+1], log(netTR4[dateRange+1, strategyName]))[[2]] ) - 1    
      } else stop("No data frame \'netTR", round(tradingCost*100), "\' exists.")
      
   } else { # constant allocation: no trading, no cost
      if (tradingCost == 0.02) 
         netTR2[, strategyName] <<- TR[, strategyName]
      else if(tradingCost == 0.04) 
         netTR4[, strategyName] <<- TR[, strategyName]
      else stop("No data frame \'netTR", round(tradingCost*100), "\' exists.")
   }
   
   #       print( c( "Time for turnover:", round(summary(proc.time())[[3]] - time1[[3]] , 2) ) )   
}

calcStatisticsForStrategy <- function(strategyName, futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   
   dateRange <- def$startIndex:numData
   if ( !(strategyName %in% stats$strategy) ) {
      stats[nrow(stats)+1, ] <<- NA
      stats$strategy[nrow(stats)] <<- strategyName
   }
   index <- which(stats$strategy == strategyName)
   if(length(index) > 1) 
      stop("There are ", length(index), " entries for ", strategyName, " in stats$strategy.")
   
   if ( is.na(stats$score[index]) | force) {
      # if data do not exist (we use 'score' to test this as it requires a lot of other data) yet or we force recalculation:   
      
      median_five <- calcStrategyFutureReturn(strategyName, futureYears, force=force)
      medianName <- paste0("median", futureYears)
      fiveName <- paste0("five", futureYears)
      stats[index, medianName]<<- median_five[[1]]
      stats[index, fiveName]  <<- median_five[[2]]
      
      if (!(strategyName %in% colnames(DD)) | force) 
         CalcAllDrawdowns(strategyName, force=force)
      stats$DD2[index]        <<- sum(DD[, strategyName]^2)
      
      indexPara <- which(parameters$strategy == strategyName)     
      if ( length(indexPara) > 0 ) { # otherwise we are probably dealing with a constant allocation
         startIndex <- parameters$startIndex[indexPara]
         def$startIndex <<- max(def$startIndex, startIndex) # update def$startIndex if need be
         def$startYear  <<- max(def$startYear, (startIndex-1)/12+1871 )
      }
      dateRange <- def$startIndex:numData
      
      #       time1 <- proc.time()      
      fit <- numeric(numData)
      fitPara <- regression(TR$numericDate[dateRange], log(TR[dateRange, strategyName]))
      a <- fitPara[[1]]
      b <- fitPara[[2]]
      fit[dateRange] <- log(TR[dateRange, strategyName]) - (a + b * TR$numericDate[dateRange])
      fit2 <- numeric(numData)
      fit2[dateRange] <- fit[dateRange] - fit[dateRange-12] # requires startIndex to be at least 13
      
      stats$TR[index]         <<- exp(b)-1
      stats$volatility[index] <<- sd(fit2[dateRange], na.rm=T)
      #       print( c( "Time for fit:", round(summary(proc.time())[[1]] - time1[[1]] , 2) ) )
      
      if ( (strategyName %in% colnames(alloc)) ) {# this means we are NOT dealing with constant allocation (e.g. stocks)
         stats$avgStockAlloc[index]    <<- mean(alloc[dateRange, strategyName], na.rm=T)
         stats$latestStockAlloc[index] <<- alloc[numData, strategyName]     
         dateRange2 <- def$startIndex:(numData-1)
         turnover <- numeric(numData)
         turnover[1:def$startIndex] <- NA
         turnover[dateRange2+1] <- abs(alloc[dateRange2+1, strategyName] - alloc[dateRange2, strategyName])
         stats$turnover[index] <<- 1/12/mean(turnover[dateRange2+1], na.rm=F)
         stats$invTurnover[index] <<- 1/stats$turnover[index]
         
         if (tradingCost == 0.02) 
            stats$netTR2[index] <<- stats$TR[index] - tradingCost/stats$turnover[index]
         else if(tradingCost == 0.04) 
            stats$netTR4[index] <<- stats$TR[index] - tradingCost/stats$turnover[index]
         else stop("No data frame \'netTR", round(tradingCost*100), "\' exists.")     
      } 
      if (tradingCost == 0.02) 
         stats$netTR2[index] <<- stats$TR[index] - tradingCost/stats$turnover[index]
      else if(tradingCost == 0.04) 
         stats$netTR4[index] <<- stats$TR[index] - tradingCost/stats$turnover[index]
      else stop("No data frame \'netTR", round(tradingCost*100), "\' exists.")     
      
      stats$score[index] <<- 75* ( 2*stats$TR[index] - stats$volatility[index]/4 - stats$DD2[index]*2/300 
                                   + stats[index, medianName] + stats[index, fiveName] 
                                   - 3*tradingCost*(1/stats$turnover[index]-0.25) )
         ## For constant allocations, the deiravtive of TR with respect to volatility is about 0.23 and 
         ## with respect to DD^2 it is about 0.65%.
         ## So for constant allocations: 2*TR - vol/4 - DD2*2/300 is about constant.
         ## These 2 coefficients make it possible to 'convert' vol and DD2 into TR equivalents.
   }
}

showSummaryForStrategy <- function(strategyName, displayName="", futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                   minTR=0, maxVol=20, maxDD2=5, minTO=0, force=F) {
   
   if ( !(strategyName %in% stats$strategy) | force) 
      calcStatisticsForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force) 
   index <- which(stats$strategy == strategyName)
   medianName <- paste0("median", futureYears)
   fiveName <- paste0("five", futureYears)
   if(displayName=="") displayName <- strategyName
   
   TO <- stats$turnover[index]
   TOcost <- tradingCost / TO
   
   avgAlloc <- 100*stats$avgStockAlloc[index]
   latestAlloc <- 100*stats$latestStockAlloc[index]    

   ret  <- 100*(stats$TR[index] - TOcost) 
   vol  <- 100*stats$volatility[index]
   med  <- 100*(stats[index, medianName] - TOcost) 
   five <- 100*(stats[index, fiveName] - TOcost) 
   DD2  <- stats$DD2[index]
   score<- stats$score[index] + 1.5*tradingCost*100
   
   if (round(ret,1)%%1 == 0) retPad = "  "
      else retPad = ""
   if (round(vol,1)%%1 == 0) volPad = "  "
      else volPad = ""
   if (round(med,1)%%1 == 0) medPad = "  "
      else medPad = ""
   if (round(five,1)%%1 == 0) fivePad = "  "
      else fivePad = ""
   if ( is.na(latestAlloc) ) latestAllocPad = " "
      else if (latestAlloc < 10-1e-6) latestAllocPad = "  "
      else if (latestAlloc == 100) latestAllocPad = ""
      else latestAllocPad = " "
   if (TO>=10) TOpad = ""
      else TOpad = " "
   if ((10*round(DD2,2))%%1 == 0) DD2Pad = " "
      else DD2Pad = ""
     
   if(ret>minTR & vol<maxVol & DD2<maxDD2 & TO>minTO) {
      print(paste0(displayName, " | ", round(ret,1), retPad, "% |   ", 
                   round(med,1), medPad, "%,  ", round(five,1), fivePad, "% | ",
                   round(vol,1), volPad, "% |     ",
                   round(avgAlloc), "%, ", latestAllocPad, round(latestAlloc), "% | ",
                   TOpad, round(TO, 1), " | ",
                   round(DD2,2), DD2Pad, " | ", 
                   round(score,1) ) )
   }
}

showSummaries <- function(futureYears=def$futureYears, tradingCost=def$tradingCost, detailed=T, force=F) {
   # force pertains only to showSummaryForStrategy, not to calc...StrategyReturn (these are all set to F)

   print(paste0("* Statistics of the strategies (trading costs = ", round(100*tradingCost,2), "% per year of turnover):"))
   print(paste0("strategy  |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score") )

   showSummaryForStrategy("stocks", displayName="stocks   ", futureYears=futureYears, tradingCost=0, force=force)
   
   if(detailed) {
      showSummaryForStrategy(def$typicalCAPE,     displayName="CAPE10   ", futureYears=futureYears, tradingCost=tradingCost, force=force)
      showSummaryForStrategy(def$typicalDetrended,displayName="detrended", futureYears=futureYears, tradingCost=tradingCost, force=force)
      showSummaryForStrategy(def$typicalBoll,     displayName="Bollinger", futureYears=futureYears, tradingCost=tradingCost, force=force)
      showSummaryForStrategy(def$typicalSMA,      displayName="SMA 12-1 ", futureYears=futureYears, tradingCost=tradingCost, force=force)   
      showSummaryForStrategy(def$typicalMomentum, displayName="momentum ", futureYears=futureYears, tradingCost=tradingCost, force=force)
      showSummaryForStrategy(def$typicalReversal, displayName="reversal ", futureYears=futureYears, tradingCost=tradingCost, force=force)
   }
   showSummaryForStrategy(def$typicalValue,     displayName="value    ", futureYears=futureYears, tradingCost=tradingCost, force=force)
   showSummaryForStrategy(def$typicalTechnical, displayName="technical", futureYears=futureYears, tradingCost=tradingCost, force=force)
   showSummaryForStrategy(def$typicalBalanced,  displayName="balanced ", futureYears=futureYears, tradingCost=tradingCost, force=force)
   
   print("")
}
# Add numeric column to DF if column does not already exist
addNumColToDat <- function(colName) {
   if (!colName %in% colnames(dat)) dat[, colName] <<- numeric(numData)
}
addNumColToNormalized <- function(colName) {
   if (!colName %in% colnames(normalized)) normalized[, colName] <<- numeric(numData)
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
requireColInNormalized <- function(colName) {
   if (!colName %in% colnames(normalized)) stop(paste0("normalized$", colName, " does not exist."))
}
requireColInAlloc <- function(colName) {
   if (!colName %in% colnames(alloc)) stop(paste0("alloc$", colName, " does not exist."))
}
requireColInTR <- function(colName) {
   if (!colName %in% colnames(TR)) stop(paste0("TR$", colName, " does not exist."))
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
   if (futureYears==20)
      median_five <- calcNext20YrsReturn(strategyName, force)
   else if (futureYears==30)
      median_five <- calcNext30YrsReturn(strategyName, force)
   return(median_five)
}

   
createGoldStrategy <- function(strategyName="", futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   if (strategyName == "") strategyName <- "gold" 
   if (!strategyName %in% colnames(TR)) TR[, strategyName] <<- numeric(numData)
   
   index1968 <- (1968-1871)*12+1
   TR[, strategyName] <<- NA
   TR[index1968, strategyName] <<- 1
   for(i in (index1968+1):numData) 
      TR[i, strategyName] <<- TR[i-1, strategyName] * dat$gold[i] / dat$gold[i-1] 
   
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

createUKhousePriceStrategy <- function(strategyName="", futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   if (strategyName == "") strategyName <- "UKhousePrice" 
   addNumColToTR(strategyName)
   
   index1975 <- (1975-1871)*12+1
   TR[, strategyName] <<- NA
   TR[index1975+1, strategyName] <<- 1
   for(i in (index1975+2):numData) 
      TR[i, strategyName] <<- TR[i-1, strategyName] * dat$UKhousePrice[i] / dat$UKhousePrice[i-1] 
   
   if ( !(strategyName %in% stats$strategy) ) {
      index <- nrow(stats)+1 # row where the info will be added
      stats[index, ] <<- NA
      stats$strategy[index] <<- strategyName
      stats$avgStockAlloc[index] <<- NA
      stats$latestStockAlloc[index] <<- NA
      stats$turnover[index] <<- Inf
   }
   stats$type[which(stats$strategy == strategyName)] <<- "UKhousePrice"
   
   #    calcStatisticsForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   ## since real UKhousePrice data are only available since 1975 UKhousePrice statistics cannot be relevantly compared with other assets or strategies
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
      stats$avgStockAlloc[index] <<- stockAllocation/100
      stats$latestStockAlloc[index] <<- stockAllocation/100
      stats$turnover[index] <<- Inf
   }
   
   calcStatisticsForStrategy(strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- "constantAlloc"
}


calcStrategyReturn <- function(strategyName, startIndex) {
   TR[1:(startIndex-1), strategyName] <<- NA
   TR[startIndex, strategyName] <<- 1
   for(i in (startIndex+1):numData) 
      TR[i, strategyName] <<- TR[i-1, strategyName] * ( 
         alloc[i-1, strategyName] * dat$monthlyDifference[i] + dat$bondsMonthly[i] ) # alloc * (stocks-bonds) + bonds = alloc * stocks + (1-alloc) * bonds
}


## Calculating allocation from normalized
calcAllocFromNorm <- function(strategyName, medianAlloc, interQuartileAlloc) {
   ## creates an allocation with median (close to) medianAlloc and 
   ## with a difference between the 2 quartiles of interQuartileAlloc
   b <- tan(pi*(medianAlloc/100-.5))
   tan2A <- tan(pi*interQuartileAlloc/100)
   a <- sqrt(1/tan2A^2 + 1 + b^2) - 1/tan2A

   requireColInNormalized(strategyName)
   addNumColToAlloc(strategyName)
   alloc[, strategyName] <<- atan(normalized[, strategyName]*a + b) /pi + .5 
}


calcSMAofStrategy <- function(inputStrategyName, avgOver=3L, futureYears=def$futureYears,
                              medianAlloc=def$medianAlloc, interQuartileAlloc=def$interQuartileAlloc, 
                              strategyName="", force=F) {
   if (strategyName=="") strategyName <- paste0(name, "_SMA", avgOver)

   if (!(inputStrategyName %in% colnames(alloc)))  stop(paste0("alloc$", inputStrategyName, " does not exist."))
   
   if (!(strategyName %in% colnames(TR)) | !(strategyName %in% colnames(alloc)) | force) { # if data do not exist yet or we force recalculation:   
      if (!(strategyName %in% colnames(alloc))) alloc[, strategyName] <<- numeric(numData)
      if (!(strategyName %in% colnames(TR))) TR[, strategyName] <<- numeric(numData)
      
      normalized[1:(avgOver-1), strategyName] <<- NA
      for (i in avgOver:numData) 
         normalized[i, strategyName] <<- mean( normalized[(i-avgOver+1):i, inputStrategyName], na.rm=F )
      calcAllocFromNorm(strategyName, medianAlloc, interQuartileAlloc)
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


calcAllForStrategy <- function(strategyName, futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {

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


calcStatisticsForStrategy <- function(strategyName, futureYears=def$futureYears, force=F) {
   
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

#       time1 <- proc.time()
      median_five <- calcStrategyFutureReturn(strategyName, futureYears, force=force)
#       print( c( "Time for calcStrategyFutureReturn():", round(summary(proc.time())[[1]] - time1[[1]] , 2) ) )
      
#       time1 <- proc.time()      
      if (!(strategyName %in% colnames(DD)) | force) 
         CalcAllDrawdowns(strategyName, force=force)
#       print( c( "Time for CalcAllDrawdowns():", round(summary(proc.time())[[1]] - time1[[1]] , 2) ) )
      
      medianName <- paste0("median", futureYears)
      fiveName <- paste0("five", futureYears)
      
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
#       print( c( "Time for fit:", round(summary(proc.time())[[1]] - time1[[1]] , 2) ) )
        
#       time1 <- proc.time()      
      if ( (strategyName %in% colnames(alloc)) ) {# this means we are not dealing with constant allocation (e.g. stocks)
         turnover <- numeric(numData)
         turnover[(def$startIndex+1):numData] <- abs(alloc[(def$startIndex+1):numData, strategyName] - 
                                                        alloc[def$startIndex:(numData-1), strategyName])
         stats$turnover[index] <<- 1/12/mean(turnover[dateRange], na.rm=F)
         stats$avgStockAlloc[index]    <<- mean(alloc[dateRange, strategyName], na.rm=T)
         stats$latestStockAlloc[index] <<- alloc[numData, strategyName]     
      }
#       print( c( "Time for turnover:", round(summary(proc.time())[[1]] - time1[[1]] , 2) ) )   
      
      stats$TR[index]         <<- exp(b)-1
      stats$volatility[index] <<- sd(fit2[dateRange], na.rm=T)
      stats[index, medianName]<<- median_five[[1]]
      stats[index, fiveName]  <<- median_five[[2]]
      stats$DD2[index]        <<- sum(DD[, strategyName]^2)
      stats$score[index]      <<- 100*stats$TR[index] - 100*stats$volatility[index]/5 + 100*stats[index, medianName] + 
         100*stats[index, fiveName] - stats$DD2[index] - 1/stats$turnover[index]   
   }
}

showSummaryForStrategy <- function(strategyName, displayName="", futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                   minTR=0, maxVol=20, maxDD2=5, minTO=0, force=F) {
   
   if ( !(strategyName %in% stats$strategy) | force) 
      calcStatisticsForStrategy(strategyName, futureYears=futureYears, force=force) 
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
   print(paste0("strategy  |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )

   showSummaryForStrategy("stocks", displayName="stocks   ", futureYears=futureYears, tradingCost=0, force=force)
   showSummaryForStrategy(def$typicalCAPE, displayName="CAPE10   ", futureYears=futureYears, tradingCost=tradingCost, force=force)
   showSummaryForStrategy(def$typicalDetrended, displayName="detrended", futureYears=futureYears, tradingCost=tradingCost, force=force)
   
   if(detailed) {
      showSummaryForStrategy(def$typicalBoll, displayName="Bollinger", futureYears=futureYears, tradingCost=tradingCost, force=force)
      showSummaryForStrategy(def$typicalSMA, displayName="SMA 12-1 ", futureYears=futureYears, tradingCost=tradingCost, force=force)   
      showSummaryForStrategy(def$typicalMomentum, displayName="momentum ", futureYears=futureYears, tradingCost=tradingCost, force=force)
   }
   showSummaryForStrategy(def$typicalValue, displayName="value    ", futureYears=futureYears, tradingCost=tradingCost, force=force)
   showSummaryForStrategy(def$typicalTechnical, displayName="technical", futureYears=futureYears, tradingCost=tradingCost, force=force)
   showSummaryForStrategy(def$typicalBalanced, displayName="balanced ", futureYears=futureYears, tradingCost=tradingCost, force=force)
   
   print("")
}
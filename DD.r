
# Loading DD list from csv file
loadDDlist <- function() {
   if (!exists("DD")) {
      DD <<- read.csv("drawdownList.csv", col.names = c("startYear", "endYear", "comments"), header=F)
      numDD <<- dim(DD)[[1]]
      DD$dates <<- numeric(numDD)
      
      for (i in 1:numDD) {
         year1 <- floor(DD$startYear[i])
         year2 <- floor(DD$endYear[i] - .01) # 1999.00 means January 1999, i.e. the DD ended in 1998
         if (year1==year2) DD$dates[i] <<- as.character(year1)
         else DD$dates[i] <<- paste0(year1, "-", year2)
      }
      calcDDinflationAll()
   }
}

# wrapper calling either drawdownFast or drawdownSlow
drawdown <- function(strategyName, startYear, endYear) {
   drawdownFast(strategyName, startYear, endYear) 
}

# faster but less accurate when drawdawn is weak (returns NA)
drawdownFast <- function(strategyName, startYear, endYear) {
   TRname <- paste0(strategyName, "TR")
   if (!(TRname %in% colnames(strategy)))  stop(paste0("strategy$", TRname, " does not exist."))
   startIndex <- (startYear-1871)*12+1
   endIndex <- min((endYear-1871)*12, numData)
   
   highValue <- -1
   monotonicallyIncreasing <- T
   for (i in startIndex:endIndex) 
      if (!is.na(strategy[i, TRname]) & (strategy[i, TRname] > highValue) ) {
         highValue <- strategy[i, TRname]
         highIndex <- i
      }
   else monotonicallyIncreasing <- F
   if (highValue < 0) return(NA) # if all TR values are NA
   if (monotonicallyIncreasing) return(0) # if TR increases monotonically then DD = 0
   
   lowValue <- Inf
   for (i in seq(endIndex, startIndex, by=-1))
      if (!is.na(strategy[i, TRname]) & (strategy[i, TRname] < lowValue) ) {
         lowValue <- strategy[i, TRname]
         lowIndex <- i
      }
   if(lowIndex > highIndex) # if min occurs after max (as it should)
      {return(lowValue/highValue - 1)}
   else 
      drawdownSlow(strategyName, startYear, endYear) 
}

# slower but accurate
drawdownSlow <- function(strategyName, startYear, endYear) {
   TRname <- paste0(strategyName, "TR")
   if (!(TRname %in% colnames(strategy))) stop(paste0("strategy$", TRname, " does not exist."))
   startIndex <- (startYear-1871)*12+1
   endIndex <- min((endYear-1871)*12, numData)
   
   DD <- 1
   for (i in startIndex:(endIndex-1) ) {
      if (!is.na(strategy[i, TRname]))
          for (j in i:endIndex) 
             if (!is.na(strategy[j, TRname]) & (strategy[j, TRname] / strategy[i, TRname] < DD ) ) 
                DD <- strategy[j, TRname]/strategy[i, TRname]
   }
   return(DD-1)
}

CalcAllDrawdowns <- function(strategyName, force=F) {
   if ( !(strategyName %in% colnames(DD)) | force) {# if data do not exist yet or we force recalculation:
      DD[, strategyName]  <<- numeric(numDD)      
      for (i in 1:numDD) 
         DD[i, strategyName] <<- drawdown(strategyName, DD$startYear[i], DD$endYear[i])
   }
}

showMajorDrawdowns <- function(strategyName, threshold=-.2, na.rm=F, force=F) {
   threshold <- -abs(threshold) # threshould can be positive or negative
   if ( !(strategyName %in% colnames(DD)) | force) # if data do not exist yet or we force recalculation:
      CalcAllDrawdowns(strategyName, force=force)
   majorDD <- DD[, strategyName]
   majorDD <- majorDD[majorDD <= threshold]
   if (na.rm) majorDD <- majorDD[!is.na(DD)] # if na.rm is True then we remove NA from major DD list
   return(majorDD)
}

showMajorDrawdownsWithReference <- function(strategyName, refStrategyName, threshold=-.2, na.rm=F, force=F) {
   # Selects drawdowns of strategy when refStrategy lost more than threshold
   threshold <- -abs(threshold) # threshould can be positive or negative

   if ( !(strategyName %in% colnames(DD)) | force) # if data do not exist yet or we force recalculation:
      CalcAllDrawdowns(strategyName, force=force)
   if ( !(refStrategyName %in% colnames(DD)) | force) # if data do not exist yet or we force recalculation:
      CalcAllDrawdowns(refStrategyName, force=force)

   majorDD <- DD[, strategyName]
   refDD <- DD[, refStrategyName]
   
   if (na.rm) # if na.rm is True then we remove NA from major DD list of refStrategy, not strategy
      return(majorDD[(refDD <= threshold) & (!is.na(refDD))])
   else return(majorDD[refDD <= threshold])
}

showWorstDrawdowns <- function(strategyName, threshold=0, na.rm=F, force=F) {
   # threshold=0 means that by default we display all DD in order of their magnitude
   worstDD <- showMajorDrawdowns(strategyName, threshold=threshold, na.rm=na.rm, force=force)
   return(worstDD[order(worstDD)])
}

showWorstDrawdownsWithReference <- function(strategyName, refStrategyName, threshold=0, na.rm=F, force=F) {
   # Calculates drawdowns of strategy when refStrategy lost the most
   worstDD <- showMajorDrawdownsWithReference(strategyName, refStrategyName, threshold=threshold, force=force)
   refDD <- showMajorDrawdowns(refStrategyName, threshold=threshold, force=force)
   if (na.rm) refDD <- refDD[!is.na(refDD)] # if na.rm is True then we remove NA from major DD list of refStrategy, not strategy
   return(worstDD[order(refDD)])
}

plotDD <- function(DDindex, padding=0, minTR=.8, maxTR=1.5, newStartYear="", newEndYear="", 
                   stratName1=typicalStrategies[[1]], stratName2=typicalStrategies[[2]], stratName3=typicalStrategies[[3]], stratName4=typicalStrategies[[4]], 
                   showAlloc=F) {
   if (newStartYear == "") 
      newStartYear <- DD$startYear[DDindex]
   else if (newStartYear < 1000) # obviously not a year
      newStartYear <- DD$startYear[DDindex] + newStartYear
   if (newEndYear == "") 
      newEndYear   <- DD$endYear[DDindex]
   else if (newEndYear < 1000) # obviously not a year either
      newEndYear <- DD$endYear[DDindex] + newEndYear
      
   print( paste0(DD$dates[DDindex], ": nominally from ", DD$startYear[DDindex], " to ", DD$endYear[DDindex],
                 " -- plotted from ", newStartYear-padding, " to ", newEndYear+padding, 
                 " -- calculations from ", newStartYear, " to ", newEndYear) )

   plotReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
              startYear = newStartYear - padding, 
              endYear   = newEndYear   + padding, 
              minTR=minTR, maxTR=maxTR, showAlloc=showAlloc)
   
   sapply(c(stratName1, stratName2, stratName3, stratName4), 
          function(x) round(drawdown(x, newStartYear, newEndYear)*100,1) )
}

showDDinflation <- function(DDindex) {
   return ( dat$CPI[(DD$endYear[DDindex]-1871)*12+1] / dat$CPI[(DD$startYear[DDindex]-1871)*12+1] - 1)
}

calcDDinflationAll <- function() {
   DD$inflation <<- numeric(numDD)
   for (i in 1:numDD) 
      DD$inflation[i] <<- showDDinflation(i)
}

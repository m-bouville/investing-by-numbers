
# Loading DD list from csv file
loadDDlist <- function(force=F) {
   if (!exists("DD") | force) {
      DD <<- read.csv("drawdownList.csv", col.names = c("startYear", "endYear", "comments"), stringsAsFactors=F, header=F)
      DD <<- DD[-1, ] # removes 1873 DD since it occurs so early that CAPE-based strategies are unavailable at the time
      DD <<- DD[-1, ] # removes 1876-77 DD since it occurs so early that CAPE-based strategies are unavailable at the time
      DD <<- DD[-48, ] # removes 2000-09 DD since it (i) overlaps with another 3 and (ii) takes a lot of time to process (it is 9 year long)
      numDD <<- dim(DD)[[1]]
      DD$dates <<- character(numDD)
#      DD$endYear[27] <<- 1949      # Bonds go down a bit more after 1949 but not much and ...
#      DD$dates[27] <<- "1946-1948" # ... the extra 4 years take a lot of time to process
      
      for (i in 1:numDD) { # creates names for drawdowns
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

# Calculate the drawdown for a strategy between 2 years 
# faster but less accurate when drawdown is weak (returns NA)
drawdownFast <- function(strategyName, startYear, endYear) {
   if (!(strategyName %in% colnames(TR)))  stop(paste0("TR$", strategyName, " does not exist."))
   startIndex <- (startYear-1871)*12+1
   endIndex <- min((endYear-1871)*12, numData)
   
   highValue <- -1
   monotonicallyIncreasing <- T
   for (i in startIndex:endIndex) 
      if (!is.na(TR[i, strategyName]) & (TR[i, strategyName] > highValue) ) {
         highValue <- TR[i, strategyName]
         highIndex <- i
      }
      else monotonicallyIncreasing <- F
   if (highValue < 0) return(NA) # if all TR values are NA
   if (monotonicallyIncreasing) return(0) # if TR increases monotonically then DD = 0
   
   lowValue <- Inf
   for (i in seq(endIndex, startIndex, by=-1))
      if (!is.na(TR[i, strategyName]) & (TR[i, strategyName] < lowValue) ) {
         lowValue <- TR[i, strategyName]
         lowIndex <- i
      }
   if(lowIndex > highIndex) # if min occurs after max (as it should)
      return(lowValue/highValue - 1)
   else {
      if (endYear-startYear < 2){
#         print(paste0( "Switching to slow algo for DD ", startYear," - ", endYear ) )
         drawdownSlow(strategyName, startYear, endYear) 
      }
      else {
         if ( highValue - TR[endIndex, strategyName] < TR[startIndex, strategyName]-lowValue ) 
            drawdownFast(strategyName, startYear, endYear-.25) # maximum value is close to endYear: reruns with narrower range
         else drawdownFast(strategyName, startYear+.25, endYear) # minimum value is close to startYear: reruns with narrower range
      }
   }
}

# slower but accurate
drawdownSlow <- function(strategyName, startYear, endYear) {
   if (!(strategyName %in% colnames(TR))) stop(paste0("TR$", strategyName, " does not exist."))
   startIndex <- (startYear-1871)*12+1
   endIndex <- min((endYear-1871)*12, numData)
   
   DD <- 1
   for (i in startIndex:(endIndex-1) ) {
      if (!is.na(TR[i, strategyName]))
          for (j in i:endIndex) 
             if (!is.na(TR[j, strategyName]) & (TR[j, strategyName] / TR[i, strategyName] < DD ) ) 
                DD <- TR[j, strategyName]/TR[i, strategyName]
   }
   return(DD-1)
}

# Adds values for all drawdowns to the DD data frame
CalcAllDrawdowns <- function(strategyName, force=F) {
   if ( !(strategyName %in% colnames(DD)) | force) {# if data do not exist yet or we force recalculation:
      DD[, strategyName]  <<- numeric(numDD)      
      for (i in 1:numDD) {
#           DD0time <- proc.time()
         DD[i, strategyName] <<- drawdown(strategyName, DD$startYear[i], DD$endYear[i])
#          if (proc.time() - DD0time > .4) {
#             print( paste("DD #", i, "(", DD$dates[i], ") time:") )
#             print(proc.time() - DD0time)
#          }
      }
   }
}

# Displays drawdowns worse than the threshold
showMajorDrawdowns <- function(strategyName, threshold=-.2, na.rm=F, force=F) {
   threshold <- -abs(threshold) # threshould can be positive or negative
   if ( !(strategyName %in% colnames(DD)) | force) # if data do not exist yet or we force recalculation:
      CalcAllDrawdowns(strategyName, force=force)
   majorDD <- DD[, strategyName]
   majorDD <- majorDD[majorDD <= threshold]
   if (na.rm) majorDD <- majorDD[!is.na(DD)] # if na.rm is True then we remove NA from major DD list
   return(majorDD)
}

# Displays drawdowns of strategy when refStrategy lost more than threshold
showMajorDrawdownsWithReference <- function(strategyName, refStrategyName, threshold=-.2, na.rm=F, force=F) {
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

# Displays drawdowns by their magnitude rather than chronologically
showWorstDrawdowns <- function(strategyName, threshold=0, na.rm=F, force=F) {
   # threshold=0 means that by default we display all DD in order of their magnitude
   worstDD <- showMajorDrawdowns(strategyName, threshold=threshold, na.rm=na.rm, force=force)
   return(worstDD[order(worstDD)])
}

# Displays drawdowns when refStrategy lost more than threshold (by their magnitude rather than chronologically)
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

# Displays the inflation during a certain drawdown event (useful to tell how much of the real drawdown was due to inflation)
showDDinflation <- function(DDindex) {
   return ( dat$CPI[(DD$endYear[DDindex]-1871)*12+1] / dat$CPI[(DD$startYear[DDindex]-1871)*12+1] - 1)
}

# Calculates the inflation during all drawdown events (useful to tell how much of the real drawdown was due to inflation)
calcDDinflationAll <- function() {
   DD$inflation <<- numeric(numDD)
   for (i in 1:numDD) 
      DD$inflation[i] <<- showDDinflation(i)
}

defCAPElow   <- 14.6
defCAPEhigh  <- 16.7
defCAPEallocLow  <- 1
defCAPEallocHigh <- 0

defCAPEyears <- 10
defCAPEavgOver   <- 24
defCAPEcheat <- 2
defInitialOffset <- (defCAPEyears+defCAPEcheat)*12 + defCAPEavgOver


## calculating CAPE
calcCAPE <- function(years=defCAPEyears, cheat=defCAPEcheat) {
  CAPEname <- paste0("CAPE", years)
  addNumColToDat(CAPEname)
  months <- 12*years
  for(i in 1:(months-12*cheat)) { dat[i, CAPEname] <<- NA }
  for(i in (months-12*cheat+1):numData) 
    dat[i, CAPEname] <<- dat$price[i] / mean(dat$earnings[1:(i-1)], na.rm=T)
  for(i in (months+1):numData) 
     dat[i, CAPEname] <<- dat$price[i] / mean(dat$earnings[(i-months):(i-1)], na.rm=T)
}


## Average CAPE over 'avgOver' months
calcAvgCAPE <- function(CAPEname="CAPE10", avgOver=defCAPEavgOver) {
   if (!(CAPEname %in% colnames(dat))) stop(paste0("dat$", CAPEname, " does not exist."))
   avgCAPEname <- paste0(CAPEname,"avg",avgOver)
   addNumColToDat(avgCAPEname)
#   message(paste0("NB: dat$", avgCAPEname, " has average of ", CAPEname, "over ", avgOver, " *months*."))
   for(i in 1:(avgOver-1)) dat[i, avgCAPEname] <<- NA # not enough data to calculate average
   for(i in avgOver:numData) dat[i, avgCAPEname] <<- mean(dat[(i-avgOver+1):i, CAPEname])  
}


## Completely _ad hoc_ manipulation of CAPE
## Not in use (does not work)
calcModifiedCAPE <- function(CAPEname="CAPE10") {
   if (!(CAPEname %in% colnames(dat))) stop(paste0("dat$", CAPEname, " does not exist."))
   modifiedCAPEname <- paste0(CAPEname,"v2")
   addNumColToDat(modifiedCAPEname)
   for(i in 1:2*12+1) dat[i, modifiedCAPEname] <<- NA # not enough data
   for(i in  (2*12+2):numData) {
      dat[i, modifiedCAPEname] <<- dat[i, CAPEname] - .1*(dat[i, CAPEname] - dat[i-12, CAPEname])
   }  
}


## Calculating allocation from CAPE
calcCAPEallocation <- function(CAPEname="CAPE10avg24", offset, CAPElow=defCAPElow, CAPEhigh=defCAPEhigh, allocLow=defCAPEallocLow, allocHigh=defCAPEallocHigh, outputName="") {
   if(outputName=="") outputName <- paste0(CAPEname, "_", CAPElow, "_", CAPEhigh)
   allocName <- paste0(outputName, "Alloc")
   UBallocName <- paste0(outputName, "UnboundAlloc")
   
   if (!(CAPEname %in% colnames(dat))) stop(paste0("dat$", CAPEname, " does not exist."))
   if (!(allocName %in% colnames(strategy))) strategy[, allocName] <<- numeric(numData)
   if (!(UBallocName %in% colnames(strategy))) strategy[, UBallocName] <<- numeric(numData)

   for(i in 1:numData) {
      if (is.na(dat[i, CAPEname])) {
         strategy[i, allocName] <<- NA
         strategy[i, UBallocName] <<- NA
      }
       else {
          #if (dat[i, CAPEname] < CAPElow) {strategy[i, allocName] <<- allocLow}
#       else if (dat[i, CAPEname] > CAPEhigh) {strategy[i, allocName] <<- allocHigh}
          strategy[i, UBallocName] <<- (dat[i, CAPEname]-CAPEhigh) / (CAPElow-CAPEhigh) * (allocLow-allocHigh) + allocHigh
          strategy[i, UBallocName] <<- max(min(strategy[i, UBallocName], 1.5), -0.5)
          strategy[i, allocName] <<- max(min(strategy[i, UBallocName], allocLow), allocHigh)
       }
   }
}


## Calculating real total returns based on CAPE allocation from previous month
calcCAPEstrategyReturn <- function(CAPEname="CAPE10avg24", offset=defInitialOffset, 
                                   CAPElow=defCAPElow, CAPEhigh=defCAPEhigh, allocLow=defCAPEallocLow, allocHigh=defCAPEallocHigh, outputName="", force=F) {
   if(outputName=="") outputName <- paste0(CAPEname, "_", CAPElow, "_", CAPEhigh)
   TRname <- paste0(outputName, "TR")
   allocName <- paste0(outputName, "Alloc")

   if (!(TRname %in% colnames(strategy)) | !(allocName %in% colnames(strategy)) | force) { # if data do not exist yet or we force recalculation:   
      calcCAPEallocation(CAPEname, offset, CAPElow, CAPEhigh, allocLow, allocHigh, outputName)
      if (!(TRname %in% colnames(strategy))) {strategy[, TRname] <<- numeric(numData)}  
      calcStrategyReturn(allocName, TRname, offset)
   }
   
   if ( !(outputName %in% stats$strategy) ) {
      #print( paste0("Adding ", outputName, " to stats.") )
      index <- nrow(stats)+1 # row where the info will be added
      stats[index, ] <<- NA
      stats$strategy[index] <<- outputName
      stats$type[index] <<- "CAPE"
      stats$parameterName1[index]  <<- "CAPElow"
      stats$parameterValue1[index] <<-  CAPElow
      stats$parameterName2[index]  <<- "CAPEhigh"
      stats$parameterValue2[index] <<-   CAPEhigh
      stats$parameterName3[index]  <<- "allocLow"
      stats$parameterValue3[index] <<-  allocLow
      stats$parameterName4[index]  <<- "allocHigh"
      stats$parameterValue4[index] <<-  allocHigh
      stats$parameterName5[index]  <<- "offset"
      stats$parameterValue5[index] <<-  offset
   }

   calcStatisticsForStrategy(strategyName=outputName, years=futureYears, tradingCost=tradingCost, force=force)
}


compareCAPE <-function(minCAPElow=4, maxCAPElow=28, byCAPElow=4, mindCAPE=0, maxdCAPE=28, bydCAPE=4, maxCAPEhigh=32, cutoffScore=17, force=F) {
   for ( CAPElow in seq(minCAPElow, maxCAPElow, by=byCAPElow) )       
      for ( dCAPE in seq(mindCAPE, maxdCAPE, by=bydCAPE) ) {
         CAPEhigh <- CAPElow + dCAPE
         if (CAPEhigh < maxCAPEhigh + 1e-6) {
            name <- paste0("CAPE10_", CAPElow, "_", CAPEhigh)
            if (dCAPE < 1e-1) CAPEhigh <- CAPElow + 1e-1 # CAPEhigh = CAPElow would not work
            
            calcCAPEstrategyReturn(CAPEname="CAPE10avg24", outputName=name, offset=10*12, 
                                   CAPElow=CAPElow, CAPEhigh=CAPEhigh, allocLow=1, allocHigh=0, force=force)
            showSummaryStrategy(name, years=years, tradingCost=tradingCost, cutoffScore=cutoffScore, force=F)
         }  
      }
   showSummaries(years=years, tradingCost=tradingCost, detailed=F, force=F)
}

CAPEstrategies <- c("CAPE10_22_22", "CAPE10_16_16", "CAPE10_16_24", "stocks")

optimizeMetaCAPE <-function(stratName1=CAPEstrategies[[1]], stratName2=CAPEstrategies[[2]], stratName3=CAPEstrategies[[3]], stratName4="",
                            minF1=20, maxF1=80, dF1=20, minF2=minF1, maxF2=maxF1, dF2=dF1, minF3=minF1, maxF3=maxF1, 
                            years=defFutureYears, tradingCost=defTradingCost, cutoffScore=17, force=F) {

   for(f1 in seq(minF1, maxF1, by=dF1)) 
      for(f2 in seq(minF2, maxF2, by=dF2)) {
         f4 <- 0   
         f3 <- round(100 - f1 - f2)
         #name = paste0("metaCAPE", "_", f1, "_", f2, "_", f3, "_",  f4)
         displayName <- paste0(f1, "_", f2, "_", f3)
         outputName <- paste0("metaCAPE", displayName)
         #print(name)
         if ((f3 > minF3 - 1e-6) & (f3 < maxF3 + 1e-6)) {
            calcMultiStrategyReturn(name1=stratName1, name2=stratName2, name3=stratName3, name4=stratName4, 
                                    f1/100, f2/100, f3/100, f4/100, outputName=outputName, 10*12, delta="", force=force)
            showSummaryStrategy(outputName, years=years, tradingCost=tradingCost, cutoffScore=cutoffScore, displayName=displayName, force=force)
         }
      }
   showSummaryStrategy("CAPE10avg24",  years=years, tradingCost=tradingCost, displayName="CAPE10  ", force=F)
   showSummaryStrategy("balanced40_25_10_25", years=years, tradingCost=tradingCost, displayName="balanced", force=F)
}


#Plotting
plotCAPEreturn <- function(stratName1=CAPEstrategies[[1]], stratName2=CAPEstrategies[[2]], stratName3=CAPEstrategies[[3]], stratName4=CAPEstrategies[[4]], 
                           startYear=1885, endYear=2014, minTR=.9, maxTR=10000, normalize=T, showAlloc=T) {
   plotReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
              startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, normalize=normalize, showAlloc=showAlloc)
}

plotCAPEfutureReturn <- function(stratName1=CAPEstrategies[[1]], stratName2=CAPEstrategies[[2]], stratName3=CAPEstrategies[[3]], stratName4=CAPEstrategies[[4]], 
                                 years=defFutureYears, startYear=1885, endYear=2014-years, minTR=0, maxTR=.2, normalize=F, showAlloc=F) {
   plotFutureReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4,
                    years=years, startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, normalize=normalize, showAlloc=showAlloc)
}

plotCAPEbothReturns <- function(stratName1=CAPEstrategies[[1]], stratName2=CAPEstrategies[[2]], stratName3=CAPEstrategies[[3]], stratName4=CAPEstrategies[[4]], 
                            years=defFutureYears, startYear=1885, endYear=2014, minTR1=.9, maxTR1=10000, minTR2=0, maxTR2=.2) {
   plotBothReturns(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4,
                               years=years, startYear=startYear, endYear=endYear, minTR1=minTR1, maxTR1=maxTR1, minTR2=minTR2, maxTR2=maxTR2) 
}

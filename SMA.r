#default values of parameters:
defSMA1         <- 12L
defSMA2         <- 1L
defSMAratioLow  <- 5
defSMAratioHigh <- 5.5 
defSMAallocLow  <- 1
defSMAallocHigh <- 0
defSMAoffset    <- "mean"

## calculating simple moving average (SMA)
calcSMA <- function(avgOver) {
   SMAname <- paste0("SMA", avgOver)
   addNumColToDat(SMAname)
   for(i in 1:avgOver) { dat[i, SMAname] <<- NA }
   for(i in (avgOver+1):numData) {
      dat[i, SMAname] <<- mean(dat$totalReturn[(i-avgOver+1):i], na.rm=F)
   }
}


## Calculating allocation from ratio of 2 SMA
calcSMAallocation <- function(SMA1=defSMA1, SMA2=defSMA2, offset=defSMAoffset, 
                              ratioLow=defSMAratioLow, ratioHigh=defSMAratioHigh, 
                              allocLow=defSMAallocLow, allocHigh=defSMAallocHigh, strategyName) {
   SMAname1 <- paste0("SMA", SMA1)
   SMAname2 <- paste0("SMA", SMA2)
   if (!(SMAname1 %in% colnames(dat))) stop(paste0("dat$", SMAname1, " does not exist."))
   if (!(SMAname2 %in% colnames(dat))) stop(paste0("dat$", SMAname2, " does not exist."))
   #   offset = max(SMA1, SMA2)
   
   allocName <- paste0(strategyName, "Alloc")
   UBallocName <- paste0(strategyName, "UnboundAlloc")
   if (!(allocName %in% colnames(strategy))) strategy[, allocName] <<- numeric(numData)
   if (!(UBallocName %in% colnames(strategy))) strategy[, UBallocName] <<- numeric(numData)
   
   if(is.numeric(offset))
      temp <- dat[, SMAname1] / dat[, SMAname2] - 1 - offset
   else if(offset=="mean") {
      temp <- dat[, SMAname1] / dat[, SMAname2] - 1
      m <- mean(temp, na.rm=T)
      temp <- temp - m
   } else stop("offset must be either numerical or \'mean\'.")
   
   ratioLow  <- ratioLow /100
   ratioHigh <- ratioHigh/100
   
   strategy[, UBallocName] <<- (temp-ratioHigh) / (ratioLow-ratioHigh) * (allocLow-allocHigh) + allocHigh
   for(i in 1:numData) {
      strategy[i, UBallocName] <<- max(min(strategy[i, UBallocName], 1.5), -0.5)
      strategy[i, allocName] <<- max(min(strategy[i, UBallocName], allocLow), allocHigh)
   }
}


createSMAstrategy <- function(SMA1=defSMA1, SMA2=defSMA2, offset=defSMAoffset, 
                                   ratioLow=defSMAratioLow, ratioHigh=defSMAratioHigh, allocLow=defSMAallocLow, allocHigh=defSMAallocHigh,
                                   strategyName="", futureYears=defFutureYears, force=F) {
   
   if (strategyName=="") strategyName <- paste0("SMA", SMA1, "_", SMA2, "_", ratioLow, "_", ratioHigh)
   TRname <- paste0(strategyName, "TR")
   allocName <- paste0(strategyName, "Alloc")
   
   if (!(TRname %in% colnames(strategy)) | force) { # if data do not exist yet or we force recalculation:   
      calcSMAallocation(SMA1, SMA2, offset, ratioLow, ratioHigh, allocLow, allocHigh, strategyName=strategyName)
      if (!(TRname %in% colnames(strategy))) strategy[, TRname] <<- numeric(numData)
      calcStrategyReturn(allocName, TRname, max(SMA1,SMA2))
   }
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index] <<- strategyName
      parameters$type[index] <<- "SMA"
      parameters$allocLow[index] <<-  allocLow
      parameters$allocHigh[index] <<-  allocHigh
      parameters$offset[index] <<-  offset
      
       parameters$name1[index] <<- "SMA1"
      parameters$value1[index] <<-  SMA1
       parameters$name2[index] <<- "SMA2"
      parameters$value2[index] <<-  SMA2
       parameters$name3[index] <<- "ratioLow"
      parameters$value3[index] <<-  ratioLow
       parameters$name4[index] <<- "ratioHigh"
      parameters$value4[index] <<-  ratioHigh
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
}



plotSMA <- function(SMA1=defSMA1, SMA2=defSMA2, offset=defSMAoffset, futureYears=defFutureYears, startYear=1885) {
   futureReturnName <- paste0("future", futureYears)
   if (!futureReturnName %in% colnames(dat)) calcStocksFutureReturn(futureYears)
   SMAname1 <- paste0("SMA", SMA1)
   SMAname2 <- paste0("SMA", SMA2)
   
   par(mar=c(2.5, 4, 1.5, 1.5))
   par(mfrow = c(2, 1))
   temp <- numeric(numData)
   
   temp <- dat[, SMAname1] / dat[, SMAname2] - 1
   if(is.numeric(offset))
      m <- offset
   else if(offset=="mean") {
      m <- mean(temp, na.rm=T)
   } else stop("offset must be either numerical or \'mean\'.")
   temp <- temp - m
   
   plot(dat$date, temp, type="l", xlim=c(dat$date[(startYear-1871)*12], dat$date[numData]), xlab="SMA ratio", 
        ylab=paste0(SMAname1," / ",SMAname2," - ", 1+round(m,2)), ylim=c(-.5,.5))
   plot(temp, dat[, futureReturnName], xlab="SMA ratio", ylab="future return", xlim=c(-.5,.5))
   mod <- lm( dat[, futureReturnName] ~ temp)
   abline(mod)
}

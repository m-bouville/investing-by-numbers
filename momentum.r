#default values of parameters:
defMomentumLow       <- 15
defMomentumHigh      <- 25
defMomentumAllocLow  <- 1
defMomentumAllocHigh <- 0
defMomentumOffset    <- "mean"
defMomentumMonths     <- 12L


## calculating TR momentum
calcMomentum <- function(months=defMomentumMonths, strategyName) {
   addNumColToDat(strategyName)
   for(i in 1:months) { dat[i, strategyName] <<- NA }
   for(i in (months+1):numData) {
      dat[i, strategyName] <<- dat$totalReturn[i-months+1] / dat$totalReturn[i] - 1
   }
}


## Calculating allocation from momentum
calcMomentumAllocation <- function(months=defMomentumMonths, offset=defMomentumOffset, 
                                   momentumLow=defMomentumLow, momentumHigh=defMomentumHigh, 
                                   allocLow=defMomentumAllocLow, allocHigh=defMomentumAllocHigh, 
                                   strategyName) {
   momentumLow <- momentumLow/100
   momentumHigh <- momentumHigh/100
   
   allocName <- paste0(strategyName, "Alloc")
   UBallocName <- paste0(strategyName, "UnboundAlloc")
   
   if (!strategyName %in% colnames(dat)) calcMomentum(months, strategyName)
   if (!(allocName %in% colnames(strategy))) strategy[, allocName] <<- numeric(numData)
   if (!(UBallocName %in% colnames(strategy))) strategy[, UBallocName] <<- numeric(numData)
   
   if(is.numeric(offset))
      temp <- dat[, strategyName] - offset
   else if(offset=="mean") {
      temp <- dat[, strategyName]
      m <- mean(temp, na.rm=T)
      temp <- temp - m
   } else stop("offset must be either numerical or \'mean\'.")
   
   for(i in 1:numData) 
      if (is.na(temp[i])) {strategy[i, UBallocName] <<- NA}
   strategy[, UBallocName] <<- (temp-momentumHigh) / (momentumLow-momentumHigh) * (allocLow-allocHigh) + allocHigh
   for(i in 1:numData) {
      strategy[i, UBallocName] <<- max(min(strategy[i, UBallocName], 1.5), -0.5)
      strategy[i, allocName] <<- max(min(strategy[i, UBallocName], allocLow), allocHigh)
   }
}



createMomentumStrategy <- function(months=defMomentumMonths, offset=defMomentumOffset, 
                                        momentumLow=defMomentumLow, momentumHigh=defMomentumHigh, 
                                        allocLow=defMomentumAllocLow, allocHigh=defMomentumAllocHigh, 
                                        strategyName="", futureYears=defFutureYears, force=F) {
   
   if (strategyName=="") strategyName <- paste0("momentum", months, "_", momentumLow, "_", momentumHigh)
   TRname <- paste0(strategyName, "TR")
   allocName <- paste0(strategyName, "Alloc")
   
   if (!(TRname %in% colnames(strategy)) | force) { # if data do not exist yet or we force recalculation:   
      #       time0 <- proc.time()
      calcMomentumAllocation(months=months, offset=offset, momentumLow=momentumLow, momentumHigh=momentumHigh, allocLow=allocLow, allocHigh=allocHigh, strategyName=strategyName)
      if (!(TRname %in% colnames(strategy))) {strategy[, TRname] <<- numeric(numData)}  
      #       print("calcMomentumAllocation:")
      #       print(proc.time()-time0)
      
      #       time0 <- proc.time()
      calcStrategyReturn(allocName, TRname, months)
      #       print("calcStrategyReturn:")
      #       print(proc.time()-time0)
   } 
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index] <<- strategyName
      parameters$type[index] <<- "momentum"
      parameters$allocLow[index] <<-  allocLow
      parameters$allocHigh[index] <<-  allocHigh
      parameters$offset[index] <<-  offset
      
      parameters$name1[index] <<- "months"
      parameters$value1[index] <<-  months
       parameters$name2[index] <<- "momentumLow"
      parameters$value2[index] <<-  momentumLow
       parameters$name3[index] <<- "momentumHigh"
      parameters$value3[index] <<-   momentumHigh
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
}


plotMomentum <- function(months=defMomentumMonths, offset=defMomentumOffset, futureYears=defFutureYears, startYear=1885L) {
   futureReturnName <- paste0("futureReturn", futureYears)
   if (!futureReturnName %in% colnames(dat)) calcFutureReturn(futureYears)
   strategyName <- paste0("momentum", months)
   if (!strategyName %in% colnames(dat)) calcMomentum(months)
   
   par(mar=c(2.5, 4, 1.5, 1.5))
   par(mfrow = c(2, 1))
   temp <- numeric(numData)
   
   temp <- dat[, strategyName]
   if(is.numeric(offset))
      m <- offset
   else if(offset=="mean") {
      m <- mean(temp, na.rm=T)
   } else stop("offset must be either numerical or \'mean\'.")
   temp <- temp - m
   
   plot(dat$date, temp, type="l", xlim=c(dat$date[(startYear-1871)*12], dat$date[numData]), xlab="SMA ratio", 
        ylab=paste0(," - ", 1+round(m,2)), ylim=c(-.5,.5))
   plot(temp, dat[, futureReturnName], xlab="momentum", ylab="future return", xlim=c(-.5,.5))
   mod <- lm( dat[, futureReturnName] ~ temp)
   abline(mod)   
}

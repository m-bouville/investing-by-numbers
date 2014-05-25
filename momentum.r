
## calculating TR momentum
calcMomentum <- function(months) {
   momentumName <- paste0("momentum", months)
   addNumColToDat(momentumName)
   for(i in 1:months) { dat[i, momentumName] <<- NA }
   for(i in (months+1):numData) {
      dat[i, momentumName] <<- dat$totalReturn[i-months+1] / dat$totalReturn[i] - 1
   }
}


## Calculating allocation from momentum
calcMomentumAllocation <- function(months=12L, offset="mean", momentumLow=.15, momentumHigh=.25, allocLow=1, allocHigh=0, outputName="") {
   if (outputName=="") outputName <- paste0("momentum", months)
   momentumName <- paste0("momentum", months)
   allocName <- paste0(outputName, "Alloc")
   UBallocName <- paste0(outputName, "UnboundAlloc")
   
   if (!momentumName %in% colnames(dat)) calcMomentum(months)
   if (!(allocName %in% colnames(strategy))) strategy[, allocName] <<- numeric(numData)
   if (!(UBallocName %in% colnames(strategy))) strategy[, UBallocName] <<- numeric(numData)
   
   if(is.numeric(offset))
      temp <- dat[, momentumName] - offset
   else if(offset=="mean") {
      temp <- dat[, momentumName]
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


## Calculating real total returns based on momentum
calcMomentumStrategyReturn <- function(months=12L, offset="mean", momentumLow=.15, momentumHigh=.25, 
                                       allocLow=1, allocHigh=0, outputName="", force=F) {
   if (outputName=="") 
      momentumName <- paste0("momentum", months)
   else 
      momentumName <- outputName
   TRname <- paste0(momentumName, "TR")
   allocName <- paste0(momentumName, "Alloc")
   
   if (!(TRname %in% colnames(strategy)) | force) { # if data do not exist yet or we force recalculation:   
      calcMomentumAllocation(months=months, offset=offset, momentumLow=momentumLow, momentumHigh=momentumHigh, allocLow=allocLow, allocHigh=allocHigh, outputName=momentumName)
      if (!(TRname %in% colnames(strategy))) {strategy[, TRname] <<- numeric(numData)}  
      calcStrategyReturn(allocName, TRname, months)
   }
}


plotMomentum <- function(months, offset="mean", futureReturnYears=20L, startYear=1885L) {
   futureReturnName <- paste0("futureReturn", futureReturnYears)
   if (!futureReturnName %in% colnames(dat)) calcFutureReturn(futureReturnYears)
   momentumName <- paste0("momentum", months)
   if (!momentumName %in% colnames(dat)) calcMomentum(months)
   
   par(mar=c(2.5, 4, 1.5, 1.5))
   par(mfrow = c(2, 1))
   temp <- numeric(numData)
   
   temp <- dat[, momentumName]
   if(is.numeric(offset))
      m <- offset
   else if(offset=="mean") {
      m <- mean(temp, na.rm=T)
   } else stop("offset must be either numerical or \'mean\'.")
   temp <- temp - m
   
   plot(dat$date, temp, type="l", xlim=c(dat$date[(startYear-1871)*12], dat$date[numData]), xlab="SMA ratio", 
        ylab=paste0(momentumName," - ", 1+round(m,2)), ylim=c(-.5,.5))
   plot(temp, dat[, futureReturnName], xlab="momentum", ylab="future return", xlim=c(-.5,.5))
   mod <- lm( dat[, futureReturnName] ~ temp)
   abline(mod)   
}

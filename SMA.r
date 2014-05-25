
## calculating simple moving average (SMA)
calcSMA <- function(avgOver) {
   SMAname <- paste0("SMA", avgOver)
   addNumColToDat(SMAname)
   for(i in 1:(avgOver-1)) { dat[i, SMAname] <<- NA }
   for(i in avgOver:numData) 
      dat[i, SMAname] <<- mean(dat$totalReturn[(i-avgOver+1):i], na.rm=F)
}


## Calculating allocation from ratio of 2 SMA
calcSMAallocation <- function(SMA1=12, SMA2=1, offset="mean", ratioLow=.05, ratioHigh=.055, allocLow=1, allocHigh=0, outputName="") {
   SMAname1 <- paste0("SMA", SMA1)
   SMAname2 <- paste0("SMA", SMA2)
   if (!(SMAname1 %in% colnames(dat))) stop(paste0("dat$", SMAname1, " does not exist."))
   if (!(SMAname2 %in% colnames(dat))) stop(paste0("dat$", SMAname2, " does not exist."))
   #   offset = max(SMA1, SMA2)
   
   if (outputName=="") 
      allocName <- paste0("SMA", SMA1, "_", SMA2, "Alloc")
   else allocName <- outputName
   UBallocName <- paste0("SMA", SMA1, "_", SMA2, "UnboundAlloc")
   if (!(allocName %in% colnames(strategy))) strategy[, allocName] <<- numeric(numData)
   if (!(UBallocName %in% colnames(strategy))) strategy[, UBallocName] <<- numeric(numData)
   
   if(is.numeric(offset))
      temp <- dat[, SMAname1] / dat[, SMAname2] - 1 - offset
   else if(offset=="mean") {
      temp <- dat[, SMAname1] / dat[, SMAname2] - 1
      m <- mean(temp, na.rm=T)
      temp <- temp - m
   } else stop("offset must be either numerical or \'mean\'.")
   
   strategy[, UBallocName] <<- (temp-ratioHigh) / (ratioLow-ratioHigh) * (allocLow-allocHigh) + allocHigh
   for(i in 1:numData) {
      strategy[i, UBallocName] <<- max(min(strategy[i, UBallocName], 1.5), -0.5)
      strategy[i, allocName] <<- max(min(strategy[i, UBallocName], allocLow), allocHigh)
   }
}


## Calculating real total returns based on SMA ratio
calcSMAstrategyReturn <- function(SMA1=12, SMA2=1, offset="mean", ratioLow=.04, ratioHigh=.05, allocLow=1, allocHigh=0, outputName="", force=F) {
   if (outputName=="") {
      TRname <- paste0("SMA", SMA1, "_", SMA2, "TR")
      allocName <- paste0("SMA", SMA1, "_", SMA2, "Alloc")
   }
   else {
      TRname <- paste0(outputName, "TR")
      allocName <- paste0(outputName, "Alloc")
   }
      
   if (!(TRname %in% colnames(strategy)) | force) { # if data do not exist yet or we force recalculation:   
      calcSMAallocation(SMA1, SMA2, offset, ratioLow, ratioHigh, allocLow, allocHigh, outputName=allocName)
      if (!(TRname %in% colnames(strategy))) strategy[, TRname] <<- numeric(numData)
      calcStrategyReturn(allocName, TRname, max(SMA1,SMA2))
   }
}


plotSMA <- function(SMA1=12, SMA2=1, offset="mean", futureReturnYears=20, startYear=1885) {
   futureReturnName <- paste0("future", futureReturnYears)
   if (!futureReturnName %in% colnames(dat)) calcStocksFutureReturn(futureReturnYears)
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

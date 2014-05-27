#default values of parameters:
setMomentumDefaultValues <- function() {
   def$momentumBearishThreshold <<- 15
   def$momentumBullishThreshold <<- 25
   def$momentumOffset           <<- "mean"
   def$momentumMonths           <<- 12L
}


## calculating momentum
calcMomentum <- function(inputDF, inputName, months=def$momentumMonths, momentumName) {
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="normalized") input <- normalized[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")
   
   addNumColToDat(momentumName)
   for(i in 1:months) { dat[i, momentumName] <<- NA }
   for(i in (months+1):numData) 
      dat[i, momentumName] <<- input[i] / input[i-months] - 1
}


## Normalize momentum
normalizeMomentum <- function(inputDF, inputName, months=def$momentumMonths, offset=def$momentumOffset,
                          bearishThreshold=def$momentumBearishThreshold, bullishThreshold=def$momentumBullishThreshold, strategyName) {
   
   bearishThreshold <- bearishThreshold/100
   bullishThreshold <- bullishThreshold/100

   momentumName <- paste0("momentum_", inputName, "_", months)
   if (!strategyName %in% colnames(dat)) calcMomentum(inputDF=inputDF, inputName=inputName, months, momentumName=momentumName)
   addNumColToNormalized(strategyName)
   
   if(is.numeric(offset))
      temp <- dat[, momentumName] - offset
   else if(offset=="mean") {
      temp <- dat[, momentumName]
      m <- mean(temp, na.rm=T)
      temp <- temp - m
   } else stop("offset must be either numerical or \'mean\'.")
   
#   normalized[1:(startIndex-1), strategyName] <<- NA  
#   dateRange <- startIndex:numData
   normalized[, strategyName] <<- 2 * (temp-bullishThreshold) / (bearishThreshold-bullishThreshold) - 1
   # at bullishThreshold, alloc will be 95% and at bearishThreshold, alloc will be 5%
}


createMomentumStrategy <- function(inputDF, inputName, months=def$momentumMonths, offset=def$momentumOffset, 
                                   bearishThreshold=def$momentumBearishThreshold, bullishThreshold=def$momentumBullishThreshold, 
                                   strategyName="", futureYears=def$futureYears, force=F) {
   
   if (strategyName=="") strategyName <- paste0("momentum_", inputName, "_", months, "_", bearishThreshold, "_", bullishThreshold)
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      normalizeMomentum(inputDF=inputDF, inputName=inputName, months=months, offset=offset, 
                        bearishThreshold=bearishThreshold, bullishThreshold=bullishThreshold, strategyName=strategyName)
      calcAllocFromNorm(strategyName)
      if (!(strategyName %in% colnames(TR))) {TR[, strategyName] <<- numeric(numData)}  
      calcStrategyReturn(strategyName, months+1)
    } 
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index] <<- strategyName
      parameters$type[index] <<- "momentum"
      parameters$inputDF[index]   <<- inputDF
      parameters$inputName[index] <<- inputName
      parameters$startIndex[index] <<- months+1
      parameters$bearishThreshold[index] <<-  bearishThreshold
      parameters$bullishThreshold[index] <<-  bullishThreshold
      parameters$offset[index] <<-  offset
      
       parameters$name1[index] <<- "months"
      parameters$value1[index] <<-  months
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
}


plotMomentum <- function(months=def$momentumMonths, offset=def$momentumOffset, futureYears=def$futureYears, startYear=1885L) {
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

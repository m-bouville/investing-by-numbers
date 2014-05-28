#default values of parameters:
setMomentumDefaultValues <- function() {
#    def$momentumBearishThreshold <<- 5
#    def$momentumBullishThreshold <<- 5
#    def$momentumOffset           <<- "mean"
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
normalizeMomentum <- function(inputDF, inputName, months=def$momentumMonths, strategyName) {
   
#    bearishThreshold <-  bearishThreshold/100
#    bullishThreshold <- -bullishThreshold/100

   momentumName <- paste0("momentum_", inputName, "_", months)
   if (!strategyName %in% colnames(dat)) 
      calcMomentum(inputDF=inputDF, inputName=inputName, months=months, momentumName=momentumName)
   addNumColToNormalized(strategyName)
   
   #    if(is.numeric(offset))
   #       temp <- dat[, momentumName] - offset
   #    else if(offset=="mean") {
   #       temp <- dat[, momentumName]
   #       m <- mean(temp, na.rm=T)
   #       temp <- temp - m
   #    } else stop("offset must be either numerical or \'mean\'.")
   
   bearish <- quantile(dat[, momentumName], 0.75, na.rm=T)[[1]] # backwards compared to others (high is good)
   bullish <- quantile(dat[, momentumName], 0.25, na.rm=T)[[1]]
   
#   normalized[1:(startIndex-1), strategyName] <<- NA  
#   dateRange <- startIndex:numData
   normalized[, strategyName] <<- 2 * (dat[, momentumName]-bullish) / (bearish-bullish) - 1
}


createMomentumStrategy <- function(inputDF, inputName, months=def$momentumMonths, 
                                   medianAlloc, interQuartileAlloc,
                                   strategyName="", futureYears=def$futureYears, force=F) {
   
   if (strategyName=="") 
      strategyName <- paste0("momentum_", inputName, "_", months, "_", medianAlloc, "_", interQuartileAlloc)
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      normalizeMomentum(inputDF=inputDF, inputName=inputName, months=months, strategyName=strategyName)
      calcAllocFromNorm(strategyName, medianAlloc=medianAlloc, interQuartileAlloc=interQuartileAlloc)
      addNumColToTR(strategyName)  
      startIndex = max(months, sum(is.na(alloc[ ,strategyName])))+1
      calcStrategyReturn(strategyName, startIndex)
   } 
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index]    <<- strategyName
      parameters$type[index]        <<- "momentum"
      parameters$inputDF[index]     <<- inputDF
      parameters$inputName[index]   <<- inputName
      parameters$startIndex[index]  <<- months+1
      parameters$medianAlloc[index] <<-  medianAlloc
      parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
#       parameters$offset[index] <<-  offset
      
       parameters$name1[index] <<- "months"
      parameters$value1[index] <<-  months
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
}

compareMomentum <-function(inputDF="dat", inputName="TR", 
                           minMonths=12L, maxMonths=12L, byMonths=6L, 
                           minMed=10, maxMed=90, byMed=20, minIQ=10, maxIQ=90, byIQ=20, 
                           futureYears=def$futureYears, tradingCost=def$tradingCost, 
                           minTR=6.2, maxVol=15, maxDD2=2.3, minTO=1.5, force=F) {
   
   print(paste0("strategy             |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   for ( months in seq(minMonths, maxMonths, by=byMonths) ) {
      for ( med in seq(minMed, maxMed, by=byMed) )       
         for ( IQ in seq(minIQ, maxIQ, by=byIQ) ) {
            strategyName <- paste0("momentum_", inputName, "_", months, "_", med, "_", IQ)
            
            createMomentumStrategy(inputDF=inputDF, inputName=inputName, months=months, 
                                   medianAlloc=med, interQuartileAlloc=IQ, strategyName=strategyName, force=force)                  
            showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                   minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
         }
      plotReturnVsBothBadWithLine()
   }
#    showSummaries(futureYears=futureYears, tradingCost=tradingCost, detailed=F, force=F)
}


plotMomentum <- function(months=def$momentumMonths, futureYears=def$futureYears, startYear=1885L) {
   futureReturnName <- paste0("futureReturn", futureYears)
   if (!futureReturnName %in% colnames(dat)) calcFutureReturn(futureYears)
   strategyName <- paste0("momentum", months)
   if (!strategyName %in% colnames(dat)) calcMomentum(months)
   
   par(mar=c(2.5, 4, 1.5, 1.5))
   par(mfrow = c(2, 1))
   temp <- numeric(numData)
   
   temp <- dat[, strategyName]
#    if(is.numeric(offset))
#       m <- offset
#    else if(offset=="mean") {
#       m <- mean(temp, na.rm=T)
#    } else stop("offset must be either numerical or \'mean\'.")
#    temp <- temp - m
   
   plot(dat$date, temp, type="l", xlim=c(dat$date[(startYear-1871)*12], dat$date[numData]), xlab="SMA ratio", 
        ylab=paste0(," - ", 1+round(m,2)), ylim=c(-.5,.5))
   plot(temp, dat[, futureReturnName], xlab="momentum", ylab="future return", xlim=c(-.5,.5))
   mod <- lm( dat[, futureReturnName] ~ temp)
   abline(mod)   
}

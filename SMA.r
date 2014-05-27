#default values of parameters:
setSMAdefaultValues <- function() {
   def$SMA1                <<- 12L
   def$SMA2                <<- 1L
   def$SMAbearishThreshold <<- 5
   def$SMAbullishThreshold <<- 5.5 
   def$SMAoffset           <<- "mean"
}

## calculating simple moving average (SMA)
calcSMA <- function(inputDF, inputName, avgOver, SMAname) {
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="normalized") input <- normalized[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")

   addNumColToDat(SMAname)
   dat[1:(avgOver-1), SMAname] <<- NA
   for(i in avgOver:numData) {
      dat[i, SMAname] <<- mean(input[(i-avgOver+1):i], na.rm=F)
   }
}


## Normalize SMA
normalizeSMA <- function(SMAname1, SMAname2, offset=def$SMAoffset, 
                          bearishThreshold=def$SMAbearishThreshold, bullishThreshold=def$SMAbullishThreshold, strategyName) {
   
   requireColInDat(SMAname1)
   requireColInDat(SMAname2)
   addNumColToNormalized(strategyName)
   
   if(is.numeric(offset))
      temp <- dat[, SMAname1] / dat[, SMAname2] - 1 - offset
   else if(offset=="mean") {
      temp <- dat[, SMAname1] / dat[, SMAname2] - 1
      m <- mean(temp, na.rm=T)
      temp <- temp - m
   } else stop("offset must be either numerical or \'mean\'.")

   bearishThreshold <- bearishThreshold/100
   bullishThreshold <- bullishThreshold/100
   
   normalized[, strategyName] <<- 2 * (temp-bullishThreshold) / (bearishThreshold-bullishThreshold) - 1
   # at bullishThreshold, alloc will be 95% and at bearishThreshold, alloc will be 5%
}


createSMAstrategy <- function(inputDF1, inputName1, SMA1=def$SMA1, inputDF2, inputName2, SMA2=def$SMA2, offset=def$SMAoffset, 
                              bearishThreshold=def$SMAbearishThreshold, bullishThreshold=def$SMAbullishThreshold, 
                              strategyName="", futureYears=def$FutureYears, force=F) {
   
   if (strategyName=="") strategyName <- paste0("SMA_", inputName1, SMA1, "_", inputName2, SMA2, 
                                                "_", bearishThreshold, "_", bullishThreshold)
   SMAname1 <- paste0("SMA_", inputName1, "_", SMA1)
   calcSMA(inputDF1, inputName1, SMA1, SMAname1)      
   SMAname2 <- paste0("SMA_", inputName2, "_", SMA2)
   calcSMA(inputDF2, inputName2, SMA2, SMAname2)      
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      normalizeSMA(SMAname1, SMAname2, offset, bearishThreshold=bearishThreshold, bullishThreshold=bullishThreshold, strategyName=strategyName)
      calcAllocFromNorm(strategyName)
#       calcSMAallocation(SMA1, SMA2, offset, ratioLow, ratioHigh, allocLow, allocHigh, strategyName=strategyName)
      if (!(strategyName %in% colnames(TR))) TR[, strategyName] <<- numeric(numData)
      calcStrategyReturn(strategyName, max(SMA1,SMA2)+1)
   }
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index] <<- strategyName
      parameters$type[index] <<- "SMA"
      parameters$startIndex[index] <<- max(SMA1,SMA2)+1
      parameters$inputDF[index]    <<- inputDF1
      parameters$inputName[index]  <<- inputName1
      
      parameters$bearishThreshold[index] <<-  bearishThreshold
      parameters$bullishThreshold[index] <<-  bullishThreshold
      parameters$offset[index] <<-  offset
      
       parameters$name1[index] <<- "SMA1"
      parameters$value1[index] <<-  SMA1
       parameters$name2[index] <<- "SMA2"
      parameters$value2[index] <<-  SMA2
       parameters$name3[index] <<- "inputDF2"
      parameters$value3[index] <<-  inputDF2
       parameters$name4[index] <<- "inputName2"
      parameters$value4[index] <<-  inputName2
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
}



plotSMA <- function(SMA1=def$SMA1, SMA2=def$SMA2, offset=def$SMAoffset, futureYears=def$FutureYears, startYear=1885) {
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

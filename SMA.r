#default values of parameters:
setSMAdefaultValues <- function() {
   def$SMA1                <<- 12L
   def$SMA2                <<- 1L
   def$SMAmedianAlloc      <<- 90
   def$SMAinterQuartileAlloc <<- 50
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
normalizeSMA <- function(SMAname1, SMAname2, strategyName) {
   
   requireColInDat(SMAname1)
   requireColInDat(SMAname2)
   addNumColToNormalized(strategyName)
   
   temp <- dat[, SMAname1] / dat[, SMAname2] - 1

   bearish <- quantile(temp, 0.25, na.rm=T)[[1]]
   bullish <- quantile(temp, 0.75, na.rm=T)[[1]]
   
   normalized[, strategyName] <<- 2 * (temp-bullish) / (bearish-bullish) - 1
}


createSMAstrategy <- function(inputDF1="dat", inputName1="TR", SMA1=def$SMA1, 
                              inputDF2="dat", inputName2="TR", SMA2=def$SMA2, 
                              medianAlloc=def$SMAmedianAlloc, interQuartileAlloc=def$SMAinterQuartileAlloc,
                              strategyName="", futureYears=def$futureYears, force=F) {
   
   if (strategyName=="") 
      strategyName <- paste0("SMA_", inputName1, SMA1, "_", inputName2, SMA2, "_", medianAlloc, "_", interQuartileAlloc)
   SMAname1 <- paste0("SMA_", inputName1, "_", SMA1)
   if (!(SMAname1 %in% colnames(dat)) | force)
      calcSMA(inputDF1, inputName1, SMA1, SMAname1)      
   SMAname2 <- paste0("SMA_", inputName2, "_", SMA2)
   if (!(SMAname2 %in% colnames(dat)) | force)
      calcSMA(inputDF2, inputName2, SMA2, SMAname2)      
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      normalizeSMA(SMAname1, SMAname2, strategyName=strategyName)
      calcAllocFromNorm(strategyName, medianAlloc=medianAlloc, interQuartileAlloc=interQuartileAlloc)
#       calcSMAallocation(SMA1, SMA2, offset, ratioLow, ratioHigh, allocLow, allocHigh, strategyName=strategyName)
      addNumColToTR(strategyName)
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
      
      parameters$medianAlloc[index] <<-  medianAlloc
      parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
      
       parameters$name1[index] <<- "SMA1"
      parameters$value1[index] <<-  SMA1
       parameters$name2[index] <<- "SMA2"
      parameters$value2[index] <<-  SMA2
       parameters$name3[index] <<- "inputDF2"
      parameters$value3[index] <<-  inputDF2
       parameters$name4[index] <<- "inputName2"
      parameters$value4[index] <<-  inputName2
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
}


searchForOptimalSMA <-function(inputDF1="dat", inputName1="TR", inputDF2="dat", inputName2="TR", 
                      minSMA1=12L, maxSMA1=12L, bySMA1=3L, minSMA2=1L, maxSMA2=1L, bySMA2=1L, 
                      minMed=10, maxMed=90, byMed=10, minIQ=10, maxIQ=90, byIQ=10, 
                      futureYears=def$futureYears, tradingCost=def$tradingCost, 
                      minTR=5.2, maxVol=14.5, maxDD2=2.2, minTO=1., force=F) {
   
   print(paste0("strategy           |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   for ( SMA1 in seq(minSMA1, maxSMA1, by=bySMA1) ) 
      for ( SMA2 in seq(minSMA2, maxSMA2, by=bySMA2) )       
         for ( med in seq(minMed, maxMed, by=byMed) )       
            for ( IQ in seq(minIQ, maxIQ, by=byIQ) ) {
               strategyName <- paste0("SMA_", inputName1, SMA1, "_", inputName2, SMA2, "_", med, "_", IQ)
               
               createSMAstrategy(inputDF1=inputDF1, inputName1=inputName1, SMA1=SMA1,
                                 inputDF2=inputDF2, inputName2=inputName2, SMA2=SMA2,
                                 medianAlloc=med, interQuartileAlloc=IQ, strategyName=strategyName, force=force)                  
               showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                      minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
            }
   #    showSummaries(futureYears=futureYears, tradingCost=tradingCost, detailed=F, force=F)
   plotReturnVsBothBadWithLine()
}


plotSMA <- function(SMA1=def$SMA1, SMA2=def$SMA2, futureYears=def$FutureYears, startYear=1885) {
   futureReturnName <- paste0("future", futureYears)
   if (!futureReturnName %in% colnames(dat)) calcStocksFutureReturn(futureYears)
   SMAname1 <- paste0("SMA", SMA1)
   SMAname2 <- paste0("SMA", SMA2)
   
   par(mar=c(2.5, 4, 1.5, 1.5))
   par(mfrow = c(2, 1))
   temp <- numeric(numData)
   
   temp <- dat[, SMAname1] / dat[, SMAname2] - 1
   
   plot(dat$date, temp, type="l", xlim=c(dat$date[(startYear-1871)*12], dat$date[numData]), xlab="SMA ratio", 
        ylab=paste0(SMAname1," / ",SMAname2," - ", 1+round(m,2)), ylim=c(-.5,.5))
   plot(temp, dat[, futureReturnName], xlab="SMA ratio", ylab="future return", xlim=c(-.5,.5))
   mod <- lm( dat[, futureReturnName] ~ temp)
   abline(mod)
}

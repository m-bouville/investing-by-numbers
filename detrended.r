

calcDetrended <- function(inputDF, inputName, detrendedName) {
   if (inputDF=="dat")             logInput <- log( dat[, inputName] )
   else if (inputDF=="normalized") logInput <- log( normalized[, inputName] )
   else if (inputDF=="alloc")      logInput <- log( alloc[, inputName] )
   else if (inputDF=="TR")         logInput <- log( TR[, inputName] )
   else if (inputDF=="next30yrs")  logInput <- log( next30yrs[, inputName] )
   else stop("data frame ", inputDF, " not recognized")
   
   fitPara <- regression(TR$numericDate[], logInput[])
   a <- fitPara[[1]]
   b <- fitPara[[2]]
#   print( c(a, b) )
   addNumColToDat(detrendedName)
   dat[, detrendedName] <<- logInput[] - (a + b * TR$numericDate[])
}



# par(mfrow = c(1, 1))
# plot(TR$numericDate, normalized$detrendedTR_90_50, type="l", col="red", ylim=c(-2,2))
# par(new=T)
# plot(TR$numericDate, normalized$CAPE10_2avg24_50_90, type="l", col="blue", ylim=c(-2,2))
# par(new=F)


## Average CAPE over 'avgOver' months
calcAvgDetrended <- function(detrendedName, avgOver=def$detrendedAvgOver) {

   requireColInDat(detrendedName)
   avgDetrendedName <- paste0(detrendedName, "avg", avgOver)
   addNumColToDat(avgDetrendedName)
   for(i in 1:(avgOver-1)) dat[i, avgDetrendedName] <<- NA # not enough data to calculate average
   for(i in avgOver:numData) dat[i, avgDetrendedName] <<- mean(dat[(i-avgOver+1):i, detrendedName])  
}


normalizeDetrended <- function(inputDF, inputName, strategyName, avgOver) {
   
   detrendedName <- paste0("detrended", inputName)
#    if (!detrendedName %in% colnames(dat)) 
      {
      calcDetrended(inputDF=inputDF, inputName=inputName, detrendedName=detrendedName)
      if( is.numeric(avgOver) ) {
          if( !paste0(detrendedName,"avg",avgOver) %in% colnames(dat) ) 
            calcAvgDetrended(detrendedName, avgOver)
         detrendedName <- paste0(detrendedName, "avg", avgOver) 
      }
      
      addNumColToNormalized(strategyName)
      
      bearish <- quantile(dat[, detrendedName], 0.25, na.rm=T)[[1]]
      bullish <- quantile(dat[, detrendedName], 0.75, na.rm=T)[[1]]
      
      normalized[, strategyName] <<- 2 * (dat[, detrendedName]-bullish) / (bearish-bullish) - 1
   }
}


createDetrendedStrategy <- function(inputDF="dat", inputName="TR", avgOver=def$detrendedAvgOver, 
                                    medianAlloc=60, interQuartileAlloc=80,
                                    strategyName="", futureYears=def$futureYears, force=F) {   
   if (strategyName=="") {
      if( is.numeric(avgOver) )
         strategyName <- paste0("detrended", inputName, "avg", avgOver, "_", medianAlloc, "_", interQuartileAlloc)
      else strategyName <- paste0("detrended", inputName, "_", medianAlloc, "_", interQuartileAlloc)
   }
   
    if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      normalizeDetrended(inputDF=inputDF, inputName=inputName, strategyName=strategyName, avgOver=avgOver)
      calcAllocFromNorm(strategyName, medianAlloc=medianAlloc, interQuartileAlloc=interQuartileAlloc)
      addNumColToTR(strategyName)  
      startIndex = avgOver # max(months, sum(is.na(alloc[ ,strategyName])))+1
      calcStrategyReturn(strategyName, startIndex)
   } 
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index]    <<- strategyName
      parameters$type[index]        <<- "detrended"
      parameters$subtype[index]     <<- inputName
      parameters$inputDF[index]     <<- inputDF
      parameters$inputName[index]   <<- inputName
      parameters$startIndex[index]  <<- startIndex
      parameters$name1[index]       <<- "avgOver"
      parameters$value1[index]      <<-  avgOver  
      parameters$medianAlloc[index] <<- medianAlloc
      parameters$interQuartileAlloc[index] <<-  interQuartileAlloc
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
}


searchForOptimalDetrended <-function(inputDF="dat", inputName="TR", minAvgOver=30, maxAvgOver=30, byAvgOver=0, 
                                     minMed=80, maxMed=95, byMed=5, minIQ=97, maxIQ=99, byIQ=1, 
                                     futureYears=def$futureYears, tradingCost=def$tradingCost, 
                                     minTR=6.4, maxVol=14., maxDD2=2., minTO=10, force=F) {
   
   print(paste0("strategy                |  TR  |", futureYears, " yrs: med, 5%| vol.  |alloc: avg, now|TO yrs| DD^2 | score  ") )
   detrendedName <- paste0("detrended", inputName)
   calcDetrended(inputDF=inputDF, inputName=inputName, detrendedName) 
   for (avgOver in seq(minAvgOver, maxAvgOver, by=byAvgOver)) {
      calcAvgDetrended(detrendedName, avgOver=avgOver)
      for ( med in seq(minMed, maxMed, by=byMed) )       
         for ( IQ in seq(minIQ, maxIQ, by=byIQ) ) {
            strategyName <- paste0(detrendedName, "avg", avgOver, "_", med, "_", IQ)
            
            createDetrendedStrategy(inputDF=inputDF, inputName=inputName, avgOver=avgOver, strategyName=strategyName, 
                                    medianAlloc=med, interQuartileAlloc=IQ, futureYears=futureYears, force=force)

            showSummaryForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, 
                                   minTR=minTR, maxVol=maxVol, maxDD2=maxDD2, minTO=minTO, force=F)
         }
      plotReturnVsFour()
   }
   #    showSummaries(futureYears=futureYears, tradingCost=tradingCost, detailed=F, force=F)
}


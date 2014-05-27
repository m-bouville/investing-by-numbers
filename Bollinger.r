#default values of parameters:
setBollDefaultValues <- function() {
   def$BollBearishThreshold  <<- 0.6
   def$BollBullishThreshold <<- -0.5
   def$BollAvgOver    <<- 21L
}

normalizeBoll <- function(inputDF, inputName, avgOver=def$BollAvgOver, 
                                bearishThreshold=def$BollBearishThreshold, bullishThreshold=def$BollBullishThreshold, strategyName="", force=F) {

   BollBandsName <- paste0(inputName, avgOver)
   avgName <- paste0("avg_", BollBandsName)
   SDname <- paste0("SD_", BollBandsName)
   
   if (inputDF=="dat")             input <- dat[, inputName]
   else if (inputDF=="normalized") input <- normalized[, inputName]
   else if (inputDF=="alloc")      input <- alloc[, inputName]
   else if (inputDF=="TR")         input <- TR[, inputName]
   else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
   else stop("data frame ", inputDF, " not recognized")
   
#    time1 <- proc.time()
   if ( !(avgName %in% colnames(dat)) | !(SDname %in% colnames(dat)) | force) {# if data do not exist yet or we force recalculation:
      addNumColToDat(avgName)
      addNumColToDat(SDname)
      dat[1:(avgOver-1), avgName] <<- NA # not enough data to calculate average or SD
      dat[1:(avgOver-1), SDname]  <<- NA 
      for(i in avgOver:numData) {
         dat[i, avgName] <<- mean(input[(i-avgOver+1):i])  
         dat[i, SDname]  <<-   sd(input[(i-avgOver+1):i])
      }
   }      
#    print( c( "Bollinger - calc-avg-&SD time:", round(summary(proc.time())[[1]] - time1[[1]] , 2) ) )
   
#    time1 <- proc.time()    
   if ( !(strategyName %in% colnames(normalized)) | !(SDname %in% colnames(dat)) | force) {# if data do not exist yet or we force recalculation:
      addNumColToNormalized(strategyName)
      normalized[(1:avgOver-1), strategyName] <<- NA
      for(i in avgOver:numData) {
         normalized[i, strategyName] <<- 1 - 2*(dat[i, avgName] + bullishThreshold*dat[i, SDname] - input[i]) / 
            dat[i, SDname] / (bullishThreshold + bearishThreshold)
         # equivalent to:
         #          BollLow   <- dat[i, avgName] - bearishThreshold * dat[i, SDname]
         #          BollHigh  <- dat[i, avgName] + bullishThreshold * dat[i, SDname]
         #          normalized[i, strategyName] <<- 1 - 2*(BollHigh - input[i]) / (BollHigh - BollLow)
      }
   }
#    print( c( "Bollinger - compare-to-bands time:", round(summary(proc.time())[[1]] - time1[[1]] , 2) ) )
}


createBollStrategy <- function(inputDF, inputName, avgOver=def$BollAvgOver, bearishThreshold=def$BollBearishThreshold, bullishThreshold=def$BollBullishThreshold, 
                               strategyName="", futureYears=def$FutureYears, force=F) {

   if(strategyName=="")  
      strategyName <- paste0("Boll_", inputName, "_", avgOver, "_", bearishThreshold, "_", bullishThreshold)

   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
#       time0 <- proc.time()
      normalizeBoll(inputDF=inputDF, inputName=inputName, avgOver=avgOver, bearishThreshold=bearishThreshold, bullishThreshold=bullishThreshold, 
                    strategyName=strategyName, force=force)
#       print( c( "Bollinger - normalizeBoll() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )

#       time0 <- proc.time()
      calcAllocFromNorm(strategyName)
#       print( c( "Bollinger - calcAllocFromNorm() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )
      
#       time0 <- proc.time()
      addNumColToTR(strategyName)  
#       print( c( "Bollinger - addNumColToTR() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )
      
#       time0 <- proc.time()
      calcStrategyReturn(strategyName, avgOver+1)
#       print( c( "Bollinger - calcStrategyReturn() time:", round(summary(proc.time())[[1]] - time0[[1]] , 2) ) )
   }
   
#    time0 <- proc.time()
   
   
#    calcBollStrategyReturn(avgOver=avgOver, bearishThreshold=bearishThreshold, bullishThreshold=bullishThreshold, 
#                           strategyName=strategyName, type=type, CAPEyears=CAPEyears, force=force)

   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$type[index] <<- "Bollinger"
      parameters$startIndex[index] <<- avgOver+1
      parameters$inputDF[index]   <<- inputDF
      parameters$inputName[index] <<- inputName
      parameters$bearishThreshold[index] <<-  bearishThreshold
      parameters$bullishThreshold[index] <<-  bullishThreshold
      parameters$name1[index]  <<- "avgOver"
      parameters$value1[index] <<-  avgOver
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
# print( c( "Bollinger - stats time:", round(summary(proc.time())[[1]] - time0[[1]] , 1) ) )
}

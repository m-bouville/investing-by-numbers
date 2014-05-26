#default values of parameters:
defBollFactorLow <- 0.6
defBollFactorHigh <- -0.5
defBollAvgOver <- 21L



## TR Bollinger bands over 'avgOver' months of CAPE
calcBollAlloc <- function(avgOver=defBollAvgOver, factorLow=defBollFactorLow, factorHigh=defBollFactorHigh, strategyName="", type, CAPEyears=defCAPEyears, force=F) {
   if(type=="") stop("type is needed: either \'TR\' or \'CAPE\'")
   else if(type=="TR") {
      if (strategyName=="") strategyName <- paste0("BollTR", avgOver, "_", factorLow, "_", factorHigh)
      typeName <- type
      colName <- "totalReturn"
   }
   else if(type=="CAPE") {
      if (strategyName=="") strategyName <- paste0("BollCAPE", CAPEyears, "_", avgOver, "_", factorLow, "_", factorHigh)
      typeName <- paste0("CAPE", CAPEyears)
      colName <- typeName
   }
   avgName <- paste0(typeName, "avg", avgOver)
   SDname <- paste0(typeName, "SD", avgOver)
   
   UBallocName <- paste0(strategyName, "UnboundAlloc")
   
   if ( !(avgName %in% colnames(dat)) | !(SDname %in% colnames(dat)) | force) {# if data do not exist yet or we force recalculation:
      dat[, avgName]  <<- numeric(numData)
      dat[, SDname] <<- numeric(numData)
      for(i in 1:(avgOver-1)) {# not enough data to calculate average or SD
         dat[i, avgName] <<- NA 
         dat[i, SDname]  <<- NA 
      }
      for(i in avgOver:numData) {
         dat[i, avgName] <<- mean(dat[(i-avgOver+1):i, colName])  
         dat[i, SDname]  <<-   sd(dat[(i-avgOver+1):i, colName])
      }
   }
   
   if (!(strategyName %in% colnames(alloc)))  alloc[, strategyName] <<- numeric(numData)
   if (!(UBallocName %in% colnames(strategy)))  strategy[, UBallocName] <<- numeric(numData)
   
   for(i in (1:avgOver-1) ) 
      alloc[i, strategyName] <<- NA
      
   for(i in avgOver:numData) {
      BollLow   <- dat[i, avgName] - factorLow * dat[i, SDname]
      BollHigh  <- dat[i, avgName] + factorHigh * dat[i, SDname]
      strategy[i, UBallocName] <<- 1 - (BollHigh - dat[i, colName]) / (BollHigh - BollLow)
      strategy[i, UBallocName] <<- max(min(strategy[i, UBallocName], 1.5), -0.5)
      alloc[i, strategyName] <<- max(min(strategy[i, UBallocName], 1), 0)   
   }
}

calcBollCAPEalloc <- function(avgOver=defBollAvgOver, factorLow=defBollFactorLow, factorHigh=defBollFactorHigh, strategyName="", CAPEyears=defCAPEyears, force=F) {
   calcBollAlloc(avgOver, factorLow, factorHigh, strategyName, CAPEyears, type="CAPE", force=force)
}

calcBollTRalloc <- function(avgOver=defBollAvgOver, factorLow=defBollFactorLow, factorHigh=defBollFactorHigh, strategyName="", force=F) {
   calcBollAlloc(avgOver, factorLow, factorHigh, strategyName, type="TR", force=force)
}

## Calculating real total returns based on Bollinger band allocation from previous month
calcBollStrategyReturn <- function(avgOver=defBollAvgOver, factorLow=defBollFactorLow, factorHigh=defBollFactorHigh, strategyName, type, CAPEyears=defCAPEyears, force=F) {
   if(type=="") stop("type is needed: either \'TR\' or \'CAPE\'")
   
   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      if(type=="TR")        
         calcBollTRalloc(avgOver, factorLow, factorHigh, strategyName, force=force)
      else if(type=="CAPE") 
         calcBollCAPEalloc(avgOver, factorLow, factorHigh, strategyName, CAPEyears=CAPEyears, force=force)
      
      if (!(strategyName %in% colnames(TR))) {TR[, strategyName] <<- numeric(numData)}
      
      #print( (defCAPEavgOver-defCAPEcheat)+12*CAPEyears )
      calcStrategyReturn( strategyName, (defCAPEavgOver-defCAPEcheat)+12*CAPEyears )
   }
}

calcBollTRstrategyReturn <- function(avgOver=defBollAvgOver, factorLow=defBollFactorLow, factorHigh=defBollFactorHigh, 
                                     strategyName="", force=F) {
   calcBollStrategyReturn(avgOver, factorLow, factorHigh, strategyName, type="TR", force)
}

calcBollCAPEstrategyReturn <- function(avgOver=defBollAvgOver, factorLow=defBollFactorLow, factorHigh=defBollFactorHigh, strategyName="", CAPEyears=defCAPEyears, force=F) {
   calcBollStrategyReturn(avgOver, factorLow, factorHigh, strategyName, type="CAPE", CAPEyears, force)
}


createBollStrategy <- function(avgOver=defBollAvgOver, factorLow=defBollFactorLow, factorHigh=defBollFactorHigh, 
                                    strategyName="", type, CAPEyears=defCAPEyears, futureYears=defFutureYears, force=F) {
   
   if(type=="") stop("parameter \'type\' is mandatory: either \'TR\' or \'CAPE\'")
   else if(type=="TR") {
      CAPEyears = 0
      if(strategyName=="")  {
         if (factorLow==factorHigh) strategyName <- paste0("BollTR", avgOver, "_", factorLow)
         else strategyName <- paste0("BollTR", avgOver, "_", factorLow, "_", factorHigh)
      }
   }
   else if(type=="CAPE") {
      if(!is.numeric(CAPEyears)) stop("parameter \'CAPEyears\' is mandatory for CAPE Bollinger.")
      if(strategyName=="")  {
         if (factorLow==factorHigh) strategyName <- paste0("BollCAPE", CAPEyears, "_", avgOver, "_", factorLow)
         else strategyName <- paste0("BollCAPE", CAPEyears, "_", avgOver, "_", factorLow, "_", factorHigh)
      }
   }
   
   calcBollStrategyReturn(avgOver=avgOver, factorLow=factorLow, factorHigh=factorHigh, 
                          strategyName=strategyName, type=type, CAPEyears=CAPEyears, force=force)

   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$type[index] <<- "Bollinger"
      parameters$subtype[index] <<- type
      parameters$name1[index]  <<- "avgOver"
      parameters$value1[index] <<-  avgOver
      parameters$name2[index]  <<- "factorLow"
      parameters$value2[index] <<-  factorLow
      parameters$name3[index]  <<- "factorHigh"
      parameters$value3[index] <<-  factorHigh
      if (type=="CAPE") {
         parameters$name4[index]  <<- "CAPEyears"
         parameters$value4[index] <<-  CAPEyears
      }
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
}

createBollTRstrategy <- function(avgOver=defBollAvgOver, factorLow=defBollFactorLow, factorHigh=defBollFactorHigh, 
                                    strategyName="", futureYears=defFutureYears, force=F) {
   createBollStrategy(avgOver=avgOver, factorLow=factorLow, factorHigh=factorHigh, 
                           strategyName=strategyName, type="TR", futureYears=futureYears, force=force)
}

createBollCAPEstrategy <- function(avgOver=defBollAvgOver, factorLow=defBollFactorLow, factorHigh=defBollFactorHigh, 
                                      strategyName="", CAPEyears=defCAPEyears, futureYears=defFutureYears, force=F) {
   createBollStrategy(avgOver=avgOver, factorLow=factorLow, factorHigh=factorHigh, 
                           strategyName=strategyName, type="CAPE", CAPEyears=CAPEyears, futureYears=futureYears, force=force)
}
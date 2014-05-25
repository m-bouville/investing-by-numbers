
## TR Bollinger bands over 'avgOver' months of CAPE
calcBollAlloc <- function(avgOver=21L, factorLow=0.6, factorHigh=-0.5, outputName="", type, years=10L, force=F) {
   if(type=="") stop("type is needed: either \'TR\' or \'CAPE\'")
   else if(type=="TR") {
      if (outputName=="") outputName <- paste0("BollTR", avgOver, "_", factorLow, "_", factorHigh)
      typeName <- type
      colName <- "totalReturn"
   }
   else if(type=="CAPE") {
      if (outputName=="") outputName <- paste0("BollCAPE", years, "_", avgOver, "_", factorLow, "_", factorHigh)
      typeName <- paste0("CAPE", years)
      colName <- typeName
   }
   avgName <- paste0(typeName, "avg", avgOver)
   SDname <- paste0(typeName, "SD", avgOver)
   
   allocName <- paste0(outputName, "Alloc")
   UBallocName <- paste0(outputName, "UnboundAlloc")
   
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
   
   if (!(allocName %in% colnames(strategy)))  strategy[, allocName] <<- numeric(numData)
   if (!(UBallocName %in% colnames(strategy)))  strategy[, UBallocName] <<- numeric(numData)
   
   for(i in avgOver:numData) {
      BollLow   <- dat[i, avgName] - factorLow * dat[i, SDname]
      BollHigh  <- dat[i, avgName] + factorHigh * dat[i, SDname]
      strategy[i, UBallocName] <<- 1 - (BollHigh - dat[i, colName]) / (BollHigh - BollLow)
      strategy[i, UBallocName] <<- max(min(strategy[i, UBallocName], 1.5), -0.5)
      strategy[i, allocName] <<- max(min(strategy[i, UBallocName], 1), 0)   
   }
}

calcBollCAPEalloc <- function(avgOver=21L, factorLow=0.6, factorHigh=-0.5, outputName="", years=10L, force=F) {
   calcBollAlloc(avgOver, factorLow, factorHigh, outputName, years, type="CAPE", force=force)
}

calcBollTRalloc <- function(avgOver=21L, factorLow=0.6, factorHigh=-0.5, outputName="", force=F) {
   calcBollAlloc(avgOver, factorLow, factorHigh, outputName, type="TR", force=force)
}



## Calculating real total returns based on Bollinger band allocation from previous month
calcBollStrategyReturn <- function(avgOver=21L, factorLow=0.6, factorHigh=-0.5, outputName="", type, years, force=F) {
   if(type=="") stop("type is needed: either \'TR\' or \'CAPE\'")
   else if(type=="TR") {
      years = 0
      if(outputName=="")  {
         if (factorLow==factorHigh) outputName <- paste0("BollTR", avgOver, "_", factorLow)
         else outputName <- paste0("BollTR", avgOver, "_", factorLow, "_", factorHigh)
      }
   }
   else if(type=="CAPE") {
      if(!is.numeric(years)) stop("parameter \'years\' is mandatory for CAPE Bollinger.")
      if(outputName=="")  {
         if (factorLow==factorHigh) outputName <- paste0("BollCAPE", years, "_", avgOver, "_", factorLow)
         else outputName <- paste0("BollCAPE", years, "_", avgOver, "_", factorLow, "_", factorHigh)
      }  
   }
   
   TRname <- paste0(outputName, "TR")
   allocName <- paste0(outputName, "Alloc")
   
   if (!(TRname %in% colnames(strategy)) | force) { # if data do not exist yet or we force recalculation:   
      if(type=="TR")        
         calcBollTRalloc(avgOver, factorLow, factorHigh, outputName, force=force)
      else if(type=="CAPE") 
         calcBollCAPEalloc(avgOver, factorLow, factorHigh, outputName, years=years, force=force)
      
      if (!(TRname %in% colnames(strategy))) {strategy[, TRname] <<- numeric(numData)}
      
      calcStrategyReturn(allocName, TRname, avgOver+12*years)
   }
}

calcBollTRstrategyReturn <- function(avgOver=21L, factorLow=0.6, factorHigh=-0.5, outputName="", force=F) {
   calcBollStrategyReturn(avgOver, factorLow, factorHigh, outputName, type="TR", force)
}

calcBollCAPEstrategyReturn <- function(avgOver=21L, factorLow=0.6, factorHigh=-0.5, outputName="", years=10, force=F) {
   calcBollStrategyReturn(avgOver, factorLow, factorHigh, outputName, type="CAPE", years, force)
}

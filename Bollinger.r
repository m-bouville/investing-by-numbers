#default values of parameters:
setBollDefaultValues <- function() {
   def$BollBearishThreshold  <<- 0.6
   def$BollBullishThreshold <<- -0.5
   def$BollAvgOver    <<- 21L
}


## Bollinger bands over 'avgOver' months
# calcBollBands <- function(inputDF, inputName, avgOver=def$BollAvgOver, BollBandsName, force=F) {
# #    BollBandsName <- paste0(inputName, avgOver)
#    avgName <- paste0("avg_", BollBandsName)
#    SDname <- paste0("SD_", BollBandsName)
#    
#    if (inputDF=="dat")             input <- dat[, inputName]
#    else if (inputDF=="normalized") input <- normalized[, inputName]
#    else if (inputDF=="alloc")      input <- alloc[, inputName]
#    else if (inputDF=="TR")         input <- TR[, inputName]
#    else if (inputDF=="next30yrs")  input <- next30yrs[, inputName]
#    else stop("data frame ", inputDF, " not recognized")
#    
#    if ( !(avgName %in% colnames(dat)) | !(SDname %in% colnames(dat)) | force) {# if data do not exist yet or we force recalculation:
#       dat[, avgName]  <<- numeric(numData)
#       dat[, SDname] <<- numeric(numData)
#       dat[1:(avgOver-1), avgName] <<- NA # not enough data to calculate average or SD
#       dat[1:(avgOver-1), SDname]  <<- NA 
#       for(i in avgOver:numData) {
#          dat[i, avgName] <<- mean(input[(i-avgOver+1):i])  
#          dat[i, SDname]  <<-   sd(input[(i-avgOver+1):i])
#       }
#    }
# }

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
   
   if ( !(avgName %in% colnames(dat)) | !(SDname %in% colnames(dat)) | force) {# if data do not exist yet or we force recalculation:
      dat[, avgName]  <<- numeric(numData)
      dat[, SDname]   <<- numeric(numData)
      dat[1:(avgOver-1), avgName] <<- NA # not enough data to calculate average or SD
      dat[1:(avgOver-1), SDname]  <<- NA 
      for(i in avgOver:numData) {
         dat[i, avgName] <<- mean(input[(i-avgOver+1):i])  
         dat[i, SDname]  <<-   sd(input[(i-avgOver+1):i])
      }
   }
   
   if ( !(strategyName %in% colnames(normalized)) | !(SDname %in% colnames(dat)) | force) {# if data do not exist yet or we force recalculation:
      addNumColToNormalized(strategyName)
      normalized[(1:avgOver-1), strategyName] <<- NA
      for(i in avgOver:numData) {
         BollLow   <- dat[i, avgName] - bearishThreshold * dat[i, SDname]
         BollHigh  <- dat[i, avgName] + bullishThreshold * dat[i, SDname]
         normalized[i, strategyName] <<- 1 - 2*(BollHigh - input[i]) / (BollHigh - BollLow)
      }
   }
}


# 
# calcBollCAPEalloc <- function(avgOver=def$BollAvgOver, bearishThreshold=def$BollBearishThreshold, bullishThreshold=def$BollBullishThreshold, strategyName="", CAPEyears=def$CAPEyears, force=F) {
#    calcBollAlloc(avgOver, bearishThreshold, bullishThreshold, strategyName, CAPEyears, type="CAPE", force=force)
# }
# 
# calcBollTRalloc <- function(avgOver=def$BollAvgOver, bearishThreshold=def$BollBearishThreshold, bullishThreshold=def$BollBullishThreshold, strategyName="", force=F) {
#    calcBollAlloc(avgOver, bearishThreshold, bullishThreshold, strategyName, type="TR", force=force)
# }

## Calculating real total returns based on Bollinger band allocation from previous month
# calcBollStrategyReturn <- function(avgOver=def$BollAvgOver, bearishThreshold=def$BollBearishThreshold, bullishThreshold=def$BollBullishThreshold, strategyName, type, CAPEyears=def$CAPEyears, force=F) {
#    if(type=="") stop("type is needed: either \'TR\' or \'CAPE\'")
#    
#    if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
#       if(type=="TR")        
#          calcBollTRalloc(avgOver, bearishThreshold, bullishThreshold, strategyName, force=force)
#       else if(type=="CAPE") 
#          calcBollCAPEalloc(avgOver, bearishThreshold, bullishThreshold, strategyName, CAPEyears=CAPEyears, force=force)
#       
#       if (!(strategyName %in% colnames(TR))) {TR[, strategyName] <<- numeric(numData)}
#       
#       #print( (def$CAPEavgOver-def$CAPEcheat)+12*CAPEyears )
#       calcStrategyReturn( strategyName, (def$CAPEavgOver-def$CAPEcheat)+12*CAPEyears+1 )
#       warning("Using \'(def$CAPEavgOver-def$CAPEcheat)+12*CAPEyears\' as parameter in calcStrategyReturn().")
#    }
# }

# calcBollTRstrategyReturn <- function(avgOver=def$BollAvgOver, bearishThreshold=def$BollBearishThreshold, bullishThreshold=def$BollBullishThreshold, 
#                                      strategyName="", force=F) {
#    calcBollStrategyReturn(avgOver, bearishThreshold, bullishThreshold, strategyName, type="TR", force)
# }
# 
# calcBollCAPEstrategyReturn <- function(avgOver=def$BollAvgOver, bearishThreshold=def$BollBearishThreshold, bullishThreshold=def$BollBullishThreshold, strategyName="", CAPEyears=def$CAPEyears, force=F) {
#    calcBollStrategyReturn(avgOver, bearishThreshold, bullishThreshold, strategyName, type="CAPE", CAPEyears, force)
# }


createBollStrategy <- function(inputDF, inputName, avgOver=def$BollAvgOver, bearishThreshold=def$BollBearishThreshold, bullishThreshold=def$BollBullishThreshold, 
                               strategyName="", futureYears=def$FutureYears, force=F) {
   if(strategyName=="")  
      strategyName <- paste0("Boll_", inputName, "_", avgOver, "_", bearishThreshold, "_", bullishThreshold)

   if (!(strategyName %in% colnames(TR)) | force) { # if data do not exist yet or we force recalculation:   
      normalizeBoll(inputDF=inputDF, inputName=inputName, avgOver=avgOver, bearishThreshold=bearishThreshold, bullishThreshold=bullishThreshold, 
                    strategyName=strategyName, force=force)
      calcAllocFromNorm(strategyName)
      addNumColToTR(strategyName)  
      calcStrategyReturn(strategyName, avgOver+1)
   }
   
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
}

# createBollTRstrategy <- function(avgOver=def$BollAvgOver, bearishThreshold=def$BollBearishThreshold, bullishThreshold=def$BollBullishThreshold, 
#                                     strategyName="", futureYears=def$FutureYears, force=F) {
#    createBollStrategy(avgOver=avgOver, bearishThreshold=bearishThreshold, bullishThreshold=bullishThreshold, 
#                            strategyName=strategyName, type="TR", futureYears=futureYears, force=force)
# }
# 
# createBollCAPEstrategy <- function(avgOver=def$BollAvgOver, bearishThreshold=def$BollBearishThreshold, bullishThreshold=def$BollBullishThreshold, 
#                                       strategyName="", CAPEyears=def$CAPEyears, futureYears=def$FutureYears, force=F) {
#    createBollStrategy(avgOver=avgOver, bearishThreshold=bearishThreshold, bullishThreshold=bullishThreshold, 
#                            strategyName=strategyName, type="CAPE", CAPEyears=CAPEyears, futureYears=futureYears, force=force)
# }
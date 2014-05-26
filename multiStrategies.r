## Calculating average alloc between 2 strategies, and corresponding results
calcMultiStrategyAlloc <- function(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                    fraction1=25, fraction2=25, fraction3=25, fraction4="", 
                                    strategyName, numNA, delta="", force=F) {
   
   allocName1 <- paste0(inputStrategyName1, "Alloc")
   allocName2 <- paste0(inputStrategyName2, "Alloc")
   allocName3 <- paste0(inputStrategyName3, "Alloc")
   allocName4 <- paste0(inputStrategyName4, "Alloc")
   
   UBallocName<- paste0(strategyName, "UnboundAlloc")
   allocName  <- paste0(strategyName, "Alloc")

   if (!(allocName1 %in% colnames(strategy))) stop(paste0("strategy$", allocName1, " does not exist."))
   if (!(allocName2 %in% colnames(strategy))) stop(paste0("strategy$", allocName2, " does not exist."))
   if (!(allocName3 %in% colnames(strategy))) stop(paste0("strategy$", allocName3, " does not exist."))
   if (!(allocName4 %in% colnames(strategy)) & fraction4 != 0) stop(paste0("strategy$", allocName4, " does not exist."))
   
   if (!(allocName %in% colnames(strategy)) | force) { # if data do not exist yet or we force recalculation:   
      if (!(allocName %in% colnames(strategy))) strategy[, allocName] <<- numeric(numData)
      if (!(UBallocName %in% colnames(strategy))) strategy[, UBallocName] <<- numeric(numData)
      
      if(fraction4==0)
         strategy[, UBallocName] <<- fraction1*strategy[, allocName1] + fraction2*strategy[, allocName2] + fraction3*strategy[, allocName3]
      else strategy[, UBallocName] <<- fraction1*strategy[, allocName1] + fraction2*strategy[, allocName2] + fraction3*strategy[, allocName3] + fraction4*strategy[, allocName4] 
      for(i in 1:numData) strategy[i, allocName] <<- max(min(strategy[i, UBallocName], 1), 0)
      
      if(is.numeric(delta)) 
         for(i in (numNA+2):numData) 
            if (abs(strategy[i, allocName] - strategy[i-1, allocName]) < delta) 
               strategy[i, allocName] <<- strategy[i-1, allocName] 
   }
}

   
createMultiStrategy <- function(inputStrategyName1, inputStrategyName2, inputStrategyName3, inputStrategyName4, 
                                    fraction1=25, fraction2=25, fraction3=25, fraction4="", 
                                    strategyName="", subtype, numNA, delta="", futureYears=defFutureYears, force=F) {
   
   if (!is.numeric(fraction1)) fraction1 <- 100 - fraction2 - fraction3 - fraction4
   if (!is.numeric(fraction2)) fraction2 <- 100 - fraction1 - fraction3 - fraction4
   if (!is.numeric(fraction3)) fraction3 <- 100 - fraction1 - fraction2 - fraction4
   if (!is.numeric(fraction4)) fraction4 <- 100 - fraction1 - fraction2 - fraction3
   sumCoeff <- fraction1 + fraction2 + fraction3 + fraction4
   if (abs(sumCoeff-100)>1e-6) stop(paste("Sum of coefficients must be 100, not", sumCoeff))
   
   if(strategyName=="") strategyName <- paste0(subtype, fraction1, "_", fraction2, "_", fraction3, "_", fraction4)
   
   calcMultiStrategyAlloc(inputStrategyName1=inputStrategyName1, inputStrategyName2=inputStrategyName2, 
                           inputStrategyName3=inputStrategyName3, inputStrategyName4=inputStrategyName4, 
                           fraction1=fraction1, fraction2=fraction2, fraction3=fraction3, fraction4=fraction4, 
                           strategyName=strategyName, numNA=numNA, delta=delta, force=force)

   allocName  <- paste0(strategyName, "Alloc")
   TRname     <- paste0(strategyName, "TR")   
   if (!(TRname %in% colnames(strategy)) | force) { # if data do not exist yet or we force recalculation:   
      if (!(TRname %in% colnames(strategy))) {strategy[, TRname] <<- numeric(numData)}
      calcStrategyReturn(allocName, TRname, numNA)
   }
   
   if ( !(strategyName %in% parameters$strategy) | force) {
      if ( !(strategyName %in% parameters$strategy) ) {
         parameters[nrow(parameters)+1, ] <<- NA
         parameters$strategy[nrow(parameters)] <<- strategyName
      }
      index <- which(parameters$strategy == strategyName)
      
      parameters$strategy[index] <<- strategyName
      parameters$type[index] <<- "multi"
      parameters$subtype[index] <<- subtype
      parameters$inputStrategyName1[index] <<- inputStrategyName1
      parameters$inputStrategyName2[index] <<- inputStrategyName2
      parameters$inputStrategyName3[index] <<- inputStrategyName3
      parameters$inputStrategyName4[index] <<- inputStrategyName4
      parameters$fraction1[index] <<- fraction1
      parameters$fraction2[index] <<- fraction2
      parameters$fraction3[index] <<- fraction3
      parameters$fraction4[index] <<- fraction4      
   }
   calcStatisticsForStrategy(strategyName=strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   stats$type[which(stats$strategy == strategyName)] <<- parameters$type[which(parameters$strategy == strategyName)]
}
   
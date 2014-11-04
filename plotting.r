
############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
##    plotting.r has various functions    ##
##         that generate plots            ##
##                                        ##
############################################



#default values of plotting parameters
setPlottingDefaultValues <- function() {
   def$yTRmin    <<- 7.
   def$yTRmax    <<- 9.5
   
   def$pngWidth  <<- 1024
   def$pngHeight <<- 768
   
   def$minVol    <<- 13
   def$maxVol    <<- 19
   def$minDD2    <<- 1.2
   def$maxDD2    <<- 2.
   
   def$plotEndYear <<- 2015.
   def$maxTR     <<- 50000
   
   #Colors consistently used for the various tsrategies
   def$colCAPE      <<- "cyan"
   def$colDetrended <<- "skyblue"
   def$colSMA       <<- "pink"
   def$colBollinger <<- "magenta"   
   def$colReversal  <<- "orange"
   def$colInflation <<- "purple"
   
   def$colValue     <<- "blue"
   def$colTechnical <<- "red"
   def$colBalanced  <<- "black"
   
   def$pch          <<- 16L
   def$Mpch         <<- 15L
   
   def$colConstantAlloc  <<- "green"
   
   # plotAllReturnsVsX()
   def$type1 <<- "CAPE"
   def$col1  <<- def$colCAPE
   def$pch1  <<- def$pch
   
   def$type2 <<- "detrended"
   def$col2  <<- def$colDetrended
   def$pch2  <<- def$pch

   def$type3 <<- "inflation"
   def$col3  <<- def$colInflation
   def$pch3  <<- def$pch
   
   def$type4 <<- "SMA"
   def$col4  <<- def$colSMA
   def$pch4  <<- def$pch
   
   def$type5 <<- "Bollinger"
   def$col5  <<- def$colBollinger
   def$pch5  <<- def$pch
   
   def$type6 <<- "reversal"
   def$col6  <<- def$colReversal
   def$pch6  <<- def$pch
   
   # plotAllReturnsVsX(): combined strategies
   def$Msubtype1 <<- "value"
   def$Mcol1     <<- def$colValue
   def$Mpch1     <<- def$Mpch
   
   def$Msubtype2 <<- "technical"
   def$Mcol2     <<- def$colTechnical
   def$Mpch2     <<- def$Mpch
   
   def$Msubtype3 <<- "balanced"
   def$Mcol3     <<- def$colBalanced
   def$Mpch3     <<- def$Mpch
   
   # plotAllReturnsVsX(): searches
   def$Stype    <<- "search"
   def$Scol     <<- NA # we need the subtype to set the color, we do that in the plotting function
   def$Spch     <<- def$pch # default symbol
   def$SpchM    <<- def$Mpch # empty square for combined strategies
   
   def$lineCol   <<- def$colConstantAlloc
}


plotAssetClassesReturn <- function(stratName1="stocks",       col1=def$colConstantAlloc, lwd1=2,
                                   stratName2="bonds",        col2="darkgreen",          lwd2=2,
                                   stratName3="UKhousePrice", col3="grey",               lwd3=2,
                                   stratName4="gold",         col4="gold",               lwd4=2, 
                                   startYear=1975.25, endYear=def$plotEndYear, 
                                   yLabel="rescaled total return", net=F, minTR=.5, maxTR=20,
                                   pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                   pngName="figures/asset_classes_returns.png") {

   requireColInDat("gold")
   requireColInDat("UKhousePrice")
   plotReturn(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
              stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
              startYear=startYear, endYear=endYear, costs="", 
              yLabel=yLabel, net=net, minTR=minTR, maxTR=maxTR,
              pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}


## this takes as input a data frame containing gross or net returns, 
plotSomeSortOfReturn <- function(returnDF1, returnDF2, returnDF3, returnDF4,
                                 stratName1, stratName2, stratName3, stratName4,
                                 col1, col2, col3, col4, lwd1, lwd2, lwd3, lwd4,
                                 xVect=TR$numericDate, startYear, endYear, 
                                 minTR, maxTR, yLabel, yLog, legendPlacement) {      

   par(mar=c(2.7, 4.2, 1.5, 1.5))
   xRange <- c(startYear, endYear)
   yRange <- c(minTR, maxTR)
   if (yLog) 
      logAxis<-"y"
   else logAxis<-""
            
   if( !is.null(returnDF1) ) {   
      plot(xVect, returnDF1, col=col1, xlab="", ylab=yLabel, 
           log=logAxis, type="l", lwd=lwd1, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if( !is.null(returnDF2) ) {   
      plot(xVect, returnDF2, col=col2, xlab="", ylab="", 
           log=logAxis, type="l", lwd=lwd2, xlim=xRange, ylim=yRange)
      par(new=T)
   }  
   if( !is.null(returnDF3) ) {   
      plot(xVect, returnDF3, col=col3, xlab="", ylab="", 
           log=logAxis, type="l", lwd=lwd3, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if( !is.null(returnDF4) ) {   
      plot(xVect, returnDF4, col=col4, xlab="", ylab="", 
           log=logAxis, type="l", lwd=lwd4, xlim=xRange, ylim=yRange)
   }
   legend( legendPlacement, c(stratName1,stratName2,stratName3,stratName4), 
          bty="n", lwd=c(lwd1, lwd2, lwd3, lwd4), lty = c(1,1,1,1), 
          col=c(col1, col2, col3, col4) )
   par(new=F)
}


##Wrapper passing the proper vectors to plotSomeSortOfReturn()
plotReturn <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                       stratName3=def$typicalValue, stratName4="stocks", 
                       xVect=TR$numericDate, 
                       col1=def$colBalanced, col2=def$colTechnical, col3=def$colValue, col4=def$colConstantAlloc, 
                       lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                       startYear=def$plotStartYear, endYear=def$plotEndYear, 
                       costs=def$tradingCost, 
                       minTR=.9, maxTR=def$maxTR, yLabel="", legendPlacement="topleft",
                       startIndex=def$startIndex, net=T, normalize=T,
                       pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                       pngName="figures/return.png") {   
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
   
   if ( yLabel=="" ) {
      if( costs==0 )  # trading cost = 0: gross returns
         yLabel <- paste0("total return (%), GROSS of costs")
      else if( costs>0 ) # trading cost > 0: net returns
         yLabel <- paste0("total return (%), net of costs of ", round(costs*100, 1), "%")   
      else # no notion of trading cost (e.g. for asset classes)
         yLabel <- paste0("total return (%)")
   }
   
   normDate <- (startYear-def$dataStartYear)*12+1
   
   stratNames <- c(stratName1, stratName2, stratName3, stratName4)
   returnDF  <- TR[, stratNames]
      
   if(net & costs>0) # reduce TR because of costs
      for (strat in 1:4) {
         index <- which(stats$strategy == stratNames[strat])
         TOcost <- costs/12/stats$turnover[index]
         for (i in startIndex:numData)
            returnDF[i, strat] <- returnDF[i, strat] * exp(-TOcost*(i-startIndex))
      }
      
   if (normalize) 
      lapply( 1:4, function(i) returnDF[, i] <<- returnDF[, i] / returnDF[normDate, i] )
      
   plotSomeSortOfReturn(returnDF1=returnDF[, 1], returnDF2=returnDF[, 2], 
                        returnDF3=returnDF[, 3], returnDF4=returnDF[, 4],
                        xVect=xVect, stratName1=stratName1, stratName2=stratName2, 
                        stratName3=stratName3, stratName4=stratName4,
                        col1=col1, col2=col2, col3=col3, col4=col4, lwd1=lwd1, lwd2=lwd2, lwd3=lwd3, lwd4=lwd4,
                        startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, 
                        yLabel=yLabel, legendPlacement=legendPlacement, yLog=T) 
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }   
}


plotAlloc <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                      stratName3=def$typicalValue, stratName4="stocks", 
                      col1=def$colBalanced, col2=def$colTechnical, col3=def$colValue, col4=def$colConstantAlloc, 
                      lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                      startYear=def$plotStartYear, endYear=def$plotEndYear,
                      pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                      pngName="figures/allocation.png") { 
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)

   normDate <- (startYear-def$dataStartYear)*12+1
   par(mar=c(2.7, 4.2, 1.5, 1.5))
   xRange <- c(startYear, endYear)
   yRange <- c(3.4, 96.9)
   
   if((stratName1 != "") && stratName1 %in% colnames(alloc)) {   
      plot(alloc$numericDate, 100*alloc[, stratName1], col=col1, xlab="", ylab="stock allocation (%)", 
           lwd=lwd1, type="l", xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if((stratName2 != "") && stratName2 %in% colnames(alloc)) {   
      plot(alloc$numericDate, 100*alloc[, stratName2], col=col2, xlab="", ylab="", 
           type="l", lwd=lwd2, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if((stratName3 != "") && stratName3 %in% colnames(alloc)) {
      plot(alloc$numericDate, 100*alloc[, stratName3], col=col3, xlab="", ylab="", 
           type="l", lwd=lwd3, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if((stratName4 != "") && stratName4 %in% colnames(alloc)) {   
      plot(alloc$numericDate, 100*alloc[, stratName4], col=col4, xlab="", ylab="", 
           type="l", lwd=lwd4, xlim=xRange, ylim=yRange)
   }
   par(new=F)  
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }
}

plotReturnAndAllocTechnical <- function(stratName1=def$typicalSMA,       col1=def$colSMA,       lwd1=1.5,
                                        stratName2=def$typicalBoll,      col2=def$colBoll,      lwd2=1.5,
                                        stratName3=def$typicalReversal,  col3=def$colReversal,  lwd3=1.5,
                                        stratName4=def$typicalTechnical, col4=def$colTechnical, lwd4=2, 
                                        startYear=def$plotStartYear, endYear=def$plotEndYear, 
                                        yLabel="", net=T, minTR=0.8, maxTR=def$maxTR, costs=def$tradingCost,
                                        pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                        pngName="figures/return_and_allocation-technical.png") {
   
   plotReturnAndAlloc(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
              stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
              startYear=startYear, endYear=endYear, costs=costs, 
              yLabel=yLabel, net=net, minTR=minTR, maxTR=maxTR,
              pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}

plotReturnAndAllocValue <- function(stratName1=def$typicalCAPE2,     col1=def$colCAPE,      lwd1=1.5,
                                    stratName2=def$typicalDetrended1,col2=def$colDetrended, lwd2=1.5,
                                    stratName3=def$typicalDetrended2,col3=def$colDetrended, lwd3=2,
                                    stratName4=def$typicalValue,     col4=def$colValue,     lwd4=2, 
                                    startYear=def$plotStartYear, endYear=def$plotEndYear, 
                                    yLabel="", net=T, minTR=0.8, maxTR=def$maxTR, costs=def$tradingCost,
                                    pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                    pngName="figures/return_and_allocation-value.png") {
   
   plotReturnAndAlloc(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
                      stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
                      startYear=startYear, endYear=endYear, costs=costs, 
                      yLabel=yLabel, net=net, minTR=minTR, maxTR=maxTR,
                      pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}

plotReturnAndAlloc <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                               stratName3=def$typicalValue, stratName4="stocks", 
                               col1=def$colBalanced, col2=def$colTechnical, 
                               col3=def$colValue, col4=def$colConstantAlloc, 
                               lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                               startYear=def$plotStartYear, endYear=def$plotEndYear, costs=def$tradingCost, 
                               minTR=.9, maxTR=def$maxTR, yLabelReturn="", 
                               legendPlacement="topleft", net=T, normalize=T,
                               pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                               pngName="figures/return_and_allocation.png") {
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)

   par(mfrow = c(2, 1))
   plotReturn(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
              stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
              startYear=startYear, endYear=endYear, costs=costs, 
              minTR=minTR, maxTR=maxTR, normalize=normalize, 
              yLabel=yLabelReturn, legendPlacement=legendPlacement, net=net)  
   plotAlloc(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
             stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
             startYear=startYear, endYear=endYear)        
   par(mfrow = c(1, 1))
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }
}


parametrizeFutureReturnPlot <- function(costs, yLabel="", futureYears, minTR, maxTR) {
   if ( yLabel=="" ) {
      if( costs==0 )  # cost = 0: gross returns
         yLabel <- paste0("annual return (%) over next ", futureYears, " years, GROSS of costs")
      else if( costs>0 ) # cost > 0: net returns
         yLabel <- paste0("annual return (%) over next ", futureYears, 
                          " years, net of costs of ", round(costs*100, 1), "%")   
      else # no notion of cost (e.g. for asset classes)
         yLabel <- paste0("annual return (%) over next ", futureYears, " years")
   }
   
   if (futureYears==5) {
      if (minTR=="") minTR <- -12
      if (maxTR=="") maxTR <-  32
   } else if (futureYears==10) {
      if (minTR=="") minTR <-  -6
      if (maxTR=="") maxTR <-  24
   } else if (futureYears==20) {
      if (minTR=="") minTR <-  -1
      if (maxTR=="") maxTR <-  15
   } else if (futureYears==30) {
      if (minTR=="") minTR <-   2
      if (maxTR=="") maxTR <-  12
   }
   return(c(yLabel=yLabel, minTR=minTR, maxTR=maxTR))
}


plotFutureReturn <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                             stratName3=def$typicalValue, stratName4="stocks", 
                             futureYears=def$futureYears, xVect=TR$numericDate, 
                             col1=def$colBalanced, col2=def$colTechnical, 
                             col3=def$colValue, col4=def$colConstantAlloc, 
                             lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                             startYear=def$plotStartYear, endYear=def$plotEndYear, 
                             costs=def$tradingCost, extrapolate=0,
                             minTR="", maxTR="", yLabel="", legendPlacement="bottomleft",
                             startIndex=def$startIndex, net=T, 
                             pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                             pngName=paste0("figures/return_over_next_", futureYears, "_years.png") ) { 
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
          
   para <- parametrizeFutureReturnPlot(costs*net, yLabel, futureYears, minTR, maxTR) 
   yLabel <- para[[1]]; minTR <- as.numeric(para[[2]]); maxTR <- as.numeric(para[[3]])
   
   normDate <- (startYear-def$dataStartYear)*12+1
   
   stratNames <- c(stratName1, stratName2, stratName3, stratName4)
   numCurves <- length(stratNames)

   if (futureYears==5) 
      returnDF <- 100 *  next5yrs[, stratNames]
   else if (futureYears==10) 
      returnDF <- 100 * next10yrs[, stratNames]
   else if (futureYears==20) 
      returnDF <- 100 * next20yrs[, stratNames]
   else if (futureYears==30) 
      returnDF <- 100 * next30yrs[, stratNames]  
   
   if(net & costs>0) # reduce TR because of costs
      for (strat in 1:numCurves) 
         if (stratNames[strat] != "") {
            index  <- which(stats$strategy == stratNames[strat])
            returnDF[, strat] <- returnDF[, strat] - 100*costs/stats$turnover[index]
         }
   
   ## adjust endYear based on non-NA data
   i <- (endYear-def$dataStartYear)*12+1
   while ( sum(is.na(returnDF[i, ])) > 0 ) 
      i <- i-1
   endYear <- round( (i-1)/12 + def$dataStartYear ) + 1
   
   ## A drawback of looking at the next 20 years is that it ends in 1994, for lack of data after 2014.
   ## In particular, we cannot observe the impact of the crash of 2000-2002.
   ## So after 1994, we look at the annual return between year y and 2014 (i.e. over less than 20 years).
   ## We notice a drop in return after 1995 because we catch less of the 1995-2000 bull market.
   ## The bottom is reached in 2000, because the next 14 years could not be good, starting from such a height
   if( extrapolate>0 )
      for ( count in 1:round(extrapolate*12) ) 
         returnDF[i+count, stratNames] <- ( (TR[dim(TR)[1], stratNames] / TR[i+count, stratNames])
                                           ^(1/((dim(TR)[1] - i - count)/12)) - 1 ) *100
   endYear <- round(endYear+extrapolate)
         
   plotSomeSortOfReturn(returnDF1=returnDF[, 1], returnDF2=returnDF[, 2], 
                        returnDF3=returnDF[, 3], returnDF4=returnDF[, 4],
                        xVect=xVect, stratName1=stratName1, stratName2=stratName2, 
                        stratName3=stratName3, stratName4=stratName4,
                        col1=col1, col2=col2, col3=col3, col4=col4, lwd1=lwd1, lwd2=lwd2, lwd3=lwd3, lwd4=lwd4,
                        startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, 
                        yLabel=yLabel, legendPlacement=legendPlacement, yLog=F)
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }   
}


plotRelativeFutureReturn <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                             stratName3=def$typicalValue, refStratName="stocks", 
                             futureYears=def$futureYears, xVect=TR$numericDate, 
                             col1=def$colBalanced, col2=def$colTechnical, 
                             col3=def$colValue, col4=def$colConstantAlloc, 
                             lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                             startYear=def$plotStartYear, endYear=def$plotEndYear, 
                             costs=def$tradingCost, 
                             minTR="", maxTR="", yLabel="", legendPlacement="topleft",
                             startIndex=def$startIndex, net=T, 
                             pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                             pngName=paste0("figures/return_over_next_", futureYears, 
                                            "_years-reference_", refStratName, ".png") ) { 
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
   
   para <- parametrizeFutureReturnPlot(costs*net, yLabel, futureYears, minTR, maxTR) 
   yLabel <- paste("relative", para[[1]])
   
   normDate <- (startYear-def$dataStartYear)*12+1
   
   stratNames <- c(stratName1, stratName2, stratName3, refStratName)
   numCurves <- length(stratNames)
   
   if (futureYears==5) 
      returnDF <- 100 *  next5yrs[, stratNames]
   else if (futureYears==10) 
      returnDF <- 100 * next10yrs[, stratNames]
   else if (futureYears==20) 
      returnDF <- 100 * next20yrs[, stratNames]
   else if (futureYears==30) 
      returnDF <- 100 * next30yrs[, stratNames]  
   
   if(net & costs>0) # reduce TR because of costs
      for (strat in 1:numCurves) 
         if (stratNames[strat] != "") {
            index  <- which(stats$strategy == stratNames[strat])
            returnDF[, strat] <- returnDF[, strat] - 100*costs/stats$turnover[index]
         }
   
   ## adjust endYear based on non-NA data
   i <- (endYear-def$dataStartYear)*12+1
   while ( sum(is.na(returnDF[i, ])) > 0 ) 
      i <- i-1
   endYear <- round( (i-1)/12 + def$dataStartYear ) + 1
   
   returnDF[, 1:numCurves] <- returnDF[, 1:numCurves] - returnDF[, 4]
   minTR <- min(returnDF,na.rm=T); maxTR <- max(returnDF,na.rm=T)
   
   plotSomeSortOfReturn(returnDF1=returnDF[, 1], returnDF2=returnDF[, 2], 
                        returnDF3=returnDF[, 3], returnDF4=returnDF[, 4],
                        xVect=xVect, stratName1=stratName1, stratName2=stratName2, 
                        stratName3=stratName3, stratName4=refStratName,
                        col1=col1, col2=col2, col3=col3, col4=col4, lwd1=lwd1, lwd2=lwd2, lwd3=lwd3, lwd4=lwd4,
                        startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, 
                        yLabel=yLabel, legendPlacement=legendPlacement, yLog=F)
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }   
}



plotFutureReturnVsStocks <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                                     stratName3=def$typicalValue, refStratName="stocks", 
                                     futureYears=def$futureYears, 
                                     col1=def$colBalanced, col2=def$colTechnical, col3=def$colValue,    
                                     pch1=1L, pch2=1L, pch3=1L,    
                                     startYear=def$plotStartYear, endYear=def$plotEndYear, 
                                     costs=def$tradingCost, showFit=T,
                                     minTR="", maxTR="", yLabel="", legendPlacement="topleft",
                                     startIndex=def$startIndex, net=T, 
                                     figureTitle="", 
                                     pngOutput=F, pngWidth=def$pngHeight, pngHeight=def$pngHeight, 
                                     pngName=paste0("figures/return_over_next_", futureYears, 
                                                    "_years_vs_", refStratName, ".png") ) { 
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
   
   para <- parametrizeFutureReturnPlot(costs*net, yLabel, futureYears, minTR, maxTR) 
   yLabel <- para[[1]]
   xLabel <- paste0("stocks annual return (%) over next ", futureYears, " years")
   if (figureTitle != "") par( oma = c( 0, 0, 1.5, 1.5 ) )
   
   stratNames <- c(stratName1, stratName2, stratName3, refStratName)
   numCurves <- length(stratNames)
   
   if (futureYears==5) 
      returnDF <- 100 *  next5yrs[, stratNames]
   else if (futureYears==10) 
      returnDF <- 100 * next10yrs[, stratNames]
   else if (futureYears==20) 
      returnDF <- 100 * next20yrs[, stratNames]
   else if (futureYears==30) 
      returnDF <- 100 * next30yrs[, stratNames]  
   
   if(net & costs>0) # reduce TR because of costs
      for (strat in 1:numCurves) 
         if (stratNames[strat] != "") {
            index  <- which(stats$strategy == stratNames[strat])
            returnDF[, strat] <- returnDF[, strat] - 100*costs/stats$turnover[index]
         }
   
   ## adjust endYear based on non-NA data
   i <- (endYear-def$dataStartYear)*12+1
   while ( sum(is.na(returnDF[i, ])) > 0 ) 
      i <- i-1
   endYear <- round( (i-1)/12 + def$dataStartYear ) + 1
   
   minTR <- min(returnDF,na.rm=T); maxTR <- max(returnDF,na.rm=T)
   
   par(mar=c(4.2, 4.2, 1.5, 1.5))
   xRange <- c(minTR, maxTR)
   yRange <- c(minTR, maxTR)
   
   if( stratName1 != "" ) {   
      plot(returnDF[, 4], returnDF[, 1], col=col1, xlab=xLabel, ylab=yLabel, pch=pch1, xlim=xRange, ylim=yRange)
      if (showFit) 
         abline( lm( returnDF[, 1] ~ returnDF[, 4], na.action=na.omit) , col=col1, lwd=2)
      par(new=T)
   }
   if( stratName2 != "" ) {   
      plot(returnDF[, 4], returnDF[, 2], col=col2, xlab="", ylab="", pch=pch2, xlim=xRange, ylim=yRange)
      if (showFit) 
         abline( lm( returnDF[, 2] ~ returnDF[, 4], na.action=na.omit) , col=col2, lwd=2)
      par(new=T)
   }  
   if( stratName3 != "" ) {   
      plot(returnDF[, 4], returnDF[, 3], col=col3, xlab="", ylab="", pch=pch3, xlim=xRange, ylim=yRange)
      if (showFit) 
         abline( lm( returnDF[, 3] ~ returnDF[, 4], na.action=na.omit) , col=col3, lwd=2)
      par(new=T)
   }
   legend( legendPlacement, c(stratName1,stratName2,stratName3), 
           bty="n", pch=c(pch1, pch2, pch3), col=c(col1, col2, col3) )
   par(new=F)
   
   if (figureTitle != "") title( figureTitle, outer = TRUE )
   par(oma = c( 0, 0, 0, 0 ))

   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }   
}

plotRelativeFutureReturnVsStocks <- function
         (stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
          stratName3=def$typicalValue, refStratName="stocks", 
          futureYears=def$futureYears, 
          col1=def$colBalanced, col2=def$colTechnical, col3=def$colValue,    
          pch1=1L, pch2=1L, pch3=1L,    
          startYear=def$plotStartYear, endYear=def$plotEndYear, 
          costs=def$tradingCost, showFit=T,
          minTR="", maxTR="", yLabel="", legendPlacement="topright",
          startIndex=def$startIndex, net=T, 
          figureTitle="", 
          pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
          pngName=paste0("figures/relative_return_over_next_", futureYears, 
                         "_years_vs_", refStratName, ".png") ) { 
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
   
   para <- parametrizeFutureReturnPlot(costs*net, yLabel, futureYears, minTR, maxTR) 
   yLabel <- paste("relative", para[[1]])
   xLabel <- paste0("stocks annual return (%) over next ", futureYears, " years")
   if (figureTitle != "") par( oma = c( 0, 0, 1.5, 0 ) )
   
   stratNames <- c(stratName1, stratName2, stratName3, refStratName)
   numCurves <- length(stratNames)
   
   if (futureYears==5) 
      returnDF <- 100 *  next5yrs[, stratNames]
   else if (futureYears==10) 
      returnDF <- 100 * next10yrs[, stratNames]
   else if (futureYears==20) 
      returnDF <- 100 * next20yrs[, stratNames]
   else if (futureYears==30) 
      returnDF <- 100 * next30yrs[, stratNames]  
   
   if(net & costs>0) # reduce TR because of costs
      for (strat in 1:numCurves) 
         if (stratNames[strat] != "") {
            index  <- which(stats$strategy == stratNames[strat])
            returnDF[, strat] <- returnDF[, strat] - 100*costs/stats$turnover[index]
         }
   
   ## adjust endYear based on non-NA data
   i <- (endYear-def$dataStartYear)*12+1
   while ( sum(is.na(returnDF[i, ])) > 0 ) 
      i <- i-1
   endYear <- round( (i-1)/12 + def$dataStartYear ) + 1
   
   for(strat in 1:3)
      returnDF[, strat] <- returnDF[, strat] - returnDF[, 4]
   
   xMin <- min(returnDF[, 4], na.rm=T);   xMax <- max(returnDF[, 4], na.rm=T)
   yMin <- min(returnDF[, 1:3], na.rm=T); yMax <- max(returnDF[, 1:3], na.rm=T)
   
   par(mar=c(4.2, 4.2, 1.5, 1.5))
   xRange <- c(xMin, xMax)
   yRange <- c(yMin, yMax)
   
   if( stratName1 != "" ) {   
      plot(returnDF[, 4], returnDF[, 1], col=col1, xlab=xLabel, ylab=yLabel, pch=pch1, xlim=xRange, ylim=yRange)
      if (showFit) 
         abline( lm( returnDF[, 1] ~ returnDF[, 4], na.action=na.omit) , col=col1, lwd=2)
      par(new=T)
   }
   if( stratName2 != "" ) {   
      plot(returnDF[, 4], returnDF[, 2], col=col2, xlab="", ylab="", pch=pch2, xlim=xRange, ylim=yRange)
      if (showFit) 
         abline( lm( returnDF[, 2] ~ returnDF[, 4], na.action=na.omit) , col=col2, lwd=2)
      par(new=T)
   }  
   if( stratName3 != "" ) {   
      plot(returnDF[, 4], returnDF[, 3], col=col3, xlab="", ylab="", pch=pch3, xlim=xRange, ylim=yRange)
      if (showFit) 
         abline( lm( returnDF[, 3] ~ returnDF[, 4], na.action=na.omit) , col=col3, lwd=2)
      par(new=T)
   }
   legend( legendPlacement, c(stratName1,stratName2,stratName3), 
           bty="n", pch=c(pch1, pch2, pch3), col=c(col1, col2, col3) )
   par(new=F)
   
   if (figureTitle != "") title( figureTitle, outer = TRUE )
   par(oma = c( 0, 0, 0, 0 ))
   
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }   
}

showPlotLegend <- function() {
   print(paste("CAPE:     ", def$colCAPE) )
   print(paste("detrended:", def$colDetrended) )
   print(paste("SMA:      ", def$colSMA) )
   print(paste("Bollinger:", def$colBollinger) )
   print(paste("reversal: ", def$colReversal) )
   print(paste("inflation:", def$colInflation) )
   print("")   
   print(paste("value:    ", def$colValue) )
   print(paste("technical:", def$colTechnical) )
   print(paste("balanced: ", def$colBalanced) )
   
#   print(paste("ConstantAlloc:", def$colConstantAlloc) )
print("")
}


## Basic scatter plot of some parameter on x and net return on y 
## (the next 4 kinds of plots are wrappers of this one)
plotAllReturnsVsSomeParameter <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, 
                                          type2=def$type2, col2=def$col2, pch2=def$pch2,
                                          type3=def$type3, col3=def$col3, pch3=def$pch3, 
                                          type4=def$type4, col4=def$col4, pch4=def$pch4,
                                          type5=def$type5, col5=def$col5, pch5=def$pch5, 
                                          type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                          Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                          Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                          Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                          lineCol=def$lineCol, searchPlotType="dots",
                                          xStatsName, xFactor=100, xLabel="volatility (%)",
                                          yStatsName="", yFactor=100,
                                          xMin, xMax, yMin=def$yTRmin, yMax=def$yTRmax, 
                                          costs=def$tradingCost, 
                                          pngOutput=F, pngWidth=def$pngWidth,
                                          pngHeight=def$pngHeight, pngName) { 
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)

   ## 'search' type is generated by searchForOptimalX() functions.
   ## Since there are many data points of this type we use open symbols for them.
   ## We look only for the last one: since they are produced in batches, 
   ## we expect them to be all of the same kind.  
   { # just so I can collapse it  
      i <- length(stats$type) 
      ## we start from the end to look for the most recent data
      while (i>=1 && (is.na(stats$type[i]) || stats$type[i] != def$Stype) )
         i <- i - 1
      
      Spch <- def$Spch
      Scol <- "purple" 
      ## we need Scol to exist even if there is no data point of type 'search'
      ## A purple point being actually plotted would be very fishy 
      ## as this color will be either overwritten or not used.
      
      if (i > 0) { # if there is at least one data point of type 'search'
         
         ## if there exist several subtypes, we throw a warning
         if (length (unique(stats$subtype[stats$type==def$Stype & !is.na(stats$type)]) ) > 1 ) 
            warning("All data points of type 'search' will be plotted with the same color.")
         
         if (stats$subtype[i] == def$type1) Scol <- def$col1
         if (stats$subtype[i] == def$type2) Scol <- def$col2
         if (stats$subtype[i] == def$type3) Scol <- def$col3
         if (stats$subtype[i] == def$type4) Scol <- def$col4
         if (stats$subtype[i] == def$type5) Scol <- def$col5
         if (stats$subtype[i] == def$type6) Scol <- def$col6
         
         if (stats$subtype[i] == def$Msubtype1) {
            Scol <- def$Mcol1
            Spch <- def$SpchM} # 
         if (stats$subtype[i] == def$Msubtype2) {
            Scol <- def$Mcol2
            Spch <- def$SpchM}
         if (stats$subtype[i] == def$Msubtype3) {
            Scol <- def$Mcol3
            Spch <- def$SpchM}
      }
   }
   
   if (searchPlotType=="line") {
      Stype <- "l"
      Spch <- ""
   } else if (searchPlotType=="dots") {
      Stype <- "p"
      Spch <- "."
   } else if (searchPlotType=="symbols")
      Stype <- "p"
   else stop(searchPlotType, " is not a legitimate value for argument searchPlotType, only line, dots or symbols")
      
   if (costs==0.5/100)
      def$yStatsName <<- "netTR0.5"
   else if (costs==2/100)
      def$yStatsName <<- "netTR2"
   else if (costs==4/100)
      def$yStatsName <<- "netTR4"
   else stop("No data frame \'netTR", round(costs*100), "\' exists.")
   
   xRange <- c(xMin, xMax)
   yRange <- c(yMin - 100*costs/2, yMax - 100*costs/4)
   par( mar=c(4.2, 4.2, 1.5, 1.5) )
      
   plot(xFactor*subset(stats[, xStatsName], stats$type==type1), 
        yFactor*subset(stats[, yStatsName], stats$type==type1), 
        pch=pch1, col=col1, xlim=xRange, ylim=yRange, 
        xlab=xLabel, ylab=paste0("total return (%), net of costs of ", round(costs*100, 1), "%") )
   points(xFactor*subset(stats[, xStatsName], stats$type==type2), 
        yFactor*subset(stats[, yStatsName], stats$type==type2), 
        pch=pch2, col=col2, xlab="", ylab="", xlim=xRange, ylim=yRange)
   points(xFactor*subset(stats[, xStatsName], stats$type==type3), 
        yFactor*subset(stats[, yStatsName], stats$type==type3), 
        pch=pch3, col=col3, xlab="", ylab="", xlim=xRange, ylim=yRange)
   points(xFactor*subset(stats[, xStatsName], stats$type==type4), 
          yFactor*subset(stats[, yStatsName], stats$type==type4), 
          pch=pch4, col=col4, xlab="", ylab="", xlim=xRange, ylim=yRange)
   points(xFactor*subset(stats[, xStatsName], stats$type==type5), 
        yFactor*subset(stats[, yStatsName], stats$type==type5), 
        pch=pch5, col=col5, xlab="", ylab="", xlim=xRange, ylim=yRange)
   points(xFactor*subset(stats[, xStatsName], stats$type==type6), 
        yFactor*subset(stats[, yStatsName], stats$type==type6), 
        pch=pch6, col=col6, xlab="", ylab="", xlim=xRange, ylim=yRange)
   ## Multistrategies:
   points(xFactor*subset(stats[, xStatsName], (stats$type=="combined" & stats$subtype==Msubtype1) ), 
        yFactor*subset(stats[, yStatsName], (stats$type=="combined" & stats$subtype==Msubtype1) ), 
        pch=Mpch1, col=Mcol1, xlab="", ylab="", xlim=xRange, ylim=yRange)
   points(xFactor*subset(stats[, xStatsName], (stats$type=="combined" & stats$subtype==Msubtype2) ), 
        yFactor*subset(stats[, yStatsName], (stats$type=="combined" & stats$subtype==Msubtype2) ), 
        pch=Mpch2, col=Mcol2, xlab="", ylab="", xlim=xRange, ylim=yRange)
   points(xFactor*subset(stats[, xStatsName], (stats$type=="combined" & stats$subtype==Msubtype3) ), 
        yFactor*subset(stats[, yStatsName], (stats$type=="combined" & stats$subtype==Msubtype3) ), 
        pch=Mpch3, col=Mcol3, xlab="", ylab="", xlim=xRange, ylim=yRange)
   ## Constant allocations:
   points(xFactor*subset(stats[, xStatsName], stats$type=="constantAlloc"), 
        yFactor*subset(stats[, yStatsName], stats$type=="constantAlloc"), 
        type="l", col=lineCol, xlab="", ylab="", xlim=xRange, ylim=yRange)
   ## search results:
   points(xFactor*subset(stats[, xStatsName], stats$type==def$Stype), 
        yFactor*subset(stats[, yStatsName], stats$type==def$Stype), 
        pch=Spch, type=Stype, col=Scol, xlab="", ylab="", xlim=xRange, ylim=yRange)
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }
}


plotAllReturnsVsVolatility <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, 
                                       type2=def$type2, col2=def$col2, pch2=def$pch2,
                                       type3=def$type3, col3=def$col3, pch3=def$pch3, 
                                       type4=def$type4, col4=def$col4, pch4=def$pch4,
                                       type5=def$type5, col5=def$col5, pch5=def$pch5, 
                                       type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                       Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                       Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                       Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                       lineCol=def$lineCol, searchPlotType="dots",
                                       xFactor=100, xLabel="volatility (%)",
                                       yStatsName=def$yStatsName, yFactor=100,
                                       xMin=def$minVol, xMax=def$maxVol, yMin=def$yTRmin, yMax=def$yTRmax, 
                                       costs=def$tradingCost,
                                       pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                       pngName="figures/return_vs_volatility.png") {
                                       
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 lineCol=lineCol, searchPlotType=searchPlotType,
                                 xStatsName="volatility", xFactor=xFactor, xLabel=xLabel,
                                 yStatsName=yStatsName, yFactor=yFactor,
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, costs=costs,
                                 pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}

plotAllReturnsVsDrawdown <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, 
                                     type2=def$type2, col2=def$col2, pch2=def$pch2,
                                     type3=def$type3, col3=def$col3, pch3=def$pch3, 
                                     type4=def$type4, col4=def$col4, pch4=def$pch4,
                                     type5=def$type5, col5=def$col5, pch5=def$pch5, 
                                     type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                     Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                     Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                     Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                     lineCol=def$lineCol, searchPlotType="dots",
                                     xFactor=1, xLabel="drawdowns",
                                     yStatsName=def$yStatsName, yFactor=100,
                                     xMin=def$minDD2, xMax=def$maxDD2, yMin=def$yTRmin, yMax=def$yTRmax, 
                                     costs=def$tradingCost,
                                     pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                     pngName="figures/return_vs_drawdown.png") { 
   
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 lineCol=lineCol, searchPlotType=searchPlotType,
                                 xStatsName="DD2", xFactor=xFactor, xLabel=xLabel,
                                 yStatsName=yStatsName, yFactor=yFactor,
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, costs=costs,
                                 pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}

plotAllReturnsVsAverageAlloc <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, 
                                         type2=def$type2, col2=def$col2, pch2=def$pch2,
                                         type3=def$type3, col3=def$col3, pch3=def$pch3, 
                                         type4=def$type4, col4=def$col4, pch4=def$pch4,
                                         type5=def$type5, col5=def$col5, pch5=def$pch5, 
                                         type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                         Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                         Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                         Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                         lineCol=def$lineCol, searchPlotType="dots",
                                         xFactor=100, xLabel="average stock allocation (%)",
                                         yStatsName=def$yStatsName, yFactor=100,
                                         xMin=40, xMax=97, yMin=def$yTRmin, yMax=def$yTRmax, 
                                         costs=def$tradingCost,
                                         pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                         pngName="figures/return_vs_average_alloc.png") { 
   
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 lineCol=lineCol, searchPlotType=searchPlotType,
                                 xStatsName="avgStockAlloc", xFactor=xFactor, xLabel=xLabel,
                                 yStatsName=yStatsName, yFactor=yFactor,
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, costs=costs,
                                 pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}

plotAllReturnsVsInverseTurnover <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, 
                                            type2=def$type2, col2=def$col2, pch2=def$pch2,
                                            type3=def$type3, col3=def$col3, pch3=def$pch3, 
                                            type4=def$type4, col4=def$col4, pch4=def$pch4,
                                            type5=def$type5, col5=def$col5, pch5=def$pch5, 
                                            type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                            Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                            Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                            Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                            lineCol=def$lineCol,  searchPlotType="dots",
                                            xFactor=100, xLabel="100 / turnover (years)",
                                            yStatsName=def$yStatsName, yFactor=100,
                                            xMin=2, xMax=100, yMin=def$yTRmin, yMax=def$yTRmax, 
                                            costs=def$tradingCost,
                                            pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                            pngName="figures/return_vs_inverse_turnover.png") { 
   
   
   stats$invTurnover <<- 1 / stats$turnover
   
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 lineCol=lineCol, searchPlotType=searchPlotType,
                                 xStatsName="invTurnover", xFactor=xFactor, xLabel=xLabel,
                                 yStatsName=yStatsName, yFactor=yFactor,
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, costs=costs,
                                 pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
   stats$invTurnover = NULL
}


plotAllReturnsVsFour <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, 
                                 type2=def$type2, col2=def$col2, pch2=def$pch2,
                                 type3=def$type3, col3=def$col3, pch3=def$pch3, 
                                 type4=def$type4, col4=def$col4, pch4=def$pch4,
                                 type5=def$type5, col5=def$col5, pch5=def$pch5, 
                                 type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                 Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                 Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                 Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                 lineCol=def$lineCol, searchPlotType="dots",
                                 xMinVol=def$minVol, xMaxVol=def$maxVol, xMinDD2=def$minDD2, xMaxDD2=def$maxDD2, 
                                 xMinAlloc=40, xMaxAlloc=98, xMinTO=2, xMaxTO=100, 
                                 yStatsName=def$yStatsName, yFactor=100,
                                 yMin=def$yTRmin, yMax=def$yTRmax, costs=def$tradingCost,
                                 figureTitle="", 
                                 pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                 pngName="figures/return_vs_four.png") {
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
   
   par(mfrow = c(2, 2))
   if (figureTitle != "") par( oma = c( 0, 0, 1.5, 0 ) )
   
   
   plotAllReturnsVsVolatility(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                              type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                              type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6,  
                              Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                              Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                              Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                              lineCol=lineCol, searchPlotType=searchPlotType,
                              yStatsName=yStatsName, yFactor=yFactor,
                              xMin=xMinVol, xMax=xMaxVol, yMin=yMin, yMax=yMax, costs=costs) 
   
   plotAllReturnsVsDrawdown(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                            type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                            type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                            Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                            Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                            Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                            lineCol=lineCol, searchPlotType=searchPlotType,
                            yStatsName=yStatsName, yFactor=yFactor,
                            xMin=xMinDD2, xMax=xMaxDD2, yMin=yMin, yMax=yMax, costs=costs) 
   
   plotAllReturnsVsAverageAlloc(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                lineCol=lineCol, searchPlotType=searchPlotType,
                                yStatsName=yStatsName, yFactor=yFactor,
                                xMin=xMinAlloc, xMax=xMaxAlloc, yMin=yMin, yMax=yMax, costs=costs) 
   
   plotAllReturnsVsInverseTurnover(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                   type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                   type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                   Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                   Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                   Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                   lineCol=lineCol, searchPlotType=searchPlotType,
                                   yStatsName=yStatsName, yFactor=yFactor,
                                   xMin=xMinTO, xMax=xMaxTO, yMin=yMin, yMax=yMax, costs=costs) 
   
   if (figureTitle != "") title( figureTitle, outer = TRUE )
   par(mfrow = c(1, 1), oma = c( 0, 0, 0, 0 ))
      
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }
}


plotAllReturnsVsTwo <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, 
                                type2=def$type2, col2=def$col2, pch2=def$pch2,
                                type3=def$type3, col3=def$col3, pch3=def$pch3, 
                                type4=def$type4, col4=def$col4, pch4=def$pch4,
                                type5=def$type5, col5=def$col5, pch5=def$pch5, 
                                type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                lineCol=def$lineCol, searchPlotType="dots",
                                xMinVol=def$minVol, xMaxVol=def$maxVol, xMinDD2=def$minDD2, xMaxDD2=def$maxDD2, 
                                xMinAlloc=40, xMaxAlloc=98, xMinTO=2, xMaxTO=100, 
                                yStatsName=def$yStatsName, yFactor=100, col=F,
                                yMin=def$yTRmin, yMax=def$yTRmax, costs=def$tradingCost,
                                figureTitle="", 
                                pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                pngName="figures/return_vs_two.png") {
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
   
   if (col)
      par(mfrow = c(2, 1))
   else par(mfrow = c(1, 2))
   if (figureTitle != "") par( oma = c( 0, 0, 1.5, 0 ) )
   
   plotAllReturnsVsVolatility(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                              type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                              type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6,  
                              Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                              Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                              Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                              lineCol=lineCol, searchPlotType=searchPlotType,
                              yStatsName=yStatsName, yFactor=yFactor,
                              xMin=xMinVol, xMax=xMaxVol, yMin=yMin, yMax=yMax, costs=costs) 
   
   plotAllReturnsVsDrawdown(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                            type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                            type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                            Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                            Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                            Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                            lineCol=lineCol, searchPlotType=searchPlotType,
                            yStatsName=yStatsName, yFactor=yFactor,
                            xMin=xMinDD2, xMax=xMaxDD2, yMin=yMin, yMax=yMax, costs=costs) 
   if (figureTitle != "") title( figureTitle, outer = TRUE )
   par(mfrow = c(1, 1), oma = c( 0, 0, 0, 0 ))
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }
}


saveAllPlotsAsPng <- function(pngWidth=def$pngWidth, pngHeight=def$pngHeight) {
   plotReturn(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
   plotAlloc(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
   plotReturnAndAlloc(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
   
   plotAllReturnsVsVolatility(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
   plotAllReturnsVsDrawdown(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
   plotAllReturnsVsAverageAlloc(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
   plotAllReturnsVsInverseTurnover(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
   plotAllReturnsVsFour(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
   plotAllReturnsVsTwo(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
}


plotCumulativeFutureReturn <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                                       stratName3=def$typicalValue, stratName4="stocks", 
                                       futureYears=def$futureYears, net=T, costs=def$tradingCost,
                                       col1=def$colBalanced, col2=def$colTechnical, 
                                       col3=def$colValue, col4=def$colConstantAlloc, 
                                       lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,                                  
                                       figureTitle="", 
                                       pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                       pngName=paste0("figures/cumulative_return_over_next_",
                                                      futureYears,"_years.png") ) {
 
   if(pngOutput) 
      png(file=pngName, width=pngWidth, height=pngHeight)
    
   if( !net | costs==0 )  # cost = 0: gross returns
      yLabel <- paste0("annual return (%) over next ", futureYears, " years, GROSS of costs")
   else if( costs>0 ) # cost > 0: net returns
      yLabel <- paste0("annual return (%) over next ", futureYears, 
                       " years, net of costs of ", round(costs*100, 1), "%")   
   else # no notion of cost (e.g. for asset classes)
      yLabel <- paste0("annual return (%) over next ", futureYears, " years")

   
   stratNames <- c(stratName1, stratName2, stratName3, stratName4)
   numCurves <- length(stratNames)
   
   if (futureYears==5) 
      returnDF <- 100 *  next5yrs[, stratNames]
   else if (futureYears==10) 
      returnDF <- 100 * next10yrs[, stratNames]
   else if (futureYears==20) 
      returnDF <- 100 * next20yrs[, stratNames]
   else if (futureYears==30) 
      returnDF <- 100 * next30yrs[, stratNames]  
      
   cp <- complete.cases(returnDF)
   
   if(net & costs>0) # reduce TR because of costs
      for (strat in 1:numCurves) 
         if (stratNames[strat] != "") {
            index <- which(stats$strategy == stratNames[strat])
            returnDF[cp, strat] <- returnDF[cp, strat] - 100*costs/stats$turnover[index]
         }
   
   minTR <- min(returnDF[cp, ])
   maxTR <- max(returnDF[cp, ])
   par(mar=c(4.2, 4.2, 1.5, 1.5))
   if (figureTitle != "") par( oma = c( 0, 0, 1.5, 0 ) )
   
   plot(quantile(returnDF[cp, 1], probs = seq(0, 1, 0.01)), type="l", col=col1, lwd=lwd1, ylim=c(minTR,maxTR),
        main="", xlab="cumulative frequency (%)", ylab=yLabel)
   if(stratName2!="")
      lines(quantile(returnDF[cp, 2], probs = seq(0, 1, 0.01)), col=col2, lwd=lwd2 )
   if(stratName3!="")
      lines(quantile(returnDF[cp, 3], probs = seq(0, 1, 0.01)), col=col3, lwd=lwd3 )
   if(stratName4!="")
      lines(quantile(returnDF[cp, 4], probs = seq(0, 1, 0.01)), col=col4, lwd=lwd4 )
   
   legend( "topleft", stratNames, 
           bty="n", lwd=c(lwd1, lwd2, lwd3, lwd4), lty=1, col=c(col1, col2, col3, col4) )
   
   if (figureTitle != "") title( figureTitle, outer = TRUE )
   par(oma = c( 0, 0, 0, 0 ))
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }   
}



createStrategiesAndSavePlots_generic <- function(futureYears, tradingCost, dataSplit,
                                                 startYear, endYear, figureTitle, figureTitleShort,
                                                 pngWidth=def$pngWidth, pngHeight=def$pngHeight) {
   
   start(dataSplit=dataSplit, futureYears=futureYears, tradingCost=tradingCost, 
         extrapolateDividends=T, smoothConstantAlloc=T, downloadAndCheckAllFiles=F,
         otherAssetClasses=F, newcomer=F, force=T)
   rangeName         <- paste0(startYear, "_", endYear)
   rangeNameShort    <- paste0(startYear, "_", endYear-futureYears)   
   
   plotAllReturnsVsFour(yMin=5, pngOutput=T, pngName=paste0("figures/", rangeName, "-return_vs_four.png"), 
                        pngWidth=pngWidth, pngHeight=pngHeight, 
                        figureTitle=figureTitle )
   plotReturnAndAlloc(pngOutput=T, pngName=paste0("figures/", rangeName, "-return_and_allocation.png"), 
                      pngWidth=pngWidth, pngHeight=pngHeight)
   plotFutureReturnVsCAPE(futureYears=futureYears, maxTR=21, 
                          pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight, 
                          pngName=paste0("figures/", rangeNameShort, "-return_over_next_", 
                                         futureYears, "_years_vs_CAPE.png"),
                          figureTitle=figureTitleShort)
   plotRelativeFutureReturn(futureYears=futureYears, refStratName="stocks", 
                            legendPlacement="topleft", 
                            pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight,
                            pngName=paste0("figures/", rangeNameShort, "-return_over_next_", 
                                           futureYears, "_years-reference_stocks.png"))
   plotFutureReturn(futureYears=futureYears, pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight,
                    pngName=paste0("figures/", rangeNameShort, "-return_over_next_", futureYears, "_years.png"))
   plotCumulativeFutureReturn(futureYears=futureYears, pngOutput=T, 
                              pngName=paste0("figures/", rangeNameShort, 
                                             "-cumulative_return_over_next_", futureYears,"_years.png"),
                              figureTitle=figureTitleShort )
   plotRelativeFutureReturnVsStocks(futureYears=futureYears, pngOutput=T, 
                                    pngName=paste0("figures/", rangeNameShort, "-relative_return_over_next_", 
                                                   futureYears, "_years_vs_stocks.png"),
                                    figureTitle=figureTitleShort )
   #    searchForOptimalBalanced(byF1=2.02, byF2=0.02)
   #    plotAllReturnsVsFour(searchPlotType="line", pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight, 
   #                         pngName="figures/1871_1942-balanced_optimization.png")
}

createStrategiesAndSavePlots_search <- function(futureYears, tradingCost, 
                                                pngWidth=def$pngWidth, pngHeight=def$pngHeight) {
   
   figureTitle      <- paste0("1871-1942 (optimization range)")
   figureTitleShort <- paste0("1871-", 1942-futureYears, " (optimization range)")   
   createStrategiesAndSavePlots_generic(futureYears=futureYears, tradingCost=tradingCost, dataSplit="search",
                                        startYear=1871, endYear=1942, figureTitle, figureTitleShort,
                                        pngWidth=pngWidth, pngHeight=pngHeight) 
}
   
createStrategiesAndSavePlots_testing <- function(futureYears, tradingCost, 
                                                 pngWidth=def$pngWidth, pngHeight=def$pngHeight) {
   
   figureTitle      <- paste0("1942-2014 (parameters optimized over 1871-1942)")
   figureTitleShort <- paste0("1942-", 2014-futureYears, " (parameters optimized over 1871-1942)")   
   createStrategiesAndSavePlots_generic(futureYears=futureYears, tradingCost=tradingCost, dataSplit="testing",
                                        startYear=1942, endYear=2014, figureTitle, figureTitleShort,
                                        pngWidth=pngWidth, pngHeight=pngHeight) 
}



createStrategiesAndSavePlots <- function(futureYears=10L, tradingCost=0.5/100, 
                                         pngWidth=def$pngWidth, pngHeight=def$pngHeight) {
   source("init.r")       # for start()
      
   createStrategiesAndSavePlots_search(futureYears=futureYears, tradingCost=tradingCost, 
                                       pngWidth=def$pngWidth, pngHeight=def$pngHeight)
   
   createStrategiesAndSavePlots_testing(futureYears=futureYears, tradingCost=tradingCost, 
                                       pngWidth=def$pngWidth, pngHeight=def$pngHeight)
   
   ## we plot returns over next 20 years (if we have not done all this already)
   ## there will be some plots that will be replotted without need (those independent of 'futureYears'), but well...
   newFutureYears <- 20L
   if (futureYears != newFutureYears)
      createStrategiesAndSavePlots_testing(futureYears=newFutureYears, tradingCost=tradingCost, 
                                        pngWidth=def$pngWidth, pngHeight=def$pngHeight)

   
}


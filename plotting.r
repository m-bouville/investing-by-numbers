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
   
   def$plotEndYear <<- 2015
   def$maxTR     <<- 10000
   
   costs <- (def$tradingCost + def$riskAsCost)
   if (costs==0.5/100)
      def$yStatsName <<- "netTR0.5"
   else if (costs==2/100)
      def$yStatsName <<- "netTR2"
   else if (costs==4/100)
      def$yStatsName <<- "netTR4"
   else stop("No data frame \'netTR", round(tradingCost*100), "\' exists.")
   
   #Colors consistently used for the various tsrategies
   def$colCAPE      <<- "cyan"
   def$colDetrended <<- "skyblue"
   def$colSMA       <<- "pink"
   def$colBollinger <<- "magenta"   
   def$colReversal  <<- "orange"

   def$colValue     <<- "blue"
   def$colTechnical <<- "red"
   def$colBalanced  <<- "black"

   def$colConstantAlloc  <<- "green"
   
   # plotAllReturnsVsX()
   def$type1 <<- "CAPE"
   def$col1  <<- def$colCAPE
   def$pch1  <<- 16L
   
   def$type2 <<- "detrended"
   def$col2  <<- def$colDetrended
   def$pch2  <<- 16L
   
   def$type4 <<- "SMA"
   def$col4  <<- def$colSMA
   def$pch4  <<- 16L
   
   def$type5 <<- "Bollinger"
   def$col5  <<- def$colBollinger
   def$pch5  <<- 16L
   
   def$type6 <<- "reversal"
   def$col6  <<- def$colReversal
   def$pch6  <<- 16L
   
   # plotAllReturnsVsX(): combined strategies
   def$Msubtype1 <<- "value"
   def$Mcol1     <<- def$colValue
   def$Mpch1     <<- 15L
   
   def$Msubtype2 <<- "technical"
   def$Mcol2     <<- def$colTechnical
   def$Mpch2     <<- 15L
   
   def$Msubtype3 <<- "balanced"
   def$Mcol3     <<- def$colBalanced
   def$Mpch3     <<- 15L
   
   # plotAllReturnsVsX(): searches
   def$Stype    <<- "search"
   def$Scol     <<- NA # we need the subtype to set the color, we do that in the plotting function
   def$Spch     <<- 16L # default symbol
   def$SpchM    <<- 15L # empty square for combined strategies
   
   def$lineCol   <<- def$colConstantAlloc
}


plotAssetClassesReturn <- function(stratName1="stocks",       col1=def$colConstantAlloc, lwd1=2,
                                   stratName2="bonds",        col2="darkgreen",          lwd2=2,
                                   stratName3="UKhousePrice", col3="grey",               lwd3=2,
                                   stratName4="gold",         col4="gold",               lwd4=2, 
                                   startYear=1975.25, endYear=def$plotEndYear, 
                                   yLabel="", net=F, minTR=.5, maxTR=15,
                                   pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                   pngName="figures/asset_classes_returns.png") {

   requireColInDat("gold")
   requireColInDat("UKhousePrice")
   plotReturn(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
              stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
              startYear=startYear, endYear=endYear, tradingCost="", 
              yLabel=yLabel, net=net, minTR=minTR, maxTR=maxTR,
              pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}


## this takes as input a data frame containing gross or net returns, 
plotSomeSortOfReturn <- function(returnDF1, returnDF2, returnDF3, returnDF4,
                                 stratName1, stratName2, stratName3, stratName4,
                                 col1, col2, col3, col4, lwd1, lwd2, lwd3, lwd4,
                                 startYear, endYear, minTR, maxTR, yLabel, yLog) {      

   par(mar=c(2.7, 4.2, 1.5, 1.5))
   xRange <- c(startYear, endYear)
   yRange <- c(minTR, maxTR)
   if (yLog) 
      logAxis<-"y"
   else logAxis<-""
            
   if( !is.null(returnDF1) ) {   
      plot(TR$numericDate, returnDF1, col=col1, xlab="", ylab=yLabel, 
           log=logAxis, type="l", lwd=lwd1, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if( !is.null(returnDF2) ) {   
      plot(TR$numericDate, returnDF2, col=col2, xlab="", ylab="", 
           log=logAxis, type="l", lwd=lwd2, xlim=xRange, ylim=yRange)
      par(new=T)
   }  
   if( !is.null(returnDF3) ) {   
      plot(TR$numericDate, returnDF3, col=col3, xlab="", ylab="", 
           log=logAxis, type="l", lwd=lwd3, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if( !is.null(returnDF4) ) {   
      plot(TR$numericDate, returnDF4, col=col4, xlab="", ylab="", 
           log=logAxis, type="l", lwd=lwd4, xlim=xRange, ylim=yRange)
   }
   legend( "topleft", c(stratName1,stratName2,stratName3,stratName4), 
          bty="n", lwd=c(lwd1, lwd2, lwd3, lwd4), lty = c(1,1,1,1), 
          col=c(col1, col2, col3, col4) )
   par(new=F)
}


##Wrapper passing the proper vectors to plotSomeSortOfReturn()
plotReturn <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                       stratName3=def$typicalValue, stratName4="stocks", 
                       col1=def$colBalanced, col2=def$colTechnical, col3=def$colValue, col4=def$colConstantAlloc, 
                       lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                       startYear=def$plotStartYear, endYear=def$plotEndYear, 
                       tradingCost=def$tradingCost, riskAsCost=def$riskAsCost,
                       minTR=.9, maxTR=def$maxTR, yLabel="", 
                       startIndex=def$startIndex, net=T, normalize=T,
                       pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, pngName="figures/return.png") {   
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
   
   costs <- (tradingCost+riskAsCost)
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
                        stratName1=stratName1, stratName2=stratName2, 
                        stratName3=stratName3, stratName4=stratName4,
                        col1=col1, col2=col2, col3=col3, col4=col4, lwd1=lwd1, lwd2=lwd2, lwd3=lwd3, lwd4=lwd4,
                        startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, 
                        yLabel=yLabel, yLog=T) 
   
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


plotReturnAndAlloc <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                               stratName3=def$typicalValue, stratName4="stocks", 
                               col1=def$colBalanced, col2=def$colTechnical, 
                               col3=def$colValue, col4=def$colConstantAlloc, 
                               lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                               startYear=def$plotStartYear, endYear=def$plotEndYear, tradingCost=def$tradingCost, 
                               minTR=.9, maxTR=def$maxTR, yLabelReturn="", net=T, normalize=T,
                               pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                               pngName="figures/return_and_allocation.png") {
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)

   par(mfrow = c(2, 1))
   plotReturn(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
              stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
              startYear=startYear, endYear=endYear, tradingCost=tradingCost, 
              minTR=minTR, maxTR=maxTR, normalize=normalize, yLabel=yLabelReturn, net=net)  
   plotAlloc(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
             stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
             startYear=startYear, endYear=endYear)        
   par(mfrow = c(1, 1))
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }
}


plotFutureReturn <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                             stratName3=def$typicalValue, stratName4="stocks", 
                             futureYears=def$futureYears,
                             col1=def$colBalanced, col2=def$colTechnical, 
                             col3=def$colValue, col4=def$colConstantAlloc, 
                             lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                             startYear=def$plotStartYear, endYear=def$plotEndYear, 
                             tradingCost=def$tradingCost, riskAsCost=def$riskAsCost,
                             minTR="", maxTR="", yLabel="", 
                             startIndex=def$startIndex, net=T, normalize=F,
                             pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                             pngName="figures/future_return.png") { 
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
          
   costs <- (tradingCost+riskAsCost)
   if ( yLabel=="" ) {
      if( costs==0 )  # cost = 0: gross returns
         yLabel <- paste0("annual return (%) over next ", futureYears, " years, GROSS of costs")
      else if( costs>0 ) # cost > 0: net returns
         yLabel <- paste0("annual return (%) over next ", futureYears, 
                          " years, net of costs of ", round(costs*100, 1), "%")   
      else # no notion of cost (e.g. for asset classes)
         yLabel <- paste0("annual return (%) over next ", futureYears, " years")
   }
   
   normDate <- (startYear-def$dataStartYear)*12+1
   
   stratNames <- c(stratName1, stratName2, stratName3, stratName4)
   
   if (futureYears==10) {
      returnDF <- 100 * next10yrs[, stratNames]
      if (minTR=="") minTR <- -5
      if (maxTR=="") maxTR <- 20
   } else if (futureYears==20) {
      returnDF <- 100 * next20yrs[, stratNames]
      if (minTR=="") minTR <-  0
      if (maxTR=="") maxTR <- 16
   } else if (futureYears==30) {
      returnDF <- 100 * next30yrs[, stratNames]
      if (minTR=="") minTR <-  3
      if (maxTR=="") maxTR <- 13
   }
   
   if(net & costs>0) # reduce TR because of costs
      for (strat in 1:4) {
         index <- which(stats$strategy == stratNames[strat])
         TOcost <- costs/12/stats$turnover[index]
         for (i in startIndex:numData)
            returnDF[i, strat] <- returnDF[i, strat] * exp(-TOcost*(i-startIndex))
      }
   
   if (normalize) 
      lapply( 1:4, function(i) returnDF[, i] <<- returnDF[, i] / returnDF[normDate, i] )
   
   ## adjust endYear based on non-NA data
   i <- (endYear-def$dataStartYear)*12+1
   while ( sum(is.na(returnDF[i, ])) > 0 ) 
   i <- i-1
   endYear <- round( (i-1)/12 + def$dataStartYear ) + 1
   print(startYear)
   
   plotSomeSortOfReturn(returnDF1=returnDF[, 1], returnDF2=returnDF[, 2], 
                        returnDF3=returnDF[, 3], returnDF4=returnDF[, 4],
                        stratName1=stratName1, stratName2=stratName2, 
                        stratName3=stratName3, stratName4=stratName4,
                        col1=col1, col2=col2, col3=col3, col4=col4, lwd1=lwd1, lwd2=lwd2, lwd3=lwd3, lwd4=lwd4,
                        startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, 
                        yLabel=yLabel, yLog=F) 
   
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
                                          yStatsName=def$yStatsName, yFactor=100,
                                          xMin, xMax, yMin=def$yTRmin, yMax=def$yTRmax, 
                                          tradingCost=def$tradingCost, riskAsCost=def$riskAsCost,
                                          pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, pngName) { 
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)

   ## 'search' type is generated by searchForOptimalX() functions.
   ## Since there are many data points of this type we use open symbols for them.
   ## We look only for the last one: since they are produced in batches, we expect them to be all of the same kind.  
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
      
   costs <- (tradingCost+riskAsCost)
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
                                       tradingCost=def$tradingCost,
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
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, tradingCost=tradingCost,
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
                                     tradingCost=def$tradingCost,
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
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, tradingCost=tradingCost,
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
                                         xMin=40, xMax=98, yMin=def$yTRmin, yMax=def$yTRmax, 
                                         tradingCost=def$tradingCost,
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
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, tradingCost=tradingCost,
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
                                            xMin=3.5, xMax=100, yMin=def$yTRmin, yMax=def$yTRmax, 
                                            tradingCost=def$tradingCost,
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
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, tradingCost=tradingCost,
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
                                 xMinAlloc=40, xMaxAlloc=98, xMinTO=3.5, xMaxTO=100, 
                                 yStatsName=def$yStatsName, yFactor=100,
                                 yMin=def$yTRmin, yMax=def$yTRmax, tradingCost=def$tradingCost,
                                 pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                 pngName="figures/return_vs_four.png") {
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
   
   par(mfrow = c(2, 2))
   
   plotAllReturnsVsVolatility(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                              type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                              type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6,  
                              Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                              Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                              Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                              lineCol=lineCol, searchPlotType=searchPlotType,
                              yStatsName=yStatsName, yFactor=yFactor,
                              xMin=xMinVol, xMax=xMaxVol, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   
   plotAllReturnsVsDrawdown(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                            type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                            type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                            Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                            Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                            Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                            lineCol=lineCol, searchPlotType=searchPlotType,
                            yStatsName=yStatsName, yFactor=yFactor,
                            xMin=xMinDD2, xMax=xMaxDD2, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   
   plotAllReturnsVsAverageAlloc(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                lineCol=lineCol, searchPlotType=searchPlotType,
                                yStatsName=yStatsName, yFactor=yFactor,
                                xMin=xMinAlloc, xMax=xMaxAlloc, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   
   plotAllReturnsVsInverseTurnover(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                   type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                   type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                   Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                   Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                   Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                   lineCol=lineCol, searchPlotType=searchPlotType,
                                   yStatsName=yStatsName, yFactor=yFactor,
                                   xMin=xMinTO, xMax=xMaxTO, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   
   par(mfrow = c(1, 1))
   
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
                                xMinAlloc=40, xMaxAlloc=100, xMinTO=0, xMaxTO=100, 
                                yStatsName=def$yStatsName, yFactor=100, col=T,
                                yMin=def$yTRmin, yMax=def$yTRmax, tradingCost=def$tradingCost,
                                pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                pngName="figures/return_vs_two.png") {
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
   
   if (col)
      par(mfrow = c(2, 1))
   else par(mfrow = c(1, 2))
   
   plotAllReturnsVsVolatility(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                              type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                              type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6,  
                              Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                              Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                              Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                              lineCol=lineCol, searchPlotType=searchPlotType,
                              yStatsName=yStatsName, yFactor=yFactor,
                              xMin=xMinVol, xMax=xMaxVol, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   
   plotAllReturnsVsDrawdown(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                            type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                            type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                            Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                            Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                            Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                            lineCol=lineCol, searchPlotType=searchPlotType,
                            yStatsName=yStatsName, yFactor=yFactor,
                            xMin=xMinDD2, xMax=xMaxDD2, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   par(mfrow = c(1, 1))
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }
}


createAllPlotsAsPng <- function(pngWidth=def$pngWidth, pngHeight=def$pngHeight) {
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

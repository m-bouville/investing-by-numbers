#default values of plotting parameters
setPlottingDefaultValues <- function() {
   def$yTRmin<<- 7
   def$yTRmax<<- 8.5

   if (def$tradingCost==0.02)
   def$yStatsName <<- "netTR2"
   else if (def$tradingCost==0.04)
      def$yStatsName <<- "netTR4"
   else stop("No data frame \'netTR", round(tradingCost*100), "\' exists.")
   
   #Colors consistently used for the various tsrategies
   def$colCAPE      <<- "cyan"
   def$colDetrended <<- "skyblue"
   def$colMomentum  <<- "orange"
   def$colSMA       <<- "orangered1"
   def$colBollinger <<- "magenta"   
   def$colReversal  <<- "yellow"

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
   
   def$type3 <<- "momentum"
   def$col3  <<- def$colMomentum
   def$pch3  <<- 16L
   
   def$type4 <<- "SMA"
   def$col4  <<- def$colSMA
   def$pch4  <<- 16L
   
   def$type5 <<- "Bollinger"
   def$col5  <<- def$colBollinger
   def$pch5  <<- 16L
   
   def$type6 <<- "reversal"
   def$col6  <<- def$colReversal
   def$pch6  <<- 16L
   
   # plotAllReturnsVsX(): multistrategies
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
   def$Spch     <<- 1L # default symbol
   def$SpchM    <<- 0L # empty square for multistrategies
   
   def$lineCol   <<- def$colConstantAlloc
}


plotAssetClassesReturn <- function(stratName1="stocks",       col1=def$colConstantAlloc, lwd1=2,
                                   stratName2="bonds",        col2="darkgreen",          lwd2=2,
                                   stratName3="UKhousePrice", col3="grey",               lwd3=2,
                                   stratName4="gold",         col4="gold",               lwd4=2, 
                                   startYear=1975.25, endYear=2014, 
                                   yLabel="", net=F, minTR=.5, maxTR=15) {
   plotReturn(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
              stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
              startYear=startYear, endYear=endYear, tradingCost="", yLabel=yLabel, net=net, minTR=minTR, maxTR=maxTR) 
}


plotReturn <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                       stratName3=def$typicalValue, stratName4="stocks", 
                       col1=def$colBalanced, col2=def$colTechnical, col3=def$colValue, col4=def$colConstantAlloc, 
                       lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                       startYear=def$startYear, endYear=2015, tradingCost=def$tradingCost, 
                       minTR=.9, maxTR=10000, yLabel="", net=T, normalize=T) { 
   
#    print(c(stratName2, stratName3, stratName4) )
   normDate <- (startYear-1871)*12+1
   par(mar=c(2.5, 4, 1.5, 1.5))
   xRange <- c(startYear, endYear)
   yRange <- c(minTR, maxTR)
   
   TR1 <- numeric(numData)
   TR2 <- numeric(numData)
   TR3 <- numeric(numData)
   TR4 <- numeric(numData)
   
   if ( yLabel=="" ) {
      if( tradingCost==0 )  # trading cost = 0: gross returns
         yLabel <- paste0("total return (%), GROSS of trading costs")
      else if( tradingCost>0 ) # trading cost > 0: net returns
         yLabel <- paste0("total return (%), net of trading costs of ", round(tradingCost*100), "%")   
      else # no notion of trading cost (e.g. for asset classes)
         yLabel <- paste0("total return (%)")
   }
   
   if(tradingCost==0) net<-F
   if(net) {      
      if (tradingCost == 0.02) {
         if (!stratName1 %in% colnames(netTR2)) calcTRnetOfTradingCost(stratName1)
         if (!stratName2 %in% colnames(netTR2)) calcTRnetOfTradingCost(stratName2)
         if (!stratName3 %in% colnames(netTR2)) calcTRnetOfTradingCost(stratName3)
         if (!stratName4 %in% colnames(netTR2)) calcTRnetOfTradingCost(stratName4)
         TR1 <- netTR2[, stratName1] 
         TR2 <- netTR2[, stratName2] 
         TR3 <- netTR2[, stratName3] 
         TR4 <- netTR2[, stratName4] 
      } 
      else if(tradingCost == 0.04)  {
         if (!stratName1 %in% colnames(netTR4)) calcTRnetOfTradingCost(stratName1)
         if (!stratName2 %in% colnames(netTR4)) calcTRnetOfTradingCost(stratName2)
         if (!stratName3 %in% colnames(netTR4)) calcTRnetOfTradingCost(stratName3)
         if (!stratName4 %in% colnames(netTR4)) calcTRnetOfTradingCost(stratName4)
         TR1 <- netTR4[, stratName1] 
         TR2 <- netTR4[, stratName2] 
         TR3 <- netTR4[, stratName3] 
         TR4 <- netTR4[, stratName4] 
      } 
      else stop("No data frame \'netTR", round(tradingCost*100,1), "\' exists.")      
   } else {
      TR1 <- TR[, stratName1]
      TR2 <- TR[, stratName2]
      TR3 <- TR[, stratName3]
      TR4 <- TR[, stratName4]
   }

   if (normalize) {
      normFactor1 <- TR1[normDate]
      normFactor2 <- TR2[normDate]
      normFactor3 <- TR3[normDate]
      normFactor4 <- TR4[normDate]
   } else {      
      normFactor1 <- 1
      normFactor2 <- 1
      normFactor3 <- 1
      normFactor4 <- 1
   }
      
   if((stratName1 != "") && stratName1 %in% colnames(TR)) {   
      plot(TR$numericDate, TR1/normFactor1, col=col1, xlab="", ylab=yLabel, log="y", type="l", lwd=lwd1, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if((stratName2 != "") && stratName2 %in% colnames(TR)) {   
      plot(TR$numericDate, TR2/normFactor2, col=col2, xlab="", ylab="", log="y", type="l", lwd=lwd2, xlim=xRange, ylim=yRange)
      par(new=T)
   }  
   if((stratName3 != "") && stratName3 %in% colnames(TR)) {   
      plot(TR$numericDate, TR3/normFactor3, col=col3, xlab="", ylab="", log="y", type="l", lwd=lwd3, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if((stratName4 != "") && stratName4 %in% colnames(TR)) {   
      plot(TR$numericDate, TR4/normFactor4, col=col4, xlab="", ylab="", log="y", type="l", lwd=lwd4, xlim=xRange, ylim=yRange)
   }
   legend( "topleft", c(stratName1,stratName2,stratName3,stratName4), 
          bty="n", lwd=c(lwd1, lwd2, lwd3, lwd4), lty = c(1,1,1,1), 
          col=c(col1, col2, col3, col4) )
   par(new=F)
}

plotAlloc <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                      stratName3=def$typicalValue, stratName4="stocks", 
                      col1=def$colBalanced, col2=def$colTechnical, col3=def$colValue, col4=def$colConstantAlloc, 
                      lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                      startYear=def$startYear, endYear=2014) { 
   
   normDate <- (startYear-1871)*12+1
   par(mar=c(2.5, 4, 1.5, 1.5))
   xRange <- c(startYear, endYear)
   yRange <- c(0, 1)
   
   if((stratName1 != "") && stratName1 %in% colnames(alloc)) {   
      plot(alloc$numericDate, alloc[, stratName1], col=col1, xlab="", ylab="stock allocation", lwd=lwd1, type="l", xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if((stratName2 != "") && stratName2 %in% colnames(alloc)) {   
      plot(alloc$numericDate, alloc[, stratName2], col=col2, xlab="", ylab="", type="l", lwd=lwd2, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if((stratName3 != "") && stratName3 %in% colnames(alloc)) {
      plot(alloc$numericDate, alloc[, stratName3], col=col3, xlab="", ylab="", type="l", lwd=lwd3, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if((stratName4 != "") && stratName4 %in% colnames(alloc)) {   
      plot(alloc$numericDate, alloc[, stratName4], col=col4, xlab="", ylab="", type="l", lwd=lwd4, xlim=xRange, ylim=yRange)
   }
   par(new=F)   
}

plotReturnAndAlloc <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                               stratName3=def$typicalValue, stratName4="stocks", 
                               col1=def$colBalanced, col2=def$colTechnical, col3=def$colValue, col4=def$colConstantAlloc, 
                               lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                               startYear=def$startYear, endYear=2015, tradingCost=def$tradingCost, 
                               minTR=.9, maxTR=10000, yLabelReturn="", net=T, normalize=T) {
   par(mfrow = c(2, 1))
   plotReturn(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
              stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
              startYear=startYear, endYear=endYear, tradingCost=tradingCost, 
              minTR=minTR, maxTR=maxTR, normalize=normalize, yLabel=yLabelReturn, net=net)  
   plotAlloc(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
             stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
             startYear=startYear, endYear=endYear)        
   par(mfrow = c(1, 1))
}


plotFutureReturn <- function(stratName1=def$typicalBalanced, stratName2=def$typicalTechnical, 
                       stratName3=def$typicalValue, stratName4="stocks", 
                       col1=def$colBalanced, col2=def$colTechnical, col3=def$colValue, col4=def$colConstantAlloc, 
                       lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=2,
                       futureYears=def$futureYears,
                       startYear=def$startYear, endYear=2015, tradingCost=def$tradingCost, 
                       minTR=.9, maxTR=10000, yLabel="", net=T, normalize=T) { 
   
   #    print(c(stratName2, stratName3, stratName4) )
   normDate <- (startYear-1871)*12+1
   par(mar=c(2.5, 4, 1.5, 1.5))
   xRange <- c(startYear, endYear)
   yRange <- c(minTR, maxTR)
   
   TR1 <- numeric(numData)
   TR2 <- numeric(numData)
   TR3 <- numeric(numData)
   TR4 <- numeric(numData)
   
   if ( yLabel=="" ) {
      if( tradingCost==0 )  # trading cost = 0: gross returns
         yLabel <- paste0("total return (%) over ", futureYears, "years, GROSS of trading costs")
      else if( tradingCost>0 ) # trading cost > 0: net returns
         yLabel <- paste0("total return (%) over ", futureYears, "years, net of trading costs of ", round(tradingCost*100), "%")   
      else # no notion of trading cost (e.g. for asset classes)
         yLabel <- paste0("total return (%)")
   }
   
   if(tradingCost==0) net<-F
   if(net) {      
      if (tradingCost == 0.02) {
         if (!stratName1 %in% colnames(netTR2)) calcTRnetOfTradingCost(stratName1)
         if (!stratName2 %in% colnames(netTR2)) calcTRnetOfTradingCost(stratName2)
         if (!stratName3 %in% colnames(netTR2)) calcTRnetOfTradingCost(stratName3)
         if (!stratName4 %in% colnames(netTR2)) calcTRnetOfTradingCost(stratName4)
         TR1 <- netTR2[, stratName1] 
         TR2 <- netTR2[, stratName2] 
         TR3 <- netTR2[, stratName3] 
         TR4 <- netTR2[, stratName4] 
      } 
      else if(tradingCost == 0.04)  {
         if (!stratName1 %in% colnames(netTR4)) calcTRnetOfTradingCost(stratName1)
         if (!stratName2 %in% colnames(netTR4)) calcTRnetOfTradingCost(stratName2)
         if (!stratName3 %in% colnames(netTR4)) calcTRnetOfTradingCost(stratName3)
         if (!stratName4 %in% colnames(netTR4)) calcTRnetOfTradingCost(stratName4)
         TR1 <- netTR4[, stratName1] 
         TR2 <- netTR4[, stratName2] 
         TR3 <- netTR4[, stratName3] 
         TR4 <- netTR4[, stratName4] 
      } 
      else stop("No data frame \'netTR", round(tradingCost*100,1), "\' exists.")      
   } else {
      TR1 <- TR[, stratName1]
      TR2 <- TR[, stratName2]
      TR3 <- TR[, stratName3]
      TR4 <- TR[, stratName4]
   }
   
   if (normalize) {
      normFactor1 <- TR1[normDate]
      normFactor2 <- TR2[normDate]
      normFactor3 <- TR3[normDate]
      normFactor4 <- TR4[normDate]
   } else {      
      normFactor1 <- 1
      normFactor2 <- 1
      normFactor3 <- 1
      normFactor4 <- 1
   }
   
   if((stratName1 != "") && stratName1 %in% colnames(TR)) {   
      plot(TR$numericDate, TR1/normFactor1, col=col1, xlab="", ylab=yLabel, log="y", type="l", lwd=lwd1, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if((stratName2 != "") && stratName2 %in% colnames(TR)) {   
      plot(TR$numericDate, TR2/normFactor2, col=col2, xlab="", ylab="", log="y", type="l", lwd=lwd2, xlim=xRange, ylim=yRange)
      par(new=T)
   }  
   if((stratName3 != "") && stratName3 %in% colnames(TR)) {   
      plot(TR$numericDate, TR3/normFactor3, col=col3, xlab="", ylab="", log="y", type="l", lwd=lwd3, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if((stratName4 != "") && stratName4 %in% colnames(TR)) {   
      plot(TR$numericDate, TR4/normFactor4, col=col4, xlab="", ylab="", log="y", type="l", lwd=lwd4, xlim=xRange, ylim=yRange)
   }
   legend( "topleft", c(stratName1,stratName2,stratName3,stratName4), 
           bty="n", lwd=c(lwd1, lwd2, lwd3, lwd4), lty = c(1,1,1,1), 
           col=c(col1, col2, col3, col4) )
   par(new=F)
}



showPlotLegend <- function() {
   print(paste("CAPE:     ", def$colCAPE) )
   print(paste("detrended:", def$colDetrended) )
   print(paste("momentum: ", def$colMomentum) )
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
plotAllReturnsVsSomeParameter <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, type2=def$type2, col2=def$col2, pch2=def$pch2,
                                          type3=def$type3, col3=def$col3, pch3=def$pch3, type4=def$type4, col4=def$col4, pch4=def$pch4,
                                          type5=def$type5, col5=def$col5, pch5=def$pch5, type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                          Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                          Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                          Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                          lineCol=def$lineCol,
                                          xStatsName, xFactor=100, xLabel="volatility (%)",
                                          yStatsName=def$yStatsName, yFactor=100,
                                          xMin, xMax, yMin=def$yTRmin, yMax=def$yTRmax, tradingCost=def$tradingCost) { 
   
   ## 'search' type is generated by searchForOptimalX() functions.
   ## Since there are many data points of this type we use open symbols for them.
   ## We look only for the last one: since they are produced in batches, we expect them to be all of the same kind.  
{   
   i <- length(stats$type) 
   ## we start from the end to look for the most recent data
   while (i>=1 && (is.na(stats$type[i]) || stats$type[i] != def$Stype) )
      i <- i - 1
   
   Spch <- def$Spch
   Scol <- "purple" 
   ## we need Scol to exist even if there is no data point of type 'search'
   ## A purple point being actually plotted would be very fishy as this color will be either overwritten or not used.
   
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

   
   xRange <- c(xMin, xMax)
   yRange <- c(yMin - 100*tradingCost/2, yMax - 100*tradingCost/4)
   par( mar=c(4, 4, 1.5, 1.5) )
      
   plot(xFactor*subset(stats[, xStatsName], stats$type==type1), 
        yFactor*subset(stats[, yStatsName], stats$type==type1), 
        pch=pch1, col=col1, xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(xFactor*subset(stats[, xStatsName], stats$type==type2), 
        yFactor*subset(stats[, yStatsName], stats$type==type2), 
        pch=pch2, col=col2, xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(xFactor*subset(stats[, xStatsName], stats$type==type3), 
        yFactor*subset(stats[, yStatsName], stats$type==type3), 
        pch=pch3, col=col3, xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(xFactor*subset(stats[, xStatsName], stats$type==type4), 
        yFactor*subset(stats[, yStatsName], stats$type==type4), 
        pch=pch4, col=col4, xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(xFactor*subset(stats[, xStatsName], stats$type==type5), 
        yFactor*subset(stats[, yStatsName], stats$type==type5), 
        pch=pch5, col=col5, xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(xFactor*subset(stats[, xStatsName], stats$type==type6), 
        yFactor*subset(stats[, yStatsName], stats$type==type6), 
        pch=pch6, col=col6, xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   ## Multistrategies:
   plot(xFactor*subset(stats[, xStatsName], (stats$type=="multi" & stats$subtype==Msubtype1) ), 
        yFactor*subset(stats[, yStatsName], (stats$type=="multi" & stats$subtype==Msubtype1) ), 
        pch=Mpch1, col=Mcol1, xlim=xRange, ylim=yRange,
        xlab=xLabel, ylab=paste0("total return (%), net of trading cost of ", round(tradingCost*100), "%") )
   par(new=T)
   plot(xFactor*subset(stats[, xStatsName], (stats$type=="multi" & stats$subtype==Msubtype2) ), 
        yFactor*subset(stats[, yStatsName], (stats$type=="multi" & stats$subtype==Msubtype2) ), 
        pch=Mpch2, col=Mcol2, xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(xFactor*subset(stats[, xStatsName], (stats$type=="multi" & stats$subtype==Msubtype3) ), 
        yFactor*subset(stats[, yStatsName], (stats$type=="multi" & stats$subtype==Msubtype3) ), 
        pch=Mpch3, col=Mcol3, xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   ## Constant allocations:
   plot(xFactor*subset(stats[, xStatsName], stats$type=="constantAlloc"), 
        yFactor*subset(stats[, yStatsName], stats$type=="constantAlloc"), 
        type="l", col=lineCol, xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(xFactor*subset(stats[, xStatsName], stats$type==def$Stype), 
        yFactor*subset(stats[, yStatsName], stats$type==def$Stype), 
        pch=Spch, col=Scol, xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=F)
}



plotAllReturnsVsVolatility <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, type2=def$type2, col2=def$col2, pch2=def$pch2,
                                       type3=def$type3, col3=def$col3, pch3=def$pch3, type4=def$type4, col4=def$col4, pch4=def$pch4,
                                       type5=def$type5, col5=def$col5, pch5=def$pch5, type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                       Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                       Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                       Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                       lineCol=def$lineCol,
                                       xFactor=100, xLabel="volatility (%)",
                                       yStatsName=def$yStatsName, yFactor=100,
                                       xMin=12.5, xMax=15.5, yMin=def$yTRmin, yMax=def$yTRmax, tradingCost=def$tradingCost) { 
   
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 lineCol=lineCol,
                                 xStatsName="volatility", xFactor=xFactor, xLabel=xLabel,
                                 yStatsName=yStatsName, yFactor=yFactor,
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
}

plotAllReturnsVsDrawdown <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, type2=def$type2, col2=def$col2, pch2=def$pch2,
                                     type3=def$type3, col3=def$col3, pch3=def$pch3, type4=def$type4, col4=def$col4, pch4=def$pch4,
                                     type5=def$type5, col5=def$col5, pch5=def$pch5, type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                     Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                     Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                     Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                     lineCol=def$lineCol,
                                     xFactor=1, xLabel="drawdowns",
                                     yStatsName=def$yStatsName, yFactor=100,
                                     xMin=1, xMax=2.2, yMin=def$yTRmin, yMax=def$yTRmax, tradingCost=def$tradingCost) { 
   
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 lineCol=lineCol,
                                 xStatsName="DD2", xFactor=xFactor, xLabel=xLabel,
                                 yStatsName=yStatsName, yFactor=yFactor,
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
}

plotAllReturnsVsAverageAlloc <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, type2=def$type2, col2=def$col2, pch2=def$pch2,
                                         type3=def$type3, col3=def$col3, pch3=def$pch3, type4=def$type4, col4=def$col4, pch4=def$pch4,
                                         type5=def$type5, col5=def$col5, pch5=def$pch5, type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                         Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                         Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                         Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                         lineCol=def$lineCol,
                                         xFactor=100, xLabel="average stock allocation (%)",
                                         yStatsName=def$yStatsName, yFactor=100,
                                         xMin=40, xMax=100, yMin=def$yTRmin, yMax=def$yTRmax, tradingCost=def$tradingCost) { 
   
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 lineCol=lineCol,
                                 xStatsName="avgStockAlloc", xFactor=xFactor, xLabel=xLabel,
                                 yStatsName=yStatsName, yFactor=yFactor,
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
}

plotAllReturnsVsInverseTurnover <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, type2=def$type2, col2=def$col2, pch2=def$pch2,
                                            type3=def$type3, col3=def$col3, pch3=def$pch3, type4=def$type4, col4=def$col4, pch4=def$pch4,
                                            type5=def$type5, col5=def$col5, pch5=def$pch5, type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                            Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                            Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                            Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                            lineCol=def$lineCol,
                                            xFactor=100, xLabel="100 / turnover (years)",
                                            yStatsName=def$yStatsName, yFactor=100,
                                            xMin=0, xMax=100, yMin=def$yTRmin, yMax=def$yTRmax, tradingCost=def$tradingCost) { 
   
   
   stats$invTurnover <<- 1 / stats$turnover
   
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 lineCol=lineCol,
                                 xStatsName="invTurnover", xFactor=xFactor, xLabel=xLabel,
                                 yStatsName=yStatsName, yFactor=yFactor,
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   stats$invTurnover = NULL
}


plotAllReturnsVsFour <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, type2=def$type2, col2=def$col2, pch2=def$pch2,
                                 type3=def$type3, col3=def$col3, pch3=def$pch3, type4=def$type4, col4=def$col4, pch4=def$pch4,
                                 type5=def$type5, col5=def$col5, pch5=def$pch5, type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                 Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                 Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                 Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                 lineCol=def$lineCol,
                                 xMinVol=12.5, xMaxVol=15.5, xMinDD=1, xMaxDD=2.2, 
                                 xMinAlloc=40, xMaxAlloc=100, xMinTO=0, xMaxTO=100, 
                                 yStatsName=def$yStatsName, yFactor=100,
                                 yMin=def$yTRmin, yMax=def$yTRmax, tradingCost=def$tradingCost) {
   
   par(mfrow = c(2, 2))
   
   plotAllReturnsVsVolatility(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                              type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                              type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6,  
                              Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                              Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                              Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                              lineCol=lineCol,
                              yStatsName=yStatsName, yFactor=yFactor,
                              xMin=xMinVol, xMax=xMaxVol, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   
   plotAllReturnsVsDrawdown(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                            type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                            type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                            Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                            Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                            Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                            lineCol=lineCol,
                            yStatsName=yStatsName, yFactor=yFactor,
                            xMin=xMinDD, xMax=xMaxDD, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   
   plotAllReturnsVsAverageAlloc(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                lineCol=lineCol,
                                yStatsName=yStatsName, yFactor=yFactor,
                                xMin=xMinAlloc, xMax=xMaxAlloc, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   
   plotAllReturnsVsInverseTurnover(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                   type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                   type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                   Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                   Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                   Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                   lineCol=lineCol,
                                   yStatsName=yStatsName, yFactor=yFactor,
                                   xMin=xMinTO, xMax=xMaxTO, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   
   par(mfrow = c(1, 1))
}


plotAllReturnsVsTwo <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, type2=def$type2, col2=def$col2, pch2=def$pch2,
                                 type3=def$type3, col3=def$col3, pch3=def$pch3, type4=def$type4, col4=def$col4, pch4=def$pch4,
                                 type5=def$type5, col5=def$col5, pch5=def$pch5, type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                 Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                 Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                 Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                 lineCol=def$lineCol,
                                 xMinVol=12.5, xMaxVol=15.5, xMinDD=1, xMaxDD=2.2, 
                                 xMinAlloc=40, xMaxAlloc=100, xMinTO=0, xMaxTO=100, 
                                 yStatsName=def$yStatsName, yFactor=100,
                                 yMin=def$yTRmin, yMax=def$yTRmax, tradingCost=def$tradingCost) {
   
   par(mfrow = c(2, 1))
   
   plotAllReturnsVsVolatility(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                              type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                              type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6,  
                              Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                              Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                              Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                              lineCol=lineCol,
                              yStatsName=yStatsName, yFactor=yFactor,
                              xMin=xMinVol, xMax=xMaxVol, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   
   plotAllReturnsVsDrawdown(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                            type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                            type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                            Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                            Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                            Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                            lineCol=lineCol,
                            yStatsName=yStatsName, yFactor=yFactor,
                            xMin=xMinDD, xMax=xMaxDD, yMin=yMin, yMax=yMax, tradingCost=tradingCost) 
   par(mfrow = c(1, 1))
}

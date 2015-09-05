
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



#default values of colors for plotting
setPlottingDefaultColors <- function() {
   # Colors consistently used for the various strategies
   def$colCAPE_NH   <<- "cyan4"
   def$colCAPE_hy   <<- "cyan"
   #def$colDetrended <<- "skyblue"         # anything detrended is quarantined
   def$colSMA       <<- "pink"
   def$colBollinger <<- "magenta"   
   def$colReversal  <<- "orange"
   def$colInflation <<- "purple"
   
   def$colBoll_CAPE     <<- "darkolivegreen4"   
   # def$colBoll_detrended<<- "seagreen3" # anything detrended is quarantined
   def$colSMA_CAPE      <<- "greenyellow"   
   def$colReversal_CAPE <<- "darkolivegreen"
   def$colBoll_Boll <<- "darkolivegreen4"
   
   def$colValue     <<- "blue"
   def$colTechnical <<- "red"
   def$colHybrid    <<- "green"
   def$colBalanced  <<- "black"
   
   def$pch          <<- 16L
   def$Mpch         <<- 15L
   
   def$colConstantAlloc <<- "chocolate"
   def$colBonds     <<- "gray"
   
   # plotAllReturnsVsX()
   def$type1 <<- "Bollinger"
   def$col1  <<- def$colBollinger
   def$pch1  <<- def$pch
   
   def$type2 <<- "SMA"
   def$col2  <<- def$colSMA
   def$pch2  <<- def$pch
   
   def$type3 <<- "reversal"
   def$col3  <<- def$colReversal
   def$pch3  <<- def$pch
   
   def$type4 <<- "CAPE_hy"
   def$col4  <<- def$colCAPE_hy
   def$pch4  <<- def$pch
   
   def$type5 <<- "CAPE_NH"
   def$col5  <<- def$colCAPE_NH
   def$pch5  <<- def$pch
   
   #    def$type5 <<- "detrended"        # anything detrended is quarantined
   #    def$col5  <<- def$colDetrended
   #    def$pch5  <<- def$pch
  
   def$type6 <<- "Boll_CAPE"
   def$col6  <<- def$colBoll_CAPE
   def$pch6  <<- def$pch
   
   #    def$type7 <<- "Boll_detrended"   # anything detrended is quarantined
   #    def$col7  <<- def$colBoll_detrended
   #    def$pch7  <<- def$pch
   
   def$type7 <<- "reversal_CAPE"
   def$col7  <<- def$colReversal_CAPE
   def$pch7  <<- def$pch
   
   def$type8 <<- "Boll_Boll"
   def$col8  <<- def$colBoll_Boll
   def$pch8  <<- def$pch
   
   def$type9 <<- "SMA_CAPE"
   def$col9  <<- def$colSMA_CAPE
   def$pch9  <<- def$pch
   
   def$type10<<- "inflation"
   def$col10 <<- def$colInflation
   def$pch10 <<- def$pch
   
   
   # plotAllReturnsVsX(): combined strategies
   def$Msubtype1 <<- "value"
   def$Mcol1     <<- def$colValue
   def$Mpch1     <<- def$Mpch
   
   def$Msubtype2 <<- "technical"
   def$Mcol2     <<- def$colTechnical
   def$Mpch2     <<- def$Mpch
   
   def$Msubtype3 <<- "hybrid"
   def$Mcol3     <<- def$colHybrid
   def$Mpch3     <<- def$Mpch
   
   def$Msubtype4 <<- "balanced"
   def$Mcol4     <<- def$colBalanced
   def$Mpch4     <<- def$Mpch
   
   # plotAllReturnsVsX(): training
   def$Stype    <<- "training"
   def$Scol     <<- NA # we need the subtype to set the color, we do that in the plotting function
   def$Spch     <<- def$pch # default symbol
   def$SpchM    <<- def$Mpch # empty square for combined strategies
   
   def$lineCol  <<- def$colConstantAlloc
}

   
#default values of other plotting parameters
setPlottingDefaultValues <- function() {
   def$yTRmin    <<-   7.5
   def$yTRmax    <<-  10.2
   def$yStatsName<<-  ""
   
   def$pngWidth  <<-1024
   def$pngHeight <<- 768
   
   def$minVol    <<-  11.5
   def$maxVol    <<-  17.5
   def$minDD2    <<-   3
   def$maxDD2    <<-  10
   def$maxTO     <<- 130
   
   def$plotEndYear <<- 2016
   def$maxTR     <<- 20000
       
   setPlottingDefaultColors()
}


plotAssetClassesReturn <- function(
         stratName1="stocks",stratName2="bonds", stratName3="UKhousePrice", stratName4="gold",
         col1=def$colConstantAlloc, col2=def$colBonds, col3="darkgreen", col4="gold",
         lwd1=2, lwd2=2, lwd3=2, lwd4=2, 
         startYear=1975.25, endYear=def$plotEndYear, 
         yLabel="rescaled total return", net=F, minTR=.5, maxTR=20,
         pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
         pngName="figures/asset_classes_returns.png") {

   if (stratName4=="gold") requireColInDat("gold")
   if (stratName3=="UKhousePrice") requireColInDat("UKhousePrice")
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
   if(legendPlacement!="none") 
      legend( legendPlacement, c(stratName1,stratName2,stratName3,stratName4), 
          bty="n", lwd=c(lwd1, lwd2, lwd3, lwd4), lty = c(1,1,1,1), 
          col=c(col1, col2, col3, col4) )
   par(new=F)
}


##Wrapper passing the proper vectors to plotSomeSortOfReturn()
plotReturn <- function(
         stratName1=typical$stratNames[1], stratName2=typical$stratNames[2], 
         stratName3=typical$stratNames[3], stratName4=typical$stratNames[4],
         col1=typical$stratCols[1], col2=typical$stratCols[2], 
         col3=typical$stratCols[3], col4=typical$stratCols[4],
         lwd1=2.5, lwd2=1.5, lwd3=1.5, lwd4=2.5,
         xVect=TR$numericDate, startYear=def$plotStartYear, endYear=def$plotEndYear, 
         costs=def$tradingCost, detrendBy=0,
         minTR=.9, maxTR=def$maxTR, yLabel="", legendPlacement="topleft",
         startIndex=def$startIndex, net=T, normalize=T,
         pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
         pngName="figures/return.png") {   
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)
   
   if ( yLabel=="" ) {
      if( costs==0 )  # trading cost = 0: gross returns
         yLabel <- paste0("total real return, gross of trading costs")
      else if( costs>0 ) # trading cost > 0: net returns
         yLabel <- paste0("total real return, net of trading costs of ", round(costs*100, 1), "%")   
      else # no notion of trading cost (e.g. for asset classes)
         yLabel <- paste0("total real return")
      if (detrendBy != 0)
         yLabel <- paste0(yLabel, " (detrended by ", detrendBy, "%)")
   }
   
   normDate <- (startYear-def$dataStartYear)*12+1
   
   ## Rescaling maxTR (with the utmost mathematical precision) and minTR ("less precisely")
   # fudgeFactor adds a margin of error to maxTR in case the maximum is not reach at the end of the range
   fudgeFactor <- detrendBy^2 / 200 
   if (maxTR==def$maxTR)
      maxTR <- maxTR * exp( -detrendBy/100 * (endYear-startYear) ) * (1+fudgeFactor)
   #orderOfMagnitude <- 10^(floor(log10(maxTR))-1)
   #maxTR <- round( maxTR / orderOfMagnitude ) * orderOfMagnitude
   if (minTR==.9) minTR <- minTR / (1+2*fudgeFactor)
   
   stratNames <- c(stratName1, stratName2, stratName3, stratName4)
   returnDF  <- TR[, stratNames]
      
   if( (net & costs>0) || detrendBy!=0 ) # reduce TR because of costs, or to detrend
      for (strat in 1:4) {
         index <- which(stats$strategy == stratNames[strat])
         if ( length(index>0) ) {
            rate <- costs/12/stats$turnover[index] + detrendBy/100/12
            for (i in startIndex:numData)
               returnDF[i, strat] <- returnDF[i, strat] * exp( -rate*(i-startIndex) )
         }
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


plotAlloc <- function(
         stratName1=typical$stratNamesSubstrategies[1], stratName2=typical$stratNamesSubstrategies[2], 
         stratName3=typical$stratNamesSubstrategies[3], stratName4=typical$stratNamesSubstrategies[4],
         col1=typical$stratColsSubstrategies[1], col2=typical$stratColsSubstrategies[2], 
         col3=typical$stratColsSubstrategies[3], col4=typical$stratColsSubstrategies[4],
         lwd1=2.5, lwd2=1.5, lwd3=1.5, lwd4=2.5,
         startYear=def$plotStartYear, endYear=def$plotEndYear,
         pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
         pngName="figures/allocation.png") { 
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)

   normDate <- (startYear-def$dataStartYear)*12+1
   par(mar=c(2.7, 4.2, 1.5, 1.5))
   xRange <- c(startYear, endYear)
   yRange <- c(2.5, 98)
   
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

plotReturnAndAllocTechnical <- function(
         stratName1=typical$SMA1,     stratName2=typical$SMA2, 
         stratName3=typical$reversal1, stratName4=typical$technical,
         col1=def$colBollinger, col2=def$colSMA, col3=def$colReversal, col4=def$colTechnical,
         lwd1=1.5, lwd2=1.5, lwd3=1.5, lwd4=2,
         startYear=def$plotStartYear, endYear=def$plotEndYear, 
         yLabel="", net=T, detrendBy=0, minTR=0.8, maxTR=def$maxTR, costs=def$tradingCost,
         pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
         pngName="figures/return_and_allocation-technical.png") {
   
   plotReturnAndAlloc(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
              stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
              startYear=startYear, endYear=endYear, costs=costs, detrendBy=detrendBy, 
              yLabel=yLabel, net=net, minTR=minTR, maxTR=maxTR,
              pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}

plotReturnAndAllocValue <- function(
         stratName1=typical$CAPE_hy1, stratName2=typical$CAPE_hy2, 
         stratName3=typical$CAPE_NH1, stratName4=typical$value,
         col1=def$colCAPE_hy, col2=def$colCAPE_hy, col3=def$colCAPE_NH, col4=def$colValue, 
         lwd1=2, lwd2=1.5, lwd3=1.5, lwd4=1.5,
         startYear=def$plotStartYear, endYear=def$plotEndYear, 
         yLabel="", net=T, detrendBy=0, minTR=0.8, maxTR=def$maxTR, costs=def$tradingCost,
         pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
         pngName="figures/return_and_allocation-value.png") {
   
   plotReturnAndAlloc(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
                      stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
                      startYear=startYear, endYear=endYear, costs=costs, detrendBy=detrendBy,
                      yLabel=yLabel, net=net, minTR=minTR, maxTR=maxTR,
                      pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}

plotReturnAndAllocHybrid <- function(
         stratName1=typical$Boll_CAPE1, stratName2=typical$SMA_CAPE2,
         stratName3=typical$Boll_Boll1, stratName4=typical$Boll_balanced1,
         col1=def$colBoll_CAPE, col2=def$colSMA_CAPE, col3=def$colBoll_Boll, col4=def$colBalanced, 
         lwd1=1.5, lwd2=1.5, lwd3=2, lwd4=2,
         startYear=def$plotStartYear+max(parameters$startIndex[which(parameters$strategy==typical$Boll_balanced1)]
                                         -def$startIndex,0)/12, # to ensure that Boll(balanced) will be included
         endYear=def$plotEndYear, 
         yLabel="", net=T, detrendBy=0, minTR=0.8, maxTR=def$maxTR, costs=def$tradingCost,
         pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
         pngName="figures/return_and_allocation-hybrid.png") {
   
   plotReturnAndAlloc(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
                      stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
                      startYear=startYear, endYear=endYear, costs=costs, detrendBy=detrendBy, 
                      yLabel=yLabel, net=net, minTR=minTR, maxTR=maxTR,
                      pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}

## same but with stocks, bonds and 80/20 alloc
plotReturnAndAlloc <- function(
         stratName1=typical$stratNames[1], stratName2=typical$stratNames[2], 
         stratName3=typical$stratNames[3], stratName4=typical$stratNames[4],
         col1=typical$stratCols[1], col2=typical$stratCols[2], 
         col3=typical$stratCols[3], col4=typical$stratCols[4],
         lwd1=2.5, lwd2=1.5, lwd3=1.5, lwd4=2.5,
         startYear=def$plotStartYear, endYear=def$plotEndYear, costs=def$tradingCost, 
         detrendBy=0, minTR=.9, maxTR=def$maxTR, yLabelReturn="", 
         legendPlacement="topleft", net=T, normalize=T,
         pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
         pngName="figures/return_and_allocation.png") {
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)

   par(mfrow = c(4, 1))
   layout( matrix(c(1,1,1,2)) ) # the return plot takes up 3/4 of the height
   plotReturn(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
              stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
              startYear=startYear, endYear=endYear, costs=costs, detrendBy=detrendBy,
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

## same but with substrategies (technical, value and hybrid)
plotReturnAndAllocSubstrategies <- function(
         stratName1=typical$stratNamesSubstrategies[1], stratName2=typical$stratNamesSubstrategies[2], 
         stratName3=typical$stratNamesSubstrategies[3], stratName4=typical$stratNamesSubstrategies[4],
         col1=typical$stratColsSubstrategies[1], col2=typical$stratColsSubstrategies[2], 
         col3=typical$stratColsSubstrategies[3], col4=typical$stratColsSubstrategies[4],
         lwd1=1.5, lwd2=1.5, lwd3=1.5, lwd4=2.5,
         startYear=def$plotStartYear, endYear=def$plotEndYear, costs=def$tradingCost, 
         detrendBy=0, minTR=.9, maxTR=def$maxTR, yLabelReturn="", 
         legendPlacement="topleft", net=T, normalize=T,
         pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
         pngName="figures/return_and_allocation.png") {

   plotReturnAndAlloc(stratName1=stratName1, col1=col1, lwd1=lwd1, stratName2=stratName2, col2=col2, lwd2=lwd2,
              stratName3=stratName3, col3=col3, lwd3=lwd3, stratName4=stratName4, col4=col4, lwd4=lwd4, 
              startYear=startYear, endYear=endYear, costs=costs, detrendBy=detrendBy,
              minTR=minTR, maxTR=maxTR, normalize=normalize, 
              yLabel=yLabelReturn, legendPlacement=legendPlacement, net=net,
              pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}


plotReturnAndInflation <- function(stratName1=typical$balanced, stratName2="stocks", stratName3="bonds", 
                               col1=def$colBalanced,  col2=def$colConstantAlloc, col3=def$colBonds, 
                               lwd1=2, lwd2=3, lwd3=1.5, lwd4=1.5, 
                               startYear=def$plotStartYear, endYear=def$plotEndYear, 
                               minTR=0.9, maxTR=def$maxTR, yLabelReturn="", 
                               legendPlacement="topleft", net=T, 
                               inflationYears=7, derivativeYears=5, inflationCol=def$colInflation) {
   
   if(derivativeYears=="" || derivativeYears==0) {
      inflationName <- paste0("inflation",inflationYears,"yr")
      if ( (!inflationName %in% colnames(dat)) || (!inflationName %in% colnames(TR)) ) {
         print(paste("Calculating inflation over", inflationYears, "years.") )
         calcInflationAndAddToDat(inflationYears)
      }
   } else { # using the derivative of the inflation
      inflationName <- paste0("derivative", derivativeYears, "yrInflation", inflationYears,"yr")
      if ( (!inflationName %in% colnames(dat)) || (!inflationName %in% colnames(TR)) ) {
         print(paste0("Calculating derivative over ", derivativeYears, " years of ", inflationYears, "-year inflation.") )
         calcInflationDerivativeAndAddToDat(inflationYears=inflationYears, derivativeYears=derivativeYears)
      }
   }
   
   par(mfrow = c(2, 1))
   plotReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=inflationName, 
              col1=col1, col2=col2, col3=col3, col4=inflationCol, lwd1=lwd1, lwd2=lwd2, lwd3=lwd3, lwd4=lwd4,
              startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR,
              yLabel=yLabelReturn, legendPlacement=legendPlacement, net=net)
   plot(dat$numericDate, 100*dat[[inflationName]], xlim=c(startYear, endYear), 
        type="l", col=inflationCol, ylab=paste0(inflationYears,"-year inflation (%)") ) # ylim=c(inflationMin, inflationMax), 
   par(mfrow = c(1, 1))
}


## 15-year-long sideways markets
plotReturnSideways <- function(stratName1=typical$balanced, stratName2="stocks", stratName3="bonds", 
                               col1=def$colBalanced,  col2=def$colConstantAlloc, col3=def$colBonds, 
                               lwd1=2, lwd2=3, lwd3=1.5, lwd4=2.5, costs=def$tradingCost,
                               inflationYears=7, derivativeYears=0, inflationCol=def$colInflation, 
                               inflationMin=0, inflationMax=12, minTR=0.7, maxTR=2.,
                               pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                               pngName="figures/sideways_markets.png") {
   if (dataSplit != "all") 
      warning("Not all data will be available in '", dataSplit, "' mode.", immediate.=T)
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)

   if(derivativeYears=="" || derivativeYears==0) {
      inflationName <- paste0("inflation",inflationYears,"yr")
      if ( (!inflationName %in% colnames(dat)) || (!inflationName %in% colnames(TR)) ) {
         print(paste("Calculating inflation over", inflationYears, "years.") )
         calcInflationAndAddToDat(inflationYears)
      }
   } else { # using the derivative of the inflation
      inflationName <- paste0("derivative", derivativeYears, "yrInflation", inflationYears,"yr")
      if ( (!inflationName %in% colnames(dat)) || (!inflationName %in% colnames(TR)) ) {
         print(paste0("Calculating derivative over ", derivativeYears, " years of ", inflationYears, "-year inflation.") )
         calcInflationDerivativeAndAddToDat(inflationYears=inflationYears, derivativeYears=derivativeYears)
      }
   }
      
   col4 <- inflationCol
   stratName4 <- inflationName
   
   par(mfrow = c(2, 2))   
   plotReturn(startYear=1909, endYear=1921.5, minTR=minTR, maxTR=maxTR, costs=costs,
              stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
              col1=col1, col2=col2, col3=col3, col4=col4, lwd1=lwd1, lwd2=lwd2, lwd3=lwd3, lwd4=lwd4,
              legendPlacement="topleft")
   par(new=T)
   plot(dat$numericDate, dat[[inflationName]], xlim=c(1909, 1921.5), type="l", lty=2, lwd=lwd4,
        col=inflationCol, ylim=c(inflationMin/100, inflationMax/100), ylab= "", yaxt='n' )
   par(new=F)
   
   plotReturn(startYear=1936, endYear=1949.5, minTR=minTR, maxTR=maxTR, costs=costs,
              stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4,
              col1=col1, col2=col2, col3=col3, col4=col4, lwd1=lwd1, lwd2=lwd2, lwd3=lwd3, lwd4=lwd4,
              legendPlacement="none")
   par(new=T)
   plot(dat$numericDate, dat[[inflationName]], xlim=c(1936, 1949.5), type="l", lty=2, lwd=lwd4, 
        col=inflationCol, ylim=c(inflationMin/100, inflationMax/100), ylab= "", yaxt='n' )
   par(new=F)
   
   plotReturn(startYear=1964.75,   endYear=1982,   minTR=minTR, maxTR=maxTR, costs=costs,
              stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4,
              col1=col1, col2=col2, col3=col3, col4=col4, lwd1=lwd1, lwd2=lwd2, lwd3=lwd3, lwd4=lwd4,
              legendPlacement="none")
   par(new=T)
   plot(dat$numericDate, dat[[inflationName]], xlim=c(1964.75, 1982), type="l", lty=2, lwd=lwd4, 
        col=inflationCol, ylim=c(inflationMin/100, inflationMax/100), ylab= "", yaxt='n' )
   par(new=F)
   
   plotReturn(startYear=1998.25,   endYear=2011.5,   minTR=minTR, maxTR=maxTR, costs=costs,
              stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4,
              col1=col1, col2=col2, col3=col3, col4=col4, lwd1=lwd1, lwd2=lwd2, lwd3=lwd3, lwd4=lwd4,
              legendPlacement="none")
   par(new=T)
   plot(dat$numericDate, dat[[inflationName]], xlim=c(1998.25, 2011.5), type="l", lty=2, lwd=lwd4, 
        col=inflationCol, ylim=c(inflationMin/100, inflationMax/100), ylab= "", yaxt='n' )
   par(new=F)
   
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
      if (maxTR=="") maxTR <-  20
   } else if (futureYears==15) {
      if (minTR=="") minTR <-  -4.5
      if (maxTR=="") maxTR <-  15
   } else if (futureYears==20) {
      if (minTR=="") minTR <-  -2
      if (maxTR=="") maxTR <-  14.5
   } else if (futureYears==30) {
      if (minTR=="") minTR <-  -2
      if (maxTR=="") maxTR <-  12
   }
   return(c(yLabel=yLabel, minTR=minTR, maxTR=maxTR))
}


## future returns
plotFutureReturn <- function(
         stratName1=typical$stratNames[1], stratName2=typical$stratNames[2], 
         stratName3=typical$stratNames[3], stratName4=typical$stratNames[4],
         col1=typical$stratCols[1], col2=typical$stratCols[2], 
         col3=typical$stratCols[3], col4=typical$stratCols[4],
         futureYears=def$futureYears, xVect=TR$numericDate, 
         lwd1=2.5, lwd2=1.5, lwd3=1.5, lwd4=2.5, 
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
   else if (futureYears==15) 
      returnDF <- 100 * next15yrs[, stratNames]
   else if (futureYears==20) 
      returnDF <- 100 * next20yrs[, stratNames]
   else if (futureYears==30) 
      returnDF <- 100 * next30yrs[, stratNames]  
   
   if(net & costs>0) # reduce TR because of costs
      for (strat in 1:numCurves) 
         if (stratNames[strat] != "") {
            index  <- which(stats$strategy == stratNames[strat])
            if (length(index)>0)
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


plotRelativeFutureReturn <- function(
         stratName1=typical$stratNamesSubstrategies[1], stratName2=typical$stratNamesSubstrategies[2], 
         stratName3=typical$stratNamesSubstrategies[3], stratName4=typical$stratNamesSubstrategies[4],
         col1=typical$stratColsSubstrategies[1], col2=typical$stratColsSubstrategies[2], 
         col3=typical$stratColsSubstrategies[3], col4=typical$stratColsSubstrategies[4],
         refStratName="stocks", futureYears=def$futureYears, xVect=TR$numericDate, 
         lwd1=1.5, lwd2=1.5, lwd3=1.5, lwd4=2.5,
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
   
   stratNames <- c(stratName1, stratName2, stratName3, stratName4, refStratName)
   numCurves <- length(stratNames)
   
   if (futureYears==5)  returnDF <- 100 *  next5yrs[, stratNames]
   else if (futureYears==10)  returnDF <- 100 * next10yrs[, stratNames]
   else if (futureYears==15)  returnDF <- 100 * next15yrs[, stratNames]
   else if (futureYears==20)  returnDF <- 100 * next20yrs[, stratNames]
   else if (futureYears==30)  returnDF <- 100 * next30yrs[, stratNames]  
   
   if(net & costs>0) # reduce TR because of costs
      for ( strat in 1:numCurves ) 
         if (stratNames[strat] != "") {
            index  <- which(stats$strategy == stratNames[strat])
            returnDF[, strat] <- returnDF[, strat] - 100*costs/stats$turnover[index]
         }
   
   ## adjust endYear based on non-NA data
   i <- (endYear-def$dataStartYear)*12+1
   while ( sum(is.na(returnDF[i, ])) > 0 ) 
      i <- i-1
   endYear <- round( (i-1)/12 + def$dataStartYear ) + 1
   
   returnDF[, 1:numCurves] <- returnDF[, 1:numCurves] - returnDF[, 5]
   minTR <- min(returnDF,na.rm=T); maxTR <- max(returnDF,na.rm=T)
   
   plotSomeSortOfReturn(returnDF1=returnDF[, 1], returnDF2=returnDF[, 2], 
                        returnDF3=returnDF[, 3], returnDF4=returnDF[, 4],
                        stratName1=stratName1, stratName2=stratName2, 
                        stratName3=stratName3, stratName4=stratName4,
                        col1=col1, col2=col2, col3=col3, col4=col4, lwd1=lwd1, lwd2=lwd2, lwd3=lwd3, lwd4=lwd4,
                        xVect=xVect, startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, 
                        yLabel=yLabel, legendPlacement=legendPlacement, yLog=F)
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }   
}



plotFutureReturnVsStocks <- function(stratName1=typical$balanced, stratName2=typical$technical, 
                                     stratName3=typical$value, refStratName="stocks", 
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
   else if (futureYears==15) 
      returnDF <- 100 * next15yrs[, stratNames]
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
         (stratName1=typical$balanced, stratName2=typical$technical, 
          stratName3=typical$value, refStratName="stocks", 
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
   else if (futureYears==15) 
      returnDF <- 100 * next15yrs[, stratNames]
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


plotCumulativeFutureReturn <- function(
         stratName1=typical$stratNames[1], stratName2=typical$stratNames[2], 
         stratName3=typical$stratNames[3], stratName4=typical$stratNames[4],
         col1=typical$stratCols[1], col2=typical$stratCols[2], 
         col3=typical$stratCols[3], col4=typical$stratCols[4],
         xVect=TR$numericDate, 
         lwd1=2.5, lwd2=1.5, lwd3=1.5, lwd4=2.5,
         futureYears=def$futureYears, net=T, costs=def$tradingCost,
         figureTitle="", pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
         pngName=paste0("figures/cumulative_return_over_next_", futureYears,"_years.png") ) {
   
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
   else if (futureYears==15) 
      returnDF <- 100 * next15yrs[, stratNames]
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



showPlotLegend <- function() {
   print(paste("Bollinger:  ", def$colBollinger) )
   print(paste("SMA:        ", def$colSMA) )
   print(paste("reversal:   ", def$colReversal) )
   print(paste("CAPE:       ", def$colCAPE_hy) )
   print(paste("detrended:  ", def$colDetrended) )
   print(paste("Boll(CAPE): ", def$colBoll_CAPE) )
   print(paste("Boll(detr.):", def$colBoll_detrended) )
   print(paste("SMA(CAPE):  ", def$colSMA_CAPE) )
   #   print(paste("inflation:  ", def$colInflation) )
   print("")   
   print(paste("technical:  ", def$colTechnical) )
   print(paste("value:      ", def$colValue) )
   print(paste("hybrid:     ", def$colHybrid) )
   print(paste("balanced:   ", def$colBalanced) )
   print("")   
   print(paste("stocks:     ", def$colConstantAlloc) )
   print(paste("bonds:      ", def$colBonds) )
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
                                          type7=def$type7, col7=def$col7, pch7=def$pch7, 
                                          type8=def$type8, col8=def$col8, pch8=def$pch8, 
                                          type9=def$type9, col9=def$col9, pch9=def$pch9, 
                                          type10=def$type10,col10=def$col10,pch10=def$pch10,
                                          Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                          Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                          Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                          Msubtype4=def$Msubtype4, Mcol4=def$Mcol4, Mpch4=def$Mpch4, 
                                          lineCol=def$lineCol, trainingPlotType="dots",
                                          xStatsName,    xFactor=100, xLabel="volatility (%)",
                                          yStatsName="", yFactor=100, yLabel="",
                                          xMin, xMax, yMin=def$yTRmin, yMax=def$yTRmax, 
                                          costs=def$tradingCost, 
                                          pngOutput=F, pngWidth=def$pngWidth,
                                          pngHeight=def$pngHeight, pngName) { 
   
   if(pngOutput)
      png(file=pngName, width=pngWidth, height=pngHeight)

   ## 'training' type is generated by searchForOptimalX() functions.
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
      ## we need Scol to exist even if there is no data point of type 'training'
      ## A purple point being actually plotted would be very fishy 
      ## as this color will be either overwritten or not used.
      
      if (i > 0) { # if there is at least one data point of type 'training'
         
         ## if there exist several subtypes, we throw a warning
         if (length (unique(stats$subtype[stats$type==def$Stype & !is.na(stats$type)]) ) > 1 ) 
            warning("All data points of type 'training' will be plotted with the same color.")
         
         if (stats$subtype[i] == def$type1) Scol <- def$col1
         if (stats$subtype[i] == def$type2) Scol <- def$col2
         if (stats$subtype[i] == def$type3) Scol <- def$col3
         if (stats$subtype[i] == def$type4) Scol <- def$col4
         if (stats$subtype[i] == def$type5) Scol <- def$col5
         if (stats$subtype[i] == def$type6) Scol <- def$col6
         if (stats$subtype[i] == def$type7) Scol <- def$col7
         if (stats$subtype[i] == def$type8) Scol <- def$col8
         if (stats$subtype[i] == def$type9) Scol <- def$col9
         if (stats$subtype[i] == def$type10)Scol <- def$col10

         if (stats$subtype[i] == def$Msubtype1) {
            Scol <- def$Mcol1
            Spch <- def$SpchM} # 
         if (stats$subtype[i] == def$Msubtype2) {
            Scol <- def$Mcol2
            Spch <- def$SpchM}
         if (stats$subtype[i] == def$Msubtype3) {
            Scol <- def$Mcol3
            Spch <- def$SpchM}
         if (stats$subtype[i] == def$Msubtype4) {
            Scol <- def$Mcol4
            Spch <- def$SpchM}      
      }
   }
   
   if (trainingPlotType=="line") {
      Stype <- "l"
      Spch <- ""
   } else if (trainingPlotType=="dots") {
      Stype <- "p"
      Spch <- "."
   } else if (trainingPlotType=="symbols")
      Stype <- "p"
   else stop(trainingPlotType, " is not a legitimate value for argument trainingPlotType, only line, dots or symbols")
         
   if (yStatsName == "") {
      if (costs==0)
         yStatsName <- "TR"
      else if (costs==0.5/100)
         yStatsName <- "netTR0.5"
      else if (round(100*costs,2) %in% c(1, 2, 3, 4, 6, 8, 10) )
         yStatsName <- paste0("netTR", round(100*costs,2) )
      else stop("No \'netTR", round(costs*100), "\' entry exists in 'stats'.")
   }
   
   if (yLabel == "") yLabel <- paste0("total return (%), net of costs of ", round(costs*100, 1), "%")

   xRange <- c(xMin, xMax)
   yRange <- c(yMin - 100*costs/2, yMax - 100*costs/4)
   par( mar=c(4.2, 4.2, 1.5, 1.5) )
      
   plot(xFactor*subset(stats[, xStatsName], stats$type==type1), 
        yFactor*subset(stats[, yStatsName], stats$type==type1), 
        pch=pch1, col=col1, xlim=xRange, ylim=yRange, xlab=xLabel, ylab=yLabel)
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
   points(xFactor*subset(stats[, xStatsName], stats$type==type7), 
          yFactor*subset(stats[, yStatsName], stats$type==type7), 
          pch=pch7, col=col7, xlab="", ylab="", xlim=xRange, ylim=yRange)
   points(xFactor*subset(stats[, xStatsName], stats$type==type8), 
          yFactor*subset(stats[, yStatsName], stats$type==type8), 
          pch=pch8, col=col8, xlab="", ylab="", xlim=xRange, ylim=yRange)
   points(xFactor*subset(stats[, xStatsName], stats$type==type9), 
          yFactor*subset(stats[, yStatsName], stats$type==type9), 
          pch=pch9, col=col9, xlab="", ylab="", xlim=xRange, ylim=yRange)
   points(xFactor*subset(stats[, xStatsName], stats$type==type10), 
          yFactor*subset(stats[, yStatsName], stats$type==type10), 
          pch=pch10,col=col10,xlab="", ylab="", xlim=xRange, ylim=yRange)
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
   points(xFactor*subset(stats[, xStatsName], (stats$type=="combined" & stats$subtype==Msubtype4) ), 
          yFactor*subset(stats[, yStatsName], (stats$type=="combined" & stats$subtype==Msubtype4) ), 
          pch=Mpch4, col=Mcol4, xlab="", ylab="", xlim=xRange, ylim=yRange)
   ## Constant allocations:
   points(xFactor*subset(stats[, xStatsName], stats$type=="constantAlloc"), 
        yFactor*subset(stats[, yStatsName], stats$type=="constantAlloc"), 
        type="l", col=lineCol, xlab="", ylab="", xlim=xRange, ylim=yRange)
   ## training results:
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
                                       type7=def$type7, col7=def$col7, pch7=def$pch7, 
                                       type8=def$type8, col8=def$col8, pch8=def$pch8, 
                                       type9=def$type9, col9=def$col9, pch9=def$pch9, 
                                       type10=def$type10,col10=def$col10,pch10=def$pch10,
                                       Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                       Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                       Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                       Msubtype4=def$Msubtype4, Mcol4=def$Mcol4, Mpch4=def$Mpch4, 
                                       lineCol=def$lineCol, trainingPlotType="dots",
                                       xFactor=100, xLabel="volatility (%)",
                                       yStatsName=def$yStatsName, yFactor=100,
                                       xMin=def$minVol, xMax=def$maxVol, yMin=def$yTRmin, yMax=def$yTRmax, 
                                       costs=def$tradingCost,
                                       pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                       pngName="figures/return_vs_volatility.png") {
                                       
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 type7=type7, col7=col7, pch7=pch7, type8=type8, col8=col8, pch8=pch8, 
                                 type9=type9, col9=col9, pch9=pch9,type10=type10,col10=col10,pch10=pch10,
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 Msubtype4=Msubtype4, Mcol4=Mcol4, Mpch4=Mpch4, 
                                 lineCol=lineCol, trainingPlotType=trainingPlotType,
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
                                     type7=def$type7, col7=def$col7, pch7=def$pch7, 
                                     type8=def$type8, col8=def$col8, pch8=def$pch8, 
                                     type9=def$type9, col9=def$col9, pch9=def$pch9, 
                                     type10=def$type10,col10=def$col10,pch10=def$pch10,
                                     Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                     Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                     Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                     Msubtype4=def$Msubtype4, Mcol4=def$Mcol4, Mpch4=def$Mpch4, 
                                     lineCol=def$lineCol, trainingPlotType="dots",
                                     xFactor=100, xLabel="drawdowns (%)",
                                     yStatsName=def$yStatsName, yFactor=100,
                                     xMin=def$minDD2, xMax=def$maxDD2, yMin=def$yTRmin, yMax=def$yTRmax, 
                                     costs=def$tradingCost,
                                     pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                     pngName="figures/return_vs_drawdown.png") { 
   
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 type7=type7, col7=col7, pch7=pch7, type8=type8, col8=col8, pch8=pch8, 
                                 type9=type9, col9=col9, pch9=pch9,type10=type10,col10=col10,pch10=pch10,
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 Msubtype4=Msubtype4, Mcol4=Mcol4, Mpch4=Mpch4, 
                                 lineCol=lineCol, trainingPlotType=trainingPlotType,
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
                                         type7=def$type7, col7=def$col7, pch7=def$pch7, 
                                         type8=def$type8, col8=def$col8, pch8=def$pch8, 
                                         type9=def$type9, col9=def$col9, pch9=def$pch9, 
                                         type10=def$type10,col10=def$col10,pch10=def$pch10,
                                         Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                         Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                         Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                         Msubtype4=def$Msubtype4, Mcol4=def$Mcol4, Mpch4=def$Mpch4, 
                                         lineCol=def$lineCol, trainingPlotType="dots",
                                         xFactor=100, xLabel="average stock allocation (%)",
                                         yStatsName=def$yStatsName, yFactor=100,
                                         xMin=40, xMax=97, yMin=def$yTRmin, yMax=def$yTRmax, 
                                         costs=def$tradingCost,
                                         pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                         pngName="figures/return_vs_average_alloc.png") { 
   
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 type7=type7, col7=col7, pch7=pch7, type8=type8, col8=col8, pch8=pch8, 
                                 type9=type9, col9=col9, pch9=pch9,type10=type10,col10=col10,pch10=pch10,
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 Msubtype4=Msubtype4, Mcol4=Mcol4, Mpch4=Mpch4, 
                                 lineCol=lineCol, trainingPlotType=trainingPlotType,
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
                                            type7=def$type7, col7=def$col7, pch7=def$pch7, 
                                            type8=def$type8, col8=def$col8, pch8=def$pch8, 
                                            type9=def$type9, col9=def$col9, pch9=def$pch9, 
                                            type10=def$type10,col10=def$col10,pch10=def$pch10,
                                            Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                            Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                            Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                            Msubtype4=def$Msubtype4, Mcol4=def$Mcol4, Mpch4=def$Mpch4, 
                                            lineCol=def$lineCol,  trainingPlotType="dots",
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
                                 type7=type7, col7=col7, pch7=pch7, type8=type8, col8=col8, pch8=pch8, 
                                 type9=type9, col9=col9, pch9=pch9,type10=type10,col10=col10,pch10=pch10,
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 Msubtype4=Msubtype4, Mcol4=Mcol4, Mpch4=Mpch4, 
                                 lineCol=lineCol, trainingPlotType=trainingPlotType,
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
                                 type7=def$type7, col7=def$col7, pch7=def$pch7, 
                                 type8=def$type8, col8=def$col8, pch8=def$pch8, 
                                 type9=def$type9, col9=def$col9, pch9=def$pch9, 
                                 type10=def$type10,col10=def$col10,pch10=def$pch10,
                                 Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                 Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                 Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                 Msubtype4=def$Msubtype4, Mcol4=def$Mcol4, Mpch4=def$Mpch4, 
                                 lineCol=def$lineCol, trainingPlotType="dots",
                                 xMinVol=def$minVol, xMaxVol=def$maxVol, xMinDD2=def$minDD2, xMaxDD2=def$maxDD2, 
                                 xMinAlloc=40, xMaxAlloc=98, xMinTO=2, xMaxTO=def$maxTO, 
                                 yStatsName=def$yStatsName, yFactor=100,
                                 yMin=def$yTRmin, yMax=def$yTRmax, costs=def$tradingCost+def$riskAsCost,
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
                              type7=type7, col7=col7, pch7=pch7, type8=type8, col8=col8, pch8=pch8, 
                              type9=type9, col9=col9, pch9=pch9,type10=type10,col10=col10,pch10=pch10,
                              Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                              Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                              Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                              Msubtype4=Msubtype4, Mcol4=Mcol4, Mpch4=Mpch4, 
                              lineCol=lineCol, trainingPlotType=trainingPlotType,
                              yStatsName=yStatsName, yFactor=yFactor,
                              xMin=xMinVol, xMax=xMaxVol, yMin=yMin, yMax=yMax, costs=costs) 
   
   plotAllReturnsVsDrawdown(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                            type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                            type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                            type7=type7, col7=col7, pch7=pch7, type8=type8, col8=col8, pch8=pch8, 
                            type9=type9, col9=col9, pch9=pch9,type10=type10,col10=col10,pch10=pch10,
                            Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                            Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                            Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                            Msubtype4=Msubtype4, Mcol4=Mcol4, Mpch4=Mpch4, 
                            lineCol=lineCol, trainingPlotType=trainingPlotType,
                            yStatsName=yStatsName, yFactor=yFactor,
                            xMin=xMinDD2, xMax=xMaxDD2, yMin=yMin, yMax=yMax, costs=costs) 
   
   plotAllReturnsVsAverageAlloc(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                type7=type7, col7=col7, pch7=pch7, type8=type8, col8=col8, pch8=pch8, 
                                type9=type9, col9=col9, pch9=pch9,type10=type10,col10=col10,pch10=pch10,
                                Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                Msubtype4=Msubtype4, Mcol4=Mcol4, Mpch4=Mpch4, 
                                lineCol=lineCol, trainingPlotType=trainingPlotType,
                                yStatsName=yStatsName, yFactor=yFactor,
                                xMin=xMinAlloc, xMax=xMaxAlloc, yMin=yMin, yMax=yMax, costs=costs) 
   
   plotAllReturnsVsInverseTurnover(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                   type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                   type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                   type7=type7, col7=col7, pch7=pch7, type8=type8, col8=col8, pch8=pch8, 
                                   type9=type9, col9=col9, pch9=pch9,type10=type10,col10=col10,pch10=pch10,
                                   Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                   Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                   Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                   Msubtype4=Msubtype4, Mcol4=Mcol4, Mpch4=Mpch4, 
                                   lineCol=lineCol, trainingPlotType=trainingPlotType,
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
                                type7=def$type7, col7=def$col7, pch7=def$pch7, 
                                type8=def$type8, col8=def$col8, pch8=def$pch8, 
                                type9=def$type9, col9=def$col9, pch9=def$pch9, 
                                type10=def$type10,col10=def$col10,pch10=def$pch10,
                                Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                Msubtype4=def$Msubtype4, Mcol4=def$Mcol4, Mpch4=def$Mpch4, 
                                lineCol=def$lineCol, trainingPlotType="dots",
                                xMinVol=def$minVol, xMaxVol=def$maxVol, xMinDD2=def$minDD2, xMaxDD2=def$maxDD2, 
                                yStatsName=def$yStatsName, yFactor=100, col=F,
                                yMin=def$yTRmin, yMax=def$yTRmax, costs=def$tradingCost+def$riskAsCost,
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
                              type7=type7, col7=col7, pch7=pch7, type8=type8, col8=col8, pch8=pch8, 
                              type9=type9, col9=col9, pch9=pch9,type10=type10,col10=col10,pch10=pch10,
                              Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                              Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                              Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                              Msubtype4=Msubtype4, Mcol4=Mcol4, Mpch4=Mpch4, 
                              lineCol=lineCol, trainingPlotType=trainingPlotType,
                              yStatsName=yStatsName, yFactor=yFactor,
                              xMin=xMinVol, xMax=xMaxVol, yMin=yMin, yMax=yMax, costs=costs) 
   
   plotAllReturnsVsDrawdown(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                            type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                            type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                            type7=type7, col7=col7, pch7=pch7, type8=type8, col8=col8, pch8=pch8, 
                            type9=type9, col9=col9, pch9=pch9,type10=type10,col10=col10,pch10=pch10,
                            Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                            Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                            Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                            Msubtype4=Msubtype4, Mcol4=Mcol4, Mpch4=Mpch4, 
                            lineCol=lineCol, trainingPlotType=trainingPlotType,
                            yStatsName=yStatsName, yFactor=yFactor,
                            xMin=xMinDD2, xMax=xMaxDD2, yMin=yMin, yMax=yMax, costs=costs) 
   if (figureTitle != "") title( figureTitle, outer = TRUE )
   par(mfrow = c(1, 1), oma = c( 0, 0, 0, 0 ))
   
   if(pngOutput) {
      dev.off()
      print( paste0("png file (", pngWidth, " by ", pngHeight, ") written to: ", pngName) )
   }
}


plotAllDrawdownsVsVolatility <- function(type1=def$type1, col1=def$col1, pch1=def$pch1, 
                                       type2=def$type2, col2=def$col2, pch2=def$pch2,
                                       type3=def$type3, col3=def$col3, pch3=def$pch3, 
                                       type4=def$type4, col4=def$col4, pch4=def$pch4,
                                       type5=def$type5, col5=def$col5, pch5=def$pch5, 
                                       type6=def$type6, col6=def$col6, pch6=def$pch6, 
                                       type7=def$type7, col7=def$col7, pch7=def$pch7, 
                                       type8=def$type8, col8=def$col8, pch8=def$pch8, 
                                       type9=def$type9, col9=def$col9, pch9=def$pch9, 
                                       type10=def$type10,col10=def$col10,pch10=def$pch10,
                                       Msubtype1=def$Msubtype1, Mcol1=def$Mcol1, Mpch1=def$Mpch1, 
                                       Msubtype2=def$Msubtype2, Mcol2=def$Mcol2, Mpch2=def$Mpch2, 
                                       Msubtype3=def$Msubtype3, Mcol3=def$Mcol3, Mpch3=def$Mpch3, 
                                       Msubtype4=def$Msubtype4, Mcol4=def$Mcol4, Mpch4=def$Mpch4, 
                                       lineCol=def$lineCol, trainingPlotType="dots",
                                       xFactor=100, xLabel="volatility (%)",
                                       yFactor=100, yLabel="drawdowns (%)",
                                       xMin=def$minVol, xMax=def$maxVol, yMin=def$minDD2, yMax=def$maxDD2, 
                                       costs=def$tradingCost,
                                       pngOutput=F, pngWidth=def$pngWidth, pngHeight=def$pngHeight, 
                                       pngName="figures/drawdowns_vs_volatility.png") {
   
   plotAllReturnsVsSomeParameter(type1=type1, col1=col1, pch1=pch1, type2=type2, col2=col2, pch2=pch2,
                                 type3=type3, col3=col3, pch3=pch3, type4=type4, col4=col4, pch4=pch4,
                                 type5=type5, col5=col5, pch5=pch5, type6=type6, col6=col6, pch6=pch6, 
                                 type7=type7, col7=col7, pch7=pch7, type8=type8, col8=col8, pch8=pch8, 
                                 type9=type9, col9=col9, pch9=pch9,type10=type10,col10=col10,pch10=pch10,
                                 Msubtype1=Msubtype1, Mcol1=Mcol1, Mpch1=Mpch1, 
                                 Msubtype2=Msubtype2, Mcol2=Mcol2, Mpch2=Mpch2, 
                                 Msubtype3=Msubtype3, Mcol3=Mcol3, Mpch3=Mpch3, 
                                 Msubtype4=Msubtype4, Mcol4=Mcol4, Mpch4=Mpch4, 
                                 lineCol=lineCol, trainingPlotType=trainingPlotType,
                                 xStatsName="volatility", xFactor=xFactor, xLabel=xLabel,
                                 yStatsName="DD2", yFactor=yFactor, yLabel=yLabel,
                                 xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax, costs=costs,
                                 pngOutput=pngOutput, pngWidth=pngWidth, pngHeight=pngHeight, pngName=pngName) 
}


## Obsolete: use createStrategiesAndSavePlots() instead
# saveAllPlotsAsPng <- function(pngWidth=def$pngWidth, pngHeight=def$pngHeight) {
#    plotReturn(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
#    plotAlloc(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
#    plotReturnAndAlloc(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
#    
#    plotAllReturnsVsVolatility(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
#    plotAllReturnsVsDrawdown(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
#    plotAllReturnsVsAverageAlloc(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
#    plotAllReturnsVsInverseTurnover(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
#    plotAllReturnsVsFour(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
#    plotAllReturnsVsTwo(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
# }



createStrategiesAndSavePlots_generic <- function(futureYears, tradingCost, dataSplit, plotMany=T,
                                                 startYear, endYear, figureTitle, figureTitleShort,
                                                 pngWidth=def$pngWidth, pngHeight=def$pngHeight,
                                                 otherAssetClasses=F) {
   
   start(dataSplit=dataSplit, futureYears=futureYears, tradingCost=tradingCost, 
         riskAsCost=0, # no riskAs Cost for plotting
         extrapolateDividends=T, smoothConstantAlloc=T, downloadAndCheckAllFiles=F,
         otherAssetClasses=otherAssetClasses, newcomer=F, force=T)
   rangeName         <- paste0(startYear, "_", endYear)
   rangeNameShort    <- paste0(startYear, "_", endYear-futureYears)   
   
   plotAllReturnsVsFour(yMin=5.3, pngOutput=T, pngName=paste0("figures/", rangeName, "-return_vs_four.png"), 
                        pngWidth=pngWidth, pngHeight=pngHeight, 
                        figureTitle=figureTitle )
   plotReturnAndAlloc(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight,
                      pngName=paste0("figures/", rangeName, "-return_and_allocation.png") )
   plotReturnAndAllocSubstrategies(detrendBy=5, pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight,
                                   pngName=paste0("figures/", rangeName, "-return_and_allocation-detrended.png") )
   #    plotFutureReturnVsCAPE(futureYears=futureYears, maxTR=21, 
   #                           pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight, 
   #                           pngName=paste0("figures/", rangeNameShort, "-return_over_next_", 
   #                                          futureYears, "_years_vs_CAPE.png"),
   #                           figureTitle=figureTitleShort)
   if (plotMany) {
      plotRelativeFutureReturn(futureYears=futureYears, refStratName="stocks", 
                               legendPlacement="topleft", 
                               pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight,
                               pngName=paste0("figures/", rangeNameShort, "-return_over_next_", 
                                              futureYears, "_years-reference_stocks.png"))
      plotFutureReturn(futureYears=futureYears, pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight,
                       pngName=paste0("figures/", rangeNameShort, "-return_over_next_", futureYears, "_years.png"))
      plotCumulativeFutureReturn(futureYears=futureYears, pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight,
                                 pngName=paste0("figures/", rangeNameShort, 
                                                "-cumulative_return_over_next_", futureYears,"_years.png"),
                                 figureTitle=figureTitleShort )
   }
   #    plotRelativeFutureReturnVsStocks(futureYears=futureYears, pngOutput=T, 
   #                                     pngName=paste0("figures/", rangeNameShort, "-relative_return_over_next_", 
   #                                                    futureYears, "_years_vs_stocks.png"),
   #                                     figureTitle=figureTitleShort )
   #    searchForOptimalBalanced(byF1=2.02, byF2=0.02)
   #    plotAllReturnsVsFour(trainingPlotType="line", pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight, 
   #                         pngName="figures/1871_1942-balanced_optimization.png")
}

createStrategiesAndSavePlots_training <- function(futureYears, tradingCost, plotMany=F, 
                                                pngWidth=def$pngWidth, pngHeight=def$pngHeight) {
   
   figureTitle      <- paste0("1871-1942 (optimization range)")
   figureTitleShort <- paste0("1871-", 1942-futureYears, " (optimization range)")   
   createStrategiesAndSavePlots_generic(futureYears=futureYears, tradingCost=tradingCost, dataSplit="training",
                                        startYear=1871, endYear=1942, figureTitle, figureTitleShort,
                                        pngWidth=pngWidth, pngHeight=pngHeight, plotMany=plotMany) 
}
   
createStrategiesAndSavePlots_testing <- function(futureYears, tradingCost, plotMany=T, 
                                                 pngWidth=def$pngWidth, pngHeight=def$pngHeight) {
   
   figureTitle      <- paste0("1942-2014 (parameters optimized over 1871-1942)")
   figureTitleShort <- paste0("1942-", 2014-futureYears, " (parameters optimized over 1871-1942)")   
   createStrategiesAndSavePlots_generic(futureYears=futureYears, tradingCost=tradingCost, dataSplit="testing",
                                        startYear=1942, endYear=2014, figureTitle, figureTitleShort,
                                        pngWidth=pngWidth, pngHeight=pngHeight, 
                                        otherAssetClasses=T, plotMany=plotMany)
   
   plotAssetClassesReturn(pngOutput=T)
   createGoldStrategy(pngOutput=T)
}

createStrategiesAndSavePlots_all <- function(futureYears, tradingCost, plotMany=F, 
                                              pngWidth=def$pngWidth, pngHeight=def$pngHeight) {
   
   figureTitle      <- paste0("1871-2014 (parameters optimized over 1871-1942)")
   figureTitleShort <- paste0("1871-", 2014-futureYears, " (parameters optimized over 1871-1942)")   
   createStrategiesAndSavePlots_generic(futureYears=futureYears, tradingCost=tradingCost, dataSplit="all",
                                        startYear=1871, endYear=2014, figureTitle, figureTitleShort,
                                        pngWidth=pngWidth, pngHeight=pngHeight, plotMany=plotMany) 
}


createStrategiesAndSavePlots <- function(futureYears=c(10L, 20L, 30L), tradingCost=4/100, plotMany=c(F, T, F), 
                                         pngWidth=def$pngWidth, pngHeight=def$pngHeight) {
   source("main.r")       # for start()
      
   if ( plotMany[1] )    
      for (i in 1:length(futureYears)) 
         createStrategiesAndSavePlots_training(futureYears=futureYears[i], tradingCost=tradingCost,
                                             plotMany=plotMany[1], pngWidth=pngWidth, pngHeight=pngHeight)
   else # we do not plot anything related to 'futureYears', so no need to do several
      createStrategiesAndSavePlots_training(futureYears=futureYears[1], tradingCost=tradingCost,
                                          plotMany=plotMany[1], pngWidth=pngWidth, pngHeight=pngHeight)
   

   if ( plotMany[2] )    
      for (i in 1:length(futureYears)) 
         createStrategiesAndSavePlots_testing(futureYears=futureYears[i], tradingCost=tradingCost,
                                              plotMany=plotMany[2], pngWidth=pngWidth, pngHeight=pngHeight)
   else # we do not plot anything related to 'futureYears', so no need to do several
      createStrategiesAndSavePlots_testing(futureYears=futureYears[1], tradingCost=tradingCost,
                                           plotMany=plotMany[2], pngWidth=pngWidth, pngHeight=pngHeight)
   
   if ( plotMany[3] )    
      for (i in 1:length(futureYears)) 
         createStrategiesAndSavePlots_all(futureYears=futureYears[i], tradingCost=tradingCost,
                                           plotMany=plotMany[3], pngWidth=pngWidth, pngHeight=pngHeight)
   else # we do not plot anything related to 'futureYears', so no need to do several
      createStrategiesAndSavePlots_all(futureYears=futureYears[1], tradingCost=tradingCost,
                                        plotMany=plotMany[3], pngWidth=pngWidth, pngHeight=pngHeight)
   
   plotReturnSideways(pngOutput=T, pngWidth=pngWidth, pngHeight=pngHeight)
}


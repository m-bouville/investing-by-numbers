
plotAssetReturn <- function(stratName1="gold", stratName2="bonds", 
                            stratName3="UKhousePrice", stratName4="stocks", 
                       startYear=1975.25, endYear=2014, minTR=.5, maxTR=15) {
   plotReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
              startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR) 
}


plotReturn <- function(stratName1=def$typicalStrategies[[1]], stratName2=def$typicalStrategies[[2]], 
                       stratName3=def$typicalStrategies[[3]], stratName4=def$typicalStrategies[[4]], 
                       startYear=def$startYear, endYear=2015, tradingCost=def$tradingCost, 
                       minTR=.9, maxTR=20000, net=T, normalize=T) { 
   
   normDate <- (startYear-1871)*12+1
   par(mar=c(2.5, 4, 1.5, 1.5))
   xRange <- c(startYear, endYear)
   yRange <- c(minTR, maxTR)
   
   TR1 <- numeric(numData)
   TR2 <- numeric(numData)
   TR3 <- numeric(numData)
   TR4 <- numeric(numData)
   
   if(net) {
      yLabel <- paste0("total return (%), net of trading costs of ", round(tradingCost*100), "%")
      if (tradingCost == 0.02) {
         TR1 <- netTR2[, stratName1] 
         TR2 <- netTR2[, stratName2] 
         TR3 <- netTR2[, stratName3] 
         TR4 <- netTR2[, stratName4] 
      } 
      else if(tradingCost == 0.04)  {
         TR1 <- netTR4[, stratName1] 
         TR2 <- netTR4[, stratName2] 
         TR3 <- netTR4[, stratName3] 
         TR4 <- netTR4[, stratName4] 
      } 
      else stop("No data frame \'netTR", round(tradingCost*100,1), "\' exists.")      
   } else {
      yLabel <- "total return, GROSS of trading costs"
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
   print(summary(TR1))
   
   if(stratName1 %in% colnames(TR)) {   
      plot(TR$numericDate, TR1/normFactor1, col="red", xlab="", ylab=yLabel, log="y", type="l", lwd=1.5, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if(stratName2 %in% colnames(TR)) {   
      plot(TR$numericDate, TR2/normFactor2, col="blue", xlab="", ylab="", log="y", type="l", lwd=1.5, xlim=xRange, ylim=yRange)
      par(new=T)
   }  
   if(stratName3 %in% colnames(TR)) {   
      plot(TR$numericDate, TR3/normFactor3, col="black", xlab="", ylab="", log="y", type="l", lwd=2, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if(stratName4 %in% colnames(TR)) {   
      plot(TR$numericDate, TR4/normFactor4, col="green", xlab="", ylab="", log="y", type="l", lwd=2, xlim=xRange, ylim=yRange)
   }
   legend("topleft", c(stratName1,stratName2,stratName3,stratName4), cex=1, bty="n", lwd=c(1.5,1.5,2,2), lty = c(1,1,1,1), col=c("red","blue","black","green"))
   par(new=F)
}

plotAlloc <- function(stratName1=def$typicalStrategies[[1]], stratName2=def$typicalStrategies[[2]], 
                      stratName3=def$typicalStrategies[[3]], stratName4=def$typicalStrategies[[4]], 
                      startYear=def$startYear, endYear=2014) { 
   
   normDate <- (startYear-1871)*12+1
   par(mar=c(2.5, 4, 1.5, 1.5))
   xRange <- c(startYear, endYear)
   yRange <- c(0, 1)
   
   if(stratName1 %in% colnames(alloc)) {   
      plot(alloc$numericDate, alloc[, stratName1], col="red", xlab="", ylab="stock allocation", type="l", xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if(stratName2 %in% colnames(alloc)) {   
      plot(alloc$numericDate, alloc[, stratName2], col="blue", xlab="", ylab="", type="l", xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if(stratName3 %in% colnames(alloc)) {
      plot(alloc$numericDate, alloc[, stratName3], col="black", xlab="", ylab="", type="l", lwd=2, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if(stratName4 %in% colnames(alloc)) {   
      plot(alloc$numericDate, alloc[, stratName4], col="green", xlab="", ylab="", type="l", xlim=xRange, ylim=yRange)
   }
   par(new=F)   
}

plotReturnAndAlloc <- function(stratName1=def$typicalStrategies[[1]], stratName2=def$typicalStrategies[[2]], 
                               stratName3=def$typicalStrategies[[3]], stratName4=def$typicalStrategies[[4]], 
                               startYear=def$startYear, endYear=2015, tradingCost=def$tradingCost, 
                               minTR=.9, maxTR=20000, net=T, normalize=T) {
   par(mfrow = c(2, 1))
   plotReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
              startYear=startYear, endYear=endYear, tradingCost=tradingCost, 
              normalize=normalize, net=net, minTR=minTR, maxTR=maxTR)  
   plotAlloc(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
             startYear=startYear, endYear=endYear)        
   par(mfrow = c(1, 1))
}


plotReturnVsVolatility <- function(tradingCost=def$tradingCost) { 
   
   xRange <- c(12, 16)
   yRange <- c(5.5, 9.5 - 100*tradingCost/2)
   par( mar=c(4, 4, 1.5, 1.5) )
   
   #    plot(100*subset(stats$volatility, stats$type!="constantAlloc"), 
   #         100*(subset(stats$TR, stats$type!="constantAlloc") - 
   #                 tradingCost/subset(stats$turnover, stats$type!="constantAlloc")), 
   #         pch=15, col="black", xlab="", ylab="", xlim=xRange, ylim=yRange)
   #    par(new=T)
   plot(100*subset(stats$volatility, stats$type=="CAPE"), 
        100*(subset(stats$TR, stats$type=="CAPE") - 
                tradingCost/subset(stats$turnover, stats$type=="CAPE")), 
        pch=18, col="cyan", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$volatility, stats$type=="detrended"), 
        100*(subset(stats$TR, stats$type=="detrended") - 
                tradingCost/subset(stats$turnover, stats$type=="detrended")), 
        pch=18, col="skyblue", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$volatility, stats$type=="momentum"), 
        100*(subset(stats$TR, stats$type=="momentum") - 
                tradingCost/subset(stats$turnover, stats$type=="momentum")), 
        pch=18, col="orange", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$volatility, stats$type=="SMA"), 
        100*(subset(stats$TR, stats$type=="SMA") - 
                tradingCost/subset(stats$turnover, stats$type=="SMA")), 
        pch=18, col="orangered1", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$volatility, stats$type=="Bollinger"), 
        100*(subset(stats$TR, stats$type=="Bollinger") - 
                tradingCost/subset(stats$turnover, stats$type=="Bollinger")), 
        pch=18, col="magenta", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$volatility, stats$subtype=="balanced"), 
        100*(subset(stats$TR, stats$subtype=="balanced") - 
                tradingCost/subset(stats$turnover, stats$subtype=="balanced")), 
        pch=15, col="black", xlim=xRange, ylim=yRange,
        xlab="volatility (%)", ylab=paste0("total return (%), net of trading cost of ", round(tradingCost*100), "%") )
   par(new=T)
   plot(100*subset(stats$volatility, stats$type=="constantAlloc"), 
        100*(subset(stats$TR, stats$type=="constantAlloc") - 
                tradingCost/subset(stats$turnover, stats$type=="constantAlloc")), 
        type="l", col="black", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$volatility, stats$subtype=="technical"), 
        100*(subset(stats$TR, stats$subtype=="technical") - 
                tradingCost/subset(stats$turnover, stats$subtype=="technical")), 
        pch=15, col="red", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$volatility, stats$subtype=="value"), 
        100*(subset(stats$TR, stats$subtype=="value") - 
                tradingCost/subset(stats$turnover, stats$subtype=="value")), 
        pch=15, col="blue", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=F)
}

plotReturnVsDrawdown <- function(tradingCost=def$tradingCost) { 
   
   xRange <- c(1, 2.5)
   yRange <- c(5.5, 9.5 - 50*tradingCost)
   par(mar=c(4, 4, 1.5, 1.5))
   
   #    plot(subset(stats$DD2, stats$type!="constantAlloc"), 
   #         100*(subset(stats$TR, stats$type!="constantAlloc") - 
   #                 tradingCost/subset(stats$turnover, stats$type!="constantAlloc")), 
   #         pch=15, col="black", xlab="", ylab="", xlim=xRange, ylim=yRange)
   #    par(new=T)
   plot(subset(stats$DD2, stats$type=="CAPE"), 
        100*(subset(stats$TR, stats$type=="CAPE") - 
                tradingCost/subset(stats$turnover, stats$type=="CAPE")), 
        pch=18, col="cyan", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(subset(stats$DD2, stats$type=="detrended"), 
        100*(subset(stats$TR, stats$type=="detrended") - 
                tradingCost/subset(stats$turnover, stats$type=="detrended")), 
        pch=18, col="skyblue", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(subset(stats$DD2, stats$type=="momentum"), 
        100*(subset(stats$TR, stats$type=="momentum") - 
                tradingCost/subset(stats$turnover, stats$type=="momentum")), 
        pch=18, col="orange", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(subset(stats$DD2, stats$type=="SMA"), 
        100*(subset(stats$TR, stats$type=="SMA") - 
                tradingCost/subset(stats$turnover, stats$type=="SMA")), 
        pch=18, col="orangered1", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(subset(stats$DD2, stats$type=="Bollinger"), 
        100*(subset(stats$TR, stats$type=="Bollinger") - 
                tradingCost/subset(stats$turnover, stats$type=="Bollinger")), 
        pch=18, col="magenta", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(subset(stats$DD2, stats$subtype=="balanced"), 
        100*(subset(stats$TR, stats$subtype=="balanced") - 
                tradingCost/subset(stats$turnover, stats$subtype=="balanced")), 
        xlab="drawdown", ylab=paste0("total return (%), net of trading cost of ", round(tradingCost*100), "%"), 
        pch=15, col="black", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(subset(stats$DD2, stats$subtype=="technical"), 
        100*(subset(stats$TR, stats$subtype=="technical") - 
                tradingCost/subset(stats$turnover, stats$subtype=="technical")), 
        pch=15, col="red", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(subset(stats$DD2, stats$subtype=="value"), 
        100*(subset(stats$TR, stats$subtype=="value") - 
                tradingCost/subset(stats$turnover, stats$subtype=="value")), 
        pch=15, col="blue", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(subset(stats$DD2, stats$type=="constantAlloc"), 
        100*(subset(stats$TR, stats$type=="constantAlloc") - 
                tradingCost/subset(stats$turnover, stats$type=="constantAlloc")), 
        type="l", col="black", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=F)
}

plotReturnVsAverageAlloc <- function(tradingCost=def$tradingCost) { 
   
   xRange <- c(40, 100)
   yRange <- c(5.5, 9.5 - 100*tradingCost/2)
   par( mar=c(4, 4, 1.5, 1.5) )
   
   #    plot(100*subset(stats$avgStockAlloc, stats$type!="constantAlloc"), 
   #         100*(subset(stats$TR, stats$type!="constantAlloc") - 
   #                 tradingCost/subset(stats$turnover, stats$type!="constantAlloc")), 
   #         pch=15, col="black", xlab="avg. stock alloc.", ylab="", xlim=xRange, ylim=yRange)
   #    par(new=T)
   plot(100*subset(stats$avgStockAlloc, stats$type=="CAPE"), 
        100*(subset(stats$TR, stats$type=="CAPE") - 
                tradingCost/subset(stats$turnover, stats$type=="CAPE")), 
        pch=18, col="cyan", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$avgStockAlloc, stats$type=="detrended"), 
        100*(subset(stats$TR, stats$type=="detrended") - 
                tradingCost/subset(stats$turnover, stats$type=="detrended")), 
        pch=18, col="skyblue", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$avgStockAlloc, stats$type=="momentum"), 
        100*(subset(stats$TR, stats$type=="momentum") - 
                tradingCost/subset(stats$turnover, stats$type=="momentum")), 
        pch=18, col="orange", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$avgStockAlloc, stats$type=="SMA"), 
        100*(subset(stats$TR, stats$type=="SMA") - 
                tradingCost/subset(stats$turnover, stats$type=="SMA")), 
        pch=18, col="orangered1", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$avgStockAlloc, stats$type=="Bollinger"), 
        100*(subset(stats$TR, stats$type=="Bollinger") - 
                tradingCost/subset(stats$turnover, stats$type=="Bollinger")), 
        pch=18, col="magenta", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)   
   plot(100*subset(stats$avgStockAlloc, stats$subtype=="balanced"), 
        100*(subset(stats$TR, stats$subtype=="balanced") - 
                tradingCost/subset(stats$turnover, stats$subtype=="balanced")), 
        pch=15, col="black", xlim=xRange, ylim=yRange, 
        xlab="avg. stock alloc. (%)", ylab=paste0("total return (%), net of trading cost of ", round(tradingCost*100), "%") )
   par(new=T)   
   plot(100*subset(stats$avgStockAlloc, stats$subtype=="technical"), 
        100*(subset(stats$TR, stats$subtype=="technical") - 
                tradingCost/subset(stats$turnover, stats$subtype=="technical")), 
        pch=15, col="red", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)   
   plot(100*subset(stats$avgStockAlloc, stats$subtype=="value"), 
        100*(subset(stats$TR, stats$subtype=="value") - 
                tradingCost/subset(stats$turnover, stats$subtype=="value")), 
        pch=15, col="blue", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)   
   plot(100*subset(stats$avgStockAlloc, stats$type=="constantAlloc"), 
        100*(subset(stats$TR, stats$type=="constantAlloc") - 
                tradingCost/subset(stats$turnover, stats$type=="constantAlloc")), 
        type="l", col="black", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=F)
}

plotReturnVsTurnover <- function(tradingCost=def$tradingCost) { 
   
   xRange <- c(0, 100)
   yRange <- c(5.5, 9.5 - 100*tradingCost/2)
   par( mar=c(4, 4, 1.5, 1.5) )
   
   plot(100/subset(stats$turnover, stats$type=="CAPE"), 
        100*(subset(stats$TR, stats$type=="CAPE") - 
                tradingCost/subset(stats$turnover, stats$type=="CAPE")), 
        pch=18, col="cyan", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100/subset(stats$turnover, stats$type=="detrended"), 
        100*(subset(stats$TR, stats$type=="detrended") - 
                tradingCost/subset(stats$turnover, stats$type=="detrended")), 
        pch=18, col="skyblue", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100/subset(stats$turnover, stats$type=="momentum"), 
        100*(subset(stats$TR, stats$type=="momentum") - 
                tradingCost/subset(stats$turnover, stats$type=="momentum")), 
        pch=18, col="orange", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100/subset(stats$turnover, stats$type=="SMA"), 
        100*(subset(stats$TR, stats$type=="SMA") - 
                tradingCost/subset(stats$turnover, stats$type=="SMA")), 
        pch=18, col="orangered1", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100/subset(stats$turnover, stats$type=="Bollinger"), 
        100*(subset(stats$TR, stats$type=="Bollinger") - 
                tradingCost/subset(stats$turnover, stats$type=="Bollinger")), 
        pch=18, col="magenta", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)   
   plot(100/subset(stats$turnover, stats$subtype=="balanced"), 
        100*(subset(stats$TR, stats$subtype=="balanced") - 
                tradingCost/subset(stats$turnover, stats$subtype=="balanced")), 
        pch=15, col="black", xlim=xRange, ylim=yRange, 
        xlab="100 / turnover (yrs)", ylab=paste0("total return (%), net of trading cost of ", round(tradingCost*100), "%") )
   par(new=T)   
   plot(100/subset(stats$turnover, stats$subtype=="technical"), 
        100*(subset(stats$TR, stats$subtype=="technical") - 
                tradingCost/subset(stats$turnover, stats$subtype=="technical")), 
        pch=15, col="red", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)   
   plot(100/subset(stats$turnover, stats$subtype=="value"), 
        100*(subset(stats$TR, stats$subtype=="value") - 
                tradingCost/subset(stats$turnover, stats$subtype=="value")), 
        pch=15, col="blue", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=F)
}

plotReturnVsFour <- function(tradingCost=def$tradingCost) {
   par(mfrow = c(2, 2))
   plotReturnVsVolatility(tradingCost=tradingCost) 
   plotReturnVsDrawdown(tradingCost=tradingCost)
   plotReturnVsAverageAlloc(tradingCost=tradingCost)
   plotReturnVsTurnover(tradingCost=tradingCost)
   par(mfrow = c(1, 1))
}

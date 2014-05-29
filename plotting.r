
plotAssetReturn <- function(stratName1="gold", stratName2="bonds", stratName3=def$typicalStrategies[[3]], stratName4="stocks", 
                       startYear=1968, endYear=2014, minTR=.65, maxTR=20, normalize=T, showAlloc=T) {
   plotReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
              startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, normalize=normalize, showAlloc=showAlloc) 
}

plotReturnAndAlloc <- function(stratName1=def$typicalStrategies[[1]], stratName2=def$typicalStrategies[[2]], 
                               stratName3=def$typicalStrategies[[3]], stratName4=def$typicalStrategies[[4]], 
                               startYear=def$startYear, endYear=2014, minTR=.9, maxTR=100000, normalize=T) {
   par(mfrow = c(2, 1))
   plotReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
              startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, normalize=normalize)  
   plotAlloc(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
              startYear=startYear, endYear=endYear)        
   par(mfrow = c(1, 1))
}

# plotFutureReturn <- function(stratName1=def$typicalStrategies[[1]], stratName2=def$typicalStrategies[[2]], stratName3=def$typicalStrategies[[3]], stratName4=def$typicalStrategies[[4]], 
#                              years=def$futureYears, startYear=def$startYear, endYear=2014-years, minTR=0, maxTR=.2, normalize=F, showAlloc=F) {
#    genericPlotReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
#                      suffix=paste0("Future",years), startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, normalize=normalize, showAlloc=showAlloc)  
# }
# 
# plotBothReturns <- function(stratName1=def$typicalStrategies[[1]], stratName2=def$typicalStrategies[[2]], stratName3=def$typicalStrategies[[3]], stratName4=def$typicalStrategies[[4]], 
#                              years=def$futureYears, startYear=def$startYear, endYear=2014, minTR1=.9, maxTR1=100000, minTR2=0, maxTR2=.2) {
#    par(mfrow = c(2, 1))
#    plotReturn(startYear=startYear, endYear=endYear, minTR=minTR1, maxTR=maxTR1, normalize=T, showAlloc=F)  
#    plotFutureReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4,
#                     years=years, startYear=startYear, endYear=endYear, minTR=minTR2, maxTR=maxTR2, normalize=F, showAlloc=F)
# }

plotReturn <- function(stratName1=def$typicalStrategies[[1]], stratName2=def$typicalStrategies[[2]], 
                       stratName3=def$typicalStrategies[[3]], stratName4=def$typicalStrategies[[4]], 
                       startYear=def$startYear, endYear=2014, minTR=.9, maxTR=100000, normalize=T) { 
   
   normDate <- (startYear-1871)*12+1
   par(mar=c(2.5, 4, 1.5, 1.5))
   xRange <- c(startYear, endYear)
   yRange <- c(minTR, maxTR)
   
   if (normalize) {
      normFactor1 <- TR[normDate, stratName1]
      normFactor2 <- TR[normDate, stratName2]
      normFactor3 <- TR[normDate, stratName3]
      normFactor4 <- TR[normDate, stratName4]
   } else {      
      normFactor1 <- 1
      normFactor2 <- 1
      normFactor3 <- 1
      normFactor4 <- 1
   }
   
   if(stratName1 %in% colnames(TR)) {   
      plot(TR$numericDate, TR[, stratName1]/normFactor1, col="red", xlab="", ylab="total return (GROSS of trading cost)", log="y", type="l", lwd=1.5, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if(stratName2 %in% colnames(TR)) {   
      plot(TR$numericDate, TR[, stratName2]/normFactor2, col="blue", xlab="", ylab="", log="y", type="l", lwd=1.5, xlim=xRange, ylim=yRange)
      par(new=T)
   }  
   if(stratName3 %in% colnames(TR)) {   
      plot(TR$numericDate, TR[, stratName3]/normFactor3, col="black", xlab="", ylab="", log="y", type="l", lwd=2, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if(stratName4 %in% colnames(TR)) {   
      plot(TR$numericDate, TR[, stratName4]/normFactor4, col="green", xlab="", ylab="", log="y", type="l", lwd=2, xlim=xRange, ylim=yRange)
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
      plot(alloc$numericDate, alloc[, stratName1], col="red", xlab="", ylab="stock alloc.", type="l", xlim=xRange, ylim=yRange)
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


# plotFutureReturnVsCAPE <- function(futureReturnYears=def$futureYears, CAPEname1="CAPE10", CAPEname2="CAPE10avg24", CAPEmax=35) { 
#    futureReturnName <- paste0("futureReturn", futureReturnYears)
#    if (!futureReturnName %in% colnames(dat)) calcFutureReturn(futureReturnYears)
#    
#    xRange <- c(5, CAPEmax)
#    par(mar=c(4, 4, 1.5, 1.5))
#    par(mfrow = c(1, 1))
#    
#    plot(dat[, CAPEname1], dat[, futureReturnName], col="red", xlab="CAPE", ylab="total return", xlim=xRange)
#    par(new=T)
#    plot(dat[, CAPEname2], dat[, futureReturnName], col="blue", xlab="", ylab="", xlim=xRange)
#    par(new=F)
#    
#    mod <- lm( dat[, futureReturnName] ~ dat[, CAPEname1] )
#    print(paste0(CAPEname1, ": TR = ", round(100*mod$coefficients[1],1), "%", round(100*mod$coefficients[2],2), "%*CAPE (r^2 = ", 
#                        round(summary(mod)$r.squared*100), "%)"))
#    abline(mod, col="red")
#    
#    mod <- lm( dat[, futureReturnName] ~ dat[, CAPEname2] )
#    print(paste0(CAPEname2, ": TR = ", round(100*mod$coefficients[1],1), "%", round(100*mod$coefficients[2],2), "%*CAPE (r^2 = ", 
#                 round(summary(mod)$r.squared*100), "%)"))
# }
# 
# 
# plotFutureReturnVsAlloc <- function(futureReturnYears=def$futureYears, name1, name2) { 
#    futureReturnName <- paste0("futureReturn", futureReturnYears)
#    if (!futureReturnName %in% colnames(dat)) calcFutureReturn(futureReturnYears)
#    
#    xRange <- c(0, 1)
#    par(mar=c(4, 4, 1.5, 1.5))
#    par(mfrow = c(1, 1))
#    
#    plot(alloc[, name1], dat[, futureReturnName], col="red", xlab="stocks alloc.", ylab="total return", xlim=xRange)
#    par(new=T)
#    plot(alloc[, name2], dat[, futureReturnName], col="blue", xlab="", ylab="", xlim=xRange)
#    par(new=F)
#    
#    mod <- lm( dat[, futureReturnName] ~ alloc[, name1] )
#    print(paste0(name1, ": TR = ", round(100*mod$coefficients[1],1), "% + ", round(100*mod$coefficients[2],2), "%*alloc (r^2 = ", 
#                 round(summary(mod)$r.squared*100), "%)"))
#    mod <- lm( dat[, futureReturnName] ~ alloc[, name2] )
#    print(paste0(name2, ": TR = ", round(100*mod$coefficients[1],1), "% + ", round(100*mod$coefficients[2],2), "%*alloc (r^2 = ", 
#                 round(summary(mod)$r.squared*100), "%)"))
# }



plotReturnVsAverageAlloc <- function(tradingCost=def$tradingCost) { 
   
   xRange <- c(40, 100)
   yRange <- c(5.5, 9.5 - 100*tradingCost/2)
   par( mar=c(4, 4, 1.5, 1.5) )
   
   #    plot(100*subset(stats$avgStockAlloc, stats$type!="constantAlloc"), 
   #         100*(subset(stats$TR, stats$type!="constantAlloc") - 
   #                 tradingCost/subset(stats$turnover, stats$type!="constantAlloc")), 
   #         pch=15, col="black", xlab="avg. stock alloc.", ylab="total return (%) net of trading cost", xlim=xRange, ylim=yRange)
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
        pch=15, col="black", xlab="avg. stock alloc. (%)", ylab="avg. stock alloc. (%), net of trading cost", xlim=xRange, ylim=yRange)
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
        pch=15, col="black", xlab="100 / turnover (yrs)", ylab="avg. stock alloc. (%), net of trading cost", xlim=xRange, ylim=yRange)
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
        pch=15, col="black", xlab="volatility (%)", ylab="total return (%), net of trading cost", xlim=xRange, ylim=yRange)
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
        pch=15, col="black", xlab="drawdown", ylab="total return (%), net of trading cost", xlim=xRange, ylim=yRange)
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

plotReturnVsFour <- function(tradingCost=def$tradingCost) {
   par(mfrow = c(2, 2))
   plotReturnVsVolatility(tradingCost=tradingCost) 
   plotReturnVsDrawdown(tradingCost=tradingCost)
   plotReturnVsAverageAlloc(tradingCost=tradingCost)
   plotReturnVsTurnover(tradingCost=tradingCost)
   par(mfrow = c(1, 1))
}

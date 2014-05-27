
plotAssetReturn <- function(stratName1="bonds", stratName2="gold", stratName3=def$typicalStrategies[[3]], stratName4="stocks", 
                       startYear=1968, endYear=2014, minTR=.65, maxTR=20, normalize=T, showAlloc=T) {
   plotReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
              startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, normalize=normalize, showAlloc=showAlloc) 
}

plotReturn <- function(stratName1=def$typicalStrategies[[1]], stratName2=def$typicalStrategies[[2]], stratName3=def$typicalStrategies[[3]], stratName4=def$typicalStrategies[[4]], 
                       startYear=def$startYear, endYear=2014, minTR=.9, maxTR=100000, normalize=T, showAlloc=T) {
   genericPlotReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
                     suffix="TR", startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, normalize=normalize, showAlloc=showAlloc)  
}

plotFutureReturn <- function(stratName1=def$typicalStrategies[[1]], stratName2=def$typicalStrategies[[2]], stratName3=def$typicalStrategies[[3]], stratName4=def$typicalStrategies[[4]], 
                             years=30, startYear=def$startYear, endYear=2014-years, minTR=0, maxTR=.2, normalize=F, showAlloc=F) {
   genericPlotReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4, 
                     suffix=paste0("Future",years), startYear=startYear, endYear=endYear, minTR=minTR, maxTR=maxTR, normalize=normalize, showAlloc=showAlloc)  
}

plotBothReturns <- function(stratName1=def$typicalStrategies[[1]], stratName2=def$typicalStrategies[[2]], stratName3=def$typicalStrategies[[3]], stratName4=def$typicalStrategies[[4]], 
                             years=30, startYear=def$startYear, endYear=2014, minTR1=.9, maxTR1=100000, minTR2=0, maxTR2=.2) {
   par(mfrow = c(2, 1))
   plotReturn(startYear=startYear, endYear=endYear, minTR=minTR1, maxTR=maxTR1, normalize=T, showAlloc=F)  
   plotFutureReturn(stratName1=stratName1, stratName2=stratName2, stratName3=stratName3, stratName4=stratName4,
                    years=years, startYear=startYear, endYear=endYear, minTR=minTR2, maxTR=maxTR2, normalize=F, showAlloc=F)
}

genericPlotReturn <- function(stratName1, stratName2, stratName3, stratName4, suffix, 
                              startYear=def$startYear, endYear=2014, minTR, maxTR, normalize, showAlloc) { 
   
   normDate <- (startYear-1871)*12+1
   par(mar=c(2.5, 4, 1.5, 1.5))
   xRange <- c(startYear, endYear)
   yRange <- c(minTR, maxTR)
   if(suffix=="TR") logScale <- "y"
   else logScale <- ""
   if(showAlloc==T) par(mfrow = c(2, 1))
     # else par(mfrow = c(1, 1))
   
   if(stratName1 %in% colnames(TR)) {   
      if(normalize==T) plot(TR$numericDate, TR[, stratName1]/TR[normDate, stratName1], col="red", xlab="", ylab="total return", log=logScale, type="l", lwd=1.5, xlim=xRange, ylim=yRange)
         else plot(TR$numericDate, TR[, stratName1], col="red", xlab="", ylab="total return", log=logScale, type="l", lwd=1.5, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if(stratName2 %in% colnames(TR)) {   
      if(normalize==T) plot(TR$numericDate, TR[, stratName2]/TR[normDate, stratName2], col="blue", xlab="", ylab="", log=logScale, type="l", lwd=1.5, xlim=xRange, ylim=yRange)
         else plot(TR$numericDate, TR[, stratName2], col="blue", xlab="", ylab="", log=logScale, type="l", lwd=1.5, xlim=xRange, ylim=yRange)
      par(new=T)
   }  
   if(stratName3 %in% colnames(TR)) {   
      if(normalize==T) plot(TR$numericDate, TR[, stratName3]/TR[normDate, stratName3], col="black", xlab="", ylab="", log=logScale, type="l", lwd=2, xlim=xRange, ylim=yRange)
         else plot(TR$numericDate, TR[, stratName3], col="black", xlab="", ylab="", log=logScale, type="l", lwd=2, xlim=xRange, ylim=yRange)
      par(new=T)
   }
   if(stratName4 %in% colnames(TR)) {   
      if(normalize==T) plot(TR$numericDate, TR[, stratName4]/TR[normDate, stratName4], col="green", xlab="", ylab="", log=logScale, type="l", lwd=2, xlim=xRange, ylim=yRange)
         else plot(TR$numericDate, TR[, stratName4], col="green", xlab="", ylab="", log=logScale, type="l", lwd=2, xlim=xRange, ylim=yRange)
   }
   legend("topleft", c(stratName1,stratName2,stratName3,stratName4), cex=1, bty="n", lwd=c(1.5,1.5,2,2), lty = c(1,1,1,1), col=c("red","blue","black","green"))
   par(new=F)

   if(showAlloc==T) {
      yRange2 <- c(0,1)
      if(stratName1 %in% colnames(alloc)) {   
         plot(alloc$numericDate, alloc[, stratName1], col="red", xlab="", ylab="stock alloc.", type="l", xlim=xRange, ylim=yRange2)
         par(new=T)
      }
      if(stratName2 %in% colnames(alloc)) {   
         plot(alloc$numericDate, alloc[, stratName2], col="blue", xlab="", ylab="", type="l", xlim=xRange, ylim=yRange2)
         par(new=T)
      }
      if(stratName3 %in% colnames(alloc)) {
         plot(alloc$numericDate, alloc[, stratName3], col="black", xlab="", ylab="", type="l", lwd=2, xlim=xRange, ylim=yRange2)
         par(new=T)
      }
      if(stratName4 %in% colnames(alloc)) {   
         plot(alloc$numericDate, alloc[, stratName4], col="green", xlab="", ylab="", type="l", xlim=xRange, ylim=yRange2)
      }
      par(new=F)
   }
}


plotFutureReturnVsCAPE <- function(futureReturnYears=def$futureYears, CAPEname1="CAPE10", CAPEname2="CAPE10avg24", CAPEmax=35) { 
   futureReturnName <- paste0("futureReturn", futureReturnYears)
   if (!futureReturnName %in% colnames(dat)) calcFutureReturn(futureReturnYears)
   
   xRange <- c(5, CAPEmax)
   par(mar=c(4, 4, 1.5, 1.5))
   par(mfrow = c(1, 1))
   
   plot(dat[, CAPEname1], dat[, futureReturnName], col="red", xlab="CAPE", ylab="total return", xlim=xRange)
   par(new=T)
   plot(dat[, CAPEname2], dat[, futureReturnName], col="blue", xlab="", ylab="", xlim=xRange)
   par(new=F)
   
   mod <- lm( dat[, futureReturnName] ~ dat[, CAPEname1] )
   print(paste0(CAPEname1, ": TR = ", round(100*mod$coefficients[1],1), "%", round(100*mod$coefficients[2],2), "%*CAPE (r^2 = ", 
                       round(summary(mod)$r.squared*100), "%)"))
   abline(mod, col="red")
   
   mod <- lm( dat[, futureReturnName] ~ dat[, CAPEname2] )
   print(paste0(CAPEname2, ": TR = ", round(100*mod$coefficients[1],1), "%", round(100*mod$coefficients[2],2), "%*CAPE (r^2 = ", 
                round(summary(mod)$r.squared*100), "%)"))
}

plotReturnVsBothBad <- function(futureReturnYears=def$futureYears, tradingCost=def$tradingCost) {
   par(mfrow = c(2, 1))
   plotReturnVsVolatility(futureReturnYears=futureReturnYears, tradingCost=tradingCost) 
   plotReturnVsDrawdown(futureReturnYears=futureReturnYears, tradingCost=tradingCost)
}


plotReturnVsVolatility <- function(futureReturnYears=def$futureYears, tradingCost=def$tradingCost) { 
   
   medianName <- paste0("median", years)
   fiveName <- paste0("five", years)
   
   xRange <- c(12, 20)
   yRange <- c(4, 10 - 100*tradingCost/2)
   yRangeFive <- c(2, 7.5 - 75*tradingCost)
   par(mar=c(4, 4, 1.5, 4))
   
   plot(100*stats$volatility, 100*(stats$TR - tradingCost/stats$turnover), pch=15, col="red", xlab="volatility", ylab="return", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*stats$volatility, 100*(stats[, medianName] - tradingCost/stats$turnover), pch=16, col="blue", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*stats$volatility, 100*(stats[, fiveName] - tradingCost/stats$turnover), pch=18, col="black", xaxt="n", yaxt="n", xlab="", ylab="", xlim=xRange, ylim=yRangeFive)
   axis(4)
   mtext("5% risk",side=4,line=3)
   legend("bottomleft", c("total return (%)", paste("median over", futureReturnYears, "years (%)"), paste("5% risk over", futureReturnYears, "years (%, right)") ), 
          cex=1, bty="n", pch=c(15,16,18,1), col=c("red","blue","black","green"))
}

plotReturnVsDrawdown <- function(futureReturnYears=def$futureYears, tradingCost=def$tradingCost) { 
   
   medianName <- paste0("median", futureReturnYears)
   fiveName <- paste0("five", futureReturnYears)
   
   xRange <- c(0, 5)
   yRange <- c(4, 10 - 50*tradingCost)
   yRangeFive <- c(2, 7.5 - 75*tradingCost)
   par(mar=c(4, 4, 1.5, 4))
   
   plot(stats$DD2, 100*(stats$TR - tradingCost/stats$turnover), pch=15, col="red", xlab="sum (drawdown ^ 2)", ylab="return", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(stats$DD2, 100*(stats[, medianName] - tradingCost/stats$turnover), pch=16, col="blue", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(stats$DD2, 100*(stats[, fiveName] - tradingCost/stats$turnover), pch=18, col="black", xaxt="n", yaxt="n", xlab="", ylab="", xlim=xRange, ylim=yRangeFive)
   axis(4)
   mtext("5% risk",side=4,line=3)
   legend("bottomleft", c("total return", paste("median over", futureReturnYears, "years"), paste("5% risk over", futureReturnYears, "years (right)") ), 
          cex=1, bty="n", pch=c(15,16,18,1), col=c("red","blue","black","green"))
}

plotFutureReturnVsAlloc <- function(futureReturnYears=def$futureYears, name1, name2) { 
   futureReturnName <- paste0("futureReturn", futureReturnYears)
   if (!futureReturnName %in% colnames(dat)) calcFutureReturn(futureReturnYears)
   
   xRange <- c(0, 1)
   par(mar=c(4, 4, 1.5, 1.5))
   par(mfrow = c(1, 1))
   
   plot(alloc[, name1], dat[, futureReturnName], col="red", xlab="stocks alloc.", ylab="total return", xlim=xRange)
   par(new=T)
   plot(alloc[, name2], dat[, futureReturnName], col="blue", xlab="", ylab="", xlim=xRange)
   par(new=F)
   
   mod <- lm( dat[, futureReturnName] ~ strategy[, paste0(name1,"Alloc")] )
   print(paste0(name1, ": TR = ", round(100*mod$coefficients[1],1), "% + ", round(100*mod$coefficients[2],2), "%*alloc (r^2 = ", 
                round(summary(mod)$r.squared*100), "%)"))
   mod <- lm( dat[, futureReturnName] ~ strategy[, paste0(name2,"Alloc")] )
   print(paste0(name2, ": TR = ", round(100*mod$coefficients[1],1), "% + ", round(100*mod$coefficients[2],2), "%*alloc (r^2 = ", 
                round(summary(mod)$r.squared*100), "%)"))
}


plotReturnVsAverageAllocWithLine <- function(tradingCost=def$tradingCost) { 
   
   xRange <- c(20, 100)
   yRange <- c(3, 10 - 100*tradingCost/2)
   par( mar=c(4, 4, 1.5, 1.5) )
   
   plot(100*subset(stats$avgStockAlloc, stats$type!="constantAlloc"), 
        100*(subset(stats$TR, stats$type!="constantAlloc") - 
                tradingCost/subset(stats$turnover, stats$type!="constantAlloc")), 
        pch=15, col="black", xlab="avg. stock alloc. (%)", ylab="total return (%)", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$avgStockAlloc, stats$type=="CAPE"), 
        100*(subset(stats$TR, stats$type=="CAPE") - 
                tradingCost/subset(stats$turnover, stats$type=="CAPE")), 
        pch=15, col="blue", xlab="avg. stock alloc. (%)", ylab="total return (%)", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$avgStockAlloc, stats$type=="momentum"), 
        100*(subset(stats$TR, stats$type=="momentum") - 
                tradingCost/subset(stats$turnover, stats$type=="momentum")), 
        pch=15, col="red", xlab="avg. stock alloc. (%)", ylab="total return (%)", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$avgStockAlloc, stats$type=="constantAlloc"), 
        100*(subset(stats$TR, stats$type=="constantAlloc") - 
                tradingCost/subset(stats$turnover, stats$type=="constantAlloc")), 
        type="l", col="black", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=F)
}

plotReturnVsVolatilityWithLine <- function(futureReturnYears=def$futureYears, tradingCost=def$tradingCost) { 
   
   xRange <- c(10, 20)
   yRange <- c(3, 10 - 100*tradingCost/2)
   par( mar=c(4, 4, 1.5, 1.5) )
   
   plot(100*subset(stats$volatility, stats$type!="constantAlloc"), 
        100*(subset(stats$TR, stats$type!="constantAlloc") - 
                tradingCost/subset(stats$turnover, stats$type!="constantAlloc")), 
        pch=15, col="black", xlab="volatility (%)", ylab="total return (%)", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$volatility, stats$type=="CAPE"), 
        100*(subset(stats$TR, stats$type=="CAPE") - 
                tradingCost/subset(stats$turnover, stats$type=="CAPE")), 
        pch=15, col="blue", xlab="volatility (%)", ylab="total return (%)", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$volatility, stats$type=="momentum"), 
        100*(subset(stats$TR, stats$type=="momentum") - 
                tradingCost/subset(stats$turnover, stats$type=="momentum")), 
        pch=15, col="red", xlab="volatility (%)", ylab="total return (%)", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(100*subset(stats$volatility, stats$type=="constantAlloc"), 
        100*(subset(stats$TR, stats$type=="constantAlloc") - 
                tradingCost/subset(stats$turnover, stats$type=="constantAlloc")), 
        type="l", col="black", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=F)
}

plotReturnVsDrawdownWithLine <- function(futureReturnYears=def$futureYears, tradingCost=def$tradingCost) { 
   
   xRange <- c(0.5, 4.5)
   yRange <- c(3, 10 - 50*tradingCost)
   par(mar=c(4, 4, 1.5, 4))
   
   plot(subset(stats$DD2, stats$type!="constantAlloc"), 
        100*(subset(stats$TR, stats$type!="constantAlloc") - 
                tradingCost/subset(stats$turnover, stats$type!="constantAlloc")), 
        pch=15, col="black", xlab="drawdown", ylab="total return (%)", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(subset(stats$DD2, stats$type=="CAPE"), 
        100*(subset(stats$TR, stats$type=="CAPE") - 
                tradingCost/subset(stats$turnover, stats$type=="CAPE")), 
        pch=15, col="blue", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(subset(stats$DD2, stats$type=="momentum"), 
        100*(subset(stats$TR, stats$type=="momentum") - 
                tradingCost/subset(stats$turnover, stats$type=="momentum")), 
        pch=15, col="red", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=T)
   plot(subset(stats$DD2, stats$type=="constantAlloc"), 
        100*(subset(stats$TR, stats$type=="constantAlloc") - 
                tradingCost/subset(stats$turnover, stats$type=="constantAlloc")), 
        type="l", col="black", xlab="", ylab="", xlim=xRange, ylim=yRange)
   par(new=F)
}

plotReturnVsBothBadWithLine <- function(futureReturnYears=def$futureYears, tradingCost=def$tradingCost) {
   par(mfrow = c(2, 1))
   plotReturnVsVolatilityWithLine(futureReturnYears=futureReturnYears, tradingCost=tradingCost) 
   plotReturnVsDrawdownWithLine(futureReturnYears=futureReturnYears, tradingCost=tradingCost)
}


# plot( abs(strategy$CAPE10avg24_14.6_16.7Alloc - strategy$momentum12_15_25Alloc), 
#       strategy$CAPE10avg24_14.6_16.7Future30, col="blue", ylim=c(0,.15) )
# par(new=T)
# plot( abs(strategy$CAPE10avg24_14.6_16.7Alloc - strategy$momentum12_15_25Alloc), 
#       strategy$momentum12_15_25Future30, col="red", ylim=c(0,.15) )
# par(new=F)






# par(mfrow = c(1, 1))
# plot(dat$date, dat$CAPE10, xlab="date", ylab="CAPE", type="l", ylim=c(5,27))
# abline(h=11.6)
# abline(h=19.7)


# plot(dat$date, dat$futureReturn20*100, xlab="date", ylab="annualized return (in %) over next 20 years")
# 
# plot(as.numeric(dat$date), log(dat$totalReturn), xlab="date", ylab="log(total return)")
# mod <- lm( log(dat$totalReturn) ~ as.numeric(dat$date) )
# abline(mod)
# title(main = paste("annual return: ", round(mod$coefficients[2]*365*24*3600*100,1), "%", sep=""))


# calcModifiedCAPE("CAPE10")
# par(mar=c(4,4,1.5,1.5))
# plot(dat$CAPE10v2, dat$futureReturn10*100, xlab="CAPE 10", ylab="annualized return (in %) over next 10 years")
# fit <- lm(100*dat$futureReturn10 ~ dat$CAPE10v2)
# abline(fit)
# print(paste0("y = ", round(fit$coefficients[[2]],2), " * x + ", round(fit$coefficients[[1]],1), "  (r^2 = ", floor(100*summary(fit)$r.squared), "%)"))
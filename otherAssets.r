
# Loading gold data from local csv file
loadGoldData <- function() {
   addNumColToDat("gold")
   dat$gold <<- NA
   nominalGold <- read.csv("gold.csv", header=T)[, 1]
   
   index1968 <- (1968-1871)*12+1  # data start in 1968
   lastGoldIndex <- length(nominalGold)
   lastDatIndex <- numData
   
   if ( lastGoldIndex + index1968 - 1 > lastDatIndex )  # if there are too many months of data for gold
      lastGoldIndex <- lastDatIndex - index1968 + 1
   else if (lastGoldIndex + index1968 - 1 < lastDatIndex)  # if there are too few months of data for gold
      lastDatIndex <- lastGoldIndex + index1968 - 1
   
   dat$gold[ index1968:lastDatIndex ] <<- nominalGold[1:lastGoldIndex]
   dat$gold <<- dat$gold  / dat$CPI * refCPI # calculating real gold prices
}

createGoldStrategy <- function(strategyName="", futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   if (strategyName == "") strategyName <- "gold" 
   if (!strategyName %in% colnames(TR)) TR[, strategyName] <<- numeric(numData)
   
   index1968 <- (1968-1871)*12+1
   TR[, strategyName] <<- NA
   TR[index1968, strategyName] <<- 1
   for(i in (index1968+1):numData) 
      TR[i, strategyName] <<- TR[i-1, strategyName] * dat$gold[i] / dat$gold[i-1] 
   
   if ( !(strategyName %in% stats$strategy) ) {
      index <- nrow(stats)+1 # row where the info will be added
      stats[index, ] <<- NA
      stats$strategy[index] <<- strategyName
      stats$avgStockAlloc[index] <<- NA
      stats$latestStockAlloc[index] <<- NA
      stats$turnover[index] <<- Inf
   }
   stats$type[which(stats$strategy == strategyName)] <<- "gold"
   
   #    calcStatisticsForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   ## since gold data start in 1968 gold statistics cannot be relevantly compared with other assets or strategies
}


# Loading gold data from Nationwide website
loadUKhousePriceData <- function(downloadAndCheckAllFiles=F) {
   
   library(XLConnect) # to handle xls file
   if(!file.exists("uk-house-prices-adjusted-for-inflation.xls") | downloadAndCheckAllFiles) # download file if not already locally available
      download.file("http://www.nationwide.co.uk/~/media/MainSite/documents/about/house-price-index/downloads/uk-house-prices-adjusted-for-inflation.xls",
                    "uk-house-prices-adjusted-for-inflation.xls", mode = "wb")
   
   wk <- loadWorkbook("uk-house-prices-adjusted-for-inflation.xls") 
   quarterlyData <- readWorksheet(wk, sheet="RealHP", startRow=3)
   quarterlyData[, 3] <- as.numeric(quarterlyData[, 3])
   
   lastUKhousePriceIndex <- dim(quarterlyData)[[1]]
   while( is.na(quarterlyData[lastUKhousePriceIndex, 3] ) ) { # remove final rows that are not data
      quarterlyData <- quarterlyData[-lastUKhousePriceIndex, ]
      lastUKhousePriceIndex <- lastUKhousePriceIndex-1      
   }
   
   addNumColToDat("UKhousePrice")
   dat$UKhousePrice <<- NA
   
   index1975 <- (1975-1871)*12+1  # available data start in 1975
   lastDatIndex <- numData
   
   if ( 3*lastUKhousePriceIndex + index1975 - 2 > lastDatIndex )  # if there are too many months of UKhousePrice data 
      lastUKhousePriceIndex <- floor( (lastDatIndex - index1975 + 2)/3 )
   else if (lastUKhousePriceIndex + index1975 - 2 < lastDatIndex)  # if there are too few months of UKhousePrice data 
      lastDatIndex <- lastUKhousePriceIndex + index1975 - 2
   
   for(i in 0:(lastUKhousePriceIndex-1) ) { #(lastDatIndex-index1975)/3 ) {
      dat$UKhousePrice[index1975+3*i+1] <<- quarterlyData[i+1, 3] # data are centred on the middle month of the quarter (february, etc.)
      # for 2 out of 3 months, we interpolate:
      dat$UKhousePrice[index1975+3*i]   <<- dat$UKhousePrice[index1975+3*i+1]*2/3 + dat$UKhousePrice[index1975+3*i-2]*1/3
      dat$UKhousePrice[index1975+3*i-1] <<- dat$UKhousePrice[index1975+3*i+1]*1/3 + dat$UKhousePrice[index1975+3*i-2]*2/3
   }
}

createUKhousePriceStrategy <- function(strategyName="", futureYears=def$futureYears, tradingCost=def$tradingCost, force=F) {
   if (strategyName == "") strategyName <- "UKhousePrice" 
   addNumColToTR(strategyName)
   
   index1975 <- (1975-1871)*12+1
   TR[, strategyName] <<- NA
   TR[index1975+1, strategyName] <<- 1
   for(i in (index1975+2):numData) 
      TR[i, strategyName] <<- TR[i-1, strategyName] * dat$UKhousePrice[i] / dat$UKhousePrice[i-1] 
   
   if ( !(strategyName %in% stats$strategy) ) {
      index <- nrow(stats)+1 # row where the info will be added
      stats[index, ] <<- NA
      stats$strategy[index] <<- strategyName
      stats$avgStockAlloc[index] <<- NA
      stats$latestStockAlloc[index] <<- NA
      stats$turnover[index] <<- Inf
   }
   stats$type[which(stats$strategy == strategyName)] <<- "UKhousePrice"
   
   #    calcStatisticsForStrategy(strategyName, futureYears=futureYears, tradingCost=tradingCost, force=force)
   ## since real UKhousePrice data are only available since 1975 UKhousePrice statistics cannot be relevantly compared with other assets or strategies
}

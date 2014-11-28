
############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
############################################


### https://github.com/m-bouville/investing-by-numbers/blob/master/README.md


showToDoList <- function() {
   print("What's still left TO DO:")
   print(" - Improve value strategy results.")
   print(" - Change the way 2 strategies are combined: right now I use constant weights.")
   print("       I'd like to tell (how?) when a strategy is likely to do well to increase its weight.")
   print(" - Automate parameter search.")
   print(" - Switch to ggplot2 for scatter plots?")
   print(" - Try to speed up code (especially parameter searches) through compiling and (or) parallelization.")
   print(" - Store constant allocations in csv file.")
   print("")
}

showUsefulFunctions <- function() {
   print("Here are some useful functions (all can be called without arguments):")
   print("## Plotting:")
   print("plotAllReturnsVsFour()")
   print("    # Zoomed-out version of the above plot:")
   print("plotAllReturnsVsFour(xMinVol=10, xMaxVol=20, xMinDD=0, xMaxDD=4.5, xMinAlloc=40, xMaxAlloc=100, xMinTO=0, xMaxTO=100, yMin=5)")
   print("plotAllReturnsVsTwo(col=T), # 'col' switches between the two plots on a row or column")
   print("plotReturnAndAlloc()")
   print("plotAssetClassesReturn()")
   print("showPlotLegend()")
   print("createStrategiesAndSavePlots()")
   print("## Searching the parameter space:")
   print("searchForOptimalBalanced() # for instance.")
   print("")
}

showForNewcomer <- function() {
   
   print(paste("Read https://github.com/m-bouville/investing-by-numbers/blob/master/README.md\n",
         "for a description of the cose and of the strategies.") )
   print("")         
   
   showUsefulFunctions()   
   
   print("Legend for plots:")
   print("Warm colors correspond to technical strategies (turning their holding every 1-2 year).")
   print("Cold colors correspond to valuation strategies (turning their holding every 10-15 years).")
   print("The diamonds are individual strategies, whereas the squares are linear combinations thereof (\'multiStrategies\').")
   print("The black square is a linear combination of the linear combinations (the most \'balanced\' strategy.")
   print("The green line corresponds to constant stock-bond allocations.")
   print("More specifically:")
   
   showPlotLegend()
   
   showToDoList()
}

# Loading and preparing data
start <- function(dataSplit="none",           # "none" for all data, "search" and "testing" for half the data
                  extrapolateDividends=T,     # whether to extrapolate missing recent dividends (or remove incomplete months)
                  smoothConstantAlloc=F,      # calculates more constant-allocation portfolios, for smoother curves in plots
                  downloadAndCheckAllFiles=F, # downloads data files even if they exist locally,
                                              #    to check whether they are up to date
                  futureYears=10L,            # to calculate the return over the next so many years
                  tradingCost=0.5/100,        # cost of turning the portfolio entirely 
                  riskAsCost=0.5/100,
                  riskAsCostTechnical=3.5/100,
                  otherAssetClasses=F,        # loads gold and UK house prices
                  newcomer=F,                 # displays some information on the code
                  force=F) {
   
   if(!file.exists("main.r")) 
      stop("Use \'setwd()\' to change the working directory to that containing the r files.")
   
   source("loading.r")    # to load data and initialize everything
   source("utils.r")      # general functions (i.e. those not in another file)
   source("DD.r")         # drawdowns
   source("plotting.r")   # various functions that generate plots
   source("inflation.r")
   
   # Strategies:
   source("CAPE.r")
   source("detrended.r")
   source("Bollinger.r")
   source("SMA.r")
   source("reversal.r")
   source("combine.r")
   
   library(stringr)
   
   totTime <- proc.time()
   
   setDefaultValues(dataSplit=dataSplit, futureYears=futureYears, 
                    tradingCost=tradingCost, riskAsCost=riskAsCost, riskAsCostTechnical=riskAsCostTechnical, force=force)
   if(!file.exists("./data")) dir.create("./data")
      
   ## if data frame does not exist, or is incomplete (had been used for search or testing), 
   ## or if we want to force the loading: we load the xls file
   if (!exists("dat") || numData<1500 || downloadAndCheckAllFiles) { 
   #       message("Starting to load the data from the xls file.")
   #       if (otherAssetClasses)
   #          message("Then we will also load a list of drawdowns, of gold prices and of UK house prices.")
   #       else message("Then we will also load a list of drawdowns.")
      loadData(downloadAndCheckAllFiles=downloadAndCheckAllFiles)
   }
   if (!exists("DD") | force) loadDDlist(otherAssetClasses=otherAssetClasses, force=force) # loading the dates of major drawdowns
   
   if (dataSplit != "none") splitData(dataSplit=dataSplit, force=force)
   else {
      dat$splitting <<- factor(numData)
      levels(dat$splitting) <<- c("search", "testing")
      dat$splitting[ 1: (numData %/% 2) ] <<- "search"
      dat$splitting[ (numData %/% 2+1):numData ] <<- "testing"
   }
   
   if (!exists("signal")| force) signal <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   if (!exists("alloc") | force) alloc  <<- data.frame(date = dat$date, numericDate = dat$numericDate)
   if (!exists("TR")    | force) TR     <<- data.frame(date = dat$date, numericDate = dat$numericDate, stocks = dat$TR)
   
   addFutureReturnsToDat(force=force)
   
   if (!exists("stats") | force)  createStatsDF(futureYears=futureYears)   
   if (!exists("parameters") | force) createParametersDF()
   
   addNumColToDat("TRmonthly")
   addNumColToDat("bondsMonthly")
   addNumColToDat("monthlyDifference")
   for(i in 2:numData) {
      dat$TRmonthly[i] <<- dat$TR[i] / dat$TR[i-1] 
      dat$bondsMonthly[i] <<- dat$bonds[i] / dat$bonds[i-1] 
      dat$monthlyDifference[i] <<- dat$TRmonthly[i] - dat$bondsMonthly[i]
   }
   
   addInflationToDat()
   addConstAllocToDat(smoothConstantAlloc, force=force)
   
   if(newcomer) showForNewcomer()
   
   createTypicalStrategies(force=force)
   
   showSummaries()

   if(otherAssetClasses) 
      if (dat$numericDate[numData] < 1968) # if there are no data after 1968 in 'dat' (search mode) 
                                           #    then other asset classes are not available
         warning("Other asset classes not created: Since 'dat' ends before 1968",
                 " (probably due to search mode) gold and UK property are not available.")
      else {
         source("otherAssetClasses.r")# gold and UK housing
         
         if (!"UKhousePrice" %in% colnames(dat) | !"UKhousePrice" %in% stats$strategy | downloadAndCheckAllFiles) {
            loadUKhousePriceData(downloadAndCheckAllFiles=downloadAndCheckAllFiles)
            createUKhousePriceStrategy(futureYears=def$futureYears, force=force)
            message("Real UK house prices were obtained from Nationwide; they are in pounds, and based on UK inflation.")
         }

         if (!"gold" %in% colnames(dat) | !"gold" %in% stats$strategy | force) {
            loadGoldData()
            createGoldStrategy(futureYears=def$futureYears, costs=def$tradingCost+def$riskAsCost, force=force)
            message("Real gold prices were obtained from a local csv file.")
         }
      }
   
   makeStringsFactors()
   
#    if ( (summary(proc.time())[[3]]-summary(proc.time())[[1]] + totTime[[1]]-totTime[[3]] ) > 2 )
#       message( "This took ", 
#                round(summary(proc.time())[[3]] - totTime[[3]] , 0), " s (with about " , 
#                round(summary(proc.time())[[1]] - totTime[[1]] , 0), " s for calculations and " ,
#                round(summary(proc.time())[[3]]-summary(proc.time())[[1]] + totTime[[1]]-totTime[[3]] , 0), 
#                " s to load files and for XLConnect)." )
#    else message( "This took ", round(summary(proc.time())[[3]] - totTime[[3]] , 0), " s.")
}

# start(dataSplit="search", futureYears=10L,   riskAsCostTechnical=3.5/100, force=T)
# start(dataSplit="none",   futureYears=15L, riskAsCost=0, riskAsCostTechnical=0, force=T)
# start(dataSplit="testing",futureYears=10L, riskAsCost=0, riskAsCostTechnical=0, force=T)
# plotAllReturnsVsFour()
# checkXlsFileIsUpToDate()



## Plotting to png files:
# plotReturnAndAlloc(pngOutput=T)
# plotAllReturnsVsFour(pngOutput=T, pngName="figures/return_vs_four_zoomed_in.png")
# plotAllReturnsVsFour(xMinVol=10, xMaxVol=20, xMinDD=0, xMaxDD=4.5, 
#                      xMinAlloc=40, xMaxAlloc=100, xMinTO=0, xMaxTO=100, yMin=5, 
#                      pngOutput=T, pngName="figures/return_vs_four_zoomed_out.png")
#createStrategiesAndSavePlots()

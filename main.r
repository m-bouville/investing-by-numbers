
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
   print(" - Prevent strategies (esp. technical and hybrid ones) from buying then selling then buying, etc. every month;")
   print("       this serves no purpose and generates lots of trading.")
   print(" - Fix detrended.")
   print(" - Combine strategies more actively. Right now I use constant weights,")
   print("       but I'd like to tell (how?) when a strategy is likely to do well to increase its weight.")
   print(" - Automate parameter searches (conjugate gradient?).")
   print(" - Switch to ggplot2 for scatter plots? (Not a priority.)")
   #print(" - Try to speed up code (especially parameter searches) through compiling and (or) parallelization.")
   #print(" - Store constant allocations in csv file.")
   print("")
}

showUsefulFunctions <- function() {
   print("Here are some useful functions (all can be called without arguments):")
   print("## Plotting:")
   print("plotAllReturnsVsFour()")
   print("    # Zoomed-out version of the above plot:")
   print("plotAllReturnsVsFour(xMinVol=10, xMaxVol=18, xMinDD=0, xMaxDD=18, xMinAlloc=40, xMaxAlloc=100, xMinTO=0, xMaxTO=200, yMin=5)")
   print("plotAllReturnsVsTwo(col=T), # 'col' switches between the two plots on a row or column")
   print("plotReturnAndAlloc() # value and balanced strategies along with stocks and bonds")
   print("plotReturnAndAllocSubstrategies() # technical, value, hybrid and balanced strategies")
   print("plotAssetClassesReturn() # requires otherAssetClasses=T in start()")
   print("showPlotLegend()")
   print("createStrategiesAndSavePlots() # creates and saves to the disks a bunch of plots (takes a bit of time)")
   print("## Searching the parameter space:")
   print("searchForOptimalBoll() # for instance.")
   print("")
}

showForNewcomer <- function() {
   
   print("Read https://github.com/m-bouville/investing-by-numbers/blob/master/README.md")
   print("for a description of what this is about (with links to info on the code and the strategies).")
   print("")         
   
   showUsefulFunctions()   
   
   print("Legend for plots:")
   print("Red hues correspond to technical strategies (turning their holding every 1-2 year).")
   print("Blue hues correspond to value strategies (turning their holding every 10-15 years).")
   print("Green hues correspond to hybrid strategies (a technical strategy applied to something other than stock price).")
   print("The diamonds are individual strategies, whereas the squares are linear combinations thereof (\'combiened strategies\').")
   print("The black square is a linear combination of the linear combinations (the most \'balanced\' strategy.")
   print("The brown line corresponds to constant stock-bond allocations.")
   print("More specifically:")
   
   showPlotLegend()
   
   showToDoList()
}

# Loading and preparing data
start <- function(dataSplit="all",            # "all" for all data, "training" and "testing" for half the data
                  futureYears=15L,            # to calculate the return over the next so many years
                  tradingCost=0.5/100,        # cost of turning the portfolio entirely 
                  riskAsCost= 1.5/100,
                  lastMonthSP500="",          # to enter by hand the value of the S&P 500 at the end of last month
                  removeDepression=F,         # stops the 'training' time range before the Depression.
                  extrapolateDividends=T,     # whether to extrapolate missing recent dividends (or remove incomplete months)
                  smoothConstantAlloc=F,      # calculates more constant-allocation portfolios, for smoother curves in plots
                  downloadAndCheckAllFiles=F, # force downloading data files to check whether they are up to date
                  otherAssetClasses=F,        # loads gold and UK house prices
                  newcomer=F,                 # displays some information on the code
                  force=T) {
   
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
   source("hybrid.r")
   source("combine.r")
   
   library(stringr)
   
   totTime <- proc.time()
   
   dataSplit <<- numeric(); dataSplit <<- dataSplit
   setDefaultValues(dataSplit=dataSplit, futureYears=futureYears, removeDepression=removeDepression,
                    tradingCost=tradingCost, riskAsCost=riskAsCost, 
                    riskAsCostTechnical=riskAsCostTechnical, force=force)
   if(!file.exists("./data")) dir.create("./data")
   if (dataSplit=="training" && lastMonthSP500!="") { # 'lastMonthSP500' makes no sense looking at 1871-1942
      lastMonthSP500="" # ignore 'lastMonthSP500'
      message("lastMonthSP500 value will not be used for 'training'.")
   }
   if (dataSplit!="training" && removeDepression)
      message("Depression cannot be removed in ", dataSplit, " mode.") # we ignore 'removeDepression'
   if (dataSplit=="training" && removeDepression) {
      #message("Depression will be removed (data will stop at the end of 1929).") # we ignore 'removeDepression'
      removeDepression <<- T   # set a global variable
   }
      
   ## if data frame does not exist or if we want to force the loading: we load the xls file
   if (!exists("dat") || downloadAndCheckAllFiles || force) { 
   #       message("Starting to load the data from the xls file.")
   #       if (otherAssetClasses)
   #          message("Then we will also load a list of drawdowns, of gold prices and of UK house prices.")
   #       else message("Then we will also load a list of drawdowns.")
      loadData(downloadAndCheckAllFiles=downloadAndCheckAllFiles, lastMonthSP500=lastMonthSP500)
   }
   if (!exists("DD") | force) loadDDlist(otherAssetClasses=otherAssetClasses, force=force) # loading the dates of major drawdowns
   
   if (dataSplit != "all") 
      splitData(dataSplit=dataSplit, removeDepression=removeDepression, lastMonthSP500Used=(lastMonthSP500!=""), force=force)
   else {
      dat$splitting <<- factor(numData)
      levels(dat$splitting) <<- c("training", "testing")
      dat$splitting[ 1: (numData %/% 2) ] <<- "training"
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
   addConstAllocToDat(smoothConstantAlloc, costs=tradingCost, force=force)
   
   if(newcomer) showForNewcomer()
   
   selectTypicalStrategiesToCreate()
   createTypicalStrategies(force=force)
   
   showSummaries()
   if (dataSplit != "training") 
      print(paste0("CURRENT RECOMMENDED STOCK ALLOCATION: ", 
                  round( 100*stats$latestStockAlloc[which(stats$strategy==typical$balanced)]), "% | ", 
                  round( 100*stats$latestStockAlloc[which(stats$strategy=="balanced50")]), "%"))

   if(otherAssetClasses) 
      if (dat$numericDate[numData] < 1968) # if there are no data after 1968 in 'dat' (training mode) 
                                           #    then other asset classes are not available
         warning("Other asset classes not created: Since 'dat' ends before 1968",
                 " (probably due to training mode) gold and UK property are not available.")
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

# Loading and preparing data for TRAINING
startTraining <- function(futureYears=10L, tradingCost=0.5/100, riskAsCost=1.5/100,
                          removeDepression=F, smoothConstantAlloc=F, newcomer=F, force=T) {
   if ( tradingCost+riskAsCost < 1/100 ) 
      warning("costs = ", (tradingCost+riskAsCost)*100, "%.", immediate.=T)
   
   start(dataSplit="training", futureYears=futureYears, tradingCost=tradingCost, riskAsCost=riskAsCost, 
         smoothConstantAlloc=smoothConstantAlloc, removeDepression=removeDepression, 
         lastMonthSP500="", extrapolateDividends=F, downloadAndCheckAllFiles=F, otherAssetClasses=F, # irrelevant to the training data range
         newcomer=newcomer, force=force)
   plotAllReturnsVsFour()
}

# Loading and preparing data for TESTING
startTesting <- function(futureYears=10L, tradingCost=0.5/100, riskAsCost=1.5/100,
                         lastMonthSP500="", extrapolateDividends=T, smoothConstantAlloc=F,   
                         downloadAndCheckAllFiles=F, otherAssetClasses=F, newcomer=F, force=T) {
   start(dataSplit="testing", futureYears=futureYears, tradingCost=tradingCost, riskAsCost=riskAsCost, 
         lastMonthSP500=lastMonthSP500, extrapolateDividends=extrapolateDividends, 
         removeDepression=F, # irrelevant to the testing data range
         smoothConstantAlloc=smoothConstantAlloc, downloadAndCheckAllFiles=downloadAndCheckAllFiles, 
         otherAssetClasses=otherAssetClasses, newcomer=newcomer, force=force)
   if (!otherAssetClasses)
      plotAllReturnsVsFour()
}
## Using S&P500 data for August 2015, up to 4th August (source: stlouisfed.org):
# startTesting(lastMonthSP500=2095, downloadAndCheckAllFiles=T, otherAssetClasses=T)
   

# plotAllReturnsVsFour()
# checkXlsFileIsUpToDate(force=T)


## Plotting to png files:
# plotReturnAndAlloc(pngOutput=T)
# plotAllReturnsVsFour(pngOutput=T, pngName="figures/return_vs_four_zoomed_in.png")
# plotAllReturnsVsFour(xMinVol=10, xMaxVol=20, xMinDD=0, xMaxDD=4.5, 
#                      xMinAlloc=40, xMaxAlloc=100, xMinTO=0, xMaxTO=100, yMin=5, 
#                      pngOutput=T, pngName="figures/return_vs_four_zoomed_out.png")
#createStrategiesAndSavePlots()

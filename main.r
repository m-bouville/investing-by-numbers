
############################################
##                                        ##
##         Investing by numbers           ##
##   a quantitative trading strategy by   ##
##         Mathieu Bouville, PhD          ##
##      <mathieu.bouville@gmail.com>      ##
##                                        ##
############################################


### "http://mathieu.bouville.name/finance/CAPE/"


source("init.r")       # loads data and initializes everything


showToDoList <- function() {
   print("What's still left TO DO:")
   print(" - Improve CAPE strategy results.")
   print(" - Change (how?) the way 2 strategies are combined.")
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



# showToDoList()

start(dataSplit="search",          # "none" for all data, "search" and "testing" for half the data
      extrapolateDividends=T,    # whether to extrapolate missing recent dividends (or remove incomplete months)
      smoothConstantAlloc=F,     # calculates more constant-allocation portfolios, to get smoother curves in plots (slower)
      downloadAndCheckAllFiles=F,# downloads data files even if they exist locally, to check whether they are up to date
      futureYears=10L,           # to calculate the return over the next so many years
      tradingCost=0.5/100,       # cost of turning the portfolio entirely 
      otherAssetClasses=F,       # loads gold and UK house prices
      newcomer=F,                # displays some information on the code
      force=T)                   # forces recalculations (useful when making modifications to the algorthm, but slower)

plotAllReturnsVsFour()


## Zoomed-out version of the above plot:
# plotAllReturnsVsFour(xMinVol=10, xMaxVol=20, xMinDD=0.15, xMaxDD=4.5, xMinAlloc=40, xMinTO=0, xMaxTO=100, yMin=5)
# plotReturnAndAlloc()
# plotAssetClassesReturn()


## Plotting to png files:
# plotReturnAndAlloc(pngOutput=T)
# plotAllReturnsVsFour(pngOutput=T, pngName="figures/return_vs_four_zoomed_in.png")
# plotAllReturnsVsFour(xMinVol=10, xMaxVol=20, xMinDD=0, xMaxDD=4.5, 
#                      xMinAlloc=40, xMaxAlloc=100, xMinTO=0, xMaxTO=100, yMin=5, 
#                      pngOutput=T, pngName="figures/return_vs_four_zoomed_out.png")
#createStrategiesAndSavePlots()



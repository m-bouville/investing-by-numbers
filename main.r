### "http://mathieu.bouville.name/finance/CAPE/"

# library(compiler)
# enableJIT(1) # compiling just-in-time (results are not convincing, but worth a second try)

source("init.r")       # loads data and initializes everything
source("utils.r")      # general functions (i.e. those not fitting in another file)
source("otherAssets.r")# gold and UK housing
source("DD.r")         # drawdowns
source("plotting.r")   # various functions that generate plots

# Strategies:
source("CAPE.r")
source("detrended.r")
source("Bollinger.r")
source("SMA.r")
source("momentum.r")
source("multiStrategies.r")

showToDoList <- function() {
   print("What's still left to do:")
   print(" - Implement trend reversal (i.e. 2nd derivative).")
   print(" - Speed up drawdown calculations.")
   print(" - Plot future returns.")
   print(" - Split data between design and testing.")
   print(" - Try to speed up code (especially parameter searches through compiling and (or) parallelization.")
   print("")
}

showUsefulFunctions <- function() {
   print("Here are some useful functions (all can be called without arguments):")
   print("* Plotting:")
   print(" - plotAllReturnsVsFour(),")
   print(" - plotReturnAndAlloc(),")
   print(" - plotAssetClassesReturn(),")
   print(" - showPlotLegend().")
   print("* Searching the parameter space:")
   print(" - searchForOptimalBalanced() -- for instance.")
   print("")
}

showFornewbie <- function() {
   showUsefulFunctions()   
   
   print("Legend for plots:")
   print("Warm colors correspond to technical strategies (turning their holding every 1-2 year).")
   print("Cold colors correspond to valuation strategies (turning their holding every 10-15 years).")
   print("The diamonds are individual strategies, whereas the squares are linear combinations thereof (\'multiStrategies\').")
   print("The black square is a linear combination of the linear combinations (the most \'balanced\' strategy.")
   print("The green line corresponds to constant stock-bond allocations.")
   print("More specifically:")
   showPlotLegend()
}





showToDoList()

start(newbie=F, force=F)
## force: forces recalculations (slower)

# plotAllReturnsVsFour()
# plotAllReturnsVsFour(xMinVol=10, xMaxVol=20, xMinDD=0, xMaxDD=4.5, xMinAlloc=40, xMaxAlloc=100, xMinTO=0, xMaxTO=100, yMin=4)
# plotReturnAndAlloc()
# plotAssetClassesReturn()



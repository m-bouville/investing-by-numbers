Investing by numbers: Code structure  --  Mathieu Bouville, PhD
===========================================================


# The files
- Each strategy is handled in a separate .r file, named after it, e.g. Bollinger.r or detrended.r;
- utils.r has generic functions (used for all strategies);
- plotting.r is in charge of generating plots;
- init.r loads the data and does some initialization;
- DD.r calculates drawdowns;
- otherAssetClasses.r loads the data and does some initialization for gold and UK housing prices (strategies for the latter can be found in https://github.com/m-bouville/investing-by-numbers/blob/master/UK_property-market.pdf).

**To get started, just source main.r and run start().**

Some useful functions can be seen by calling 'showUsefulFunctions()'.


# Data structure
## 'dat' dataframe
The data are initially loaded into a dataframe named 'dat'. Everything this dataframe contains is in real terms. Each row corresponds to a month (starting in January 1871). Columns include: 
- 'date';
- 'numeric date', the date as a real number (convenient for plotting);
- 'CPI', the consumer price index (used to calculation inflation, and hence real prices);
- 'dividend' (real);
- 'earnings' (real);
- 'TR', i.e. (real) total return = stock price + dividends;
- 'bonds', (real) bond prices;
- some data calculated for the strategy, such as CAPE or moving averages.


## 'signal', 'alloc' and 'TR' dataframes
In all three, each row corresponds to a month and each column to a strategy. The signal is a number representing the opinion of the strategy, it is a sort of precursor to the stock allocation; the big difference being that the allocation has to be between 0 and 1. 

The reason for this extra step is that an allocation of 0 does not tell us whether the strategy is simply bearish at the time or extremely bearish. So the average allocation between 2 strategies at 0% and 40% would be 20% even if the former is super pessimistic. With 'signal' on the other hand, one strategy could count as -40% and the other as 40%, with an average of 0% -- accounting for the difference between a mild pessimism and an extreme one.


## 'stats' and 'parameters' dataframes
The 'stats' dataframe contains the summary of statistics for the strategies. It is filled by calcStatisticsForStrategy() and its contents displayed by showSummaryForStrategy(); the function showSummaries() displays the statistics of the basic strategies in one go (all three functions are in utils.r). It has column names like:
- 'TR', the average real total return;
- 'netTR2', the average real total return, net of 2% of costs (tradingCost + riskAsCost) - because we demand that strategies that trade more also earn more, as said before;
- 'volatility';
- 'avgStockAlloc' and 'latestStockAlloc', the latter indicating what the strategy says we should be doing now;
- 'turnover' (in years) and its inverse 'invTurnover' (used in plots);
- 'DD2' is the sum of the squares of the drawdowns (i.e. price drops).

The 'parameters' dataframe contains the parameters used to generate the strategies.  This is mostly for the record and is not commonly read. Note that the strategy names themselves include the values of the main parameters, e.g. 'CAPE10_avg30_90_95'.


## Miscellaneous
- 'numData' is the number of months in the data (and often the last index in ranges and loops). Currently at 1720.
- 'def' stores a bunch of default values for function arguments as well as for parameters. It is created by setDefaultValues() in init.r.


# How allocation and return are calculated
## The sequence
The figure https://github.com/m-bouville/investing-by-numbers/blob/master/figures/structure.jpg shows the sequence for a given strategy:
- A function with a name like createXstrategy() is called; the necessary parameters are passed as arguments. For instance: for CAPE, it will be createCAPEstrategy(), found in CAPE.r.
- With some strategies, the data have to be preprocessed. In the case of the CAPE strategy, for instance, a column named 'CAPE10avg30' will be added to the 'dat' dataframe.
- A signal (a column in the dataframe 'signal') is created based on the algorithm for the strategy and parameters for that strategy. This is done by the function calcXsignal() in the strategy R file (e.g. calcCAPEsignal() found in CAPE.r).
- An allocation (a column in the dataframe 'alloc') is generated from the signal (this has no parameter) by the function calcAllocFromSignal(), found in utils.r.
- A return (in the dataframe 'TR', for total return) is calculated based on the allocation by calcStrategyReturn(), found in utils.r. (All returns are in real terms, i.e. net of inflation.)
- From this, one can calculate things like drawdowns and volatility.


## Combining strategies
In combine.r, one can put together several strategies based on a weighted average of their 'signal'. 

In practice, this is done hierarchically: we combine value strategies with value strategies, and technical strategies with technical strategies. Then we combine the 2 resulting strategies to make up a 'balanced' strategy.
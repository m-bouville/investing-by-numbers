Investing by numbers
=====================

The world of investing has many tribes: 
some buy and hold,
others pick stocks (which takes time and skill, and incurs trading costs). 
The coolest kids on the block are into day-trading (which is time-consuming and more lucrative for the intermediaries than for the traders themselves). The latter kind of speculation generally relies on charts (hence "chartism"). Part of it is magic and superstition, part of it is mathematical tools. The mathematical strategies that are objective can be tested, i.e. we can see what would have happened had we applied them in the past. 


### Keeping it slow
A key characteristic of any strategy is its time scale. A strategy that can tell that the stock market is overpriced may need years to be proven right: in the nineties the market was more overpriced than in 1929, but it stayed overpriced for years before crashing. Such strategies will necessarily trading infrequently, making their move and waiting to be proven right (eventually).

Some indicators aim at telling that the market is in a trend (up or down). If one expects the market to keep trending up one buys, if one expects a downward trend one sells. Such trends can exist over minutes or years. Trading on the shorter time scales is a full-time activity.



### Anybody can use it...
We focus on time scales of years, which essentially means investing in stocks except  when the market is unfavorable. Such asset allocation is suitable for normal, non-professional investors:
- it is based solely on dynamic changes to the stock-bond allocation,
- there is no short position,
- there is no stock-picking,
- there is no leverage,
- one needs to act only infrequently (this is far from a full-time activity),
- there are no asset classes other than stocks and bonds,
- the strategies use only data that are publically available.

### ... but not anybody can create it
Coming up with the strategies on the other hand requires more: 
- knowledge of finance, math, programming (the code is in R);
- finding more data (e.g. so far I have data only for the U.S.);
- coming up with new ideas (or reusing other people's);
- testing a wide range of parameters for these strategies and keeping the best ones.



# Strategies

## Value-based strategies

### Cyclically adjusted price-to-earnings ratio (CAPE)
The price-to-earnings ratio (P/E) is a common tool for valuing a stock or a group of stock (such as an index). A difficulty is that earnings may be negative in a recession, making the P/E ratio negative as well. And if earnings drop temporarily due to the state of the economy and the stock price drops to the same extent, the P/E ratio does not change; yet it is a good time to buy because earnings will recover along with the economy. So the P/E ratio is not a very reliable guide to time the market.

The cyclically adjusted P/E (CAPE) is simply a P/E ratio calculated using a longer-term historical number, for instance the average earnings over the past ten years. The idea is that such a number gives a reasonable idea of what the earnings will be over an economic cycle. When the CAPE is high, stocks are deemed expensive, and cheap when the CAPE is low. The CAPE was at or above 30 for three months in 1929, and we all know what followed. It was even above 40 in 1999 and 2000!

The current project is based on something I did some time ago, based intially on CAPE strategies. So you can look at http://mathieu.bouville.name/finance/CAPE/ for a more extensive introdction to the subject.


### Detrended price
We calculate a long-term trend (exponential regression) for the stock prices and compare the current price to this trend. Is the current price much higher than we would expect? This would be cause for caution. If stock prices are much lower than their long-term trend we conclude that they are cheap and buy.

### Final remarks
A drawback of these strategies is that they may require to stay out of the stock market for over a decade. And I am not sure whether one would have the nerve to try to beat stocks by shunning them for so long. 

A common claim regarding any strategy is that if it were so easy to outperform, everybody would do it. But one must remember that mutual funds care about returns only indirectly. Why will they not stay out of the market for half a dozen years when there is a bubble? Because by the time they are proven right (having a higher return than competitors with lower volatility), the fund has been closed down because all investors fled. The main risk for fund managers is a drop of asset under management, not of asset prices.




## Technical strategies
These are based on variations of the prices only. There is no attempt at finding out whether the price is high or low (as with the value strategies) but instead the question is 'will it go up or down in the near future'. In the late 1990s for instance, prices were high but still increasing, so value strategies were out of the stock market whereas technical ones were in.

### Bollinger bands
The basic idea is to see whether the price is much higher than it was recently in terms of how many standard deviations away it is (somewhat similar to the z-statistics). See https://en.wikipedia.org/wiki/Bollinger_Bands for instance for a description. 

### Simple moving averages (SMA)
A simple moving average over a year is simply the average price over the past year. If the current price is higher then prices are currently going up, which is used as indication of a bull market.

### Momentum
This compares the current price to the price a year ago, again to spot a trend.

### Reversal
The trend reversal strategy does not find out whether prices are going up (time to be in the market), or going down (we should be out of the market).
Instead it finds out whether a rise or fall is starting.
So when the signal is positive, instead of _setting_ the allocation to a high value, we _increase_ its value.
For this reason the algorithm is rather different from other strategies.

### A note on trading costs
They have three sources:
- buying and selling assets costs money;
- buying and selling assets takes time, and strategies that trade often require more sustained attention (even when not actually trading);
- a strategy that trades more is more likely to need up-to-date information (whereas a strategy like CAPE that turns its portfolio once in a decade can afford to use data that are a month old), and in historical testing it is not always easy to tell what precise information was available at the time -- so we penalize heavy-trading strategies because of the extra risk, not just the extra cost.

This last reason explains why the trading costs used in the code may seem too high.


## Multi-strategy
I take a weighted average of the best CAPE strategy and the best detrended strategy to get a new strategy. Likewise I take a weighted average of Bollinger, SMA and momentum strategies. I then take again an average to obtain a 'balanced' strategy, which is the best of the whole bunch.





# Code
### The files
- Each strategy is handled in a separate .r file, named after it, e.g. Bollinger.r or detrended.r;
- utils.r has generic functions (used for all strategies);
- plotting.r is in charge of generating plots;
- init.r loads the data and does some initialization;
- DD.r calculates drawdowns;
- otherAssetClasses.r loads the data and does some initialization for gold and housing prices.

**To get started, just source main.r.**

Some useful functions can be seen by calling 'showUsefulFunctions()'.


## Data structure
### 'dat' dataframe
The data are initially loaded into a dataframe named 'dat'. Everything this dataframe contains is in real terms. Each row corresponds to a month (starting in January 1871). Columns include: 
- 'date';
- 'numeric date', the date as a real number (convenient for plotting);
- 'CPI', the consumer price index (used to calculation inflation, and hence real prices);
- 'dividend' (real);
- 'earnings' (real);
- 'TR', i.e. (real) total return = stock price + dividends;
- 'bonds', (real) bond prices;
- some data calculated for the strategy, such as CAPE or moving averages.


### 'signal', 'alloc', 'TR' and 'netTR2' dataframes
In all three, each row corresponds to a month and each column to a strategy. The signal is a number representing the opinion of the strategy, it is a sort of precursor to the stock allocation; the big difference being that the allocation has to be between 0 and 1. 

The reason for this extra step is that an allocation of 0 does not tell us whether the strategy is simply bearish at the time or extremely bearish. So the average allocation between 2 strategies at 0% and 40% would be 20% even if the former is super pessimistic. With 'signal' on the other hand, one strategy could count as -40% and the other as 40%, with an average of 0% -- accounting for the difference between a mild pessimism and an extreme one.

'TR' stores the value of the investment with time. And 'netTR2' is 'TR' net of 2% of trading costs (because we demand that strategies that trade more also earn more).


### 'stats' and 'parameters' dataframes
The 'stats' dataframe contains the summary of statistics for the strategies. It is filled by calcStatisticsForStrategy() and its contents displayed by showSummaryForStrategy(); the function showSummaries() displays the statistics of the basic strategies in one go (all three functions are in utils.r). It has column names like:
- 'TR', the average real total return;
- 'netTR2', the average real total return, net of 2% of trading costs;
- 'volatility';
- 'avgStockAlloc' and 'latestStockAlloc', the latter indicating what the strategy says we should be doing now;
- 'turnover' (in years) and its inverse 'invTurnover' (used in plots);
- 'DD2' is the sum of the squares of the drawdowns (i.e. price drops).

The 'parameters' dataframe contains the parameters used to generate the strategies.  This is mostly for the record and is not commonly read. Note that the strategy names themselves include the values of the main parameters, e.g. 'CAPE10_2avg30__90_95'.


### Miscellaneous
- 'numData' is the number of months in the data (and often the last index in ranges and loops). Currently at 1720.
- 'def' stores a bunch of default values for function arguments as well as for parameters. It is created by setDefaultValues() in init.


## How allocation and return are calculated
The figure https://github.com/m-bouville/investing-by-numbers/blob/master/figures/structure.jpg shows the sequence for a given strategy:
- To create a new strategy a function with a name like createXstrategy() is called; the necessary parameters are passed as arguments. For instance: for CAPE, it will be createCAPEstrategy(), found in CAPE.r.
- With some strategies, the data have to be preprocessed. In the case of the CAPE strategy, for instance, a column named 'CAPE10_2avg30' will be added to the 'dat' dataframe.
- A signal (a column in the dataframe 'signal') is created based on the algorithm for the strategy and parameters for that strategy. This is done by the function calcXsignal() in the strategy R file (e.g. calcCAPEsignal() found in CAPE.r).
- An allocation (a column in the dataframe 'alloc') is generated from the signal (this has no parameter) by the function calcAllocFromSignal(), found in utils.r.
- A return (in the dataframe 'TR', for total return) is calculated based on the allocation by calcStrategyReturn(), found in utils.r. (All returns are in real terms, i.e. net of inflation.)
- From this, one can calculate things like drawdowns and volatility.


### Combining strategies
In multiStrategies.r, one can put together several strategies based on a weighted average of their 'signal'. 

In practice, this is done hierarchically: we combine value strategies with value strategies, and technical strategies with technical strategies. Then we combine the 2 resulting strategies to make up a 'balanced' strategy.


## Preliminary results
Here are preliminary results (compared to stocks), in 'table' form:

strategy  |  TR  | vol.  |alloc: avg, now|TO yrs| DD^2| 

stocks    | 6.5% | 18.7% |    100%, 100% |  Inf | 4.2 | 

CAPE10    | 7.4% | 14.1% |     50%,   1% |  9.1 | 1.6 |

detrended | 7  % | 13.9% |     48%,  98% | 14.7 | 1.4 | 

Bollinger | 7.2% | 14  % |     74%,  99% |  1.3 | 1.6 | 

SMA 12-1  | 6.7% | 13.3% |     70%,  93% |  1.1 | 1.3 | 

momentum  | 6.6% | 14.4% |     77%,  97% |  1.5 | 1.8 | 

reversal  | 7.2% | 14.5% |     76%,  99% |  1.7 | 1.8 | 

value     | 7.2% | 13.9% |     47%,   1% | 12.6 | 1.5 | 

technical | 6.9% | 13.8% |     74%,  97% |  1.3 | 1.5 | 

balanced  | 7  % | 13.8% |     51%,   2% |  4.8 | 1.5 | 

TR: total return (net of trading costs), vol: volatility, TO: turnover (in years), DD^2: sum of the squares of the magnitude of drawdowns

These figures may also help:
- https://github.com/m-bouville/investing-by-numbers/blob/master/figures/net%20return%20and%20alloc.png
- https://github.com/m-bouville/investing-by-numbers/blob/master/figures/net%20return%20vs%20four%20-%20zoomed%20out.png
- https://github.com/m-bouville/investing-by-numbers/blob/master/figures/net%20return%20vs%20four%20-%20zoomed%20in.png



### Issues
https://github.com/m-bouville/investing-by-numbers/blob/master/figures/net%20return%20vs%20volatility%20-%20balanced%20-%20weird%20behavior.png shows that the weighted average between two strategies does not behave at all as expected.

showToDoList() (in main.r) shows some of what is left to do.

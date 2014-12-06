Investing by numbers  --  Mathieu Bouville, PhD
===============================================

The world of investing has many tribes: some buy and hold, others pick stocks (which takes time and skill, and incurs trading costs). The coolest kids on the block are into day-trading (which is time-consuming and more lucrative for the intermediaries than for the traders themselves). The latter kind of speculation generally relies on charts (hence "chartism"). Part of it is magical thinking and superstition, part of it is mathematical tools. The mathematical strategies that are objective can be backtested, i.e. we can see what would have happened had we applied them in the past: no need to believe in magic. 


### Keeping it slow
A key characteristic of any strategy is its time scale. A strategy that can tell that the stock market is overpriced will necessarily trade infrequently because it may take years to be proven right: in the late nineties the market was more overpriced than in 1929, but it stayed overpriced for years before crashing.

Some indicators aim at telling that the market is in a trend (up or down). If one expects the market to keep trending up one buys, if one expects a downward trend one sells. Such trends can exist over minutes or years. Trading on the shorter time scales is a full-time activity.

I use monthly data starting in the 19th century (I deal with stocks as an asset class rather than with individual stocks), not daily or tick-by-tick data from the past few years. Two advantages of strategies trading less often are:
- they are less time consuming, i.e. not a full-time job, 
- the trading costs are not as huge an issue.
I test the strategies,
- taking costs into account and 
- splitting the data between searching for parameters and testing them.
So I do not claim that a certain strategy should work in theory, I actually test it and show the results as plots: https://github.com/m-bouville/investing-by-numbers/tree/master/figures (so you can make up your own mind as to whether this is working or not).


### Strategies for the average investor
I focus on time scales of years, which essentially means investing in stocks except when the market is unfavorable. Such asset allocation is suitable for normal, non-professional investors:
- it is based solely on dynamic changes to the stock-bond allocation,
- there is no short position,
- there is no stock-picking,
- there is no leverage,
- one needs to act only infrequently (this is far from a full-time activity),
- there are no asset classes other than stocks and bonds,
- the strategies use only data that are publically available.


### Diving into the details

The strategies are described in https://github.com/m-bouville/investing-by-numbers/tree/master/docs/strategies.md

The code is described in https://github.com/m-bouville/investing-by-numbers/tree/master/docs/code_structure.md



### Results
The parameters are optimized using data from 1871 to 1942:
- https://github.com/m-bouville/investing-by-numbers/blob/master/figures/1871_1942-return_and_allocation.png
- https://github.com/m-bouville/investing-by-numbers/blob/master/figures/1871_1942-return_vs_four.png

Then tested between 1942 and 2014:
- https://github.com/m-bouville/investing-by-numbers/blob/master/figures/1942_2014-return_and_allocation.png
- https://github.com/m-bouville/investing-by-numbers/blob/master/figures/1942_2014-return_vs_four.png

- https://github.com/m-bouville/investing-by-numbers/blob/master/figures/return_strategy_with_gold.png shows results with gold
- https://github.com/m-bouville/investing-by-numbers/blob/master/figures/sideways_markets.png shows 4 time periods when markets move sideways.



### Color codes for the plots
- CAPE:      cyan
- detrended: skyblue
- SMA:       pink
- Bollinger: magenta
- reversal:  orange

combined strategies (squares):
- value:     blue
- technical: red
- balanced:  black

constant-allocation stock-bond portfolios: green line


### Discussion
The whole point of splitting data between a search for optimal parameters and testing is that what looked like the best strategy may in fact be due to a strange turn of event which made us create a great strategy for too specific a situation. The test between 1942 and 2014 shows that value strategies fare a lot less well than from 1871 to 1942. So the masochism worked just fine.

The reason why the value strategies fall harder than technical strategies is that they turn their portfolio infrequently. Sixty years of data for a strategy with a natural cycle time of twenty years means that we fit to only three cycles. In a sense there are more data available for technical strategies, not in terms of number of years but in terms of number of cycles.

Also, since the optimization takes place over 1871-1942 the process is controlled by the Depression: any strategy that handles it well will look good (even if it does not do so well elsewhere).

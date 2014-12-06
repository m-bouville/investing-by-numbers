Investing by numbers: Strategies  --  Mathieu Bouville, PhD
===========================================================


# Value-based strategies

## Cyclically adjusted price-to-earnings ratio (CAPE)
The price-to-earnings ratio (P/E) is a common tool for valuing a stock or a group of stock (such as an index). A difficulty is that earnings may be negative in a recession, making the P/E ratio negative as well. And if earnings drop temporarily due to the state of the economy and the stock price drops to the same extent, the P/E ratio does not change; yet it is a good time to buy because earnings will recover along with the economy. So the P/E ratio is not a very reliable guide to time the market.

The cyclically adjusted P/E (CAPE) is simply a P/E ratio calculated using a longer-term historical number, for instance the average earnings over the past ten years. The idea is that such a number gives a reasonable idea of what the earnings will be over an economic cycle. When the CAPE is high, stocks are deemed expensive, and cheap when the CAPE is low. The CAPE was at or above 30 for three months in 1929, and we all know what followed. It was even above 40 in 1999 and 2000!

The current project is based on something I did some time ago, based initially on CAPE strategies. So you can look at http://mathieu.bouville.name/finance/CAPE/ for a more extensive introduction to the subject.


## Detrended price
We calculate a long-term trend (exponential regression) for the stock prices and compare the current price to this trend. Is the current price much higher than we would expect? This would be cause for caution. If stock prices are much lower than their long-term trend we conclude that they are cheap and buy.


## Final remarks
A drawback of these strategies is that they may require to stay out of the stock market for over a decade. And I am not sure whether one would have the nerve to try to beat stocks by shunning them for so long. 

Another issue is that since they are slow-moving, even 70 years of data include only a few cycles, which means that optimization has to rely on limited information.

A common claim regarding any strategy is that if it were so easy to outperform, everybody would do it. But one must remember that mutual funds care about returns only indirectly. Why will they not stay out of the market for half a dozen years when there is a bubble? Because by the time they are proven right (having a higher return than competitors with lower volatility), the fund has been closed down because all investors fled. The main risk for fund managers is a drop of asset under management, not of asset prices.




# Technical strategies
These are based on variations of the prices only. There is no attempt at finding out whether the price is objectively high or low (as with the value strategies) but instead the question is 'will it go up or down in the near future'. In the late 1990s for instance, prices were high but still increasing, so value strategies were out of the stock market whereas technical ones were in.

## Bollinger bands
The basic idea is to see whether the price is much higher than it was recently in terms of how many standard deviations away it is (somewhat similar to the z-statistics). See https://en.wikipedia.org/wiki/Bollinger_Bands for instance for a description. 

## Simple moving averages (SMA)
A simple moving average over a year is simply the average price over the past year. If the current price is higher then prices are currently going up, which is used as indication of a bull market.

## Reversal
The trend reversal strategy does not find out whether prices are going up (time to be in the market), or going down (we should be out of the market). Instead it finds out whether a rise or fall is starting. So when the signal is positive, instead of _setting_ the allocation to a high value, we _increase_ its value. For this reason the algorithm is rather different from other strategies.


# Hybrid strategies
They apply a strategy to another strategy instead of to the stock price. For instance, instead of calculating Bollinger bands for the (total return, real) price of stocks, one can calculate Bollinger bands for the cyclically adjusted price-earning ratio (CAPE); such a strategy would be labelled "(Boll(CAPE)".



# Combining strategies
I take a weighted average of the best CAPE strategies and the best detrended strategy to get a new strategy: "value". Likewise I take a weighted average of Bollinger, SMA and reversal strategies to calculate a "technical" strategy; and I do the same for the "hybrid" strategies. I then take again an average to obtain a "balanced" strategy, which is the best of the whole bunch.
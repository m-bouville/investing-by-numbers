Investing by numbers
=====================

This maths-based (quantitative) asset allocation (stocks and bonds) is based on something I wrote up some time ago: http://mathieu.bouville.name/finance/CAPE/ . I had initially done it in Excel and am now switching to R.

# Target audience

## Anybody can use it...
Such asset allocation is suitable for normal, non-professional investors:
- it is based solely on dynamic changes to the stock–bond allocation;
- there is no short position; 
- there is no stock-picking; 
- there is no leverage; 
- there are no asset classes other than stocks and bonds; 
- the strategies use only data that are publically available.

## ... but not anybody can create it
Coming up with the strategies on the other hand is more involved: 
- knowledge of finance, maths, programming;
- finding more data (e.g. so far I have data only for the U.S.);
- coming up with new ideas (or reusing other people's);
- testing a wide range of parameters for these strategies and keeping the best ones.


# Strategies

## Value-based strategies

### Cyclically adjusted price-to-earnings ratio (CAPE)
The price-to-earnings ratio (P/E) is a common tool for valuing a stock or a group of stock (such as an index). A difficulty is that there are multiple choices for the earnings. One generally uses forecasts, but these are basically guesses. One can instead prefer earnings for the past year. In any case, either of these two earnings may be negative in a recession, making the P/E ratio negative as well. And if earnings drop temporarily due to the state of the economy and the stock price drops to the same extent, the P/E ratio does not change; yet it is a good time to buy because earnings will recover along with the economy. So the P/E ratio is not a very reliable guide to time the market.

A third choice is to use a longer-term historical number, for instance the average earnings over the past ten years. The idea is that such a number gives a reasonable idea of what the earnings will be over an economic cycle. The so-called cyclically-adjusted P/E (CAPE) is simply a P/E ratio calculated using such an average over an economic cycle for earnings.

The historical average of the S&P 500 CAPE was 16.4 (and the median was 15.8). When the CAPE is quite above this value, stocks are deemed expensive, and cheap when the CAPE is low. The CAPE was at or above 30 for three months in 1929, and we all know what followed. It was even above 40 in 1999 and 2000!

### Detrended price
We calculate a long-term trend (exponential regression) for the stock prices and compare the current price to this trend. Is the current price much higher than we would expect? This would be cause for caution. If stock prices are much lower than their long-term trend we conclude that they are cheap and buy.

### Common features
Both strategies have a number of features in common. On top of those already mentioned above, we can make two remarks:
- It seems that a weakness of these strategies is that they buy bonds when stocks are deemed expensive, regardless of the intrinsic appeal of bonds. I tried taking bond yields into account (using a kind of equity premium), but results are similar. The most plausible reason is that when the stock market is set for a crash, what matters is simply not being in the middle of it.
- A drawback of these strategies is that they may require to stay out of the stock market for over a decade. And I am not sure whether one would have the nerve to try to beat stocks by shunning equity for so long. 

### Final remark
A common claim regarding any strategy is that if it were so easy to outperform, everybody would do it. But one must remember that mutual funds care about returns only indirectly. Why will they not stay out of the market for half a dozen years when there is a bubble? Because by the time they are proven right (having a higher return than competitors with lower volatility), the fund has been closed down because all "investors" fled. The main risk for fund managers is outflows, the rest matters only inasmuch as it can lead to outflows.




## Technical strategies
These are based on variations of the prices only. There is no attempt at finding out whether the price is high or low (as with the value strategies) but instead the question is 'will it go up or down in the near future'. So in the late 1990s for instance, prices were high but still increasing, so value strategies were out of the stock market whereas technical ones were in.

### Bollinger bands
The basic idea is to see whether the price is much higher than it was recently in terms of how many standard deviations away it is (somewhat similar to the z-statistics). See https://en.wikipedia.org/wiki/Bollinger_Bands for instance for a description. 

### Simple moving averages (SMA)
A simple moving average over a year is simply the average price over the past year. If the current price is higher then prices are currently going up, which is used as indication of a bull market.

### Momentum
This compares the current price to the price a year ago, again to spot a trend.



## Multi-strategy
I take a weighted average of the best CAPE strategy and the best detrended strategy to get a new strategy. Likewise I take a weighted average of Bollinger, SMA and momentum strategies. I then take again an average to obtained a 'balanced' strategy, which is the best of the whole bunch.





## Code


### Current status
I switched to R recently and the code needs a bit of cleaning before it can be used by someone not in my mind. I hope to upload it this weekend.



## Preliminary results
Here they are (compared to stocks), in 'table' form:

strategy  |  TR  | vol.  |alloc: avg, now|TO yrs| DD^2 | 

stocks    | 6.5% | 18.7% |    100%, 100% |  Inf | 4.22 | 

CAPE10    | 7.4% | 14.1% |     50%,   1% |  9.1 | 1.62 |

detrended | 7  % | 13.9% |     48%,  98% | 14.7 | 1.48 | 

Bollinger | 6.9% | 14  % |     76%,  97% |  1.4 | 1.67 | 

SMA 12-1  | 6.7% | 13.3% |     70%,  93% |  1.1 | 1.36 | 

momentum  | 6.6% | 14.4% |     77%,  97% |  1.5 | 1.8  | 

value     | 7.2% | 13.9% |     46%,   1% | 12.5 | 1.51 | 

technical | 7  % | 13.6% |     72%,  97% |  1.2 | 1.48 | 

balanced  | 8  % | 14.6% |     78%,  94% |  3.7 | 1.88 | 

TR: total return (net of trading costs), vol: volatility, TO: turnover (in years), DD^2: sum of the squares of the magnitude of drawdowns

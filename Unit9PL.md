# Unit9Postlive
Aravind Veluchamy  
July 20, 2016  
##Visitor Example
##Loading Library and Data

```r
library(fpp)
```

```
## Loading required package: forecast
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```
## Loading required package: timeDate
```

```
## This is forecast 7.1
```

```
## Loading required package: fma
```

```
## Loading required package: tseries
```

```
## Loading required package: expsmooth
```

```
## Loading required package: lmtest
```

```r
data(visitors)
```
###Plotting Time Series
###Looking at the time series data there is a seasonal trend where the data is where usage is reduces towards the end of each year.Overall the usage has been increasing till the year 2000 and then a slight decrease till 2005 followed by an increase till 2010 where the usage again dips. 

```r
plot(visitors)
```

![](Unit9PL_files/figure-html/unnamed-chunk-2-1.png)<!-- -->
###Classical decomposition of data


```r
fitd <- decompose(visitors)
plot(fitd)
```

![](Unit9PL_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
###The results are consistent with time series.However the decomposition gives seasonal trends,overall trends and random fluctuations in a detailed representation.
###Seasonal Adjusted Data

```r
eeadj <- seasadj(fitd)
plot(eeadj)
```

![](Unit9PL_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
###Adding Outliers and after  recomputing seasonally adjusted data I could not find any noticeable difference except for peak at where the outlier was added

```r
Visitors2 <- ts(c(visitors[1:60],visitors[61]+500,visitors[62:240]),start=c(1986,1),frequency=12)
fitd1 <- decompose(Visitors2)
eeadj1 <- seasadj(fitd1)
plot(eeadj1)
```

![](Unit9PL_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
###Adding a outlier did not really make a difference whether added to the middle or end of the timeseries.
### Using STL to decompose the data

```r
fit <- stl(Visitors2, s.window=5)
plot(fit)
```

![](Unit9PL_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
##Volatility using Stocks
###Installing Tseries and adding library

```r
library(tseries)
```
###Downloading URTY stock

```r
URTYData <- get.hist.quote('urty',quote="Close")
```

```
## time series starts 2010-02-11
```
###Calculating Log Returns

```r
URTYret <- log(lag(URTYData)) - log(URTYData)
```
###Calculating Volatility Measure

```r
URTYvol <- sd(URTYret) * sqrt(250) * 100
```
###Volatility Function

```r
get
```

```
## function (x, pos = -1L, envir = as.environment(pos), mode = "any", 
##     inherits = TRUE) 
## .Internal(get(x, envir, mode, inherits))
## <bytecode: 0x7fd05295fff8>
## <environment: namespace:base>
```

```r
Vol <- function(d, logrets)
{

	var = 0

	lam = 0

	varlist <- c()

	for (r in logrets) {

		lam = lam*(1 - 1/d) + 1
	
	var = (1 - 1/lam)*var + (1/lam)*r^2

		varlist <- c(varlist, var)

	}

	sqrt(varlist)
}
```
###Volatility Measure

```r
volest <- Vol(10,URTYret)
volest2 <- Vol(30,URTYret)
volest3 <- Vol(100,URTYret)
```
###Calculate volatility measure with a continuous lookback window.Plot the results with a volatility curve overlay.

```r
plot(volest,type="l")
lines(volest2,type="l",col="Red")
lines(volest3, type = "l", col="blue")
```

![](Unit9PL_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

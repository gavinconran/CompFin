## Assignment 5
# Part 1: Getting the financial data
# Load relevant packages
library(PerformanceAnalytics);library(zoo);library(tseries);

# Get the monthly adjusted closing price data on VBLTX, FMAGX and SBUX from Yahoo! 
# using the tseries function get.hist.quote(). Set the sample to Jan 1998 through Dec 2009.

# Get the adjusted closing prices from Yahoo!
VBLTX_prices <- get.hist.quote(instrument="vbltx", start="1998-01-01",end="2009-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)

FMAGX_prices <- get.hist.quote(instrument="fmagx", start="1998-01-01",end="2009-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)

SBUX_prices <- get.hist.quote(instrument="sbux", start="1998-01-01",end="2009-12-31", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet=TRUE)

  
  # Change the class of the time index to yearmon which is appropriate for monthly data
  # index() and as.yearmon() are functions in the zoo package  
  
index(VBLTX_prices) <- as.yearmon(index(VBLTX_prices))
index(FMAGX_prices) <- as.yearmon(index(FMAGX_prices))
index(SBUX_prices) <- as.yearmon(index(SBUX_prices))

# Inspect your data
start(SBUX_prices)
end(SBUX_prices)

# Part 2: Calculating the returns
# The variables VBLTX_prices, FMAGX_prices and SBUX_prices are preloaded in your workspace

# Create merged price data
all_prices = merge(VBLTX_prices, FMAGX_prices, SBUX_prices)
# Rename columns
colnames(all_prices) <- c("VBLTX", "FMAGX", "SBUX")

# Calculate cc returns as difference in log prices
VBLTX = diff(log(VBLTX_prices))
FMAGX = diff(log(FMAGX_prices))
SBUX = diff(log(SBUX_prices))
all_returns = merge(VBLTX,FMAGX,SBUX)
colnames(all_returns) <- c("VBLTX", "FMAGX", "SBUX")

# Look at the return data
start(all_returns)
end(all_returns)
colnames(all_returns) 
head(all_returns)

# Part 3: Plotting financial data with PerformanceAnalytics
# Plot returns after using the PerformanceAnalytics function chart.TimeSeries().
# This function creates a slightly nicer looking plot than plot.zoo()
chart.TimeSeries(all_returns, legend.loc="bottom", main=" ") 

# The previous charts are a bit hard to read. The PerformanceAnalytics function
# chart.Bar makes it easier to compare the returns of different assets on the 
# same plot
chart.Bar(all_returns, legend.loc="bottom", main=" ")

# Cumulative return plot - must use simple returns (!) and not cc returns for this
# Use PerformanceAnalytics function chart.CumReturns()
simple_returns <- diff(all_prices)/lag(all_prices, k=-1)
chart.CumReturns(simple_returns, wealth.index = TRUE, main="Future Value of $1 invested", legend.loc="topleft")

# Part 4: Create graphical summary for a return series

# Create matrix with returns
return_matrix <- coredata(all_returns);

# Generate four panel plots
par(mfrow=c(2,2))
hist(return_matrix[,"VBLTX"],main="VBLTX monthly returns",
     xlab="VBLTX", probability=T, col="slateblue1")
boxplot(return_matrix[,"VBLTX"],outchar=T, main="Boxplot", col="slateblue1")
plot(density(return_matrix[,"VBLTX"]),type="l", main="Smoothed density",
     xlab="monthly return", ylab="density estimate", col="slateblue1")
qqnorm(return_matrix[,"VBLTX"], col="slateblue1")
qqline(return_matrix[,"VBLTX"])
par(mfrow=c(1,1))

# Part 5: Return distribution comparison
# Create matrix with returns
return_matrix <- coredata(all_returns);

# Show boxplot of three series on one plot
boxplot(return_matrix[,"VBLTX"], return_matrix[,"FMAGX"], return_matrix[,"SBUX"],
        names=colnames(return_matrix), col="slateblue1")

# Do the same thing using the PerformanceAnalytics function chart.Boxplot
chart.Boxplot(all_returns)

## Part 6: Compute univariate descriptive statistics
# Create matrix with returns
return_matrix <- coredata(all_returns);

summary(return_matrix)

# Compute descriptive statistics by column using the base R function apply()
args(apply)
apply(return_matrix, 2, mean)
apply(return_matrix, 2, var)
apply(return_matrix, 2, sd)
apply(return_matrix, 2, skewness)
apply(return_matrix, 2, kurtosis)

# A nice PerformanceAnalytics function that computes all of the relevant descriptive statistics is table.Stats
table.Stats(all_returns)


## Part 7: Annualized monthly estimates
# Annualized continuously compounded mean 
12*apply(return_matrix, 2, mean)
# Annualized simple mean
exp(12*apply(return_matrix, 2, mean)) - 1;

# Annualized standard deviation values
sqrt(12)*apply(return_matrix, 2, sd)

## Part 8: Bivariate graphical analysis
# Display all possible pair-wise scatter plots
pairs(return_matrix, pch=16, col="slateblue1")

# Compute 3 x 3 covariance and correlation matrices
var(return_matrix)
cor(return_matrix)


#
# Part 9. Time series descriptive statistics
#
# autocorrelations for simulated gaussian data and SP500

acf(return_matrix, lwd=2)



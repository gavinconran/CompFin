## Assignment 6: Constant Expected Return Model
# Part 1: Download the data and calculate the returns
# load tseries, zoo and boot packages
options(digits=4, width=70)
library(tseries)
library(boot)	
library(zoo)	

VBLTX_prices <- get.hist.quote(instrument="vbltx", start="2005-09-01", end="2010-09-30", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet = TRUE)
FMAGX_prices <- get.hist.quote(instrument="fmagx", start="2005-09-01", end="2010-09-30", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet = TRUE)
SBUX_prices <- get.hist.quote(instrument="sbux", start="2005-09-01",end="2010-09-30", quote="AdjClose",provider="yahoo", origin="1970-01-01",compression="m", retclass="zoo", quiet = TRUE)

# Change the class of the time index to yearmon, which is appropriate for monthly data.
# index() and as.yearmon() are functions in the zoo package 
index(VBLTX_prices) <- as.yearmon(index(VBLTX_prices))
index(FMAGX_prices) <- as.yearmon(index(FMAGX_prices))
index(SBUX_prices) <- as.yearmon(index(SBUX_prices))

# Create merged price data
all_prices <- merge(VBLTX_prices, FMAGX_prices, SBUX_prices)
# Rename columns
colnames(all_prices) <- c("VBLTX", "FMAGX", "SBUX")

# Calculate cc returns as difference in log prices
all_returns <- diff(log(all_prices))

# Create matrix with returns
return_matrix <- coredata(all_returns)

# Part 2: The standard error of the variances
# Remember from the course that an estimator for σ2i is just the sample variance. 
# Once you have these estimates, your second task is to investigate the precision of the estimates. 
# More specifically, you should estimate the standard errors in order to get SE^(σ2i^). 
# Number of observations
n_obs <- nrow(return_matrix)

# Estimates of sigma2hat
sigma2hat_vals <- apply(return_matrix, 2, var)
  
# Standard Error of sigma2hat
se_sigma2hat <- sigma2hat_vals/sqrt(n_obs/2)
  
se_sigma2hat

# Part 3: Estimate the standard error of the correlation parameter
# Calculate the correlation matrix
cor_matrix <- cor(return_matrix)

# Get the lower triangular part of that 'cor_matrix'
rhohat_vals <- cor_matrix[lower.tri(cor_matrix)]
  
# Set the names
names(rhohat_vals) <- c("VBLTX,FMAGX","VBLTX,SBUX","FMAGX,SBUX")

# Compute the estimated standard errors for correlation
se_rhohat <- (1 - rhohat_vals^2)/sqrt(nrow(return_matrix))
  
se_rhohat

# Part 4: Hypothesis test for the mean
# Use the t.test to perform the t-test for μVBLTX and print the result to the console.
t.test.VBLTX = t.test(all_returns[,"VBLTX"], 
                     alternative="two.sided",
                     mu=0, conf.level=0.95)

t.test.VBLTX

# Part 5: Hypothesis test for the mean
t.test.FMAGX = t.test(all_returns[,"FMAGX"], 
                      alternative="two.sided",
                      mu=0, conf.level=0.95)

t.test.FMAGX

t.test.SBUX = t.test(all_returns[,"SBUX"], 
                      alternative="two.sided",
                      mu=0, conf.level=0.95)

t.test.SBUX

# Part 6: Hypothesis test for the correlation
?cor.test
cor.test(all_returns[,"VBLTX"], all_returns[,"FMAGX"],
         alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE)

# part 7: Interpretation of the hypothesis test for correlation
cor.test(all_returns[,"VBLTX"], all_returns[,"SBUX"],
         alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE)

cor.test(all_returns[,"FMAGX"], all_returns[,"SBUX"],
         alternative = c("two.sided", "less", "greater"),
         method = c("pearson", "kendall", "spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE)

# Part 7: Normality of the asset returns of VBLTX
jarque.bera.test(all_returns[,"VBLTX"])

# Part 8: Interpretation of the normality test
jarque.bera.test(all_returns[,"FMAGX"])

jarque.bera.test(all_returns[,"SBUX"])


# Part 9: Bootstrapping
# Function for bootstrapping sample mean: 
mean_boot <- function(x, idx) {
  ans <- mean(x[idx])
  ans 
} 
# Construct VBLTX_mean_boot:
VBLTX_mean_boot <- boot(return_matrix[,"VBLTX"], statistic=mean_boot, R=999)

# Print the class of VBLTX_mean_boot
class(VBLTX_mean_boot)

# Print VBLTX_mean_boot
VBLTX_mean_boot

# Plot bootstrap distribution and qq-plot against normal
plot(VBLTX_mean_boot)

# Function for bootstrapping sample sd: 
sd_boot <- function(x, idx) {
  ans <- sd(x[idx])
  ans 
} 
# Construct VBLTX_mean_boot:
VBLTX_sd_boot <- boot(return_matrix[,"VBLTX"], statistic=sd_boot, R=999)

# Print the class of VBLTX_mean_boot
class(VBLTX_sd_boot)

# Print VBLTX_mean_boot
VBLTX_sd_boot

# Plot bootstrap distribution and qq-plot against normal
plot(VBLTX_sd_boot)

# Function for bootstrapping sample correlation: 
cor_boot = function(x, idx) {
  # x.mat	n x 2 data matrix to be resampled
  # idx		vector of scrambled indices created by boot() function
  # value:
  # ans		correlation value computed using resampled data
  
  ans = cor(x[idx,])[1,2]
  ans
}

# Construct VBLTX_cor_boot:
VBLTX_cor_boot <- boot(return_matrix[,c("VBLTX","SBUX")], statistic=cor_boot, R=999)

# Print the class of VBLTX_mean_boot
class(VBLTX_cor_boot)

# Print VBLTX_mean_boot
VBLTX_cor_boot

# Plot bootstrap distribution and qq-plot against normal
plot(VBLTX_cor_boot)

# VaR
# 5% Value-at-Risk
ValueAtRisk.boot = function(x, idx, p=0.05, w=100000) {
  # x.mat	data to be resampled
  # idx		vector of scrambled indices created by boot() function
  # p		probability value for VaR calculation
  # w		value of initial investment
  # value:
  # ans		Value-at-Risk computed using resampled data
  
  q = mean(x[idx]) + sd(x[idx])*qnorm(p)
  VaR = (exp(q) - 1)*w
  VaR
}

# Construct VBLTX_cor_boot:
VBLTX_VaR_boot <- boot(return_matrix[,"VBLTX"], statistic=ValueAtRisk.boot, R=999)

# Print the class of VBLTX_mean_boot
class(VBLTX_VaR_boot)

# Print VBLTX_mean_boot
VBLTX_VaR_boot

# Plot bootstrap distribution and qq-plot against normal
plot(VBLTX_VaR_boot)


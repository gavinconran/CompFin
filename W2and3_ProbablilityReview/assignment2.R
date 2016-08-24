### Assignment 2: Random variables and probability distributions
## Part 1: Compute probabilities
# X ~ N(0.05, (0.10)^2)
mu_x = 0.05
sigma_x = 0.10
# Pr(X > 0.10)
1 - pnorm(0.10, mu_x, sigma_x)
# Pr(X < -0.10)
pnorm(-0.10, mu_x, sigma_x)
# Pr(-0.05 < X < 0.15)
pnorm(0.15, mu_x, sigma_x) - pnorm(-0.05, mu_x, sigma_x) 

## part 2: Compute quantiles
# The mean (mu_x) and the standard deviation (sigma_x) are still in your workspace
# 1%, 5%, 95% and 99% quantile
c(qnorm(0.01, mu_x, sigma_x), qnorm(0.05, mu_x, sigma_x), qnorm(0.95, mu_x, sigma_x), qnorm(0.99, mu_x, sigma_x))

## part 3: Compute densities
# Normally distributed monthly returns
x_vals <- seq(-0.25, 0.35, length.out = 100)
MSFT <- dnorm(x_vals, 0.05, 0.10)
SBUX <- dnorm(x_vals, 0.025, 0.05)

## part 4: Plot normal curve
# Normal curve for MSFT
plot(x_vals, MSFT , type = "l", col = "blue", ylab = "Normal curves", ylim = c(0,8))

## part 5: Add second normal curve
lines(x_vals, SBUX, col = "red")
# Add a plot legend
legend("topleft", legend = c("Microsoft", "Starbucks"), 
       col = c("blue", "red"), lty = 1)

## part 6: Determine the value-at-risk of simple monthly returns
# R ~ N(0.04, (0.09)^2) 
mu_R <- 0.04
sigma_R <- 0.09

# Initial wealth W0 equals $100,000
W0 <- 100000

# The 1% value-at-risk
# q.01.R = qnorm(0.01, mean = mu_R, sd = sigma_R)
q.01.R = mu_R + sigma_R*qnorm(0.01)
VaR.01 = abs(q.01.R*W0)
print(VaR.01)

# The 5% value-at-risk
# q.05.R = qnorm(0.05, mean = mu_R, sd = sigma_R)
q.05.R = mu_R + sigma_R*qnorm(0.05)
VaR.05 = abs(q.05.R*W0)
print(VaR.05)

## part 7: Determine the value-at-risk of continuously compounded monthly returns
# r ~ N(0.04, (0.09)^2) 
mu_r <- 0.04
sigma_r <- 0.09

# Initial wealth W0 equals $100,000
W0 <- 100000

# The 1% value-at-risk
W0 * (exp(qnorm(0.01, mean = mu_r, sd = sigma_r)) -1)

# The 5% value-at-risk
W0 * (exp(qnorm(0.05, mean = mu_r, sd = sigma_r)) - 1)

## part 8: Compute simple monthly returns
# Vectors of prices
PA <- c(38.23, 41.29)
PC <- c(41.11, 41.74)

# Simple monthly returns
RA <- (PA[2] / PA[1]) - 1
RC <- (PC[2] / PC[1]) - 1

## part 9: Compute continuously compounded monthly returns
# The simple returns on Amazon (RA) and Costco (RC) are still in your workspace

# Continuously compounded returns
rA <- log(1 + RA)
rC <- log(1 + RC)

## part 10: Compute simple total returns and dividend yields
# Cash dividend per share
DA <- 0.10

# Simple total return
RA_total <- ((PA[2] - PA[1]) / PA[1]) + (DA / PA[1])

# Dividend yield
DY <- (DA / PA[1])

## part 11: Compute annual returns
# Simple annual return
RA_annual <- (1 + RA)^12 -1

# Continuously compounded annual return
rA <- log(1 + RA)
rA_annual <- 12 * rA

## part 12: Compute portfolio shares and portfolio returns
# Portfolio shares
xA <- 8000/10000
xC <- 2000/10000

# Simple monthly return
(RA * xA) + (RC * xC)


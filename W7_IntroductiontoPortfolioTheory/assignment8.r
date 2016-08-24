### Compute Efficient Portfolios with Matrix Algebra
# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

## Part 1: Loading in your data set
# Load the relevant packages
library("zoo")

# Load the data
data <- url("http://s3.amazonaws.com/assets.datacamp.com/course/compfin/lab9.RData")
load(data)
# Explore the data set
head(returns_df)
tail(returns_df)


# Timeplots with stocks on individual graphs
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot(returns_df, lwd=2, panel=my.panel, col="blue")

# Timeplots with stocks on same graph
plot(returns_df, plot.type = "single", main="Returns", col=1:4, lwd=2)
abline(h=0)
legend(x="bottomleft", legend=colnames(returns_df), col=1:4, lwd=2)

## Part 2: The CER model
# Parameters CER model
mu_hat_month <- apply(returns_df, 2, mean)
mu_hat_month
sigma2_month <- apply(returns_df, 2, var)
sigma2_month
sigma_month <-sqrt(sigma2_month)
sigma_month
cov_mat_month <- var(returns_df)
cov_mat_month
cor_mat_month <- cor(returns_df)
cor_mat_month

# Pairwise scatterplots
pairs(coredata(returns_df), col="slateblue1", pch=16, cex=1.5)

## Part 3: Question: What is the correlation between the Nordstrom stock and the Boeing stock?
cor_mat_month # 0.1025

## Part 4: The global minimum variance portfolio - Part One
# Calculate the global minimum variance portfolio
args(globalMin.portfolio)
global_min_var_portfolio = globalMin.portfolio(mu_hat_month, cov_mat_month, shorts=TRUE)
global_min_var_portfolio

# Plot the portfolio weights of our four stocks
plot(global_min_var_portfolio)

## Part 5: Standard deviation
## Question: What is the standard deviation of the global minimum variance portfolio that you have just calculated? 
# Compute mean, variance and std deviation
mu.gmin = global_min_var_portfolio$er      # mean
sig.gmin = global_min_var_portfolio$sd     # standard deviation
mu.gmin
sig.gmin

## Part 6: The global minimum variance portfolio - Part Two
# set restriction matrices
D_matrix <- 2* cov_mat_month
D_matrix
d_vector <- rep(0,4)
d_vector
A_matrix <- cbind(rep(1,4),diag(4))
A_matrix
b_vector <- c(1,rep(0,4))
b_vector

# use solve.QP to minimize portfolio variance
quad_prog <- solve.QP(Dmat=D_matrix, dvec=d_vector,
                      Amat=A_matrix, bvec=b_vector, meq=1)
quad_prog$solution

## Part 7: The global minimum variance portfolio - End game
# The global minimum variance portfolio
global_min_var_portfolio = globalMin.portfolio(mu_hat_month, cov_mat_month, shorts=FALSE)
global_min_var_portfolio

## part 8: An efficient portfolio
# highest average return
mu_target <- max(mu_hat_month)
  
# short sales allowed
args(efficient.portfolio)
efficient_porfolio_short <- efficient.portfolio(mu_hat_month, cov_mat_month, mu_target, shorts = TRUE)
  
efficient_porfolio_short
plot(efficient_porfolio_short)

# no short sales allowed
efficient_porfolio_no_short <- efficient.portfolio(mu_hat_month, cov_mat_month, mu_target, shorts = FALSE)
  
efficient_porfolio_no_short
plot(efficient_porfolio_no_short)

## Part 9: Question The weight of Boeing
# What is the weight of the Boeing stock under the "shorting not allowed" condition? 
efficient_porfolio_no_short

## Part 10: The efficient frontier
# The efficient frontier of risky assets 
args(efficient.frontier)
efficient_frontier <- efficient.frontier(mu_hat_month, cov_mat_month, alpha.min=-1, alpha.max=1)
summary(efficient_frontier)

# The plot
plot(efficient_frontier, plot.assets=TRUE, col="blue", lwd=2)

###QUIZ QUESTION 8:
# Using the fact that all efficient portfolios can be written as a convex combination of two efficient portfolios, 
# compute efficient portfolios as convex combinations of the global minimum variance portfolio and the efficient portfolio that was computed in question six.
# What is the expected return of the portfolio when α=.5?
# z=α∗m+(1−α)∗x
z = 0.5 * global_min_var_portfolio$weights + (1-0.5)*efficient_porfolio_short$weights
sum(z*mu_hat_month) # expected return of portfolio = 3.06%


m## part 11: The tangency portfolio
# risk free rate
t_bill_rate <- 0.005
tangency_portfolio_short <- tangency.portfolio(mu_hat_month, cov_mat_month, t_bill_rate, shorts = TRUE)

summary(tangency_portfolio_short)
#plot
plot(tangency_portfolio_short)


# Tangency portfolio short sales not allowed
tangency_portfolio_no_short <- tangency.portfolio(mu_hat_month,cov_mat_month, t_bill_rate, shorts = FALSE)

summary(tangency_portfolio_no_short)
#plot
plot(tangency_portfolio_no_short)

## Part 12: The weight of Boeing ... again
# Question: If short sales is not allowed in your tangency portfolio, what is the weight of Boeing stock?
tangency_portfolio_no_short$weights




# bootstrapPortfolio.r
# 
# author: Eric Zivot
# created: August 20, 2013
# update history:
# August 21, 2013
#   Minor changes

# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

options(digits=4, width=70)
library(boot) # note: don't actually use the boot library for examples
library(PerformanceAnalytics)
cex.val = 2


# load portfolio theory functions
# source portfolio functions
source(file="http://faculty.washington.edu/ezivot/econ424/portfolio_noshorts.r")

# load four firm example data
singleIndexPrices.df = read.csv("~/Economics/ComputationalFinance/CompFin/W7_IntroductiontoPortfolioTheory/singleIndexPrices.csv",
                                stringsAsFactors=F)
colnames(singleIndexPrices.df)
colnames(singleIndexPrices.df)
#
# 2. Create zoo object from data and dates in singleIndexPrices.df
#
td = seq(as.Date("1998-01-01"), as.Date("2003-01-01"), by="months")
singleIndexPrices.z = zoo(singleIndexPrices.df[,-1], td)

my.panel <- function(...) {
  lines(...)
  abline(h=0)
}                              
plot(singleIndexPrices.z, main="", lwd=2, col="blue")
#
# 3. create returns
#
si.z = Return.calculate(singleIndexPrices.z, method="discrete")
si.z = si.z[-1,]
si.df = as.data.frame(si.z)
head(si.df)

# returns excluding market
ret.z = si.z[, -1]
ret.mat = as.matrix(si.df[,-1])
n.obs = nrow(ret.mat)

# plot returns over full sample
plot(ret.z, main="", plot.type="single",ylab="returns",
     cex.axis = 1.5, cex.lab = 1.5, lwd=2, col=1:4)
abline(h=0)
legend(x="bottomright", legend=colnames(ret.z),
       col=1:4, lwd=2, cex=1.5)

pairs(ret.mat, col="slateblue1", pch=16, cex=1.5)

# calculate means, volatilities, correlations
mu.hat = colMeans(ret.mat)
sd.hat = apply(ret.mat, 2, sd)
cov.hat = cov(ret.mat)
cor.hat = cor(ret.mat)

# estimated means
mu.hat
# estimated sds
sd.hat
# estimated correlations
cor.hat

# show risk return tradeoffs
plot(sd.hat, mu.hat,  ylim=c(0, 0.04), xlim=c(0, 0.20), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75)     
text(sd.hat, mu.hat, labels=names(mu.hat), pos=4, cex = cex.val)

# standard errors for mu.hat and sd.hat
se.mu.hat = sd.hat/sqrt(n.obs)
se.sd.hat = sd.hat/sqrt(2*n.obs)

# estimated means
rbind(mu.hat, se.mu.hat)
rbind(sd.hat, se.sd.hat)


# re-sample means and sd values
n.boot = 200
mu.boot = matrix(0, n.boot, ncol(ret.mat))
sd.boot = matrix(0, n.boot, ncol(ret.mat))
colnames(mu.boot) = colnames(sd.boot) = colnames(ret.mat)

set.seed(123)
for (i in 1:n.boot) {
  boot.idx = sample(n.obs, replace=TRUE)
  ret.boot = ret.mat[boot.idx, ] 
  mu.boot[i, ] = colMeans(ret.boot)
  sd.boot[i, ] = apply(ret.boot, 2, sd) 
}

plot(sd.boot[, "sbux"], mu.boot[, "sbux"], col="black", pch=16,
     ylim=c(-0.03, 0.07), xlim=c(0, 0.20),
     ylab=expression(mu[p]),
     xlab=expression(sigma[p]), cex.lab=1.5)
points(sd.hat["sbux"], mu.hat["sbux"], pch=16, col="black", cex=2.5)
text(sd.hat["sbux"], mu.hat["sbux"], labels="SBUX", pos=4, cex = 2)
# plot boeing
points(sd.boot[, "boeing"], mu.boot[, "boeing"], col="blue", pch=16)
points(sd.hat["boeing"], mu.hat["boeing"], pch=16, col="blue", cex=2.5)
text(sd.hat["boeing"], mu.hat["boeing"], labels="BOEING", pos=4, cex = 2)
# plot nordstrom
points(sd.boot[, "nord"], mu.boot[, "nord"], col="green", pch=16)
points(sd.hat["nord"], mu.hat["nord"], pch=16, col="green", cex=2.5)
text(sd.hat["nord"], mu.hat["nord"], labels="NORD", pos=4, cex = 2)
# plot microsoft
points(sd.boot[, "msft"], mu.boot[, "msft"], col="red", pch=16)
points(sd.hat["msft"], mu.hat["msft"], pch=16, col="red", cex=2.5)
text(sd.hat["msft"], mu.hat["msft"], labels="MSFT", pos=4, cex = 2)

# show global minimum variance portfolio
gmin.port = globalMin.portfolio(mu.hat, cov.hat)
gmin.port

# show risk return tradeoffs with global min
plot(sd.hat, mu.hat,  ylim=c(0, 0.04), xlim=c(0, 0.20), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75)     
text(sd.hat, mu.hat, labels=names(mu.hat), pos=4, cex = cex.val)
points(gmin.port$sd, gmin.port$er, pch=16, cex=2.5, col="green")
text(gmin.port$sd, gmin.port$er, labels="Global Min", pos=2, cex = cex.val)

# bootstrap global min portfolio
mu.gmin.boot = matrix(0, n.boot, 1)
sd.gmin.boot = matrix(0, n.boot, 1)
w.gmin.boot = matrix(0, n.boot, 4)
colnames(mu.gmin.boot) = colnames(sd.gmin.boot) = "global.min"
colnames(w.gmin.boot) = names(mu.hat)

set.seed(123)
for (i in 1:n.boot) {
  boot.idx = sample(n.obs, replace=TRUE)
  ret.boot = ret.mat[boot.idx, ] 
  mu.boot = colMeans(ret.boot)
  cov.boot = cov(ret.boot) 
  gmin.boot = globalMin.portfolio(mu.boot, cov.boot)
  mu.gmin.boot[i, ] = gmin.boot$er
  sd.gmin.boot[i, ] = gmin.boot$sd
  w.gmin.boot[i, ] = gmin.boot$weights
}

plot(sd.hat, mu.hat,  ylim=c(-0.01, 0.04), xlim=c(0, 0.20), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75)     
abline(h=0, v=0)
text(sd.hat, mu.hat, labels=names(mu.hat), pos=4, cex = cex.val)
points(gmin.port$sd, gmin.port$er, pch=16, cex=2.5, col="black")
text(gmin.port$sd, gmin.port$er, labels="Global Min", pos=2, cex = cex.val)
# plot bootstrapped global min
points(sd.gmin.boot, mu.gmin.boot, col="green", pch=16)

# look at bootstrap distribution
par(mfrow=c(2,2))
hist(mu.gmin.boot, col="slateblue1")
qqnorm(mu.gmin.boot, col="slateblue1", pch=16)
qqline(mu.gmin.boot)

hist(sd.gmin.boot, col="slateblue1")
qqnorm(sd.gmin.boot, col="slateblue1", pch=16)
qqline(sd.gmin.boot)
par(mfrow=c(1,1))

bias.mu.gmin = mean(mu.gmin.boot) - gmin.port$er
se.mu.gmin = sd(mu.gmin.boot)
ci.mu.gmin.95 = c(gmin.port$er-2*se.mu.gmin, 
                  gmin.port$er+2*se.mu.gmin)
bias.mu.gmin
se.mu.gmin
ci.mu.gmin.95

bias.sd.gmin = mean(sd.gmin.boot) - gmin.port$sd
se.sd.gmin = sd(sd.gmin.boot)
ci.sd.gmin.95 = c(gmin.port$sd-2*se.sd.gmin, 
                  gmin.port$sd+2*se.sd.gmin)
bias.sd.gmin
se.sd.gmin
ci.sd.gmin.95

# min var portfolio weights
par(mfrow=c(2,2))
hist(w.gmin.boot[, "sbux"], main="SBUX", xlab="Weight", col="slateblue1")
hist(w.gmin.boot[, "msft"], main="MSFT", xlab="Weight", col="slateblue1")
hist(w.gmin.boot[, "nord"], main="NORD", xlab="Weight", col="slateblue1")
hist(w.gmin.boot[, "boeing"], main="BOEING", xlab="Weight", col="slateblue1")
par(mfrow=c(1,1))

# compute bias, mse and 95% CI for weights
bias.w.gmin = colMeans(w.gmin.boot) - gmin.port$weights
se.w.gmin = apply(w.gmin.boot, 2, sd)
ci.w.gmin.95 = rbind(gmin.port$weights-2*se.w.gmin, 
                     gmin.port$weights+2*se.w.gmin)
rownames(ci.w.gmin.95) = c("lower", "upper")
bias.w.gmin
se.w.gmin
ci.w.gmin.95


# sort bootstrap values by mean
tmp.w.boot = w.gmin.boot[1:20, ]
tmp.mu.boot = mu.gmin.boot[1:20, ]
tmp.sd.boot = sd.gmin.boot[1:20, ]
sort.idx = order(tmp.mu.boot)

# look at weights in stacked bar charts
chart.StackedBar(tmp.w.boot[sort.idx,], 
                 xaxis.labels=round(tmp.mu.boot[sort.idx],digits=3), 
                 xlab="Portfolio SD", ylab="Weights", cex.lab=1.5,
                 cex.axis=1.5)

# look at correlation between min var weights
cor(w.gmin.boot)
pairs(w.gmin.boot)

qqnorm(w.gmin.boot[, "sbux"], col="slateblue1", pch=16)
qqline(w.gmin.boot[, "sbux"])


#
# bootstrap efficient frontier
#

# show efficient frontier from sample estimates
ef = efficient.frontier(mu.hat, cov.hat)

# plot efficient portfolios
plot(ef$sd, ef$er, type="b", ylim=c(0, 0.04), xlim=c(0, 0.17), 
     pch=16, col="blue", cex=2, ylab=expression(mu[p]), xlab=expression(sigma[p]))

points(sd.hat, mu.hat, pch=16, cex=2, col="black")
points(gmin.port$sd, gmin.port$er, pch=16, cex=2, col="green")
text(sd.hat, mu.hat, labels=names(mu.hat), pos=4, cex=2)
text(gmin.port$sd, gmin.port$er, labels="Global min", pos=4, cex=2)

# bootstrap efficient frontier

ef.list = list()
set.seed(123)
for (i in 1:n.boot) {
  boot.idx = sample(n.obs, replace=TRUE)
  ret.boot = ret.mat[boot.idx, ] 
  mu.boot = colMeans(ret.boot)
  cov.boot = cov(ret.boot) 
  ef.boot = efficient.frontier(mu.boot, cov.boot)
  ef.list[[i]] = ef.boot
}

# plot sample efficient frontier with first 20 bootstrap 
# efficient frontiers
# plot efficient portfolios
plot(ef$sd, ef$er, type="b", ylim=c(-0.02, 0.06), xlim=c(0, 0.17), 
     pch=16, col="blue", cex=2, ylab=expression(mu[p]), xlab=expression(sigma[p]))

points(sd.hat, mu.hat, pch=16, cex=2, col="black")
text(sd.hat, mu.hat, labels=names(mu.hat), pos=4, cex=2)

# plot bootstrap efficient frontiers
for (i in 1:20) {
  points(ef.list[[i]]$sd, ef.list[[i]]$er, type="b",
         pch=16, col=i, cex=1.5)
}



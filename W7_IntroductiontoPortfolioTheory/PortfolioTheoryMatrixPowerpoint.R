### Walk through of Portfolio Theory Matrix Powerpoint
# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

# 3 Asset Example Data
asset.names <- c("MSFT", "NORD", "SBUX")
mu.vec = c(0.0427, 0.0015, 0.0285)
names(mu.vec) = asset.names
sigma.mat = matrix(c(0.0100, 0.0018,   0.0011,
                        0.0018, 0.0109, 0.0026,
                        0.0011, 0.0026, 0.0199),
                      nrow=3, ncol=3)
dimnames(sigma.mat) = list(asset.names, asset.names)
mu.vec
sig.vec = sqrt(c(0.01, 0.0109, 0.0199))
names(sig.vec) = asset.names
sig.vec

plot(sig.vec, mu.vec, pch=16, col="blue",
     ylim=c(0, max(mu.vec) + .04), xlim=c(0, max(sig.vec) + 0.07))
text(x=sig.vec[1], y=mu.vec[1], labels="MSFT", cex=0.8, pos=4)
text(x=sig.vec[2], y=mu.vec[2], labels="NORD", cex=0.8, pos=4)
text(x=sig.vec[3], y=mu.vec[3], labels="SBUX", cex=0.8, pos=4)

# Example Portfolios: Equally Weighted
x.vec = rep(1,3)/3
names(x.vec) = asset.names
sum(x.vec)

# Compute mean, variance and std deviation
mu.p.x = crossprod(x.vec, mu.vec)
sig2.p.x = t(x.vec)%*%sigma.mat%*%x.vec
sig.p.x = sqrt(sig2.p.x)
mu.p.x
sig.p.x

# # long-short portfolio
y.vec = c(0.8, 0.4, -0.2)
names(y.vec) = asset.names
sum(y.vec)

# compute mean, variance and std deviation
mu.p.y = crossprod(y.vec, mu.vec)
sig2.p.y = t(y.vec)%*%sigma.mat%*%y.vec
sig.p.y = sqrt(sig2.p.y)
mu.p.y
sig.p.y

points(sig.p.x, mu.p.x, type="b",  col="black", pch=16)
text(x=sig.p.x, y=mu.p.x, labels="EQUAL WEIGHT",cex=0.8, pos=4)
points(sig.p.y, mu.p.y, type="b",  col="black", pch=16)
text(x=sig.p.y, y=mu.p.y, labels="LONG SHORT",  cex=0.8, pos=4)

## Covariance and Correlation of Portfolio Returns
# covariance and correlation between equally weighted
# and long-short portfolios
sig.xy = t(x.vec)%*%sigma.mat%*%y.vec
sig.xy
rho.xy = sig.xy/(sig.p.x*sig.p.y)
rho.xy

## Compute Global Minimum Variance Portfolio
# method 1 (Lagrangian): use full system matrix algebra 
top.mat = cbind(2*sigma.mat, rep(1, 3))  # equation "green" 1 in notes
bot.vec = c(rep(1, 3), 0)                # equation "green" 2 in notes
Am.mat = rbind(top.mat, bot.vec)         # Matrix A
b.vec = c(rep(0, 3), 1)                  # vector b
z.m.mat = solve(Am.mat)%*%b.vec          # vector z = A^-1 * b
m.vec = z.m.mat[1:3,1]                   # first 3 elements of vector z are the portfolio weights m = (Ma, Mb, Mc)
                                         # for the global min. variance portfolio
                                         # the fourth element is the Lagrange multiplier which we don't care about
m.vec

# Compute mean, variance and std deviation
mu.gmin = crossprod(x.vec, mu.vec)                      # mean
sig2.gmin = as.numeric(t(m.vec)%*%sigma.mat%*%m.vec)   # variance
sig.gmin = sqrt(sig2.gmin)                             # standard deviation
mu.gmin
sig2.gmin
sig.gmin
points(sig.gmin, mu.gmin, type="b",  col="red", pch=16)
text(x=sig.gmin, y=mu.gmin, labels="GLOBAL MIN",  cex=0.8, pos=2)

# method 2: direct calculation of m using matrix algebra
one.vec = rep(1, 3)
sigma.inv.mat = solve(sigma.mat)
top.mat = sigma.inv.mat%*%one.vec
bot.val = as.numeric((t(one.vec)%*%sigma.inv.mat%*%one.vec))
m.mat = top.mat/bot.val
m.mat[,1]


## Efficient Portfolio with Same Mean as MSFT
# Note that MSFT anf our portfolio have teh same mean but
# the volatilty of our portfolio is leass than that for MSFT
top.mat = cbind(2*sigma.mat, mu.vec, rep(1, 3))
mid.vec = c(mu.vec, 0, 0)
bot.vec = c(rep(1, 3), 0, 0)
Ax.mat = rbind(top.mat, mid.vec, bot.vec)
bmsft.vec = c(rep(0, 3), mu.vec["MSFT"], 1)
z.mat = solve(Ax.mat)%*%bmsft.vec
x.vec = z.mat[1:3,]
x.vec

# compute mean, variance and std deviation
mu.px = as.numeric(crossprod(x.vec, mu.vec))
mu.px  # μ MSFT = 0.0427
sig2.px = as.numeric(t(x.vec)%*%sigma.mat%*%x.vec)
sig2.px
sig.px = sqrt(sig2.px)
sig.px  # σ MSFT = 0.10
points(sig.px, mu.px, type="b",  col="green", pch=16)
text(x=sig.px, y=mu.px, labels="E1",  cex=0.8, pos=2)

## Efficient Portfolio with Same Mean as SBUX
# same methos as before
bsbux.vec = c(rep(0, 3), mu.vec["SBUX"], 1)
z.mat = solve(Ax.mat)%*%bsbux.vec
y.vec = z.mat[1:3,]
y.vec

# compute mean, variance and std deviation
mu.py = as.numeric(crossprod(y.vec, mu.vec))
sig2.py = as.numeric(t(y.vec)%*%sigma.mat%*%y.vec)
sig.py = sqrt(sig2.py)
mu.py   # μ SBUX = 0.0285
sig.py  # σSBUX = 0.1411
points(sig.py, mu.py, type="b",  col="green", pch=16)
text(x=sig.py, y=mu.py, labels="E2",  cex=0.8, pos=2)

##### 2nd Lecture om Matrices
## Find Efficient Portfolio from 2 Efficient Portfolios
# Efficient portfolio half way between efficient
# portfolio with same mean as SBUX and efficient
# portfolio with same mean as MSFT
a = 0.5
z.vec = a*x.vec + (1-a)*y.vec
z.vec

# compute mean, variance and std deviation
sigma.xy = as.numeric(t(x.vec)%*%sigma.mat%*%y.vec)
mu.pz = as.numeric(crossprod(z.vec, mu.vec))
sig2.pz = as.numeric(t(z.vec)%*%sigma.mat%*%z.vec)
sig.pz = sqrt(sig2.pz)
mu.pz
sig.pz
points(sig.pz, mu.pz, type="b",  col="green", pch=16)
text(x=sig.pz, y=mu.pz, labels="E3",  cex=0.8, pos=2)


### Compute Efficient Frontier
# Compute efficient portfolios as convex combinations
# of global min portfolio and efficient portfolio with
# same mean as MSFT
a = seq(from=1, to=-1, by=-0.1)
n.a = length(a)
z.mat = matrix(0, n.a, 3)
mu.z = rep(0, n.a)
sig2.z = rep(0, n.a)
sig.mx = t(m.vec)%*%sigma.mat%*%x.vec

for (i in 1:n.a) {
  z.mat[i, ] = a[i]*m.vec + (1-a[i])*x.vec
  mu.z[i] = a[i]*mu.gmin + (1-a[i])*mu.px
  sig2.z[i] = a[i]^2 * sig2.gmin + (1-a[i])^2 * sig2.px + 2*a[i]*(1-a[i])*sig.mx
}
points(sqrt(sig2.z), mu.z, type="b",  col="green", pch=16)

### Compute Tangency Portfolio
rf = 0.005
sigma.inv.mat = solve(sigma.mat)
one.vec = rep(1, 3)
mu.minus.rf = mu.vec - rf*one.vec
top.mat = sigma.inv.mat%*%mu.minus.rf
bot.val = as.numeric(t(one.vec)%*%top.mat)
t.vec = top.mat[,1]/bot.val
t.vec

# # compute mean, var and sd
mu.t = as.numeric(crossprod(t.vec, mu.vec))
sig2.t = as.numeric(t(t.vec)%*%sigma.mat%*%t.vec)
sig.t = sqrt(sig2.t)
mu.t
sig.t
# Plot tangency
points(sig.t, mu.t, type="b",  col="black", pch=16)
text(x=sig.t, y=mu.t, labels="Tangency",  cex=0.8, pos=4)
# Draw Tangency Portfolio line
sharpe  = (mu.t - rf) / sig.t
abline(a = rf, b = sharpe, col = "blue")

## Find Efficient Portfolio with Target Volatility
# find efficient portfolio with target volatility (risk)
# equal 0.02
x.t.02 = 0.02/sig.t
x.t.02
1-x.t.02

# shares in msft, nord and sbux
x.t.02*t.vec

# comute mean var and sd
mu.t.02 = x.t.02*mu.t + (1-x.t.02)*rf
sig.t.02 = x.t.02*sig.t
mu.t.02
sig.t.02
points(sig.t.02, mu.t.02, type="b",  col="blue", pch=16)
text(x=sig.t.02, y=mu.t.02, labels="E1",  cex=0.8, pos=4)

## Find Efficient Portfolio with Target Expected Return
# find efficient portfolio with target expected return
# equal to 0.07
x.t.07 = (0.07 - rf)/(mu.t - rf)
x.t.07
1-x.t.07

# shares in msft, nord and sbux
x.t.07*t.vec

# compute mean, var and sd
mu.t.07 = x.t.07*mu.t + (1-x.t.07)*rf
sig.t.07 = x.t.07*sig.t
mu.t.07
sig.t.07
points(sig.t.07, mu.t.07, type="b",  col="blue", pch=16)
text(x=sig.t.07, y=mu.t.07, labels="E2",  cex=0.8, pos=4)

## vaR
# Step 1: Determine efficient portfolio that has same expected return in Starbucks
mu.SBUX = 0.0285
mu.t
x.t = (mu.SBUX - rf) / (mu.t - rf)
x.f = 1- x.t
# Step 2: Compare VaR_0.5 for Starbucks & efficient portfolio based on $100K investment
sig.SBUX  = 0.1411
q_SBUX_05 = mu.SBUX + sig.SBUX* qnorm(0.05)
VaR_SBUX_05 = 100000*q_SBUX_05

# find efficient portfolio with target expected return
# equal to SBUX
sig.SBUX = 0.0285
x.t.SBUX = (sig.SBUX - rf)/(mu.t - rf)
x.t.SBUX
1-x.t.SBUX

# shares in msft, nord and sbux
x.t.SBUX*t.vec

# compute mean, var and sd
mu.t.SBUX = x.t.SBUX*mu.t + (1-x.t.SBUX)*rf
sig.t.SBUX = x.t.SBUX*sig.t
mu.t.SBUX
sig.t.SBUX
points(sig.t.SBUX, mu.t.SBUX, type="b",  col="blue", pch=16)
text(x=sig.t.SBUX, y=mu.t.SBUX, labels="Efficient SBUX",  cex=0.8, pos=2)

e_EFFICIENT_05 = mu.py + sig.t.SBUX * qnorm(0.05) 
e_EFFICIENT_05
VaR_EFFICIENT_05 = 100000*e_EFFICIENT_05
VaR_EFFICIENT_05

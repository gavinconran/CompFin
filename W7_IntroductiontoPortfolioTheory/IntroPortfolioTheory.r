# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

### Intro to Portfolio Theory Walk Through
## Portfolios of 2 Risky Assers
# Set asset return distribution parameters
mu.A = 0.175
sig.A = 0.258
sig2.A = sig.A^2
mu.B = 0.055
sig.B = 0.115
sig2.B = sig.B^2
rho.AB = -0.164
sig.AB = rho.AB*sig.A*sig.B

## Example 1: Long only
# Specify equally weighted portfolio
x.A = 0.5
x.B = 0.5

# Compute portfolio mean, variance, and sd
mu.p1 = x.A*mu.A + x.B*mu.B
sig2.p1 = x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*sig.AB
sig.p1 = sqrt(sig2.p1)
mu.p1
sig2.p1
sig.p1

## Example 2: long-short portfolio
# Specify long-short portfolio
x.A = 1.5
x.B =-0.5
# Compute portfolio mean, variance, and sd
mu.p2 = x.A*mu.A + x.B*mu.B
sig2.p2 = x.A^2 * sig2.A + x.B^2 * sig2.B + 2*x.A*x.B*sig.AB
sig.p2 = sqrt(sig2.p2)
mu.p2
sig2.p2
sig.p2

## Asset and Portfolio VaR
w0 = 100000
# Individual asset VaR
VaR.A = (mu.A + sig.A*qnorm(0.05))*w0
VaR.A
VaR.B = (mu.B + sig.B*qnorm(0.05))*w0
VaR.B

# Asset weighted VaR for equally weighted portfolio
x.A*VaR.A + x.B*VaR.B

# VaR for equally weighted portfolio
VaR.p1 = (mu.p1 + sig.p1*qnorm(0.05))*w0
VaR.p1

# VaR for non-equally weighted portfolio
VaR.p2 = (mu.p2 + sig.p2*qnorm(0.05))*w0
VaR.p2

## Create Portfolio Frontier
x.A = seq(from=-0.4, to=1.4, by=0.1)
x.B = 1 - x.A
mu.p = x.A*mu.A + x.B*mu.B
sig2.p = x.A^2 * sig2.A + x.B^2 * sig2.B 
+  2*x.A*x.B*sig.AB
sig.p = sqrt(sig2.p)

plot(sig.p, mu.p, type='b', pch=16, 
     ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)), 
     xlab=expression(sigma[p]),
     ylab=expression(mu[p]), col=c(rep("red", 6), rep("green", 13))
)
text(x=sig.A, y=mu.A, labels="Asset A", pos=4)
text(x=sig.B, y=mu.B, labels="Asset B", pos=4)

## Computing the Minimum Variance Portfolio
# minimum variance portfolio
xA.min = (sig2.B - sig.AB)/(sig2.A + sig2.B - 2*sig.AB)
xB.min = 1 - xA.min
xA.min
xB.min

# compute mean, variance and volatility
mu.p.min = xA.min*mu.A + xB.min*mu.B
sig2.p.min = xA.min^2 * sig2.A + xB.min^2 * sig2.B
  2*xA.min*xB.min*sig.AB
sig.p.min = sqrt(sig2.p.min)
mu.p.min
sig.p.min

## Portfolios of T-Bills and 1 Risky Asset
# Risk-free (T-Bill) rate
r.f = 0.03

# T-bills + asset A
x.A = seq(from=0, to=1.4, by=0.1)
mu.p.A = r.f + x.A*(mu.A - r.f)
sig.p.A = x.A*sig.A
sharpe.A = (mu.A - r.f)/sig.A
sharpe.A


# T-bills + asset B
x.B = seq(from=0, to=1.4, by=0.1)
mu.p.B = r.f + x.B*(mu.B - r.f)
sig.p.B = x.B*sig.B
sharpe.B = (mu.B - r.f)/sig.B
sharpe.B

# plot portfolios of T-Bills and assets A and B
plot(sig.p.A, mu.p.A, type="b", col="black", ylim=c(0, max(mu.p.A)),
     xlim=c(0, max(sig.p.A, sig.p.B)), pch=16,
     xlab=expression(sigma[p]), ylab=expression(mu[p]))
points(sig.p.B, mu.p.B, type="b", col="blue", pch=16)
text(x=sig.A, y=mu.A, labels="Asset A", pos=4)
text(x=sig.B, y=mu.B, labels="Asset B", pos=1)
text(x=0, y=r.f, labels=expression(r[f]), pos=2)

# Tangent Portfolio
top = (mu.A - r.f)*sig2.B - (mu.B - r.f)*sig.AB
bot = (mu.A - r.f)*sig2.B + (mu.B - r.f)*sig2.A -
  (mu.A - r.f + mu.B - r.f)*sig.AB
x.A.tan = top/bot
x.B.tan = 1 - x.A.tan
x.A.tan

x.B.tan
mu.p.tan = x.A.tan*mu.A + x.B.tan*mu.B
sig2.p.tan = x.A.tan^2 * sig2.A + x.B.tan^2 * sig2.B 
  2*x.A.tan*x.B.tan*sig.AB
sig.p.tan = sqrt(sig2.p.tan)
mu.p.tan
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           









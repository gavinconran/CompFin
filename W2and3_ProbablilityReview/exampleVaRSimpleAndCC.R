## VaR calculations for MSFT stock.
## Assume R ~ N(0.05, (0.10)^2), W0 = $10,000
## Compute 1% and5% VaR values over the next month
# Simple monthly returm
mu.R = 0.05
sd.R = 0.10
W0 = 10000
q.01.R = mu.R + sd.R*qnorm(0.01)
q.05.R = mu.R + sd.R*qnorm(0.05)
VaR.01 = abs(q.01.R*W0)
Var.05 = abs(q.05.R*W0)
VaR.01
Var.05

# Continously Compounded monthly returm
mu.r = 0.05
sd.r = 0.10
q.01.R = exp(mu.r + sd.r*qnorm(0.01)) -1
q.05.R = exp(mu.r + sd.r*qnorm(0.05)) -1
VaR.01 = abs(q.01.R*W0)
Var.05 = abs(q.05.R*W0)
VaR.01
Var.05



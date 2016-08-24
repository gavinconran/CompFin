##Boot straping
library(UsingR)
data(father.son)
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x, n * B, replace = TRUE), B, n)
medians <- apply(resamples, 1, median)
sd(medians)

quantile(medians, c(0.025, 0.975))

hist(medians)

##Permutation test
subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"), ]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1:10000, function(i) testStat(y, sample(group)))
observedStat

mean(permutations > observedStat)

##Quiz
#q1
g1 <- c(140, 138, 150, 148, 135)
g2 <- c(132, 135, 151, 146, 130)
t.test(g1, g2, pair=TRUE)


#q2
1100 + c(-1,1) * qt(.975, 8) * (30/sqrt(9))

#q3
pbinom(2, size=4, prob=0.5, lower.tail=FALSE)

#q4
## Poisson interval - see notes
# normal approximation
x <- 1
t <- 100
lambda <- x/t
round(lambda + c(-1, 1) * qnorm(0.995) * sqrt(lambda/t), 3)

# Poisson
poisson.test(x, T = t, conf.level = 0.995)$conf

#q7
power.t.test(n = 100, delta = 0.01, sd = 0.04, type = "one.sample", alt = "one.sided")$power

#q8
power.t.test(power = 0.9, delta = 0.01, sd = 0.04, type = "one.sample", alt = "one.sided")$n


#q10
xbar = 44.02            # sample mean 
mu0 = 44             # hypothesized value 
sigma = 12            # population standard deviation 
n = 288                 # sample size 
z = (xbar−mu0)/(sigma/sqrt(n)) 
pval = 2 ∗ pnorm(z)
pval / 2


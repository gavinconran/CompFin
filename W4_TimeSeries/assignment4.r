## Part 1: Simulate data from a MA(1) model
# Consider the MA(1) model
# Yt=0.05+ϵt+θϵt−1,
# with |θ|<0 and ϵt  iid N(0,(0.1)2).
# Simulate 250 observations from the above MA(1) model with θ=0.5 and the constant 0.05. Assign the result to ma1_sim.
set.seed(123);
# Simulate 250 observations from the described MA(1) model
ma1.model = list(ma=0.5)
mu=0.05
ma1_sim <- mu + arima.sim(model=ma1.model,n=250,mean=0, sd=0.1)


##Part 2: Plot the data from the simulated MA(1) model
plot(ma1_sim,main= "MA(1) Process: mu=0.05, theta=0.5",
        xlab="time",ylab="y(t)", type="l")
abline(h=0)

##part 3: Plotting the theoretical and the sample ACF
set.seed(123);
# Simulate 250 observations from the described MA(1) model
ma1_sim <- arima.sim(model=list(ma=0.5), n=250, mean=0, sd=0.1) + 0.05;
# Generate the theoretical ACF with upto lag 10
acf_ma1_model <- ARMAacf(ma=0.5, lag.max=10)

# Split plotting window in three rows
par(mfrow=c(3,1))

# First plot: The simulated observations
plot(ma1_sim, type="l",main="MA(1) Process: mu=0.05, theta=0.5",xlab="time",ylab="y(t)")
abline(h=0)

# Second plot: Theoretical ACF
plot(1:10, acf_ma1_model[2:11], type="h", col="blue",  ylab="ACF", main="theoretical ACF")

# Third plot: Sample ACF
# Assign to tmp the Sample ACF
tmp <- acf(ma1_sim, lag.max = 10, type = "correlation", main="Sample ACF")

# Reset graphical window to only one graph
par(mfrow=c(1,1))

## Part 4: A different MA(1) model
# Now assume that θ=0.9 instead of θ=0.5. 
# Rewrite the code on the right such that it simulates data from the correct model and displays the three graphs correctly. 
set.seed(123);
# Simulate 250 observations from the described MA(1) model
ma1_sim <- arima.sim(model=list(ma=0.9), n=250, mean=0, sd=0.1) + 0.05;
# Generate the theoretical ACF with upto lag 10
acf_ma1_model <- ARMAacf(ma=0.9, lag.max=10)

# Split plotting window in three rows
par(mfrow=c(3,1))

# First plot: The simulated observations
plot(ma1_sim, type="l",main="MA(1) Process: mu=0.05, theta=0.9",xlab="time",ylab="y(t)")
abline(h=0)

# Second plot: Theoretical ACF
plot(1:10, acf_ma1_model[2:11], type="h", col="blue", main="theoretical ACF")

# Third plot: Sample ACF
tmp=acf(ma1_sim, lag.max=10, main="Sample ACF")

# Reset graphical window to only one graph
par(mfrow=c(1,1))

## part 5: An AR(1) model
set.seed(123);
# Simulate 250 observations from the described AR(1) model
ar1.model = list(ar=0.5)
mu = 0.05
# ar1_sim <- mu + arima.sim(model=ar1.model,n=250)
ar1_sim <- arima.sim(ar1.model, n=250, mean=0, sd=0.1) + mu;
# Generate the theoretical ACF with ten lags

acf_ar1_model <- ARMAacf(ar=0.5, lag.max=10)

# Split plotting window in three rows
par(mfrow=c(3,1))

# Generate the same three graphs as in the previous exercise 
par(mfrow=c(3,1))

# First plot: The simulated observations
plot(ar1_sim, type="l", main="AR(1) Process: mu=0.05, phi=0.5",xlab="time",ylab="y(t)")
abline(h=0)

# Second plot: Theoretical AFC
plot(1:10, acf_ar1_model[2:11], type="h", col="blue", main="theoretical ACF")

# Third plot: Sample AFC
tmp <- acf(ar1_sim, lag.max=10, main="Sample ACF")

# Reset plotting window to default
par(mfrow=c(1,1));






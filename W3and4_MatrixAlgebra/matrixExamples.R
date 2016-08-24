## Examples from matrixReviewPowerPoint.pdf
#matrix
matA = matrix(data=c(1,2,3,4,5,6), nrow=2, ncol = 3)
class(matA)
dim(matA)
nrow(matA)

ncol(matB)

# Vector
xvec = c(1,2,3)
class(xvec)

## Coercion vector --> matrix
xvec.mat = as.matrix(xvec)
xvec.mat

class(xvec.mat)

# Check for Symmetry
matS = matrix(c(1,2,2,1),2,2)
matS == t(matS)

# Matrix aarithemtic operators (+, -, %*% (dot product), scalar multipication, * (entry wise multiplication))
matA = matrix(c(4,9,2,1),2,2,byrow=T)
matB = matrix(c(2,0,0,7),2,2,byrow=T)
# Addition
matC = matA + matB

# Subtraction
matC = matA - matB

# Multiplication %*% --> dot product
matA = matrix(1:4, 2,2, byrow=T)
matB = matrix(5:8, 2,2,byrow=T)
matC = matA%*%matB
matC

# Scalar multiplication
2 * matA

# Entry wise multiplication
matA * matB

# IDENTITY MATRIX
# create identity matrix
matI = diag(2)
matI

matI %*% matA
matA %*%matI

# dot  product of two vectors
vecA = c(1,8)
vecB = c(2, -7)
vecA%*%vecB

# MATRIX INVERSION & Transpose
# Transpose
t(matA)
t(xvec.mat)

# Inversion
# inversion is a little more complex, partly because the function you’d want to use has a non-obvious name: solve.
# The reason that solve is called solve is that it’s a general purpose function you can use to solve matrix equations without 
# wasting time computing the full inverse, which is often inefficient. 
matA.inv = solve(matA)
matA.inv

# definition of a matrix’s inverse is that the product of the matrix and its inverse is the identity matrix, if the inverse exists
matA%*%matA.inv # returns the Identity matrix
matA.inv%*%matA # also returns the Identity matrix

## Solving systems of equations
A <- matrix(c(2, 4, -2, 4, 9, -3, -2, -3, 7), 3, 3)
b <- c(2, 8, 10)
solve(A,b)

## LU decomposition
library(matrixcalc)
A <- matrix(c(2,3,1,4,7,5,0,-2,2), ncol=3, nrow=3,byrow=T)
luA <- lu.decomposition( A )
L <- luA$L
U <- luA$U

### MATRIX FACTORISATION
## SINGULAR VALUE FACTORISATION: Singular value Decomposition (SVD)
# Eaxmple 1: data
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
library(MASS)
X <- hilbert(9)[, 1:6]

svdR <- svd(X)

U <- svdR$u
S <- diag(svdR$d)
V <- svdR$v

all.equal(U %*% S %*% t(V), X)

# Example 2: Image (tux)
library(imager)
im <- load.image('Tux.jpg')
layout(t(1:2)) 
plot(im,main="Original image")
im.bw <- grayscale(im)
plot(im.bw,main="BW Image image")

svdIM <- svd(im.bw)
d <- diag(svdIM$d)
dim(d)

u <- svdIM$u
v <- svdIM$v
layout(t(1:1)) 
plot(1:length(svdIM$d), svdIM$d)


## EIGENVALUES & EIGENVECTORS
# Eigenvectors are surely the bane of every starting student of linear algebra, 
# though their considerable power to simplify problems makes them the darling of every applied mathematician. 
# Thankfully, R makes it easy to get these for every matrix:
eig <- eigen(matA)
S <- eig$vectors
lambda <- eig$values

## QR FACTORISATION: Least Squares Example
# First, get the data
library(quantmod)
getSymbols(c("C", "GSPC"))
citi <- c(coredata(monthlyReturn(C["2010"])))
sp500 <- c(coredata(monthlyReturn(GSPC["2010"])))

# The x variable is sp500, bind a column of ones to get matrix X
X <- cbind(1, sp500)

# Compute QR factorization of X and extract the Q and R matrices
qrX <- qr(X)
Q <- qr.Q(qrX, complete = TRUE)
R <- qr.R(qrX, complete = TRUE)

# Compute u = Q T y
u <- t(Q) %*% citi

# Solve for αˆ and β ˆ
backsolve(R[1:2,1:2], u[1:2])

# Compare with built-in least squares fitting function
coef(lsfit(sp500, citi))

## OPTIMISATION
# Example 1
A <- matrix(c(2, 1, 1, 2), 2, 2)
eigen(A)$values
# Since both eigenvalues [3 1] are greater than 0 =⇒ (0, 0) a local minimum

# Example 2
A <- matrix(-c(2, 1, 1, 2), 2, 2)
eigen(A)$values
# Since both eigenvalues [-1 -3] are less than 0 =⇒ (0, 0) a local miaximum

# Example 3
A <- matrix(-c(2, 3, 3, 2), 2, 2)
eigen(A)$values
# Since one +ive and one -ive eigenvalue [5 -1] =⇒ (0, 0) a sagddle point

## Optimization using solve
# Example: Question 1 of Week 7 Problem set
# Ammended: Both ojective function and constraints are LINEAR equations
# In actual problem both the constraints are quadratic
# Not using Lagrange
library(lpSolve)
obj.fun <- c(3,-4,1,-2)
# Costraints
constr <- matrix(c(0,-1,1,1,3,0,1,2), ncol=4, byrow=TRUE)
constr.dir <- c("=","=")
rhs <- c(1, 6)
# Solve
prod.sol <- lp("min", obj.fun, constr, constr.dir, rhs, compute.sens = TRUE)
prod.sol$objval # objective function value
prod.sol$solution # decision variables value

## NON-LINEAR optimisation (Lecture 8 - Kjell)
## Implied Volatility
# R function to compute Black-Scholes call price
bsc <- function(S, T, t, K, r, s, q) {
  d1 <- (log(S/K)+(r-q+0.5*s^2)*(T-t))/(s*sqrt(T-t))
  d2 <- d1-s*sqrt(T-t)
  S*exp(-q*(T-t))*pnorm(d1)-K*exp(-r*(T-t))*pnorm(d2)
}
# Since R treats all variables as vectors
bsc(50, 0.5, 0.0, 45, 0.06, 0.2, 0.02)
bsc(50, 0.5, 0.0, 45, 0.06, c(0.15, 0.2, 0.25), 0.02)

# Suppose the option sold for $7, find σ
# Plot f(σ) over a range of values and see where it crosses the x axis
sigmas <- seq(0.05, 0.5, by = 0.01)
fsig <- bsc(50, 0.5, 0.0, 45, 0.06, sigmas, 0.02) - 7
plot(sigmas, fsig, type = "l")
# The implied volatility is σ implied = 0.25

# To compute σ implied , had to evaluate Black-Scholes formula 46 times
# Computed answer still not very precise
bsc(50, 0.5, 0.0, 45, 0.06, 0.25, 0.02) - 7

# GOAL: compute σ implied to within a pre-specified tolerance with minimum number of function evaluations
# Methods are called NON-LINEAR SOLVERS

## Method 1: Bisection Method
# Function of ONE variable
# No error checking
bisection <- function(f, a, b, tol = 0.001) {
  while(b-a > tol) {
    c <- (a+b)/2
    if(sign(f(c)) == sign(f(a)))
      a <- c
    else
      b <- c
  }
  (a+b)/2
}

# Write f (σ) as a function of one variable
fsig <- function(sigma)
  bsc(50, 0.5, 0.0, 45, 0.06, sigma, 0.02) - 7

# Use bisection to solve f (σ) = 0
bisection(fsig, 0.1, 0.3)

# Check computed solution
bsc(50, 0.5, 0.0, 45, 0.06, 0.2511719, 0.02)

## Method 2; newton's Method for n-dimensions non-linear problem
# Let g(x, y) = 1 - (x - 10^4 - (y - 1)^4)

# Gradient of g(x, y) = F(x, y)
F <- function(x,y)
  c(4*(x-1)^3, 4*(y-1)^3)

# Gradient of F(x, y)
DF <- function(x,y)
  + diag(c(12*(x-1)^2, 12*(y-1)^2))

# Starting point
x <- c(0,0)

# Do 25 Newton iterations
for (i in 1:25)
  x <- x - solve(DF(x[1], x[2]), F(x[1], x[2]))

x


## Langrange' Method + Newton's Method
# Example 1: Revisit the example from the Lagrange’s method slides
# max/min: 4x2 − 2x3
#  subject to: 2x1 − x2 − x3 = 0 AND x1^2 + x2^2 − 13 = 0

# R function for evaluating the function F
F <- function(x)
  c(-4+2*x[4]*x[1],
    6+2*x[4]*x[2],
    2*x[1]-x[2]-x[3],
    x[1]^2+x[2]^2-13)

# R function for evaluating the gradient of F
DF <- function(x)
  matrix(c(2*x[4], 0, 0, 2*x[1],
           0, 2*x[4], 0, 2*x[2], 
           2,-1, -1, 0, 
           2*x[1], 2*x[2], 0,  0),
            4, 4, byrow = TRUE)

# Starting point
x <- rep(1, 4)

#15 Newton iterations
for(i in 1:15)
  x <- x - solve(DF(x), F(x))
x

# Starting point (-1,-1,-1,-1)
x <- - rep(1, 4)

#15 Newton iterations
for(i in 1:15)
  x <- x - solve(DF(x), F(x))
x

# Lagrange + Newton's Method: Example 2
# minimize: 3x1 − 4x2 + x3 − 2x4
# subject to: −x2^2 + x3^2 + x4^2 = 1 AND 3x1^2 + x3^2 + 2x4^2 = 6

# Function to compute G(x , λ)
G <- function(x)
  c(3 + 6*x[6]*x[1], -4 - 2*x[5]*x[2],
    1 + 2*x[5]*x[3] + 2*x[6]*x[3],
    -2 + 2*x[5]*x[4] + 4*x[6]*x[4],
    -x[2]^2 + x[3]^2 + x[4]^2 - 1,
    3*x[1]^2 + x[3]^2 + 2*x[4]^2 - 6)

# Function to compute D G(x , λ)
DG <- function(x) {
  grad <- matrix(0, 6, 6)
  grad[1,] <- c(6*x[6], 0, 0, 0, 0, 6*x[1])
  grad[2,] <- c(0, -2*x[5], 0, 0, -2*x[2], 0)
  grad[3,] <- c(0, 0, 2*x[5] + 2*x[6], 0, 2*x[3], 2*x[3])
  grad[4,] <- c(0, 0, 0, 2*x[5] + 4*x[6], 2*x[4], 2*x[4])
  grad[5,] <- c(0, -2*x[2], 2*x[3], 2*x[4], 0, 0)
  grad[6,] <- c(6*x[1], 0, 2*x[3], 4*x[4], 0, 0)
  grad
}

# Starting point
x <- c(1, -1, 1, -1, 1, -1)
# Newton iterations
for(i in 1:25)
  x <- x - solve(DG(x), G(x))
# Numeric solution
x
# Does the point (x c , λ c ) correspond to a minimum or a maximum?
# Already know x c is a critical point of F (x , λ c )
# xc ∼ minimum if D2F (xc , λc ) positive definite (all matrix eigennvales > 0)
# xc ∼ maximum if D2F (xc , λc ) negative definite (all matrix eigennvales < 0)
# Already have D2F (xc , λc ), upper-left 4 × 4 block of DG(xc , λc )
round(DG(x), digits=3)
# The diagonal entries of the 4*4 matrix are the eigenvalues
# Follows that x c corresponds to a maximum

# Example 3: Maximum Expected Returns Optimization
# Vector of expected returns
mu = c(0.08, 0.10, 0.13, 0.15, 0.20)
# Asset returns covariance matrix
Sigma = matrix(c(0.019600, -0.007560, 0.012880, 0.008750, -0.009800, 
      -0.007560, 0.032400, -0.004140, -0.009000, 0.0094506,
      0.012880, -0.004140, 0.052900, 0.020125, 0.020125,
      0.008750, -0.009000,  0.020125, 0.062500, -0.013125,
      -0.009800, 0.009450, 0.020125, -0.013125, 0.122500),
      nrow=5, ncol=5)
# Target risk: σ P 2 = 0.25 2 = 0.0625
sigma2P = 0.25**2

# Function to compute G(w , λ)
G <- function(x, mu, Sigma, sigmaP2)
{
  n <- length(mu)
  c(mu + rep(x[n+1], n) + 2*x[n+2]*(Sigma %*% x[1:n]),
    sum(x[1:n]) - 1,
    t(x[1:n]) %*% Sigma %*% x[1:n] - sigmaP2)
}

# Function to compute D G(w , λ)
DG <- function(x, mu, Sigma, sigmaP2)
{
  n <- length(mu)
  grad <- matrix(0.0, n+2, n + 2)
  grad[1:n, 1:n] <- 2*x[n+2]*Sigma
  grad[1:n, n+1] <- 1
  grad[1:n, n+2] <- 2*(Sigma %*% x[1:n])
  grad[n+1, 1:n] <- 1
  grad[n+2, 1:n] <- 2*t(x[1:n]) %*% Sigma
  grad
}

# From starting point
x <- c(rep(0.5, 5), 1, 1)
x

# Newton iterations
for(i in 1:25)
  x <- x - solve(DG(x, mu, Sigma, 0.25^2),
                 G(x, mu, Sigma, 0.25^2))
x

# Recall: upper-left n × n block of D G(w , λ c ) ∼ Hessian of F (w , λ c )
DG(x, mu, Sigma, sigmaP2)[1:5, 1:5]

# Can check second order condition by computing eigenvalues
eigen(DG(x, mu, Sigma, sigmaP2)[1:5, 1:5])$values
# Eigenvalues are negative: Follows that x c corresponds to a maximum
# Computed w is a constrained maximum
t(x[1:5]) %*% mu


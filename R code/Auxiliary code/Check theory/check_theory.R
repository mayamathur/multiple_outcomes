
################## CHECK ALGEBRA IN REMARK ABOUT SUFFICIENT CONDITIONS ################## 

N = 100

##### Scenario 1: mean-centered ##### 
varX = 2

int = rep(1,N)
X1 = rnorm(N, mean=4, sd=sqrt(varX))
X2 = X1 + rnorm(N, mean=0, sd = 1)  # correlated but not collinear
#X2 = X1*3  # collinear
sig2w = 1.2^2 # residual SE

cor(X1, X2)

X = as.matrix( data.frame( int, X1, X2 ) )
Y = rnorm( n=N, mean = X1 + X2, sd = sqrt(sig2w) )  # both betas = 1

XXinv = solve( t(X) %*% X )

m = lm( Y ~ X1 + X2 )
summary(m)
sig2hat = summary(m)$sigma^2
V = vcov(m)

# see correlations implied by this covariance matrix
cov2cor(V)
# indeed, the X1-X2 coefficient estimates are highly correlated


######## Check Math ########

# check the covariance-correlation relationship
SEs = sqrt(diag(V))
cov2cor(V)[2,] * SEs[2] * SEs; V[2,]
# yes :) 

# check the first line after "With the LHS in view"
round( V, 3 ); round( XXinv * sig2w, 3 )  # should be very close but not equal because finite sample
round( V, 3 ); round( XXinv * sig2hat, 3 ) # should be identical
# yes :) 

# next line (entry of (X'X)^{-1}X')
n = 5  # arbitrary n
LHS = XXinv %*% t(X)
( LHS = LHS[2,n] )
( RHS = (1/sig2hat) * V[2,] %*% t(X)[,n] )
# yes :)

# check the line above "Re-expressing"
# we should get the correct asymptotic constant this way

get_term = function(i, j) {
  V[2,i] * V[2,j] * sum( X[,i] * X[,j] )
}

p = ncol(X)
doublesum = 0
for (i in 1:p) {
  for (j in 1:p) {
    doublesum = doublesum + get_term(i,j)
  }
}

RHS = N * (1 / sig2hat^2) * doublesum

A = solve( t(X) %*% X ) %*% t(X)
Arow = A[2,]
B = t(Arow)
SSB = sum( B^2 )
LHS = SSB * N
RHS; LHS
# yessss :) 



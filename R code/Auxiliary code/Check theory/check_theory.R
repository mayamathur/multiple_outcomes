
# Goal: See if the convergence result seems to hold for OLS. 

N = 10000

##### Scenario 1: mean-centered ##### 
# variables are mean-centered
set.seed(451)
varX = 3
X = as.matrix( data.frame( int = rep(1, N), 
                X1 = rnorm(N, mean=0, sd=sqrt(varX)) ) )
              #  X2 = rnorm(N, mean=0, sd = 10) ) )

##### Scenario 2: not mean-centered ##### 

set.seed(451)
mean = 5
varX = mean^2
X = as.matrix( data.frame( int = rep(1, N), 
                           X1 = rnorm(N, mean=mean, sd=sqrt(varX)) ) )
#  X2 = rnorm(N, mean=0, sd = 10) ) )


t(X) %*% X
S1
S2

A = solve( t(X) %*% X ) %*% t(X)
dim(A)  # should be p X N

# we're interested in the second regression coefficient (X1)
Arow = A[2,]
B = t(Arow)

# sum of squared elements of B
SSB = sum( B^2 )
SSB * N
# doesn't seem to blow up :) 



##### Check Math - Version 2 ##### 

### do I have the right expression for (X'X)?
X1 = X[,2]

S1 = sum(X1)
S2 = sum(X1^2)

# elements of matrix
S1
S2
t(X) %*% X
# yes :) 


### do I have the right expression for (X'X)^{-1}?
X1 = X[,2]

c1 = 1 / ( N * S2 - S1^2 )

# elements of matrix
c1 * S2
c1 * -S1
c1 * N
solve( t(X) %*% X )
# yes :) 


### do I have the right expression for A = (X'X)^{-1}?

A = solve( t(X) %*% X ) %*% t(X) 

# reproduce first column of A
A[1,1]; c1 * ( S2 - X1[1]*S1 )
A[2,1]; c1 * ( -S1 + X1[1]*N )
# yes :) 


### do I have the right expression for thing that's supposed to go to 0?
# RHS of first line after "the regularity condition concerns the term"
# vs. the next line

c2 = 1 / ( N * S2^2 - 2 * S2 * S1^2 + (S1^4 / N) )
#c2 = 1 / ( N * S2^2 - 2 * S2 * S1^2 + (S2 / N) )
term = sum( ( (N * X1) - S1 )^2 )

N * sum( A[2,]^2 ); c2 * term
# yes :) 
# ~~~~~ DOESN'T WORK IF NOT MEAN-CENTERED


### keep simplifying the sum term
# pull sum through
( N^2 * S2 - 2 * N * S1^2 + N*S1^2 ) * c2

# penultimate
(N^2 * S2 - N*S1^2 ) / (N * S2^2 - 2 * S2*S1^2 + (S1^4)/N )
# still matches!! :D

# last line
num = (S2/N - S1^2/(N^2) )
denom =  ( S2^2/(N^2) - 2 * (S2/N) * S1^2/(N^2) + (S1^4)/(N^4) )
num/denom
# still matches!! :D


# if mean-centered, should approach this as n -> infty
1/varX

# if not mean-centered
varX; num
varX^2; denom

### understand what happens with mean-centering

# if Var(X) = (E(X))^2, then denom goes to 0 becasue:
( E.X2 = varX + mean^2 )
E.X2^2 - 2 * E.X2 * mean^2
# this is zero







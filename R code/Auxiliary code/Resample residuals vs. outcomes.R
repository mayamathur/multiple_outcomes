
########################### REGRESSION ON ORIGINAL DATA ########################### 

# simulate from multiple regression model, comparing residual resampling to full-case resampling
# super bimodal on sex
n = 1000
male = rbinom( n=n, size=1, prob=0.5 )
X = rnorm( n, mean = 4 * male, sd = 1 )
bmale = 15
bX = -1
Y = rnorm( n, mean = bmale * male + bX * X, sd = 1 )

d = data.frame( X, Y, male )

plot(X,Y)

# original data
library(ggplot2)

colors = c("pink", "blue")
ggplot( data = d, aes( x = X, y = Y, color = as.factor(male) ) ) + geom_point(size=3) +
  scale_color_manual(values = colors) + theme_bw()

# super bimodal marginally on sex
ggplot( data = d, aes( x = Y, color = as.factor(male) ) ) + geom_density() +
  scale_color_manual(values = colors) + theme_bw()

# regression
( mo = lm( Y ~ X + male, data = d ) )
lm(Y~X, data=d)

# are the residuals normal?
# yes, as expected
hist(mo$residuals)


########################### REGRESSION ON DATA RESAMPLING JUST Y ###########################

# THIS DOES NOT WORK BECAUSE RESIDUALS ARE NO LONGER NORMAL. 
# SO WE DEFINITELY CANNOT DO THIS APPROACH. 

d$Y2 = sample( d$Y, replace = TRUE )

# this plot shows that covariates are correlated, but association between X and Y
#  key point: association between sex and Y also lost
ggplot( data = d, aes( x = X, y = Y2, color = as.factor(male) ) ) + geom_point(size=3) +
  scale_color_manual(values = colors) + theme_bw()

ggplot( data = d, aes( x = Y2, color = as.factor(male) ) ) + geom_density() +
  scale_color_manual(values = colors) + theme_bw()

# regression
( m2 = lm( Y2 ~ X + male, data = d ) )

# are the residuals normal?
# NOPE. THIS IS A PROBLEM. 
hist(m2$residuals)


########################### REGRESSION ON DATA RESAMPLING RESIDUALS (WESTFALL) ###########################

# WORKS (NORMAL RESIDUALS)

d$Yr = sample( mo$residuals, replace = TRUE )

ggplot( data = d, aes( x = X, y = Yr, color = as.factor(male) ) ) + geom_point(size=3) +
  scale_color_manual(values = colors) + theme_bw()

# now the outcomes are normal
ggplot( data = d, aes( x = Yr, color = as.factor(male) ) ) + geom_density() +
  scale_color_manual(values = colors) + theme_bw()

# regression
( m3 = lm( Yr ~ X + male, data = d ) )

# are the residuals normal?
hist(m3$residuals)


########################### REGRESSION ON DATA RESAMPLING RESIDUALS (FOX) ###########################

# WORKS (NORMAL RESIDUALS)

# resample from residuals and attach to Y-hats
d$Yf = fitted(mo) + sample( mo$residuals, replace = TRUE )

# this looks exactly like original data, as promised
ggplot( data = d, aes( x = X, y = Yf, color = as.factor(male) ) ) + geom_point(size=3) +
  scale_color_manual(values = colors) + theme_bw()

# now the outcomes are normal
ggplot( data = d, aes( x = Yf, color = as.factor(male) ) ) + geom_density() +
  scale_color_manual(values = colors) + theme_bw()

# regression
# in practice with this approach, we would test vs. H0: beta = beta.hat
( m3 = lm( Yr ~ X + male, data = d ) )

# are the residuals normal?
# YES
hist(m3$residuals)

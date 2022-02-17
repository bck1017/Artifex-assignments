# Making a Gamma sample

set.seed(1)
n = 1000 # number of observations
X = rnorm(n) # predictor
beta_0 = 2 # intercept
beta_1 = 3 # slope
y = rnorm(n , mean = beta_0 + beta_1 * X, sd = 1 )

mod = lm(y ~ X)

pander(summary(mod))

nIter = 10000
beta.vec = array(NA, dim = c(nIter, 2))

for(i in 1:nIter) {
  inds = sample(n, n, replace = T)
  y.t = y[inds]
  X.t = X[inds]
  m = lm(y.t ~ X.t)
  beta.vec[i, ] = coefficients(m)
}


print(sd(beta.vec[,2]))


set.seed(2)
ns = 1000
gam.sample = rgamma(ns, shape = 1, rate = 2)
median (gam.sample)
mean (gam.sample)
# Sample mean will likely vary more due to the change in the number of outliers.

set.seed(2)
orig.sample = rexp(1000, rate = 2)
# 3
B = 1e4
meaner = rep(0, B)
for(i in 1:B) {
  newdat.inds = sample(1000, 1000, replace = T)
  newdat = orig.sample[newdat.inds]
  meaner[i] = mean(newdat)
}

print(c(mean(meaner), sd(meaner)))
# 4
set.seed(2)
orig.sample = rexp(1000, rate = 2)

B = 1e4
medians = rep(0, B)
for(i in 1:B) {
  newdat.inds = sample(1000, 1000, replace = T)
  newdat = orig.sample[newdat.inds]
  medians[i] = median(newdat)
}

print(c(mean(medians), sd(medians)))

# Tstar
# #5
  b[i] = mean ( gam.sample [ sample ( ns, ns, replace = T )])
  
  b[i] = median ( gam.sample [ sample ( ns, ns, replace = T )])
  
  
  p1 <- hist(medians)
  p2 <- hist(meaner)
  plot( p1, col=rgb(0,0,1,1/4), xlim=c(0, .6))
  plot( p2, col=rgb(0, 1, 0, 1/4), xlim=c(0, .6), add=T)
  
  # #6
  library(openintro)
  
  plot ( loan50$total_income,loan50$loan_amount)
  model = lm(loan_amount~total_income, data=loan50)
  summary(model)
  
  library(openintro)
  
  B = 1E4
  beta.ones = rep(0, B)
  for(i in 1:B) {
    xstr = loan50[sample (1000, 1000, replace = T) ,]
    tstars = coefficients (lm (loan_amount ~ total_income, data = xstr ) ) [2]
    beta.ones[i] = tstars
  }
  
  sd(beta.ones)
  
summary( lm(loan_amount ~ total_income, data = loan50))


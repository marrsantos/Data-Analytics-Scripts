# Every distribution that R handles has four functions. 
# There is a root name, for example, the root name for the normal 
# distribution is norm. This root is prefixed by one of the letters
# 
#  p for "probability", the cumulative distribution function (c. d. f.)
#  q for "quantile", the inverse c. d. f.
#  d for "density", the density function (p. f. or p. d. f.)
#  r for "random", a random variable having the specified distribution
#
# Suppose widgit weights produced at Acme Widgit Works have weights 
# that are normally distributed with mean 17.46 grams and variance 375.67 grams. 
# What is the probability that a randomly chosen widgit weighs more then 19 grams?
1 - pnorm(19, mean=17.46, sd=sqrt(375.67))
#
# qnorm is the R function that calculates the inverse c. d. f. 
# F-1 of the normal distribution The c. d. f. and the inverse c. d. f. are related by
#   p = F(x)
#   x = F-1(p)
# So given a number p between zero and one, qnorm looks up the 
# p-th quantile of the normal distribution. As with pnorm, optional 
# arguments specify the mean and standard deviation of the distribution.
#
# Question: Suppose IQ scores are normally distributed with mean 100 and standard deviation 15. What is the 95th percentile of the distribution of IQ scores?
# Rephrased: What is F-1(0.95) when X has the N(100, 152) distribution?
qnorm(0.95, mean=100, sd=15)
# dnorm example
x <- rnorm(1000, mean=100, sd=15)
hist(x, probability=TRUE)
xx <- seq(min(x), max(x), length=100)
lines(xx, dnorm(xx, mean=100, sd=15))

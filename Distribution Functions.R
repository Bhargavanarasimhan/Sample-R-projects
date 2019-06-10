# Probability distribution had used the include
# statistical inference, simulation, and modeling

# 0. Setup

library(tidyverse)
source('hw.R')


# 1. Probability distributions
#
#    There are many families of probability
#    distributions that are commonly used
#    in statistics.
#
#    The RFE Table 17.1 row names consist of
#    of 18 family names start with the
#    starting with the normal distribution.
#
#    The table column entries are the names of
#    R distribution functions.
#    The names have prefix followed a family
#    base name.
#
#    In the first row we see function names
#    rnorm, dnorm, pnorm and qnorm.
#    The prefixes are r, d, p, and q.
#    The base name is "norm" which stands
#    for the the normal distribution.
#
#    The function rnorm generate random numbers
#         given a sample size.

#    The function dnorm computes densities
#         given a vector of quantiles.

#    The function pnorm computes the cumulative
#         probabilies give a vector of quantiles.

#    The function qnorm computes quantiles
#        given a vector of cumulative probabilities.
#
#    Reading down the table's row names we see
#    family names of discrete distributions:
#    Binomial, Poisson, Geometric, Negative Binomal,
#    Multinomial and Hypergeometric.
#
#    Note that the "density" function for discrete
#    distribution is really a probility mass function
#    so it returns probabilities and not densities.

# 1.1 Generating random samples

# We set the random number seed
# so we can reproduce our work
# so can others.

set.seed(37)
sample1 <- rnorm(5, mean=10, sd=3)
sample1
round(sample1)

set.seed(37)
sample2 <- rnorm(5, mean=10, sd=0.1)
sample2
round(sample2)

# Yes the standard deviation can make
# a big difference in the variability
# of the random numbers.

# 1.2 Produce the theoretical density plot
#     for a standard normal distribution

cumProb <- seq(.0001,.9999,length=1000)
quants <- qnorm(cumProb, mean=0, sd=1)
den <- dnorm(quants, mean=0, sd=1)

tib1 <- tibble(cumProb,quants,den)

ggplot(tib1,aes(x=quants,y=den))+
  geom_polygon(fill='cyan',color='black',size=.5) +
  labs(x='Quantiles',y='Density',
     title='Standard Normal Density  Plot') + hw

#1.3 Produce a Cumulative Probability plot
#    for a standard normal distribution.

ggplot(tib1,aes(x=quants,y=cumProb))+
  geom_line(color='blue',size=1.7) +
  labs(x='Quantiles',y='Cumulative Probability',
     title='Standard Normal Cumulative Probability Plot') + hw

# 1.4 Produce the corresponding quantile plot

ggplot(tib1,aes(x=cumProb,y=quants))+
  geom_line(color='blue',size=1.7) +
  labs(x='Cumulative  Probabilty',y='Quantiles',
     title='Standard Normal Quantile Plot') + hw

# Yes this just switches the x and y axes and their labels.

# 2. Poisson and Gamma distribution parameters
#
# Suppose number of fish caught per hour at a given location
# has Poisson distribution.  Suppose the expected number
# of fish caught per hour is 2.  Then the family parameter,
# lambda, is 2.
#
# We can sample from the distribution, compute the mean
# and see if this is close to the parameter.

set.seed(41)
samp = rpois(n=1000,lambda=2)
mean(samp)

# The sample mean is pretty close the theoretical
# distribution mean.

# We can table the counts

table(samp)

# Oh!  Some lucky people might catch 8 fish in an hour.
# Many may not catch any fish.

# The gamma distribution has two parameters:
# shape and rate (or its inverse, scale)
#
# When the value of the parameter, shape,
# is an integer the distribution is also
# called the Erlang distribution.
# This can be thought of as the
# waiting time for "shape" Poisson events to
# occur when the gamma rate parameter is the same as the
# Poisson lambda parameter.

set.seed(41)
samp <- rgamma(n=1000,shape=5,rate=2)
mean(samp)

# So, when we expect to catch 2 first per hour
# The expected waiting time to catch 5 fish
# is about 2.5 hours.

# The Poisson distribution is a reasonable
# family of choice for much phenomena, but
# not a perfect match. It has only one
# parameter that controls both the mean
# and the variance.  In many real world
# problems the mean and variance are controlled
# by different variables.
#
# The experienced fisherperson knows that the
# fish catching rate is not a constant.  Changes
# in variables such temperature, sunlight, and
# alternative food sources can make difference.
# Also the number of fish available in a fishing
# hole can be limited.

# Some statistical results, such as the Central
# Limit Theorem, hold when the assumptions are true.
#
# The sum of n independent identically distributed
# finite variance random variables converges to a
# normal distribution a n increases.

# The time to catch 50 fish is sum of 50 times
# to catch 1 fish.

cumProb <- seq(.0001,.9999,length=1000)
quants <- qgamma(cumProb,shape=50, rate=1)
den <- dgamma(quants,shape=50,rate=1)

tib2 <- tibble(cumProb,quants,den)

ggplot(tib2,aes(x=quants,y=den))+
  geom_polygon(fill='cyan',color='black',size=.5) +
  labs(x='Quantiles',y='Density',
     title='Gamma Distribution Shape Parameter 50') + hw

# The right tail looks a little long

mean(quants)
median(quants)



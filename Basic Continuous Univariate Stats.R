# Basic statistics review and R functions
# r continuous univariate data.
# 0. Setup

library(tidyverse)
source('hw.R')

set.seed(37)
sample1 <- rnorm(50,mean=10,sd=3)

# 1. Common location statistics
#
#    Location statistics are typically
#    a single number that describes position
#    of the center of a distribution
#    on the real number line.
#    The mean and median are common
#    location statistics

mean(sample1)
median(sample1)

# 2. Common scale statistics
#
#    Scale indicates how spread out
#    values are on the real line.
#    The standard deviation and variance
#    The range (min and max)
#    and the interquartile range may
#    also be consider scale statistics
#
sd(sample1)
var(sample1)
sd(sample1)**2 #Check against the variance

# The data range is defined to be the
# maximum minus minimum.
# In R the range function returns
# both the minimum and the maximum.
# We can compute the difference
# using the diff() function

xr <- range(sample1)
xr
diff(xr) # the interval length
xr[2] - xr[1] # Should be the same

# The interquartile is the difference
# between the 1st and 3rd quartile
# We talk more about quartiles later

IQR(sample1)

# 3. Missing data

# In R, NA is the missing data code for numbers.
# Many statistics function detect the missing the code
# and by default will not do the calculation. However
# they often have a keyword, na.rm that removes the
# NAs so it can do the calculations

sample2 = c(NA, sample1)
sample2
mean(sample2)
mean(sample2, na.rm=TRUE)

#  4. Quantiles and cumulative probabilities

# Theoretical probability, both discrete and
# continuous, have paired cumulative probabilities
# and quantiles. It is desirable to have
# these for data distributions. We can compute
# approximate data quantiles for specified cumulative
# probabilities.  When the probabilities are multiplied
# by 100 in the labeling we refer to the values as
# percentiles rather than quantiles.

probs <- c(0, .25, .50, .75, 1.0)
quants <- quantile(sample1,prob=probs)
quants

# We specified the probabilities as 0, .25
# .50, .75, 1.0.  The .25 quantile is 8.47917
# and also called the 25th percentile.

# In simple special cases the ascending
# cumulative probabilities are associated
# with the sorted data values (order statistics)
# The data values are quantile, and the
# associated cumulative probabilities give them
# their names.interpolation between the sorted data
# values produces the quantile.
#
# There are many algorithms for computing
# quantiles from data. They basically
# sort the data, but have different ways of
# producing the cumulative probabilities used
# as  quantile names and many have different ways
# of interpolation to produce more quantiles
# for continuous distributions.
# You can use the default in the R quantile
# function which is type 7.

# There are also more alternative names for quantiles.
# The .25 quantile is also called the 1st quartile
# The .50 quantile is also called the median
# The .75 quantile is also called the 3rd quartile
#
# The .10 quantile is also called the 1st decile
# The .01 quantile is aldo called the 1st percentile

# There is a function to compute the
# interquartile range.

iqr <- IQR(sample1)
iqr
quants[4] - quants[2]  # the calculations match.

# 5. Boxplots

# The boxplot has long been used as quick visual
# statistical summary for comparing two more
# distributions.

boxplot(sample1,col="cyan",horizontal=TRUE)

# The thick black lines is at the median. The ends
# of the cyan rectangle are the 1st and 3rd quartiles.
# The lower value fence (not show) is the 1st
# quartile minus 1.5* the interquartile range. The three
# points shown on the left are outliers.  They are outside
# the fence. The vertical line as the end of the dashed line
# on the left is the location of the lower adjacent value.
# This the smallest data value inside the fence.
#
# On the right of the box plot there are not points outside
# the higher value fence. The vertical line at the far right
# give the location of the maximum value.

# The description of boxplots are many places
# include your text visualizing Data Patterns
# With Micromaps on page 62. There are some
# variations on boxplots. For example quartiles
# may be approximated  a different way. In this
# class we use the R defaults.

# 5.1  ggplot Box Plot examples

head(ToothGrowth)
?ToothGrowth

# Unfortunately the unit of measure for tooth length
# is not provided.

Tooth <- ggplot(ToothGrowth,aes(x=supp,y=len))+
    geom_boxplot(fill='red')+
    stat_summary(fun.y='mean',geom='point',shape=23,size=3,fill='white')+
    labs(x='Orange Juice and Vitamin C',y='Tooth Length Units?',
        title='Guinea Pig Tooth Growth')+hw
Tooth

# Above the stat_summary show the mean unit a diamond shape
# More can be done such as showing confidence bounds for
# the median line

# Below, transpose facilites the reading bar and boxplot labels

Tooth+coord_flip()









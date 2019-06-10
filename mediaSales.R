# Advertizing and MSE


# 0. Setup

library(tidyverse)
source('hw.R')
library(rgl)
source('hw.R')

fixLights <- function(specular=gray(c(.3,.3,0))){
  clear3d(type = "lights")
  light3d(theta = -50,phi = 40,
    viewpoint.rel = TRUE, ambient = gray(.7),
    diffuse = gray(.7),specular = specular[1])

  light3d(theta = 50,phi = 40,
    viewpoint.rel = TRUE, ambient = gray(.7),
    diffuse = gray(.7),specular = specular[2])

  light3d(theta = 0,phi = -70,
    viewpoint.rel = TRUE, ambient = gray(.7),
    diffuse = gray(.7),specular = specular[3])
}

# 1. Read the advertizing data to make a tibble

adSales <- read_csv('Advertising.csv')
adSales

# Remove the integers counts in column 1
adSales <- adSales[,-1]
adSales

# we are going fit a linear model with
# Sales as the dependent or
# the model output variable.
#
# Radio, TV and Newspaper expenditures are
# are candidate explanatory variables or
# input variables. They may also be called
# independent or predictor variables.

# 2. Scatterplots and models y = f(x) + e where
#    y is Sales and x is one of the three
#    variables
#
#    The plots are similar to those
#    the ISLR text, Secion 2.

pTV <- ggplot(adSales,aes(x=TV,y=Sales))+
       geom_point()+
       geom_smooth(method='lm')+hw +
       labs(x='TV Budget in $1000s',
            y='Thousands of Units Sold',
            title='Advertising Results')
pTV

# The vertical spread of the points
# increases from left to right.

pRadio <- ggplot(adSales,aes(x=Radio,y=Sales))+
       geom_point()+
       geom_smooth(method='lm')+
       labs(x='Radio Budget in $1000s',
            y='Thousands of Units Sold',
            title='Advertising Results')+hw
pRadio

# Same is true here

pNews <- ggplot(adSales,aes(x=Newspaper,y=Sales))+
      geom_point()+
      geom_smooth(method='lm')
      labs(x='Newspaper Budget in $1000s',
          y='Thousands of Units Sold',
          title='Advertising Results')+hw
pNews

# Here the vertical spread points looks fairly uniform
# going from left to right.  There is only
# a few points  with large budgets so the spread

# 3. The one-at-a-time input variable plots
#    are misleading if we make a quick visual
#    assessment of the fitted line slopes for two
#    reasons.
#
#    1) The Sales are the combined
#    results from all types of advertising. The
#    plots just provide 2-D margin views of the
#    higher dimensional data structure.
#
#    2) The ranges of the x-axis scales are different
#    and these influence the angles of regression lines
#    in the  plot.  Below we make the plots using the same axis scale
#    to assess the slopes.
#
#    One way to this is to use the ggplot facet_grid option.
#    The facet-grids are controlled levels of factor, so
#    to create new tibble where the column names,
#    (TV, Radio and Newspaper) are levels of a factor. The
#    budget variables are stacked  in a single column the we
#    will "Media", and three copies of Sales column are
#    stacked single column all Sales.
#
#
#    The gather() function below makes this relatively easy.
#    Then":" in TV:Newspaper grabs all of the tibble columns
#    from TV to Newspaper and stacks them in a value column that
#    we name 'Budget'.
#    The select columns names are repeated in the key column
#    that we name 'Media'.
#    The factor_key=TRUE argument is an instruction
#    to use column names in the order specified as the
#    levels of the factor.

adSales

adSalesG <- gather(adSales,key='Media',value="Budget",
  TV:Newspaper,factor_key=TRUE)

adSalesG
levels(adSalesG$Media)


ggplot(adSalesG,aes(x=Budget,y=Sales,))+
   geom_point()+
   geom_smooth(method='lm') +
   labs(x='Budget in $1000s',
        y='Thousands of Units Sold',
        title='Advertising Results') +
    facet_grid(Media~.)  + hw

# Note the media labels on the right of the
# three panels.
#
# Now the x-axes are the same and we
# clearly see that Radio has the steepest slope.
# Why the least spent?
#
# Whick slope is larger,
# TV or Newspaper?

# For comparison proposes superposed panels
# provide an alternative juxtaposed comparision
# Below we use color aesthetic in the geosmooth
# to distinguish the Media .


ggplot(adSalesG,aes(x=Budget,y=Sales,))+
   geom_point()+
   geom_smooth(method='lm',aes(color=Media))+hw +
   labs(x='Budget in $1000s',
        y='Thousands of Units Sold',
        title='Advertising Results')+ hw

# It looks to me like the TV slope is smaller.
# Let's check.

lm(Sales~TV,data = adSales)
lm(Sales~Newspaper,data = adSales)

# Why was so much money spent of TV advertizing!
# when sale increase per $1000 is smallest.
# Below we will see why Newspaper spending
# drops out of the multiple linear regression
# model.
#


# 3. The regression input variable domain.

# The domain of the input variables
# determines the extend to which
# the model output is supported by actual
# observations. Using input values
# outside the domain to obtain estimates is
# a kind of extrapolation..
#
# Extrapolation should always be viewed as
# risky endeavor. Knowledge about the
# phenomena may suggest taking a little risk.


#3.1  Below we produce a 3D scatterplot
#     to show the variable domain

domain <- as.matrix(adSales[,-4])
open3d(FOV = 0)
fixLights()
plot3d(domain,type = "s",radius = 3.5,col=rgb(1,.2,.2),
       aspect = TRUE) # observed points in red

# The rgl plot may appear in a small
# window at a strange place on your screen,
# such along the base of the screen or behind
# the R Studio window.
#
# Left click lower right corner
# and drag to change its size.
#
# Left click on the top blue bar
# and drag to move the plot.
#
# Left click in the plot and drag
# to rotate the cube.
#
# Right click and move down or up
# to zoom in or out respectively.

# Look at the 8 corners of the plot.
# Are there are data points near the
# corners? If so that reduces extrapolation
# concerns.

# For starters we see a large empty volume
# around the high Newspaper,low Radio and
# low TV corner.  However move on to a 2D domain.
# In general other places to for large empty
# volumes in 3D  are near the faces of cube
# (or a rectangular parallelipiped).


# 3.2  A look at 2D Radio and TV Domain

ggplot(adSales,aes(x = Radio,y = TV)) +
  geom_point(shape = 21,fill = 'red',
     color = 'black',size = 2.5) +
  labs(x = 'Radio Budget in $1000s',
       y = 'TV Budget in $1000s',
       title = 'Advertising Data') + hw

# This look like reasonable domain for
#  and different locations such on regular
# grid to produce surface estimates
# There are point close to the corners and near
# the edges. There are no gaping internal
# gaps.

# 3.3 Importance of data domain

# What we can learn from the data is limited
# by the data domain.  This include different kinds
# input variables and observed combinations.
#
# One input variable at time analysis can be very
# inefficient and misleading when the phenomena
# of interest involves multiple interacting variables.
#
# The field of experimental design addresses input
# value selection when seleectis subject to experimental
# control.

# The data domain, with possible transformation,
# feeds into the linear model design  matrix. The
# matrix impacts both model coefficeint and their
# uncertainty.
#
# In the deep learning world a recurrent challenge is for
# analysts to gather data to support the learning of
# patterns.  Concerns about gathering dangers, may emerge
# as later topic

# 4.  Linear Regression

# Below  Sales is the dependent variable
# .  means all other variable are input
#    variables

adModel1 <- lm(Sales~.,data = adSales)
adModel1
summary(adModel1)

# Assuming the standardize residuals have roughly a normal distribution
# the F-statistics assessing the fitted model is highly significant.
# The tests for TV and Radio coefficients be zero provide highly significant
# to the contrary. The surprise Newspaper coefficient is basically zero.

# The correlation matrix shows that Newspaper budget substantially
# correlated (.354) with the Radio budget.
cor(adSales[,1:3])

# In the 1 variable newspaper model newspaper got some credit for high sales because it
# was high when Radio sales were high. However when  TV and Radio are in the model
# its budgut has almost np impact.


4.1 Regression diagnnosic plots

plot(adModel1)

# As indicated in the console hit Return to see the next
# plot in the set of 4 regression diagnostics plots.
#
# Plot:  Residual versus fitted values plot
# In this plot there are low outliers for points numbered
# 131, 6 and 179. We might consider deleting the points
# if we have a good reason to think that one or more of
# their value are flawed. Of course then should also
# wonder there also flawed points are that don't stand
# out as outliers.

# When the  residuals are plotted against a variable
# and the smooth looks like a parabola, including the square
# of variable values in the model  will like provide yield a better
# fitting model. Here any variable highly correlated to the
# fitted values will likely be helpful.
#
# Plot 2 is Normal Q-Q plot. We see the outliers and a thick
# left tail.  That is, points on the left are far below the
# reference line. The residuals do have an approximately
# normal distribution.  Statistical inference (
# hypothesis tests and confidence intervals) for the
# model as a whole and for the individual terms
# are not justified.

# The right tail is thin. The right-side points
# are on the center-of-the-plot side of the reference line.
# Thin tails are of less concerned in linear regression.

# Plot 3, the scale-location plot
# The y-axis is the square root of the absolute standardized residuals.
# The regression residuals have covariance matrix that is based on the
# design matrix. In general the correlations are ignorable. The variances
# are not.  Standardized residual have been divide by their estimated
# standard deviations.
#
# The absolute value transformation puts all the
# negative residuals on the positive side of the zero.
#
# The square-root transformation helps balance the
# small and large absolute residuals. The red smooth
# line should be y = 1.  The curved linear mean the error
# don't have the same variance. Our independent
# identically distributed errors assumption
# fails in terms of the mean (Plot 1) and the
# variance (Plot 3)
#
# Plot 4 Standardized Residuals versus Leverage point
#
# The high leverage points are on the far right.  The influence of a
# depends on it leverage and have far it would be from the regression
# line if it were omitted.  Point 131 and 6 are high influence points.
# The have substantial levelage and the large stamndardized residuals.

# 4.2 A TV and Radio model

adModel2 <- lm(Sales~TV+Radio, data = adSales)
summary(adModel2)
plot(adModel2)

# Dropping the term didn't change much.

# 4.3 Adding an interaction terms for TV and Radio

# In the R linear model syntax
# TV:Radio is an interaction term
# This multiples the TV and Radio vectors
# include the resulting vector in the model.
#
# TV*Radio is interpreted as TV + Radio + TV:Radio

# There are three different ways to specify the same model

adModel3 <- lm(Sales~TV*Radio, data = adSales)
summary(adModel3)

# This result is the same
adModel3a <- lm(Sales~ TV+Radio + TV:Radio, adSales)
summary(adModel3a)

# Results of direct mathmatical opertion on
# variables need to be surrounded by ()
# This result is the same
adModel3b  <- lm(Sales~TV + Radio + (TV*Radio),adSales)
summary(adModel3b)

#4.4 Adding a square term

# We can squares the TV budget vector and
# include it in the model

adModel4 <- lm(Sales~ TV*Radio+I(TV^2), adSales)
summary(adModel4)

# There is almost no variance left to explain.
# Is this data real for was in generate with
# two outliers included.


plot(adModel4)

# The two outliers are still present on the left but
# the curvature in the residuals has been reduced.

4.5 Specifying a quadratic response surface

# The polym() function can be used specify
# quadratic response surface.


adModel5 <- lm(Sales~polym(TV,Radio,degree=2),adSales)
summary(adModel5)

# 4.6  The Mean Squared error
#
# The Q-Q plot discourages us form making strong claimes terms of p-values.
# In any case we can compare  models using the MSE.

mean((adSales - fitted(adModel4))^2)
#
# Perhaps we have overfit the data.  We will soon look at 10 fold
# crossvalidation models


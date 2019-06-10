# Advertizing and MSE

# Nothing Due

# 0. Setup
library(tidyverse)
source('hw.R')
library(rgl)

fixLights <- function(specular=gray(c(.3,.3,0))){
  clear3d(type="lights")
  light3d(theta=-50,phi=40,
    viewpoint.rel=TRUE, ambient=gray(.7),
    diffuse=gray(.7),specular=specular[1])

  light3d(theta=50,phi=40,
    viewpoint.rel=TRUE, ambient=gray(.7),
    diffuse=gray(.7),specular=specular[2])

  light3d(theta=0,phi=-70,
    viewpoint.rel=TRUE, ambient=gray(.7),
    diffuse=gray(.7),specular=specular[3])
}


# 1. Read data

adSales <- read_csv('Advertising.csv')
adSales

# Remove the integers counts in column 1
adSales <- adSales[,-1]
adSales

# we are going fit a linear model with
# Sales is the dependent variable or
# the model output variable.
#
# Radio, TV and Newspaper expenditures are
# are candidate explanatory variables or
# input variables. They may also be called
# independent or predictor variables.

# 2. Scatterplots and models y=f(x)+e where
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

pRadio <- ggplot(adSales,aes(x=Radio,y=Sales))+
       geom_point()+
       geom_smooth(method='lm')+
       labs(x='Radio Budget in $1000s',
            y='Thousands of Units Sold',
            title='Advertising Results')+hw
pRadio

pNews <- ggplot(adSales,aes(x=Newspaper,y=Sales))+
      geom_point()+
      geom_smooth(method='lm')
      labs(x='Newspaper Budget in $1000s',
          y='Thousands of Units Sold',
          title='Advertising Results')+hw
pNews

# The plots are just an introducton.
# Linear models are made to fit more
# than one variable.

3.Looking at the regression domain.

# The domain of the explanatory variables
# used as input determines the extend to which
# the model output is supported by actual
# observations. Using input values
# outside the domain to obtain estimates is
# a kind of extrapolation.
#
# Extrapolation should always be view as
# risky endeavor. Knowledge about the
# phenomena may suggest little risk.
#

# Below we produce a 3D scatterplot
# to show the domain

domain <- as.matrix(adSales[,-4])
open3d(FOV=0)
fixLights()
plot3d(domain,type="s",radius=3.5,col=rgb(1,.2,.2),
       aspect=TRUE) # observed points in red

# The rgl plot window may appear in a strange place
# such as behind the R Studio window.
# It will appear in a relative small window
#
# Left click lower right corner
# and drag to change its size.
# Left click on the top blue bar
# and draw to move the plot.
# Left click in the plot and drag
# to rotate the cube.
# Right click and move down or up
# to zoom in or out respectively.

#  Note the lack of large newspaper
#  values.  We could have remember
#  the from the early plot about.
#  The cover Radio and TV values
#  look pretty got.

# A look at 2D Domain
ggplot(adSales,aes(x = Radio,y = TV)) +
  geom_point(shape = 21,fill = 'red',
     color = 'black',size = 2.5) +
  labs(x = 'Radio Budget in $1000s',
       y = 'TV Budget in $1000s',
       title = 'Advertising Data') + hw



#  You can left click the blue at the top move it. y

# Sales is the dependent variable
# .  means all other variable
#     are explanatory variables

adModel1 <- lm(Sales~.,data = adSales)
adModel1
summary(adModel1)
plot(adModel1)

# In the  residual versus fitted values there are
# low outliers on the curves.
# We might delete to points
#
# There is curvature in the model residual
# Squaring one the variables and putting it
# in the model might help fix this.
#
# The Q-Q plot indicated the residual do NOT
# have a standard normal. There is thick
# tail on the left (it far below the straight
# line.)  Statical inference for the
# model as a whole and for individual terms
# in the model is questionable.

# Nonetheless the extreme p-values fpr
# for TV and Radio are suggestive
# and the Newpaper term is so far from
# from being statisticlly significant it can
# be dropped.

adModel2 <- lm(Sales~TV+Radio, data = adSales)
summary(adModel2)
plot(adModel2)

# Dropping the term didn't change much.
#
# Below we see the syntax for and an interacton term
# betwen TV and Radio. Later we see the syntax
# for adding a quadratic TV term.
#
# In the R linear model syntax
# TV:Radio is an interaction term
# This mulltiples the TV and Radio vectors
# include the resulting vector in the model.
#
# TV*Radio is interpreted as TV + Radio + TV:Radio
adModel3 <- lm(Sales~TV*Radio, data = adSales)
summary(adModel3)

# This result is the same
adModel3a <- lm(Sales~ TV+Radio + TV:Radio, adSales)
summary(adModel3a)

# Results of direct mathmatical opertion on
# variables need to be surrounded by ()
# This results is the same
adModel3b  <- lm(Sales~TV + Radio + (TV*Radio),adSales)
summary(adModel3b)

# We can squares the TV budget vector and
# include it in the model

adModel4 <- lm(Sales~ TV*Radio+I(TV^2), adSales)
summary(adModel4)
plot(adModel4)

# The outlier are still present on the left but
# the curvature in the residuals has been reduced.

# Thepolym() function can be used specify
# quadratic response surface.
adModel5 <- lm(Sales~polym(TV,Radio,degree=2),adSales)
summary(adModel5)


ggplot(adSales,aes(x=,y=Sales))+
  geom_point()+
  geom_smooth(method='lm')+hw

ggplot(adSales,aes(x=TV,y=Sales))+
  geom_point()+
  geom_smooth(method='lm')+hw


#  Title                    ggplot2 examples

# Sections===========================================

# 0. Setup

# 1. The ggplot diamond dataset variables

# 2. Using bar height to encode factors level counts
# 2.1 Diamond color categorical variable encoded as a factor
# 2.2 Modify the previous plot

# 3. Stacked Bar Plots

# 4. Plots for continuous variables
# 4.1 Histograms
# 4.2 Kernel Density plots
# 4.3 Superposed Density Plot
# 4.4 Scatterplots and smooths
# 4.5 Juxtaposed scatterplots using facet_wrap
# 4.6 Two-way juxtaposed superposed scatterplots
#     using facet_grid
# 4.7 Restricting the x-axis scale and better labeling for axes

#5. A quick modeling example


# 0. Setup===================================

library(tidyverse)
source('hw.R')


#1. The ggplot diamond dataset variables===================

diamonds
str(diamonds)

# We see that
#   carat is continuous variable.
#   cut is an ordered factor,
#       a categorical variable with an explicit ordering
#   price is an integer variable
#      We may choose to treat it is continuous variable
#      (Counts are typically treated as discrete variables)
#
# $ carat  : num  0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...
# $ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...
# $ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
# $ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...
# $ depth  : num  61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...
# $ table  : num  55 61 65 58 58 57 57 55 61 61 ...
# $ price  : int  326 326 327 334 335 336 336 337 337 338 ...
# $ x      : num  3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
# $ y      : num  3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
# $ z      : num  2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...

# 2. Using bar height to encode factors level counts========

# 2.1 Diamond color as a factor

# Show  the  levels of the diamond color on the x-axis.
# Use  geom_bar to show the counts for the levels

ggplot(diamonds , aes( x = color) ) +
   geom_bar() +
   labs(x="Diamond Color Classes",
        y="Count",
        title="Diamond Data Set")+ hw

# Also use the diamond color factor to control
# the bar fill color.
# This example used default fill colors

ggplot(diamonds , aes( x=color, fill=color))+ geom_bar() + hw

# Make the bar outline black
# Provide  x axis, y axis and title labels
# Remove the redundant legend

ggplot(diamonds, aes(x=color, fill=color))+
geom_bar(color="black") +        # black outline
labs(x="Diamond Color Classes",
     y="Count",
     title="Diamond Data Set")+ hw +
theme(legend.position="none")

# Can you name the colors used to fill the rectangles
# for diamonds in classes E, F, G, H, and I.  Can you
# use the names to effectively communicate with others?
#
# There 11 colors that are identifed in many languages
# albeit using language specific names.  For example
# The color that the we refer as red in English has a
# name in most languages. In many circumstance there
# is merit in using colors the people can name.

# 2.2 Modify the previous script and
#     produce the plot using the
#     diamond factor cut rather
#     than the factor color
#     There are three place to make changes,
#     Don't forget the label

# 3. Stacked Bar Plots================================

# Stack bar plots are not among my favorite plot designs.

ggplot(diamonds, aes( x=color, fill=cut) )+
geom_bar(color="black") +
labs(x="Diamond Color Classes",
     y="Count",
     title="Diamond Data Set",
     fill="Cut") + hw

# Colin Ware says "In the design of color codes,
# the two primary considerationa must be visual
# distinctness to support visual search operations,
# and learnability, so that particular colors
# come 'to stand' for paricular entities.
#
# If we don't have names for the colors that
# makes the task harder. The order of appearance
# of color names in languages around the
# world is (white and black), red, (green and yellow),
# blue, brown and (pink purple orange gray).  The
# parentheses indicate approximate ties. The order
# is fixed except that the (green and yellow)
# are sometimes reversed.
#
# Ref:  Colin Ware, Information Visualization:
# Perception for Design, Third Edition
#
# What is the color name
# for "Very Good" color in the legend.
# It is easier to think "look for yellow"
# than look for greenish blue.
#
# Decoding is easier when the legend fill color order
# matches the stack bar fill color order. This recently
# became the ggplot2 default.
#
# Color linking get increaging difficulat
# when going beyond  four colors.

# The example below the shows controlling the fill colors
# using scale_fill_manual.

ggplot(diamonds,aes(x=color,fill=cut))+
  geom_bar(color=gray(.55)) +
  labs(x="Diamond Color Classes",
     y="Count",
     title="Diamond Data Set",
     fill="Cut")+
  scale_fill_manual(
     values=c("red","orange",rgb(0,.65,0),'cyan','violet'),
     na.value="grey30")+ hw

# In section 6.6.2 of the second edition
# of Hadley Wickham's book ggplot2 Elegant Graphics
# for Data Analysis he says
# "Note that one set of colours is not uniformly
# good for all purposes. Bright colours work
# well for points, but are overwhelming on bars."
# Subtle colours work well for bars but are
# hard to see on points."

# Yes, the cartograpy community has long emphasized the
# use of desaturated colors. They are "gentler" to our
# eyes.  However, when have work to do, using colors
# with familiar names has merit.
# The example above uses somewhat
# less common color names, cyan and violet.
# More familiar choice are blue and purple.

# 4. Plots for continuous variables============================

# 4.1 Histograms

ggplot(data=diamonds, aes(x=carat))+
  geom_histogram(fill="cyan",color="black") + hw

# Increase the number of x-axis bins

ggplot(data=diamonds, aes(x=carat))+
  geom_histogram(fill="cyan",color="black",bins=50) + hw

ggplot(data=diamonds, aes(x=carat))+
  geom_histogram(fill="cyan",color="black",bins=50) +
  xlim(0,3.5) +  hw

# 4.2 Kernel Density plots

ggplot(data=diamonds, aes(x=carat))+
  geom_density(fill="cyan",color="black")+ hw

# Change the kernel width

ggplot(data=diamonds, aes(x=carat))+
  geom_density(fill="cyan",color="black",adjust=2) + hw

# Change the kernal shape
# See ?density for options

ggplot(data=diamonds, aes(x=carat, color=cut))+
  geom_density(fill="cyan", color="black",
  kernel="epanechnikov", adjust=2) + hw

4.3 Superposed Density Plots

# Superposed density plots for each level
# of the factor cut, also us this
# as the density fill color
# The group argument groups data by level of cut
# so there is density plot for each level.
# Set the transparency to .2. The transparency scale
# from 0 (transparent) to 1 (no color mixing).

ggplot(data=diamonds, aes( x=carat, group=cut, fill=cut) )+
  geom_density( color="black", adjust=2, alpha= .2) +
  scale_fill_manual(
     values=c("red", "yellow", "green","cyan", "violet"),
     na.value="grey70") + hw

# The mixing of multiple colors is hard to decode.
# Note that with alpha blending one color plot on a gray and
# on a white background will have a different appearance.
# One could switch to a white plot background with gray grid lines
# so the color in plot and the legend would be the same.

# Omit some high carat outliers using xlim() for a better
# resolution view of the densities

ggplot(data=diamonds, aes(x=carat,group=cut,fill=cut))+
  geom_density(color="black",adjust=2,alpha=.2)+
  xlim(0,3.5)

#4.4 Scatterplots and smooths
#    see R For Everyone 7.7.2

# Saving the plot setup

scat <- ggplot(diamonds, aes( x=carat, y=price) )

# Show the points

scat + geom_point() + stat_smooth() + hw

# 4.5 Superposed scatterplot
#      with point color encoding diamond color

scat + geom_point( aes( color = color) ) + hw

# 4.5 Juxtaposed scatterplots using facet_wrap

scat + geom_point(aes(color=color))+ facet_wrap(~color) +hw

# 4.6 Two-way juxtaposed scatterplots
#     using facet_grid

scat + geom_point(aes(color=color))+ facet_grid(clarity~color)+hw

# 4.7  Restricting the x-axis scale and better labeling for axes

scat2 <- ggplot(diamonds, aes( x=carat, y=price/1000) )
scat2 + geom_point(aes( color = color))+
  facet_grid( clarity~cut )+
  hw + xlim(0,3.5)+
  labs(x="Carats, 9 Diamonds With Carats > 3.5 removed",
  y="Price in $1000",
  title=paste("Diamonds: Row Panels for Clarity Classes",
    "Column Panels for Cut Classes",sep="\n"),
  color="Color") + hw

#5. A quick modeling example

# The scatterplot smooths clearly show price increasing with Carats.
# Models can help bring out price modifications related to
# factor levels of Cut and Clarity.

# Separate smooths for the cut classes

ggplot(diamonds,aes(x=carat,y=price/1000,group=cut,color=cut))+
  geom_point() + stat_smooth(size=2) + hw +
 scale_color_manual(
     values=c("red", "yellow", "green","cyan", "violet"),
     na.value="grey70") + hw +
 labs(x="Carats",
   y="Price in $1000",
   title="Price to Carat Relationship Varies with Cut",
   color="Cut")+ ylim(0,20)

# The Fair cut smooth shows lower prices for the same carat
# up to around 3.2 but extends to very large carats diamonds.
# Perhaps the other cuts are problematic with larger carat
# diamonds. The down turn for the Ideal cut suggests difficulties.


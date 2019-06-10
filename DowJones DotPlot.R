# A Row-labeled dot plot


# Item Due=======================================

# Read the DowJones2012Jan28.csv file into a tibble
# and give it a new name.
#
# Modify the script for the last row labeled plot
# section 2 below:
#
# Use the new tibble, change the shape to a diamond,
# the fill color to yellow, and fix the date in the title
# to match file read into the tibble above.
#
# Put your script and the plot in the assignment paper
# and post it on blackboard.

# 2 points

# Sections===================================

# 0.  Setup
#
# 1.  Reading comma delimited files
#     to create tibbles and data frames
# 1.1 Accessing rows and columns of data frames

# 2.  Row-labeled dot plots and controlling row order

# 0. Setup =============================================

library(hexbin)
library(tidyverse)
source('hw.R')

# 1. Reading comma delimited files
#    to create tibbles and data.frames
#
# R is evolving.  We now routinely use
# tibbles in place of data.frames.
# (Some situations may call for using data.frames)
#
# A common task is to read a comma separated value
# row and column file into R.
# read_csv() will read such a file and create a tibble.
# read.csv() will read such a file and create a
#  data.frame.

DowJones <- read_csv(file="DowJones2014Jan18.csv")
is.tibble(DowJones)

# A tibble can be used in many situations where
# its structure and class membership allows this.
class(DowJones)

head(DowJones)  #the first data row has "3M Co" data
tail(DowJones)  # the last row has "Walt Disney" data

DowJonesDf1 <- read.csv(file="DowJones2014Jan18.csv")
head(DowJonesDf1)
is.data.frame(DowJonesDf1)
class(DowJonesDf1)

# Both tibbles and data.frames have column names.
# Both read_csv() and  read.csv() have a default
# argument, header=TRUE.
#    The treats the first row in the input file as
#    column names.

# A read.csv argument default converts a
# character string vector into a factor.
# Factors encode categorical variables.
# One use of a factor is to give
# categorical values a plotting order.

DowJonesDf1$Company

# Above, note that a column
# in a data.frame or tibble
# can be specified after the symbol $.
#
# We see that 3M Co is listed
# first in the factor levels.
# It will plot in position
# 1 when the factor is plotted
# on either the x or y axis.
#
# 1 will be at the bottom of
# the y-axis.

is.character(DowJonesDf1$Company)
is.factor(DowJonesDf1$Company)

# A read.csv argument can give
# a data.frame row names
#
DowJonesDf2 <- read.csv(file="DowJones2014Jan18.csv",
               row.names=1)
head(DowJonesDf2)

# 1.1 Accessing rows and columns of
#     data.frame and tibbles using
#     the square bracket notation
#
#     Tidyverse make scripts easier to read
#     by avoiding this notation.
#
#        It accesses rows using the
#        filter() function
#
#        It access column using the
#        select() function

# The square bracket notation, [ , ],
# supports accessing portions of a data frame
# by row and column selection.
#
# A row selection vector goes on the
#   left of the comma
# A column selection vector goes on the
#   right of the comma

# Selection vectors can be row and column
# names, numbers or logical values.
#
# Exception:  names can only used
# for row selection in data.frames
# with row.names and data.frame
# row.names are required to be unique.
#
# Omit the row or column selection vector if all
# rows or columns are to be select.
# The comma in the brackets is still required.

# Selection by name
DowJonesDf2["Boeing", ]   # selection vector with 1 item
DowJonesDf2[c("Boeing", "3M Co"), ]
DowJonesDf2[, "Weekly"]
DowJonesDf2[c("Boeing", "3M Co"), "Weekly" ]

# Remove unneeded objects from the workspace

rm(DowJonesDf1,DowJonesDf2)

# Subscripts can also be
# of type integer or logical.
# A vector of all negative integers
# can be used to omit rows or columns.

rowSubs <- c(1, 2, 4)
colSubs <- c(2, 3)
DowJones[rowSubs, colSubs]
DowJones[rowSubs, -1]

# 1.2  Logical vectors and vector replication

# Logical vector lengths need to match
# the number of rows or columns. A
# vector is replicated if it is too short.
# R complains if a fractional replication
# is required to match the number of rows
# or columns.

# The example below select the rows with
# even numbers. The logical vector is of
# length 2 and there are 30 rows so there
# are 15 replicates. If there were 31
# rows there would be a warning message.

DowJones[c(FALSE, TRUE), ]

# 2. Row-labeled dot plots and reordering rows.

# We can easily make a row-labeled dot plot by associating
# the early Yearly column with the x-axis and
# and the Company column with the y-axis.

ggplot(DowJones, aes( x=Yearly, y=Company ) )+
  geom_point(shape=21, fill="blue", size=3, color="black")+
  labs(x="One Year Percent Change",
       title="Dow Jones January 28, 2012")+hw

# When we read the company names in the row-labeled dot plot
# we may notice they are in descending alphabetical order
# from top to bottom.
#
# Alphabetic order may be good for finding a company names
# but people are often interested in the rank order of the
# companies based on a variable such as the one-year percent
# change.

# The reorder() function provides simple way
# to control the row plotting order so that
# the company with the largest percent
# change in yearly value appears at the top.
#
# The second argument of reorder() specifies tibble
# variable that we want to control the Company order
# on the y-axis.  In table terminology y-axis order is
# the row order.

ggplot(DowJones, aes(x=Yearly,  y=reorder(Company, Yearly)))+
  geom_point(shape=21, fill="blue", size=3, color="black")+
  labs(x="One Year Percent Change",
       y="Company",
       title="Dow Jones January 18, 2014")+hw

# We can use -Yearly reverse the company plotting order.

ggplot(DowJones, aes(x=Yearly, y=reorder(Company, -Yearly)))+
  geom_point(shape=21, fill="blue", size=3, color="black")+
  labs(x="One Year Percent Change",
       y="Company",
       title="Dow Jones January 18, 2014")+hw

# In plot production, sorting cases and variables
# often serves to simplify plot appearance!
# This can also bring out patterns that were
# obscure and call attention
# to deviations from the patterns.




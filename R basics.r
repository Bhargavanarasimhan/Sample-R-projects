

#1.  R Prompt, comments and assignment syntax

>    R default prompt
#    Comment on the right
<-   Assignment right to left
=    Assignment right to left

## Run: assign the number 3 to the name b
#       access it by name

b <- 3
b #   print object b

## End

#2.   Functions

#Function have names that are followed
#by parentheses. The function c()
###combines arguments to produce a vector.
Commas separate function arguments

## Run

d <- c(3, 9, 2) #commas separate arguments
d

## End

#2.1  Reading a comma delimited file,
     naming, storing and printing objects

# Function arguments have keywords that are used
# in the function.  We can assign values or R expressions.
# to the keywords.
#
# In the read.csv() example below we
# see three keywords, "file","header",
# and "as.is" being assigned values.

file = "DowJones.csv"
header = TRUE
as.is = TRUE

## Run: reading a comma delimited file

DJ <- read.csv(file="DowJones2014Jan18.csv",
   as.is=TRUE,header=TRUE)
head(DJ)         # returns first six rows by default
tail(DJ, n = 4)  # last 4 rows
str(DJ)          # structure synopsis

## End

# The read.csv() function reads a comma delimited
# file and created an object called a data.frame.
# As instructed, R assigned the data.frame to the
# name DJ and stored it in the R session workspace
# where it can be accessed by name.
#
# The head() function accessed the
# data.frame called DJ and by default extracted
# with the first six rows.  Since the
# result is not operated on by another function, R
# passed the result to a suitable print function
# that displayed the result in the R console.
#
# The tail() function above includes a second argument, 4.
# R used this instead of the default of 6. The tail
# function then extracted the last four rows of the data.frame.
#
# Object names normally start with a Roman letter.
# R is case sensitive.  Digits can appear after the first
# letter.  The period . is also allowed in a name.
#
# Avoid defining pi, F and T values unless you plan to use them
# in nonstandard way and mask R's default values.
# R looks for named objects in your workspace before
# for it looks in the R folders and package folders.

## Run

pi
pi <- 13
pi

rm(pi) # remove pi from your workspace
pi     # R's version of pi is now accessible

## End
#
# The search function show the places R will search
# to find named objects.

## Run
search()
## End

# 2.2  Generating random numbers, function documentation
#      and more about function arguments

## Run: generate random numbers, access documentation

x <- rnorm(50, mean=100, sd = 5)
x
?rnorm

## End

# The function rnorm() creates a vector
# with random numbers from a normal distribution.
# The prefix r calls for random numbers.  The "norm"
# indicates the normal distribution.
# The first argument tell R we want 50 random observations.
# The next two argument specify which normal distribution,
# the one with a mean of 100 and standard deviation of 50.
# R assigned the name x to the vector and stores it in the
# session workspace.
#
# Entering the name of an workspace object retreives
# the object and passes it to the appropriate
# print function and by default the print result appears
# on the R Console.
#
# ?rnorm accesses the fuction documentation.
# The document title indicates it is about the Normal Distribution.
# There are similar documentions for other families
# of  distributions such as the t distribution.
#
# The Description section of the document
# indicates the four basics functions
# provided for all of R's continuous distributions.
# The function names, what they return and the
# type of required argument appears below
# Required function arguments do not have a default.
#
# Name    Returns                 Required argument
# dnorm:  densities                  quantiles
# pnorm:  cumulative probabilities   quantiles
# qnorm:  quantiles                  cum. probs.
# rnorm:  random numbers             sample size.
#
# Document Sections
#
# In the Usage section we see rnorm(n, mean = 0, sd = 1),
# The first argument n is the sample size and has not default. It
# much specified when the using the function.  The normal
# family of distribution has two parameters, the mean and the
# standard deviation use to specify the particular normal
# distribution.  The defaults for the mean is 0 and default for
# standard deviation (designated sd) is 1.  The default is
# the standard normal distribution.
#
# In the Argument section we see that n
# is the number of observations to produce.
#
# The See Also section often shows related functions
#
# The Example section often has instructive examples
# that clarify what function needs as input and what
# it returns. Addressing a task frequently requires
# more than one function, and some examples many address
# a task similar to ours.
#
# 2.3  Omitted key words and argument order
#
# When specifing functions, specifying keywords can
# make it easier to follow the R script, but typing
# can get burdensome. R provides partial matching
# so entering enough characters for a unique match
# to keywords will suffice.
#
# So what happen when we don't provide keywords?

# R first extract the arguments based on matching key words.
# Then it matches required arguments to remaining
# provided arguments in left to right order.
#

## Run

# arguments in order
set.seed(37)
rnorm(6, mean = -5, sd=2)

# s matches sd and m matches mean
set.seed(37)
rnorm(6, s = 2, m =-5)

# remaining arguments in order
set.seed(37)
rnorm(6, s = 2, -5)

# last two arguments out of order
set.seed(37)
rnorm(6, 2, -5) # sd=-5 does make sense

## End

# 3. Vectors, statistical functions
#    and Basic Datum Types call Modes
#
# We create vectors to use as function
# arguments for a variety of tasks. Vectors
# may contain many values. Below we
# create a vector with 100,000 numeric
# values, check its length, look at
# of the first and last values, and then
# compute some common statistics.

## Run

y <- rnorm(100000, mean=3, sd = 4)
length(y)
head(y)

mean(y)
median(y)
sd(y)
var(y)
IQR(y)  # interquartile range

## End

# Different tasks require different types of
# data. To support a host of common tasks
# R supports storing basic R datum types
# called modes in it's vectors.  A vector's
# values must all be of the same mode.
# (Later we address lists which are more
# general data structure whose elements
# can even be other data structures.)
#
# Modes
#
# numeric:
#   integer:  3L, -5L
#   floating: 2.45,  -1.8e20
# complex:    3+4i, -2i, ...
# logical:    TRUE or FALSE (T or F work if not redefined)
# character   "Henry", "Henry's", 'Sue'
#
# Note that character strings delimiters can be
# either quotes and apostrophes.  The first one used
# for a string is the delimiter.  If the other
# appears it is just another character in the string.
#
# 4. Vector construction
#
# The basic constructor for a vector is the function
# c().  The arguments can be a sequence of individual
# values, vectors, or functions that produce vectors.
# Below we use the function rep() that will repeat
# constant or vector a give number of times.

## Run

a <- c(3,  -5)
b <- c(2, 4, a, 9, a, 7)
b

d <- c("Henry", "Sarah", "Chungling")
e <- c('Jose', d, "Yongping")
e

f <- c("TRUE", "FALSE", "FALSE")
g <- c(f, rep("TRUE",6), "FALSE")
g

## End

# Sometimes we put a constant in a vector of length 1
# and give the vector a name so it willed be saved in
# our workspace.  R stores some constants such
# as pi in its workspace.
#
# 4.1  Functions automating vector construction
#
# Thoughtful people often think about repetitive tasks
# to find or create ways of saving time that can be used
# to think about more interesting things. For some people,
# automation itself much more interesting than
# repetitive typing.
#
# Thoughtfyk people have created a variety of functions
# to expedite vector creation.  Why not let R help?
#
# The function seq() is handy for generating sequences.
# The function argument and defaults are
#
# seq(from = 1, to = 1,
#     by = ((to - from)/(length.out - 1)),
#     length.out = NULL, along.with = NULL, ...)
#
# Commonly used arguments are from and to
# and either by or length.out.
#
# The sequence operator, : is also handy

## Run sequence generation

x <- seq(3, 23, length=11) # equal intervals,
x

y <- seq(3, 23, by = 4) # increments of 4
y

ytest <- seq(3, 22, 4) # values must be <= 22

v <- 33:45
v

w <- 33:26
w

## End

## Application: Drawing a circle

angle <- seq(0, 2*pi, length=100)
x <- cos(angle)
y <- sin(angle)
plot(x, y, type='n')
polygon(x, y, density=-1, col="red", border="black")

## End

# The angles above are in radians.  The
# last angle, 2*pi, is equivalent to first, 0.
# Hence first and last components of cosine
# and sine vector are them. There are 99 distinct
# pairs of points based on x and y values.
#
# Change one number in the script above to draw a
# hexagon and run the script.
#
# 4.2  R's vector replication for mathematics
#
# Mathematical calculations for a pair of vectors work
# on the corresponding elements of the vectors. This could
# make adding a value to a vector or multiplying a vector
# by a single value tedious.

## Run

x <- 1:5
x
xPlus1 <- x + rep(1,length(x))
xPlus1

xTimes2 <- x*rep(2, length(x))
xTimes2

## End

# Thoughtful developers provided a way to avoid explict
# value replication shown above.  For matched vector
# element operations like addition it replicates the shorter
# vector to be the same length as the longer vector.
# It provides a warning message if the shorter vector
# is NOT an exact multiple of the longer vector.

## Run

x <- 1:6
x + 1
2*x

y <- 1:2
x + y

y  <- 1:4

x+y

## End

## Run Polygon example with vector replication

angle <- seq(0, 2*pi, length=100)
x <- cos(angle)
y <- sin(angle)

# Set up plot scale but don't plot
# x and y values range from -1 to 1
# type='n' prohibits plottings
# axes=FALSE hides the axes
plot(x, y, type='n',axes=FALSE,
  ylab='',
  main = 'More Time to Think',
  xlab="This script could be turned into a HappyFace function")

# The happy face is centered at (0,0)
polygon(x, y,
  density=-1, col="red", border="black")

# plot the left eye: shrink circle by .1,
# center it at (-.3, .2)
polygon(.1*x-.3, .1*y+.2,
  density=-1, col=rgb(.8,1,.8),border="white")
polygon(.03*x-.3, .03*y+.2,
  density=-1, col="black")

# plot the right eye: shrink circle by .1,
# center it at (.3, .2)
polygon(.1*x+.3, .1*y+.2,
  density=-1, col=rgb(.8,1,.8),border="white")
polygon(.03*x+.3, .03*y+.2,
  density=-1, col="black")

# Add a parabolic line for a smile
xSmile <- seq(-.4 ,.4,length= 50)
ySmile <- -.4 + 1.2*xSmile^2
lines(xSmile, ySmile, lwd=3, col="white")

## End

# We can do more with the smile if we use a
# polygon instead of a line. The script below
# use the rev()function to reverse coordinates
# and c() to combine ySmile curve and ySmile2
# curve into a polygon boundary.  The two curves
# have the same values at x=-.4 and x=.4.
#
# Run: Plot Mouth

ySmile2 <- -.432 + 1.4*xSmile^2
polyX <- c(xSmile, rev(xSmile) )
polyY <- c(ySmile, rev(ySmile2) )
polygon(polyX, polyY, col="white", border="white")

## End

# 5. Operators and Precedence
#
# Does 2 * 3 + 7 = 13 or 20?
#
# R does the calculation as
# (2 * 3) + 7 = 13.
#
# Many people are used to the convention
# that multiplication takes precedence
# over addition and subtraction. Precedence
# conventions reduce the need to include a
# lot of parenthesis to dictate the order of
# operations.
#
# The list below includes more than
# you will likely need to know for
# this class. Read through the list to get
# a rough idea about the scope of
# operators. You can refer this list later
# as needed
#
# The operator precedence is top down
# and left to right in each line
#
#         indices delimiters
#         I often call indices subscripts.
#         We indices use to access part of
#         data structures.
# []      vectors and list
# [[ ]]   and individual component of a list
# [,]     matrix subscripts
# [,,,]   array subscripts (not limited to 3)
#
# ::  ::: Access variable name space
#
# $ @    Component extraction, slot extraction
#
# ** ^   exponentiation right to left
#
# - +    unary minus and plus
#
# :      sequence creation
#
# %any%  special operators
#        %*%  matrix multiplication
# 	 %o%  outer product
#        %%   remainder from division
#        (I don't about precedence among these)
#
# * /    multiplication, division
#
# + -    addition, subtraction
#
# == !=  Comparisons: equal, not equal
# < >    less than, greater than
# <= >=  less than or equal, greater than or equal
#
# !      logical negation
#
# & &&   logical "and",  short-cut "and"
# | ||   logical "or", short-cut "or"
#
# ~      formula
#
# -> ->> rightward assignment
# =      leftward assignment   Assigns the object on the right to the name on the left
# <- <<- leftward assignment  "gets" assigns the object on the right to the name on the left
#
# ?      help
#
# 6.  Vector names, indices and element access
#
# Vector elements can be given names
#
# ## Run

height<- c(60, 72, 64)
nam <- c("Sue","Jim","Ru")
names(height) <- nam

height

## End
#
# Vector elements can be accessed by
# vectors used as indices.
#
# Index types can be
#   integers indicated position
#   character string with element names
#   logical values.
#
# The index vector access the same
# element of a vector more than once.


## Run

ind <- c("Ru", "Sue")
height[ind]

subs <- c(3,2,1,3)
height[subs]

height[-3]  # negative values
            # for positions
            # omit the elements

subset <- c(TRUE, FALSE)
height[subset]

taller <- height> 62
taller    # logical vector

height[taller]

height[ height>62]

## End

# Observe that R will evaluate indexing
# arguments and make use of the result.
#
# We can replace values in vector

## Run

height
height["Jim"] <- 74
height
height[height<70] <- c(61,65)
height

## End

# 6.  Matrix construction, indices, access
#     and restructuring
#
# Vectors, matrices and data.frames are commonly used
# data structures. Later we will use listsand perhaps
# data.tables and arrays.
#
# We start with matrices that restructure a single vector
# into rows and columns. The function matrix() is the
# basic matrix constructor.
# The first argument is typically vector of values used
# to fill the matrix.  Two other arguments tell R how
# many rows and columns to make.  Normally one of the
# arguments suffices since R can calculate other argument
# based on the input vector length.  An input vector with 18
# elements and specification of 3 rows implicitly means
# there will be 6 columns.
#
# If the input vector needs to be replicated to fill in the matrix,
# then specifying both the number rows and the number of columns is
# required.  Sometimes it desirable to intialize matrix with all zeros
# or NA for not applicaable.
#
# By default R varies the row index fastest when putting
# the vector values in the matrix.  Include the argument
# byrow = TRUE in the matrix function if you want R to vary
# the column index fastest.
#
# Matrices can also be constructed by binding together
# a set of vectors as rows or columns.

## Run______________________________

dat <- rnorm(24, mean=10, sd = 2)

mat1<- matrix(dat, nrow= 6)
mat1

mat2 <- matrix(dat, ncol=4)
mat2

# Test for equality of all components
all.equal(mat1, mat2)

## End

# We can transpose a mat2 with the
# transpose function t().  As shown below,
# the resulting matrix will have six columns.
# We can also create this transposed
# matrix with matrix() by settting the
# number of columns to six and including the
# argument byrow=TRUE.

## Run

t(mat2)
mat3 <- matrix(dat, ncol=6, byrow = TRUE)
all.equal(t(mat2), mat3)

## End

# Matrices can be created by binding vectors
# or matrices together if they have compatible
# rows or column.

## Run
vec1 <- 1:3
vec2 <- 4:6
vec3 <- 7:9
vec4 <- 10:12

mat4 <- rbind(vec1, vec2, vec3, vec4)
mat4

mat5 <- cbind(vec1, vec2, vec3, vec4)
mat5

mat6 <- rbind(mat4, vec1)
rbind(mat5, vec1) # Fails, vec1 to short
rbind(mat5, c(vec1,0))

## End

# Matrix rows and columns can have names
# Row and column indices can be these names.
# The following shows one way to add
# names.

## Run

vals <- c(105, 126, 207, 63, 67,74, 3, 4, 5, 22, 20, 21)
mat <- matrix(vals, nrow=3)
rownames(mat) <- c("Mary","Jane","Troy")
colnames(mat) <- c("Weight","Height", "Family Size", "Age")
mat

## End

# Matrices can have separate row and column indices separated
# by a comma. (With a single index the matrix will be
# treated as a vector.) For row and column access
# the comma is required. This allows the omission of
# a row indices to mean select rows. The same applies to
# columns.

## Run

mat[c("Mary", "Troy"), ] # Select the 2 rows and all columns

mat[, 2:3] # Select all rows and the 2 columns
mat[, -1]  # Select all rows, omit column 1

# Select both  rows and columns
# Note that Height will now appear as first column
mat[c(1,3) , c("Height","Weight")]

## End

# A matrix with a single row or column will degenerate to
# a vector.

## Run

is.vector(mat)

mat[,2]
is.vector(mat[,2])

mat[3,]
is.vector(mat[3,])


## End

# Logical subscripts work too.

## Run

height <- mat[,"Height"]
mat[,height>70]

## End

# Just like a vector, matrix values need to be
# of the same mode
# such as numeric, character and logical.
#
# Numeric matrices can be used in matrix
# multiplication. There are functions
# to calculation statistics for each of the rows
# or each of the columns.
#
# data.frames
#
# A common data set stucture has rows for
# cases and columns for variables. The column
# are vectors and the different vectors
# can have different modes.
# Often some columns contain numbers
# others contains character strings.
# A matrix can't hold such data sets.

# R provide the data.frame structure for
# such data sets.
#
# 7. Data.frame construction, indices,
#     access and restructuring
#
# Often we create data.frames using R functions
# that read files.
#
# The Dow Jones data read in section 2.1
# was stored as an object called DJ


# What class is the object
class(DJ)

# Yes it is a data.frame

# Provide a structure synopsis
str(DJ)

# Access to rows and column
# is similar to matrices

# Access by row numbers
DJ[c(2,3,5),]

# Access by column numbers
DJ[,c(3,4)]

# For data.framees we can
# access a column using $ syntax
DJ$Company

# Assign row names
rownames(DJ) = DJ$Company

# Select rows using row names

DJ[c("Caterpillar","AmExp"),]

# Access columns by number
DJ[,c(2,3)]

# Access columns by names
DJ[,c("Weekly","Yearly")]

# Degeneration
# A row doesn't turn into vector
# even though all value are of the same type.
DJ[1,2:3]
is.vector(DJ[1,2:3])

# A single column becomes a vector
# an losses it labels
DJ[1:5,2]
is.vector()



# 8. Factors______________________________________________
#
# R sometimes produces objects in a different class than
# we expect. By default R encodes a character string vector
# as a factor reading data using function like read.csv().
# External data table will often contain categorical variables
# such as sex or race with character string values such as
# "Male" and "Female" or "White","Black","Hispanic",and "Asian",
# respectively.
#
# Section 2.1 showed using the argument as.is=TRUE
# prevent the encoding the character strings as factors.
# Here we address the factor encoding since factorsare useful in model
# specification and in controlling the plotting locations associated
# factors levels.

## Creating a factor in two ways
## A simple example with 5 cases and two levels

# a character string vector
gender <- c("Female","Male","Male","Female","Male")

# convertion to a factor with as.factor()
gender1 = as.factor(gender)
gender1
levels(gender1)
as.numeric(gender1)

# The first character string in the levels vector,
# "Female" is associated with the number 1
# The second character string in the levels vector
# "Male", is associated with the number 2.  The pattern
# continues if there for each unique character string
# in the character string vector.


# R often uses the factors numbers as plotting coordinates
# for the character strings. One way to control the
# plotting order to put the levels in the plotting order that
# we want and make a new factor. The default order is alphabetical
# some in this example "Female" appears before "Male"

# We can put "Male" first if we want.
gender2 <- factor(gender,levels=c("Male","Female"))
as.numeric(gender2)
gender2

as.character(gender1)
as.character(gender1)==as.character(gender2)

# The underlying order of the character string
# vector has remained the same. The encoding
# method has change to obtain difference numeric
# subscripts to the levels

subs <- as.numeric(gender2)
subs

levels(gender2)[subs]
as.character(gender2)

# If you noted that there were more subscripts
# character strings congratulations. This works
# and does what we want.

##End

# 9. Lists_____________________________________________
#
# A list structure can have elements of different modes.
# The elements themselves can be lists. A data.frame
# is a special case of list.
#
# (Technically vectors are lists because they include attributes
# like length, mode, and element names.)
#
## Run

x = c(1,2,3,4)
y = c(1+3i,4-2i)
z = matrix(1:25,ncol=5,byrow=T)
txt = "A variety of things"

mylist = list(www=x,y=y,matz=z,txt)
# www,y,matz,txt will be the named components
mylist

mylist$www         # The $ syntax allows components
                   # to be accessed using the component name

nam = c('www','matz')
mylist[nam]        # Using vector of component names
                   # to returns a list with the given components.

subs = c(1,2,4)    # Subscripts given the position also works
mylist[subs]

mylist[3]          # Note that the result is a list
mylist[[3]]        # A double script will return just the component

## End







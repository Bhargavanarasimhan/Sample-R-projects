# Mother at Risk from Child Birt
#
# The old Washington Post article
# motivated the graph redesign.
# The article cited
# the Population Reference Bureau
#
# The bureau has 2014 data on
# a variety of topics:
# See http://www.prb.org/

# 0. Setup

library(tidyverse)
source('hw.R')
source('hwLeft.R')
source('hwRight.R')
library(gridExtra)


# 1. Enter data as vectors

attend <- c(25,8,12,31,2,17,15,30,8,8,6,16,24,26,24,15,47,38,44,20,31)
deaths <- c(18,17,16,16,16,15,15,15,15,14,14,14,13,13,12,12,12,12,11,10,10)*100

nams <- c('Sierra Leone','Afghanistan','Bhutan',
'Guinea',
'Somalia',
'Angola',
'Chad',
'Mozambique',
'Nepal',
'Ethiopia',
'Eritrea',
'Yemen',
'Burundi',
'Rwanda',
'Mali',
'Niger',
'Senegal',
'Uganda',
'Gambia',
'Haiti',
'Nigeria')

# 2. Make a data.frame

motherDeaths = data.frame(Countries=nams,
  DeathRates=deaths, Attended=attend)


# 3. Make a scatterplot with smooth

plt <-  ggplot(motherDeaths, aes(x=Attended, y=DeathRates))+
  geom_point(shape=21,size=4,color="black",fill="red") +
  geom_smooth(method="loess",span=.90,method.args=list(degree=1),
      size=1.5,color="blue")+
  labs(x="Percent Births Attended By Trained Personnel",
       y="Maternal Deaths\n Per 100,000 Live Births",
       title="Maternal Risk") + hw
plt


# 4. Select Countries and Add labels to Points

# Below %>% pipes the motherDeaths data.frame into
# the first argument of filter(). The filter function
# selects rows of the data.frame.

ptLabs <- motherDeaths %>% filter(Countries %in%
     c('Sierra Leone','Guinea','Haiti','Nigeria'))
ptLabs

# Below we use the selected Country
# names as labels. We have nudged the labels
# upward by 45 y-axis units.
plt + geom_label(data=ptLabs,
        aes(label=Countries),
        nudge_y=45)

# 5. Make row labeled plot with juxtaposed columns for two
#    different variables

# The ggplot facet_grid and facet_wrap are often useful
# for producing multiple panel layouts. We will make
# frequent use of this latter.
#
# This script below shows an alternative way to produce
# multiple panel plots. It uses grid.arrange() from the
# gridExtra package. We save plots as named objects and
# put them the panels. We can control the number rows and
# columns of panels.  As as far as I know we cannot control
# the spacing between the panels.
#
# The micromap examples in the Carr and Pickle text were produced
# using are panelLayout functions that provided extensive control
# for base level R graphics.

# 5.1 Test the left dot plot

ggplot(motherDeaths,aes(x=Attended,
  y=reorder(Countries,-Attended)))+
  geom_point(shape=21,fill="cyan",color='black',size=3)+
  labs(x="Percent Births Attended\nBy Trained Personnel",
     y="Countries",
     title="Maternal Risk") + hw

# 5.2 Make the Left Dot Plot with the Country Names
#     Omit the title and the y axis label use the theme in hwLeft


pltLeft <- ggplot(motherDeaths,aes(x=Attended,
  y=reorder(Countries,-Attended)))+
  geom_point(shape=21,fill="cyan",color='black',size=3.5)+
  labs(y="",
     x="Percent Births\n Attended By Trained Personnel",
     title="") + hwLeft
pltLeft

# 5.3 Make the Right Dot Plots without the Country names
#     Omit the title, the


pltRight <- ggplot(motherDeaths,aes(x=DeathRates,
  y=reorder(Countries,-Attended)))+
  geom_point(shape=21,fill="red",color='black',size=3.5)+
  labs(y="",
     x="Maternal Deaths (1990-1996)\nPer 100,000 Live Births",
     title="") + hwRight

# 5.4 put the two together and add a title using grid.arrange

grid.arrange(pltLeft,pltRight,ncol=2,widths=c(2.4,2),
  top="Maternal Risk Giving Birth\n For Selected Countries")

# The large gap between the panels wastes space and increases the
# visual traversal distance.  Later we use perceptual grouping
# to reduce errors in pairing point from one panel to the next.
# Of course the previous scatterplot with a smooth is superior
# for should a functional relationship.






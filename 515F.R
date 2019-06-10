library(tidyverse)  # CSV file I/O, e.g. the read_csv function
library(ggplot2)  # Data visualization
library(caret) #predictive modeling 
library(kknn) #modeling package
library(randomForest) #modeling package
library(kernlab) #modeling package
library(corrplot)
library(data.table)
library(lubridate)
library("Hmisc")
# Any results you write to the current directory are saved as output.
autos <- fread("C:/Users/User/Desktop/515 project/autos.csv",stringsAsFactors = T)

#My extended quantile function
c_pct <- function(x){
  z = c(0.00001,0.01,0.1,0.25,0.4,0.5,0.6,0.75,.95,0.99,0.99999)
  quantile(x,probs = z,na.rm=TRUE)
}
#============================
# Cleaning Data
auto<-autos
auto$dateCrawled <- ymd_hms(auto$dateCrawled) # Format time
auto$dateCreated <- ymd_hms(auto$dateCreated)
auto$lastSeen <- ymd_hms(auto$lastSeen)
auto$nrOfPictures <- NULL #Delete useless columns
auto$seller <- NULL
auto$offerType <- NULL
#auto <- auto[price<150000&price>60] # Price between 60 and 150000 dollars
#auto <- auto[yearOfRegistration>=1863&yearOfRegistration<2017]
#auto <- auto[powerPS>0&powerPS<1100]
nom <- strsplit(as.character(auto$name),split = "_")
auto$model <- as.factor(sapply(nom,"[[",1))
# Summary
summary(auto)

# Plot Correlations
# The narrow elpipses have high correction.
# The positive slope and blue indicates
# positive correlations
au <- autos[,c(price,powerPS,kilometer,monthOfRegistration,yearOfRegistration)]
windows()
corrplot.mixed(cor(au))
# Vehicle Price
ggplot(auto,aes(price))+
  stat_density(fill="cyan",color="black")+
  scale_x_log10(labels = scales::dollar_format(suffix = "$", prefix = ""))+
  labs(title="Vehicle Price")
# PowerPS
ggplot(auto,aes(powerPS))+
  stat_density(fill="cyan",color="black")+scale_x_log10()+
  labs(title="Vehicle PowerPS",subtitle="PowerPS")
# Vehicles by month and year of registration{.tabset}
windows()
ggplot(auto[yearOfRegistration>1989,.N,by=.(monthOfRegistration,yearOfRegistration)],aes(x = monthOfRegistration,y = N,fill=N))+
  geom_bar(stat = "identity")+
  labs(title="Vehicles by month and year of registration",subtitle="Number of vehicles")+
  xlab("Month")+ylab(NULL)+facet_wrap(~yearOfRegistration)

## Vehicles by month of registration
ggplot(au[,.N,by=monthOfRegistration],aes(x = monthOfRegistration,y = N,fill=N))+
  geom_bar(stat = "identity")+
  labs(title="Vehicles by month of registration",subtitle="Number of vehicles",x="Month",y=NULL)
## Gearbox
ggplot(auto[gearbox%in%c("automatik","manuell")],aes(y = price,x=gearbox,fill=gearbox))+
  geom_boxplot()+labs(title="Vehicles by Gearbox",subtitle="Price")+
  xlab(NULL)+ylab("Price")+
  scale_y_log10(labels = scales::dollar_format(suffix = "???", prefix = ""))
#Automatic cars are more expensive than the manual

# Price by Vehicle Type and Fuel Type
ggplot(auto[!vehicleType%in%c("")&!fuelType%in%c("")],aes(y = vehicleType,x=fuelType))+
  geom_tile(aes(fill=log(price)))+
  labs(title="Price by Vehicle Type and Fuel Type")+xlab("Fuel Type")+ylab("Vehicle Type")
#=====================================================
#Convert names to lowercase for easy reference
names(autos)=tolower(names(autos))

#Investigate and Remove Variables with Little or No Variation
nzv = nearZeroVar(autos,saveMetrics = TRUE)
nzv

count(autos,nrofpictures)
count(autos,seller)
count(autos,offertype)
autos=autos[,-c(3,4,18)]
#Year of Registration - Sorting out antiques and nonsensical value
autos %>% select(yearofregistration) %>%
  filter(yearofregistration<1990|yearofregistration>2016) %>% count(.)
autos<- autos %>% 
  mutate(yearofregistration = replace(yearofregistration,which(yearofregistration<1990|yearofregistration>2016),NA))
c_pct(autos$yearofregistration)
#Sorting Out Nonsensical Pricing
autos %>% select(price) %>%
  filter(price<1000|price>50000) %>% count(.)
autos<- autos %>% 
  mutate(price = replace(price,which(price<1000|price>50000),NA))
#Post Scrub Histogram 
ggplot(autos,aes(x=autos$price))+
  geom_histogram(xlab="Price",color="black")
c_pct(autos$price)
#Remove Nonsensical PS
autos %>% select(name,fueltype,powerps) %>%
  filter(powerps>600) %>% count(.)
autos<- autos %>% 
  mutate(powerps = replace(powerps,which(powerps<50|powerps>600),NA))
c_pct(autos$powerps)
#Test ABTest for Predictive Power & Ultimately Remove Variable From Dataset
count(autos,abtest)
ggplot(autos,aes(x=abtest,y=price))+geom_boxplot(fill="grey",colour="blue")
abaov = aov(price~as.factor(abtest),data = autos)
summary(abaov)
autos = select(autos,-abtest) #abtest appears to be a random variable and little direct relationship
#Convert Categorical Variables to Factors and Generate Model Matrix
char_vars <- names(select_if(autos,is.character))
char_vars <- char_vars[c(2,3,4,5,6,7)]
autos = mutate_at(autos,char_vars,funs(as.factor(.)))
#===============================================================================
auto = sample_n(autos,1000)
mdf <- auto[,c(3,6,8,10)]
mdf<- mdf %>% na.omit()
mn = model.matrix(~0+.,data=mdf)
mn = as.tbl(as.data.frame(mn))
onDiag <- function(x, ...){
  yrng <- current.panel.limits()$ylim
  d <- density(x, na.rm=TRUE)
  d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
  panel.lines(d,col=rgb(.83,.66,1),lwd=2)
  diag.panel.splom(x, ...)
}

# Below hexbin function is to modifed have xbins= 15
# bins across each of the x-axes.
# the  trans() function power is set to the
# exponent, 0.5. This will take the square
# root of the counts be before converting the
# counts to gray level.  This privide more
# gray scale resolutin for the small counts.
#
# We omit the panel.loess for moment.

offDiag <- function(x,y,...){
  panel.grid(h=-1,v=-1,...)
  panel.hexbinplot(x,y,xbins=15,...,border=gray(.7),
                   trans=function(x)x^.5)
  #    panel.loess(x , y, ..., lwd=2,col='red')
}

splom(mn,as.matrix=TRUE,
      xlab='',main="Cars",
      pscale=0, varname.cex=0.8,axis.text.cex=0.6,
      axis.text.col="purple",axis.text.font=2,
      axis.line.tck=.5,
      panel=offDiag,
      diag.panel = onDiag
)
#========================================================================
mdf <- autos[,c(3:7,9,11:13)]
mdf<- mdf %>% na.omit() #remove observations with missing values
summary(mdf)
mdf_s = sample_n(mdf,1000,replace = TRUE)
mm = model.matrix(~0+.,data=mdf_s)
mm = as.tbl(as.data.frame(mm))
#Partition the Dataset
#First train/test sets 
set.seed(12345)
intrain <- createDataPartition(mdf_s$price,p=0.70,list=FALSE)
train = slice(mdf_s,intrain) 
test = slice(mdf_s,-intrain)

label = train$price
predictors = as.data.frame(select(train,-1))
label_t = test$price
predictors_t = as.data.frame(select(test,-1))


#Second train/test expanded factor variables
set.seed(12345)
intrain_mm <- createDataPartition(mm$price,p=0.70,list=FALSE)
train_mm = slice(mm,intrain_mm) 
test_mm = slice(mm,-intrain_mm)
#Random Forest  #Predictor variable importance and selection
set.seed(12345)
trctrl = trainControl(method="repeatedcv",number = 5,repeats = 2)
mtry=1:3
tunegrid = expand.grid(.mtry=mtry)
rf_mod_1 = randomForest(x = predictors, y=label,
                        importance=TRUE, proximity=FALSE, ntree=500, keepForest=FALSE)
print(rf_mod_1)
varImpPlot(rf_mod_1)
#===========================================================================
# 3.4  Regression diagnostic plots
## 2.3 Extracting estimates
#     coefficients, predicted values, ...
#
# The R script above saves the components of
# linear regression in the object lm.fit
# This is a list object.  We could extract
# the results using the list component
# names.  However there are extractor
# functions written for model list objects
# that are very handy.  Some
# extractor functions include
# useful computations. Examples appear
# below and in section 
linearmodel = lm(yearofregistration ~ powerps , data = predictors)
# See component names
names(linearmodel)

# Access by name
linearmodel$coefficients

# Some extractor functions

coef(linearmodel)
predict(linearmodel)
residuals(linearmodel)
rstandard(linearmodel)

inf <- influence(linearmodel)
range(inf$hat) # hat values indicate point leverage

# 2.4 Computing confidence intervals
#     based on linear regression models
#
# Compute confidence intervals for
# regression coefficients

confint(linearmodel) # 95% is the default
confint(linearmodel,level=.99)

# R will compute confidence intervals
# for the mean response at given predictor
# values.
#
# R will compute confidence intervals
# for future observations at given
# predictor values. Each new value
# also includes is own random error.
# This increases the size of the
# confidence interval.


# Change the layout to 2x2 to accommodate all plots
par(mfrow=c(2,2))
par(mar = rep(2, 4))

# Diagnostic Plots
plot(linearmodel)

#===============================================================================
#Run a Linear Model
lmfit = train(x = predictors,
              y=label,
              trControl=trctrl,
              method="lm")
lmfit
summary(lmfit)



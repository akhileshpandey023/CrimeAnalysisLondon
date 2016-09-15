#### An Adaptive Model for Crime Detection and Smart Policing ####

#### INSTALLING PACKAGES#########
install.packages("quantreg", dependencies = TRUE)
install.packages("relaimpo", dependencies = TRUE)
install.packages("boot", dependencies = TRUE)
install.packages("DAAG", dependencies = TRUE)
install.packages("bootstrap", dependencies = TRUE)
install.packages("discretization", dependencies = TRUE)
install.packages("randomForest", dependencies = TRUE)
install.packages("xgboost", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("leaps", dependencies = TRUE)
install.packages("bestglm", dependencies = TRUE)
install.packages("lars", dependencies = TRUE)
install.packages("sp", dependencies = TRUE)
install.packages("glmnet", dependencies = TRUE)

library(ggplot2)
library(car)
library(xgboost)
library(randomForest)
library(MASS)
library(quantreg)
library(plyr)
library(ggplot2)
library(glmnet)
library(relaimpo)
library(lars)
library(DAAG)
library(bootstrap)
library(leaps)
library(bestglm)
library(discretization)
library(plyr)
library(neuralnet)
library(miscTools)

################################################

#### Loading Data Sets #####
## Loading London Crime Data Set ##
Crime_London_2011_2015 <- read.csv("C:/Users/Akhilesh Pandey/Desktop/Course Material/Dissertation/Work Done/Crime Data Set/Crime_London_2011_2015.csv")

## Loading the Population Data set for the MSOA ##
Census.MSOA.2011 <- read.csv("C:/Users/Akhilesh Pandey/Desktop/Course Material/Dissertation/Work Done/Population.2011.csv")

## Loading Depriviation Index Data ##
XIMDScore <- read.csv("C:/Users/Akhilesh Pandey/Desktop/MSOA/London.IMD.Poverty.MSOA.csv")

## Extracting Data for year 2011 ##
Xcrime.2011 <- subset(Crime_London_2011_2015, Month == '2011-01' | Month == '2011-02' |Month== '2011-03'| 
                        Month == '2011-04' | Month == '2011-05' | Month == '2011-06' | 
                        Month == '2011-07'| Month == '2011-08' | Month == '2011-09' 
                      | Month == '2011-10'| Month == '2011-11' |Month == '2011-12' 
                      ,select=c(LSOA.name, Month, Crime.type))
###########################

#### Analysis of Crime Data Set #######

XCrime.London.2011.freq <- as.data.frame(table(subset(Xcrime.2011, select = c("Crime.type", "Month")))) 

XCrime.London.2011.freq$proportion <- XCrime.London.2011.freq$Freq/sum(XCrime.London.2011.freq$Freq)

XCrime.London.Month.2011.freq <- as.data.frame(table(subset(Xcrime.2011, select = 
                                                              c(Crime.type, Month))))
View(XCrime.London.2011.freq)

### Plot different Crime Frequencies throughout the year ###

ggplot(data=Crime.London.Month.2011.freq, aes(x=Crime.London.Month.2011.freq$Month, 
                                              y=Crime.London.Month.2011.freq$Freq, 
                                              group=Crime.London.Month.2011.freq$Crime.type, 
                                              colour=Crime.London.Month.2011.freq$Crime.type)) +
  geom_line() + geom_point() + 
  xlab("Month of the Year") + ylab("Crime Types") + 
  ggtitle("Crime Incidents by Month") +
  scale_colour_hue(name="Crime Types")+  theme_bw()

### Plot cyclicity of Crime Events in London ###
XCrime.London.Month.2011.cyc <- as.data.frame(table(subset(
  Crime_London_2011_2015, select = c(Month))))

## Plotting the Charts ##
plot(XCrime.London.Month.2011.cyc$Var1, XCrime.London.Month.2011.cyc$Freq)
## Adding Lines to the Chart
lines(XCrime.London.Month.2011.cyc$Var1, XCrime.London.Month.2011.cyc$Freq, type="l")

###########################################

#### Joining the Tables on MSOA #######

## Adding the feature MSOA on Crime Table
## It is created by removing the LSOA signifier at the end of LSOA Code

Xcrime.2011$MSOA.name <- substr(Xcrime.2011$LSOA.name,1,
                                nchar(as.vector(Xcrime.2011$LSOA.name))-1)
##Xcrime.2012$MSOA.name <- substr(Xcrime.2012$LSOA.name,1,nchar(as.vector(Xcrime.2012$LSOA.name))-1)

## We need only the MSOA name and the Crime Type for Our Model

requiredCols <- c("MSOA.name", "Crime.type")
Xcrime.2011 <- Xcrime.2011[requiredCols]
#Xcrime.2012 <- Xcrime.2012[requiredCols]

## This table keeps the count of Total Crime events in the LSOA throughout 2011

XTotal.2011 <- as.data.frame(table(Xcrime.2011$MSOA.name))
#XTotal.2012 <- as.data.frame(table(Xcrime.2012$MSOA.name))

## Replacing the 'Var' and 'Freq' variables created by count operation in the
## above step
names(XTotal.2011)[1] <- paste("MSOA.name")
#names(XTotal.2012)[1] <- paste("MSOA.name")
names(XTotal.2011)[2] <- paste("Total.Crime.2011")
#names(XTotal.2012)[2] <- paste("Total.Crime.2012")

## This table keeps the count of Violent Crime events in the LSOA throughout 2011
## We can change the 'Crime.type' to perform Analysis on any other Crime

XViolent.2011 <- subset(Xcrime.2011, Crime.type=='Violent crime')
XViolent.2011 <- as.data.frame(table(XViolent.2011$MSOA.name))

## Replacing the 'Var' and 'Freq' variables created by count operation in the
## above step
names(XViolent.2011)[1] <- paste("MSOA.name")
names(XViolent.2011)[2] <- paste("Total.Violent.2011")

#XViolent.2012 <- subset(Xcrime.2012, Crime.type=='Violent crime')
#XViolent.2012 <- as.data.frame(table(XViolent.2012$MSOA.name))
#names(XViolent.2012)[1] <- paste("MSOA.name")
#names(XViolent.2012)[2] <- paste("Total.Violent.2012")

## Removing Nulls- This includes the Crime events with no MSOA Names
## The rows with No Crime.type were removed in the count operation

XTotal.2011[!(XTotal.2011$MSOA.name== ""),] -> XTotal.2011
XTotal.2011[!(XTotal.2011$MSOA.name== ""),] -> XTotal.2011

#XViolent.2011[!(XViolent.2011$MSOA.name== ""),] -> XViolent.2011
#XViolent.2012[!(XViolent.2012$MSOA.name== ""),] -> XViolent.2012

## Selecting the Demographic Parameters from the Census Dataset
XPopulation.data <- Census.MSOA.2011[,c("MSOA.name", "All.Ages", "X16.29", "X30.44", "X65.",
                                        "Total.youth", "Life.Expectancy", "Unemployment.Rate",
                                        "No.qualifications", "Level.1.qualifications", 
                                        "Total.Low.Qualification",
                                        "White.percent", "Non.White.percent", 
                                        "Country.of.birth.Not.United.Kingdom.percent",
                                        "Household.spaces.with.no.usual.residents.percent",
                                        "Land.Area.Hectares", 
                                        "Population.Density","Mean.Annual.Income", 
                                        "Median.Annual..Income", 
                                        "Percent.living.in.income.deprived.households.on..benefit",
                                        "Road.casualties.2011")]


## Merging the Information From different Datasets
merge(XPopulation.data,XViolent.2011, by="MSOA.name") -> Xcrime.population.MSOA
#merge(Xcrime.population.MSOA,XViolent.2012, by="MSOA.name") -> Xcrime.population.MSOA
#merge(Xcrime.population.MSOA,XTotal.2012, by="MSOA.name") -> Xcrime.population.MSOA
merge(Xcrime.population.MSOA,XTotal.2011, by="MSOA.name") -> Xcrime.population.MSOA

#############################################

#### Plotting the Histograms ####
## Plotting histograms of Number of Crime Incidents ##

## Skewed Histogram Distribution ##
x <- Xcrime.population.MSOA$Total.Violent.2011 
h<-hist(x, breaks=10,  xlab="Number of Violent Incidents", ylab= "MSOA Count",
        main="Skewed Distribution") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, lwd=2)

## Normalised Distribution ##
x <- log(Xcrime.population.MSOA$Total.Violent.2011) 
h<-hist(x, breaks=10,  xlab="Log Number of Violent Incidents", ylab= "MSOA Count",
        main="Normal Distribution") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, lwd=2)
####

#### Preliminary Analysis of Crime-Demographics Dataset ####
##Frequency distribution curve of Number of Violent Incidents

hist(Xcrime.population.MSOA$Total.Violent.2011, xlab = "Violent Incidents", 
     main = "Violent Crime in Diff MSOAs")

#Frequency distribution curve of Log(Number of Violent Incidents)

hist(log(crime.population.MSOA$Total.Violent.2011), xlab = "Log Violent Incidents", 
     main = "Log Violent Crime Frequency")

## Calculating Pearson Amongst all the selected Variables Correlation
## This will help us to understand corelation between all the predicting variables
## as well as the independent Variable
cor(Xcrime.population.MSOA[,-1], use="complete.obs", method="pearson") -> pearson.cor
View(pearson.cor)
####

#### Splitting Data into Train and Test Data Sets#####

set.seed(1237)

#Randomly select the rows which will be included in traning set
#80% data for training and 20% for testing
split.factor <- createDataPartition(log(Xcrime.population.MSOA$Total.Violent.2011), 
                                    p = .8, list = FALSE)

#create the training set
Xtrain.crime.population.MSOA <- Xcrime.population.MSOA[split.factor,]

#create the testing set
Xtest.crime.population.MSOA <- Xcrime.population.MSOA[-split.factor,]

##########

#### Build a Regression Model with all the Variables ####
linear.regression.basic <- lm (log(Total.Violent.2011) ~ log(Total.youth)  
                               + log(Unemployment.Rate)
                               + log(Population.Density)
                               + log(X65.)
                               + log(Total.Low.Qualification)
                               + log(Road.casualties.2011 + 1)
                               + log(Country.of.birth.Not.United.Kingdom.percent)
                               + log(Non.White.percent)
                               + log(Life.Expectancy)
                               + log(Median.Annual..Income)
                               + log(Percent.living.in.income.deprived.households.on.benefit)
                               + log(Land.Area.Hectares.)
                               + log(IMD.score), data = Xtrain.crime.population.MSOA.A) 

## Evaluating the performance of the Model
summary(linear.regression.basic)
## The model gives an R squared value of 0.738 but has lot insignificant predictors

#### Calculating RMSE, MAE and MAPE for the basic linear regression #####

##Predicting Valus for test data set
predicted.linear.basic <- (predict(linear.regression.basic,Xtest.crime.population.MSOA.A)) 

##Calculating the Root Mean Square of Errors value
RMSE.linear.basic <- sqrt(mean((Xtest.crime.population.MSOA.A$Total.Violent.2011 
                                - exp(linear.regression.basic))^2))
## 53.54

## Calculating Mean Average Error
MAE.linear.basic <- mean(abs(Xtest.crime.population.MSOA.A$Total.Violent.2011 
                             - exp(linear.regression.basic)))
## 35.3

## Calculating MAPE
MAPE.linear.basic <- mean(abs((Xtest.crime.population.MSOA$Total.Violent.2011 -
                                 exp(predicted.linear.basic))/
                                Xtest.crime.population.MSOA$Total.Violent.2011))
## 0.257 

#### Parameter Selections for the Model ####

XPredicting.Variables <- c("X65.","Total.youth", "Life.Expectancy",
                           "Unemployment.Rate","Total.Low.Qualification",
                           "White.percent", "Country.of.birth.Not.United.Kingdom.percent",
                           "Land.Area.Hectares.", "Population.Density", "Median.Annual..Income", 
                           "Percent.living.in.income.deprived.households.on.benefit",
                           "Road.casualties.2011","IMD.score")


## Test 1: Using Step Regression
## Each Variable is added and removed in a STEP wise fashion
## based on their t statistics
step.reg.linear <- stepAIC(linear.regression.basic, direction="both")

## Checking the Results
step.reg.linear$anova
## Parameters  Total.Low.Qualification, Unemployment.Rate, Life.Expectancy, 
## Land.Area.Hectares. and  Non.White.Percent  removed

## New Predicting Variables Set
XPredicting.Variables <- c("Total.youth", "X65.", 
                           "Country.of.birth.Not.United.Kingdom.percent",
                           "Land.Area.Hectares.", "Population.Density", "Median.Annual..Income", 
                           "Percent.living.in.income.deprived.households.on.benefit",
                           "Road.casualties.2011","IMD.score")

## Test 2: Using Regsubsets
## Regsubsets Finds the best subset to get the best result for some measurement criteria
## This is an automated selection process
## Parameters of this method:- 
## 1. method= "exhaustive" ensures all the possible combinations are explored
## 2. nbest = 6 gives the first or the best model for with 7 best predictors
## 3. nvmax = NULL for no limit on number of variables
linear.regression.basic.update1 <- regsubsets (log(Total.Violent.2011) ~ log(Total.youth)  
                                               + log(Population.Density)
                                               + log(X65.)
                                               + log(Total.Low.Qualification)
                                               + log(Road.casualties.2011 + 1)
                                               + log(Country.of.birth.Not.United.Kingdom.percent)
                                               + log(Median.Annual..Income)
                                               + log(Percent.living.in.income.deprived.households.on.benefit)
                                               + log(Land.Area.Hectares.)
                                               + log(IMD.score),
                                               data = Xcrime.population.MSOA,
                                               nbest = 1,      
                                               nvmax = 6,    
                                               force.in = NULL, force.out = NULL,
                                               method = "exhaustive")  

## Checking the result of the regression
summary(linear.regression.basic.update1) -> reg.summary.out

## We will select the parameters with the highest Coefficient of Determination
reg.summary.out$which[which.max(reg.summary.out$adjr2),]
## The parameters that were removed are:-
## Country.of.birth.Not.United.Kingdom.percent,
## Life.Expectancy, Land.Area.Hectares., "X65.", "Unemployment.Rate"


## Test 3 Variance Inflation Factor ##
XPredicting.Variables <- c("Total.youth", 
                           "Household.spaces.with.no.usual.residents.percent",
                           "Population.Density", "Median.Annual..Income", 
                           "Percent.living.in.income.deprived.households.on.benefit",
                           "Road.casualties.2011","IMD.score")

linear.regression.basic.update2 <- lm ( log(Total.Violent.2011/All.Ages) ~ log(Total.youth)
                                        + log(Road.casualties.2011 + 1)
                                        + log(Population.Density)
                                        + log(Median.Annual..Income)
                                        + log(Percent.living.in.income.deprived.households.on.benefit)
                                        + log(IMD.score),
                                        data = Xcrime.population.MSOA)

#Save the Variance Inflation Factor in a Variable
vif(linear.regression.basic.update2) > 5
#Percent.living.in.income.deprived.households.on.benefit  removed
#####

#### Reduced Linear Regression Model #####

## This is our reduced Model with all the significant predictors
Xlinear.regression.reduced <- lm (log(Total.Violent.2011) ~ log(Total.youth)
                                  + log(Population.Density)
                                  + log(Road.casualties.2011 + 1)
                                  + log(Median.Annual..Income)
                                  + log(IMD.score),
                                  data = Xtrain.crime.population.MSOA)

summary(Xlinear.regression.reduced)
## This gave a R squared value of 0.718

#### Calculating RMSE, MAE and MAPE for the Reduced Linear Regression Model#####

##Predicting Values for test data set
predicted.linear.reduced <- predict(Xlinear.regression.reduced, Xtest.crime.population.MSOA)

##Calculating the Root Mean Square of Errors value
RMSE.linear.reduced <- sqrt(mean((Xtest.crime.population.MSOA$Total.Violent.2011 
                                  - exp(predicted.linear.reduced))^2))
## 54.03

##Calculating Mean Average Error
MAE.linear.reduced <- mean(abs(Xtest.crime.population.MSOA$Total.Violent.2011 
                               - exp(predicted.linear.reduced)))
## 35.12

## Calculating MAPE
MAPE.linear.reduced <- mean(abs((Xtest.crime.population.MSOA$Total.Violent.2011 -
                                   exp(predicted.linear.reduced))/
                                  Xtest.crime.population.MSOA$Total.Violent.2011))
## 25.4
#####

#### Check for overfit #########
## Randomise the data
Xcrime.population.MSOA.cv <-Xcrime.population.MSOA[sample(nrow(Xcrime.population.MSOA)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(Xcrime.population.MSOA.cv)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test.Data.Violent.cv <- Xcrime.population.MSOA.cv[testIndexes, ]
  Xlinear.reduced.cv <- lm (log(Total.Violent.2011) ~ log(Total.youth)
                                    + log(Population.Density)
                                    + log(Road.casualties.2011 + 1)
                                    + log(Median.Annual..Income)
                                    + log(IMD.score),
                                    data = test.Data.Violent.cv)
  
  print(summary(Xlinear.reduced.cv))
}

#####################################

#### Random Forest Regression ####

Xtrain.crime.population.MSOA$l.v.t <- log(Xtrain.crime.population.MSOA$Total.Violent.2011)
Xtest.crime.population.MSOA$l.v.t <- log(Xtest.crime.population.MSOA$Total.Violent.2011)
Xcrime.population.MSOA$l.v.t <- log(Xcrime.population.MSOA$Total.Violent.2011)

Xrandom.forest.violent.reduced <- randomForest(l.v.t ~ Total.youth  
                                       + Unemployment.Rate 
                                       #+ Population.Density
                                       #+ X65.
                                       #+ Total.Low.Qualification
                                       + Road.casualties.2011 
                                       #+ Country.of.birth.Not.United.Kingdom.percent
                                       + Non.White.percent
                                       + Life.Expectancy
                                       + Median.Annual..Income
                                       + Percent.living.in.income.deprived.households.on.benefit
                                       #+ Land.Area.Hectares.
                                       + IMD.score,
                                       data = Xtrain.crime.population.MSOA,
                                       ntree=10000) 

print(Xrandom.forest.violent.reduced)

#Checking the IncNodePurity
Xrandom.forest.violent.reduced$importance

#### Calculating RMSE and MAE For Random Forest ####
## Predicting Crime Incidents on Test Set
Xpredicted.Rand.For <- predict(Xrandom.forest.violent.reduced, Xtest.crime.population.MSOA)

## Calculating RMSE
RMSE.RF.reduced <- sqrt(mean((Xtest.crime.population.MSOA$Total.Violent.2011 
                              - exp(Xpredicted.Rand.For))^2))
## 57.07

## Calculating MAE of the Model
MAE.RF.reduced <- mean(abs(Xtest.crime.population.MSOA$Total.Violent.2011 
                           - exp(Xpredicted.Rand.For)))
## 35.29

## Calculating MAPE
MAPE.RF.reduced <- mean(abs((Xtest.crime.population.MSOA$Total.Violent.2011 -
                               exp(Xpredicted.Rand.For))/
                              Xtest.crime.population.MSOA$Total.Violent.2011))
## 0.2514
####

#### Quantile Regression Analysis of Parameters ####

##Creating OLS model for whole data 
Xlinear.regression.OLS <- lm (log(Total.Violent.2011) ~ log(Total.youth)
                              + log(Population.Density)
                              + log(Road.casualties.2011 + 1)
                              + log(Median.Annual..Income)
                              + log(IMD.score),
                              data = Xcrime.population.MSOA)

summary(Xlinear.regression.OLS)

##Creating QR Model at 25th Quantile
Xquant.regression.25 <- rq (log(Total.Violent.2011) ~ log(Total.youth)
                            + log(Population.Density)
                            + log(Road.casualties.2011 + 1)
                            + log(Median.Annual..Income)
                            + log(IMD.score),
                            data = Xcrime.population.MSOA,
                            tau=0.25,model = TRUE) 

##Calculating Coefficients
summary(Xquant.regression.25) 

anova(Xquant.regression.50,p, joint= FALSE, test = "Wald")
##Creating QR Model at 50th Quantile
Xquant.regression.50 <- rq (log(Total.Violent.2011) ~ log(Total.youth)
                            + log(Population.Density)
                            + log(Road.casualties.2011 + 1)
                            + log(Median.Annual..Income)
                            + log(IMD.score),
                            data = Xcrime.population.MSOA,
                            tau=0.50, model = TRUE)
##Calculating Coefficients
summary(Xquant.regression.50)

##Calculating QR model at 75th Quantile
Xquant.regression.75 <- rq (log(Total.Violent.2011) ~ log(Total.youth)
                            + log(Population.Density)
                            + log(Road.casualties.2011 + 1)
                            + log(Median.Annual..Income)
                            + log(IMD.score),
                            data = Xcrime.population.MSOA,
                            tau=0.75, model = TRUE)

#Calculating Coefficients
summary(Xquant.regression.75)

##Calculating Quantile Model from 25th to 75th Quantile
Xquant.regression.25.to.75 <- rq (log(Total.Violent.2011) ~ log(Total.youth)
                                  + log(Population.Density)
                                  + log(Road.casualties.2011 + 1)
                                  + log(Median.Annual..Income)
                                  + log(IMD.score),
                                  data = Xcrime.population.MSOA,
                                  tau=c(0.25,0.75), model = TRUE)

#Calculating Coefficients
summary(Xquant.regression.25.to.75)

## Adjusted R square Value
fit0 <- rq(log(Total.Violent.2011)~1,tau=0.75,data = Xcrime.population.MSOA)
rho <- function(u,tau=.5)u*(tau - (u < 0))
adj.r.square <- 1 - Xquant.regression.75$rho/fit0$rho

#Plotting residuals
quantreg.all <- rq(log(Total.Violent.2011) ~ log(Total.youth)
                   + log(Population.Density)
                   + log(Road.casualties.2011+1) 
                   + log(Median.Annual..Income)
                   + log(IMD.score),
                   data = Xcrime.population.MSOA, 
                   tau = seq(0.05, 0.95, by = 0.05))



quantreg.plot <- summary(quantreg.all)
plot(quantreg.plot)

summary(lm(IMD.score ~ Unemployment.Rate + 
             Percent.living.in.income.deprived.households.on.benefit, data = Xcrime.population.MSOA))

# ANOVA test for coefficient significance differences
anova(Xquant.regression.75,Xquant.regression.25, joint = FALSE, test="Wald")
anova(Xquant.regression.75,Xquant.regression.25)
####

#### Post Analysis Code ####
#Analysing Relation between IMD.score and Unemployment.Rate 
summary(lm(Unemployment.Rate ~ IMD.score, 
           data = Xcrime.population.MSOA))

#Analysing Relation between IMD.score and 
#Percent.living.in.income.deprived.households.on.benefit
summary(lm(Unemployment.Rate ~ IMD.score, 
           data = Xcrime.population.MSOA))

summary(lm(Percent.living.in.income.deprived.households.on.benefit ~ IMD.score, 
           data = Xcrime.population.MSOA))
####


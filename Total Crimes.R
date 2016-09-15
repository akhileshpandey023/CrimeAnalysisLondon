#### An Adaptive Model for Crime Detection and Smart Policing ####


#### Splitting Data into Train and Test Data Sets#####

set.seed(1237)

#Randomly select the rows which will be included in traning set
#80% data for training and 20% for testing
split.factor.total <- createDataPartition(log(Xcrime.population.MSOA$Total.Crime.2011), 
                                    p = .8, list = FALSE)

#create the training set
Xtrain.total.crime.population.MSOA <- Xcrime.population.MSOA[split.factor.total,]

#create the testing set
Xtest.total.crime.population.MSOA <- Xcrime.population.MSOA[-split.factor.total,]

#### Build a Regression Model with all the Variables ####
linear.regression.basic.total <- lm (log(Total.Crime.2011) ~ log(Total.youth)
                               + log(Unemployment.Rate )
                               + log(Population.Density)
                               + log(X65.)
                               + log(Total.Low.Qualification)
                               + log(Road.casualties.2011 +1)
                               + log(Country.of.birth.Not.United.Kingdom.percent)
                               + log(Non.White.percent)
                               + log(Life.Expectancy)
                               + log(Median.Annual..Income)
                               + log(Percent.living.in.income.deprived.households.on.benefit)
                               + log(Land.Area.Hectares.)
                               + log(IMD.score), data = Xtrain.total.crime.population.MSOA)

## Evaluating the performance of the Model
summary(linear.regression.basic.total)
## The model gives an R squared value of 0.725 but has lot insignificant predictors

#### Calculating RMSE, MAE, MAPE and plotting residuals for the basic linear regression #####

##Predicting Valus for test data set
predicted.linear.total.basic <- (predict(linear.regression.basic.total, 
                                         Xtest.total.crime.population.MSOA))

##Calculating the Root Mean Square of Errors value
RMSE.linear.total.basic <- sqrt(mean((Xtest.total.crime.population.MSOA$Total.Crime.2011 
                                - exp(predicted.linear.total.basic))^2))
## 529.9

## Calculating Mean Average Error
MAE.linear.total.basic <- mean(abs(Xtest.total.crime.population.MSOA$Total.Crime.2011 
                             - exp(predicted.linear.total.basic)))
## 273.98

## Calculating MAPE
MAPE.linear.total.basic <- mean(abs((Xtest.total.crime.population.MSOA$Total.Crime.2011 -
                                 exp(predicted.linear.total.basic))/
                                Xtest.total.crime.population.MSOA$Total.Crime.2011))
## 0.196  

## Plotting the residuals

plot(linear.regression.basic.total$residuals)
abline(h=0)

#### Parameter Selections for the Model ####

XPredicting.Total.Variables <- c("X65.","Total.youth", "Life.Expectancy",
                           "Unemployment.Rate","Total.Low.Qualification",
                           "White.percent", "Country.of.birth.Not.United.Kingdom.percent",
                           "Land.Area.Hectares.", "Population.Density", "Median.Annual..Income", 
                           "Percent.living.in.income.deprived.households.on.benefit",
                           "Road.casualties.2011","IMD.score")




## Test 1: Using Regsubsets
## Regsubsets Finds the best subset to get the best result for some measurement criteria
## This is an automated selection process
## Parameters of this method:- 
## 1. method= "exhaustive" ensures all the possible combinations are explored
## 2. nbest = 6 gives the first or the best model for with 7 best predictors
## 3. nvmax = NULL for no limit on number of variables
linear.regression.basic.total.update1 <- regsubsets (log(Total.Crime.2011) ~ log(Total.youth)
                                              + log(Unemployment.Rate )
                                              + log(Population.Density)
                                              + log(X65.)
                                              + log(Total.Low.Qualification)
                                              + log(Road.casualties.2011 +1)
                                              + log(Country.of.birth.Not.United.Kingdom.percent)
                                              + log(Non.White.percent)
                                              + log(Life.Expectancy)
                                              + log(Median.Annual..Income)
                                              + log(Percent.living.in.income.deprived.households.on.benefit)
                                              + log(Land.Area.Hectares.)
                                              + log(IMD.score), 
                                              data = Xtrain.total.crime.population.MSOA,
                                              nbest = 1,      
                                              nvmax = 7,    
                                              force.in = NULL, force.out = NULL,
                                              method = "exhaustive") 


## Checking the result of the regression
summary(linear.regression.basic.total.update1) -> reg.summary.total.out

## We will select the parameters with the highest Coefficient of Determination
View(reg.summary.total.out$which[which.max(reg.summary.out$adjr2),])
## The parameters that were removed are:-
##Unemployment.Rate, Land.Area.Hectares., Total.Low.Qualification,
##Population.Density, Country.of.birth.Not.United.Kingdom.percent, Non.White.percent
##Life.Expectancy, Land.Area.Hectares., X65., 

## Test 3 Variance Inflation Factor ##
XPredicting.Variables <- c("Total.youth", 
                           "Country.of.birth.Not.United.Kingdom.percent",
                           "Population.Density", "Median.Annual..Income", 
                           "Percent.living.in.income.deprived.households.on.benefit",
                           "Road.casualties.2011","IMD.score")

linear.regression.basic.total.update2 <- lm (log(Total.Crime.2011) ~ log(Total.youth)
                                        + log(Road.casualties.2011 + 1)
                                        + log(Population.Density)
                                        + log(Country.of.birth.Not.United.Kingdom.percent)
                                        + log(Percent.living.in.income.deprived.households.on.benefit)
                                        + log(IMD.score),
                                        data = Xtrain.total.crime.population.MSOA)

#Save the Variance Inflation Factor in a Variable
vif(linear.regression.total.reduced) > 5
#Percent.living.in.income.deprived.households.on.benefit  removed
#####

#### Reduced Linear Regression Model #####

## This is our reduced Model with all the significant predictors
linear.regression.total.reduced <- lm (log(Total.Crime.2011) ~ log(Total.youth)
                                  + log(Road.casualties.2011 + 1)
                                  + log(Median.Annual..Income)
                                  + log(Population.Density)
                                  + log(Country.of.birth.Not.United.Kingdom.percent)
                                  + log(IMD.score),
                                  data = Xtrain.total.crime.population.MSOA)

summary(linear.regression.total.reduced)
## This gave a R squared value of 0.712

#### Calculating RMSE, MAE and MAPE for the Reduced Linear Regression Model#####

##Predicting Values for test data set
predicted.linear.total.reduced <- predict(linear.regression.total.reduced,
                                          Xtest.total.crime.population.MSOA)

##Calculating the Root Mean Square of Errors value
RMSE.linear.total.reduced <- sqrt(mean((Xtest.total.crime.population.MSOA$Total.Crime.2011 
                                  - exp(predicted.linear.total.reduced))^2))
## 532.73

##Calculating Mean Average Error
MAE.linear.reduced <- mean(abs(Xtest.total.crime.population.MSOA$Total.Crime.2011 
                               - exp(predicted.linear.total.reduced)))
## 274.33

## Calculating MAPE
MAPE.linear.reduced <- mean(abs((Xtest.total.crime.population.MSOA$Total.Crime.2011  -
                                   exp(predicted.linear.total.reduced))/
                                  Xtest.total.crime.population.MSOA$Total.Crime.2011 ))
## 0.198

## Plotting residuals

plot(linear.regression.total.reduced$residuals)
abline(h=0)

#####

#### Check for overfit #########
Xcrime.population.MSOA.cv <-Xcrime.population.MSOA[sample(nrow(Xcrime.population.MSOA)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(Xcrime.population.MSOA.cv)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test.Data.Violent.cv <- Xcrime.population.MSOA.cv[testIndexes, ]
  Xlinear.reduced.cv <- lm (log(Total.Crime.2011) ~ log(Total.youth)
                            + log(Road.casualties.2011 + 1)
                            + log(Median.Annual..Income)
                            + log(Population.Density)
                            + log(Country.of.birth.Not.United.Kingdom.percent)
                            + log(IMD.score),
                            data = test.Data.Violent.cv)
  
  print(summary(Xlinear.reduced.cv))
}

#####################################

#### Random Forest Regression ####

Xtrain.total.crime.population.MSOA$l.t.c <- log(Xtrain.total.crime.population.MSOA$Total.Crime.2011)
Xtest.total.crime.population.MSOA$l.t.c <- log(Xtest.total.crime.population.MSOA$Total.Crime.2011)
Xcrime.population.MSOA$l.t.c <- log(Xcrime.population.MSOA$Total.Crime.2011)

Xrandom.forest.total.reduced <- randomForest(l.t.c ~ Total.youth  
                                       + Unemployment.Rate 
                                       #+ Population.Density  3 -> 7.78
                                       #+ X65.  2 -> 5.81
                                       #+ Total.Low.Qualification  4 -> 9.13
                                       + Road.casualties.2011 
                                       + Country.of.birth.Not.United.Kingdom.percent
                                       + Non.White.percent
                                       + Life.Expectancy
                                       #+ Median.Annual..Income 3 -> 7.44
                                       + Percent.living.in.income.deprived.households.on.benefit
                                       #+ Land.Area.Hectares.  1 -> 5.38
                                       + IMD.score,
                                       data = Xtrain.total.crime.population.MSOA,
                                       ntree=10000) 

print(Xrandom.forest.total.reduced)

#Checking the IncNodePurity
Xrandom.forest.total.reduced$importance

#### Calculating RMSE and MAE For Random Forest ####
## Predicting Crime Incidents on Test Set
Xpredicted.Rand.total.For <- predict(Xrandom.forest.total.reduced, Xtest.total.crime.population.MSOA)

## Calculating RMSE
RMSE.RF.total.reduced <- sqrt(mean((Xtest.total.crime.population.MSOA$Total.Crime.2011
                              - exp(Xpredicted.Rand.total.For))^2))
## 592.19

## Calculating MAE of the Model
MAE.RF.total.reduced <- mean(abs(Xtest.total.crime.population.MSOA$Total.Crime.2011 
                           - exp(Xpredicted.Rand.total.For)))
## 291.78

## Calculating MAPE
MAPE.RF.reduced <- mean(abs((Xtest.total.crime.population.MSOA$Total.Crime.2011 -
                               exp(Xpredicted.Rand.total.For))/
                              Xtest.total.crime.population.MSOA$Total.Crime.2011))
## 0.2005
####

#### Quantile Regression Analysis of Parameters ####
Xquant.total.regression.25 <- rq (log(Total.Crime.2011) ~ log(Total.youth)
                            + log(Road.casualties.2011 + 1)
                            + log(Median.Annual..Income)
                            + log(Population.Density)
                            + log(Country.of.birth.Not.United.Kingdom.percent)
                            + log(IMD.score),
                            data = Xcrime.population.MSOA,
                            tau=0.25, model = TRUE)

#Calculating Coefficients
summary(Xquant.total.regression.25)

Xquant.total.regression.50 <- rq (log(Total.Crime.2011) ~ log(Total.youth)
                                  + log(Road.casualties.2011 + 1)
                                  + log(Median.Annual..Income)
                                  + log(Population.Density)
                                  + log(Country.of.birth.Not.United.Kingdom.percent)
                                  + log(IMD.score),
                                  data = Xcrime.population.MSOA,
                                  tau=0.50, model = TRUE)

#Calculating Coefficients
summary(Xquant.total.regression.50)

Xquant.total.regression.75 <- rq (log(Total.Crime.2011) ~ log(Total.youth)
                            + log(Road.casualties.2011 + 1)
                            + log(Median.Annual..Income)
                            + log(Population.Density)
                            + log(Country.of.birth.Not.United.Kingdom.percent)
                            + log(IMD.score),
                            data = Xcrime.population.MSOA,
                            tau=0.75, model = TRUE)

#Calculating Coefficients
summary(Xquant.total.regression.75)

Xquant.total.regression.25.to.75 <- rq (log(Total.Crime.2011) ~ log(Total.youth)
                                  + log(Road.casualties.2011 + 1)
                                  + log(Median.Annual..Income)
                                  + log(Population.Density)
                                  + log(Country.of.birth.Not.United.Kingdom.percent)
                                  + log(IMD.score),
                                  data = Xcrime.population.MSOA,
                                  tau=c(0.25,0.75), model = TRUE)

#Calculating Coefficients
summary(Xquant.total.regression.25.to.75)


## Adjusted R square Value
fit0 <- rq(log(Total.Violent.2011)~1,tau=0.50,data = Xcrime.population.MSOA)
rho <- function(u,tau=.5)u*(tau - (u < 0))
adj.r.square <- 1 - Xquant.total.regression.50$rho/fit0$rho


#Plotting residuals
quantreg.total.all <- rq(log(Total.Violent.2011) ~ Total.youth
                   + Population.Density 
                   + Road.casualties.2011
                   + Median.Annual..Income 
                   + Percent.living.in.income.deprived.households.on.benefit
                   + IMD.score, 
                   data = Xcrime.population.MSOA, 
                   tau = seq(0.05, 0.95, by = 0.05))

quantreg.total.plot <- summary(quantreg.total.all)
plot(quantreg.total.plot)


# ANOVA test for coefficient differences
anova(Xquant.total.regression.75,Xquant.total.regression.25, joint = FALSE, test="Wald")
anova(Xquant.total.regression.25, Xquant.total.regression.75)

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


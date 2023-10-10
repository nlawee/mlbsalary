rm(list=ls())
cat("\f")
attach(data)

# These are ZIP code-level data on prices for various items at fast-food restaurants, along with characteristics of the zip code population, in New Jersey and Pennsylvania.

## For the relationship between price of medium soda and proportion of population that is black, is it medium soda explained by proportion?

# In this dataset, our analysis will stem from 3 variables: 
  # psoda = price of medium soda
  # prpblck = proportion of the population that is black
  # income = median family income

## Simple Linear Regression Model (SLR): 
  # psoda = B0 + B1prpblck + u
## Multiple Linear Regression Model (MLR): 
  # psoda = B0 + B1prpblck + B2income + u

# First, we will analyze our mean and standard deviation amongst our three variables.
mean(psoda)
mean(prpblck)
mean(income)
# Mean psoda = 1.04486
# Mean prpblck = 0.114955
# Mean income = 46999.4
sd(psoda)
sd(prpblck)
sd(income)
# sd psoda = 0.0887978
# sd prpblck = 0.183875
# sd income = 13215.3

# Price of Soda (in dollars $)
# Proportion Black Ethnicity (in percentage of zip-code)
# Median Family Income (in dollars $)

OLS_SLR<-lm(psoda ~ prpblck)
summary(OLS_SLR)
# psoda = 1.03740 + 0.06493prpblck + u

OLS_MLR<-lm(psoda ~ prpblck + income)
summary(OLS_MLR)
options("scipen"=999, digits=5) # <- This avoids scientific notation when reporting estimates
# psoda = 0.95632 + 0.11499prpblck + 0.000001603income + u

# SLR R^2 = 0.0181 = 1.81%
# MLR R^2 = 0.0642 = 6.42%
# R^2 increases by 0.0461 or 4.61% because we added another independent explanatory variable

prpblckinc<-prpblck+0.10

OLS_SLRinc<-lm(psoda ~ prpblckinc)
summary(OLS_SLRinc)
# psoda = 1.03740 + 0.06493prpblck + u
# (new) psoda = 1.03091 + 0.06493prpblck + u
# prpblck does not change with an increase of 0.10 percentage points
# However, the intercept changes by -0.00649

OLS_MLRinc<-lm(psoda ~ prpblckinc + income)
summary(OLS_MLRinc)
# psoda = 0.95632 + 0.11499prpblck + 0.000001603income + u
# (new) psoda = 0.94482 + 0.11499prpblck + 0.000001603income + u
# prpblck does not change with an increase of 0.10 percentage points
# However, the intercept changes by -0.0115
# The discrimination effect is larger in the MLR with income than the SLR.

cor(prpblck,income)
# -0.43461
# This means that prpblck and income are moderately negatively correlated. As one increases, the other decreases. Based on the data, the higher the income, the lower the proportion population of black.
# This says that estimating the price of soda by the proportion of the population that is black would make the price lower than estimating the price of soda with proportion population black and income. 

# Let's run some estimations.
fv1<-OLS_MLR$coef[1]+(OLS_MLR$coef[2]*0)+OLS_MLR$coef[3]
fv1
# Price of soda is equal to roughly $0.96 when there is no black population in the communities.
fv2<-OLS_MLR$coef[1]+(OLS_MLR$coef[2]*.99)+OLS_MLR$coef[3]
fv2
# Price of soda is equal to roughly $1.07 when there is almost all black population in the communities.

fv3<-OLS_MLR$coef[1]+(OLS_MLR$coef[2]*0.35)+(OLS_MLR$coef[3]*40000)
fv3
# Predicted price of soda would be $1.06 with 35% black population and median family income of $40,000.

# Let's revert back to our original MLR model by changing our income (in $1,000) and proportion of black population as percentgaes.
income_1000s<-income/1000
prpblck_percent<-prpblck*100

OLS_MLR_new<-lm(psoda ~ prpblck_percent + income_1000s)
summary(OLS_MLR_new)
# psoda = 0.95632 + 0.11499prpblck + 0.000001603income + u
# psoda = 0.95632 + 0.001150prpblck + 0.001603income + u
# Changing the units of measurement for prpblck into a % and income into $1000's, adjusted the decimal placement of the variables by 2 and 3, respectively.
# These estimate changes make sense corresponding with the unit changes. 

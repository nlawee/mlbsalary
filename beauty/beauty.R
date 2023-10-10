# beauty.RData contains a subset of the variables reported by Hamermesh and Biddle (1994) who study discrimination based on looks.
# The survey asked the interviewer to rate the respondent's physical appearance on a five-point scale for 'looks' variable, from 1 (homely) to 5 (strikingly handsome/beautiful)

library(MASS)

# Let's calculate the fractions of having above average looks.

# New dataset: Above Average population only
abvavg_1<- subset(data, abvavg == 1)
## Above Average / Total
abvavg_fraction<-383/1260
## 383/1260 lowest reduction

# New dataset: Above Average Men only
abvavg_men <- subset(data, female == 0 & abvavg == 1)
# Total Men / Total
men_fraction <- 824/1260
## 206/315 lowest reduction

# New dataset: Above Average Women only
abvavg_women <- subset(data, female == 1 & abvavg == 1)
# Total Women / Total
female_fraction <- 436/1260
## 109/315 lowest reduction

male <- data

# above average women / total women
abvfemale_fraction <- 144/436
# 0.33

# above average men / total men
abvmale_fraction <- 239/824
# 0.29

# The difference between men and women for above average looking across the population is 0.04.
OLS_abvfemale <- lm(female ~ abvavg, data=data)
summary(OLS_abvfemale)
# The t statistic is about 1.48 with a two-sided p-value of 0.14, there isn't necessarily strong evidence against the null that the population fractions are the same.

# Two new datasets: men or women
men <- subset(data, female == 0)
women <- subset(data, female == 1)


# Let's estimate the following model: 
# log(wage) = β0 + β1belavg + β2abvavg + β3exper + β4exper2 + u 
  # For all observations
OLS_all <- lm(lwage ~ belavg + abvavg + exper + expersq, data=data)
summary(OLS_all)
  # Men only
OLS_men <- lm(lwage ~ belavg + abvavg + exper + expersq, data=men)
summary(OLS_men)
  # Women only
OLS_women <- lm(lwage ~ belavg + abvavg + exper + expersq, data=women)
summary(OLS_women)
  # Make a table using stargazer for our OLS models
library(stargazer)
stargazer(OLS_all,OLS_men,OLS_women, type="text", keep.stat=c("n","rsq"), title="Table 1. Wage Determinants")

# The base group would be average or above average looking people, if looks were >= 3. So, if a person were below average looking, wage would decrease by ~21.45%.
OLS_abvwomen <- lm(lwage ~ belavg + abvavg, data=women)
summary(OLS_abvwomen)

# Women with above average looks earn ~3.4% more than women with average looks. 
# However, we cannot say that this is statistically significant because we fail to reject the null hypothesis at the 5% level. 0.5442>0.05

OLS_all <- lm(lwage ~ belavg + abvavg + exper + expersq, data=data)
summary(OLS_all)

# Time to perform a Chow F statistic to test whether we have to specify the model differently for men and women at the 5% level.

# Sum of Squares due to regression (SSR) for all observations
SSRA <- sum(OLS_all$residuals^2)
# Sum of Squares due to regression (SSR) for female
olsF <- lm(lwage ~ belavg + abvavg + exper + expersq, data=data, subset=(female==1))
summary(olsF)
SSRF <- sum(olsF$residuals^2)
# Sum of Squares due to regression (SSR) for male
olsM <- lm(lwage ~ belavg + abvavg + exper + expersq, data=data, subset=(female==0))
summary(olsM)
SSRM <- sum(olsM$residuals^2)
# Chow Test Statistic
# k = number of explanatory variables, minus one bc we don't want B0
# obsn = number of observations
obsn<-nobs(OLS_all)

ChowS <-((SSRA - (SSRF+SSRM))/(k+1))/((SSRF+SSRM)/(obsn-2*(k+1)))
ChowS

FC<-qf(.95, df1=k+1, df2=(obsn-2*(k+1))) 
FC

olschow <- lm(lwage ~ belavg + abvavg + exper + expersq + female + belavg:female + abvavg:female + exper:female + expersq:female, data=data)
library(car)
linearHypothesis(olschow, c("female = 0","belavg:female=0","abvavg:female=0","exper:female=0","expersq:female=0"))
ChowS
# The p-value for Chow Test is 2.2e-16<0.05, so we reject the null hypothesis and say that men and women cannot share the same regression function and we have to specify the model differently.

# Turning Point of the semi-elasticity of wage with respect to the experience for all observations, holding look variables fixed.
OLS_all$coefficients[4]/2*OLS_all$coefficients[5]*10
# At a particular value, in this case, 10 years of workforce experience, the marginal effect of additional year of experience decreases wage by 0.000239.

# Estimate the model below for all observations:
  # married = β0 + β1belavg + β2abvavg + β3bigcity + β4educ + u
OLS_marr <- lm(married ~ belavg + abvavg + bigcity + educ, data=data)
summary(OLS_marr)
# Report OLS Summary, people with above average looks have a lower chance of being married.


data$lpm.fitted <- OLS_marr$fitted.values
sum(with(data, lpm.fitted>1))
sum(with(data, lpm.fitted<0))
data$fit.marr[data$lpm.fitted>=0.5]<-1
data$fit.marr[data$lpm.fitted<0.5]<-0

sum(with(data, fit.marr==married))
sum(with(data, fit.marr==married))/nrow(data)
# Percent Correctly Predicted: ~69.13%

install.packages("lmtest")
library(lmtest)

bptest(OLS_marr)
# Using the Breusch-Pagan Test to test for heteroskedasticity, where the variance can take different values, we observe a p-value from the model of 0.1082. 
# Which means that we fail to reject the null hypothesis (0.1082>0.05), which means that there is evidence of homoskedasticity. The OLS estimation is valid for the sample we have!
  
  
  
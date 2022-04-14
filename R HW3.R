# lsalary = β0 + β1years + β2gamesyr + β3hrunsyr + u
# lsalary = β0 + β1years + β2gamesyr + β3hrunsyr + B4allstar + B5sbasesyr +  u
# We want to observe the effects of various explanatory variables on the salary of MLB players.

options("scipen"=999, digits=5)

# We run OLS regression analyses to show the effect of the variables on dependent salary.
OLSA<-lm(lsalary ~ years + gamesyr + hrunsyr, data=data)
summary(OLSA)
# ^ Regression explaining the effect of years in major league games, games per year in league, and homeruns per year

OLSB<-lm(lsalary ~ years + gamesyr + hrunsyr + allstar + sbasesyr, data=data)
summary(OLSB) 
# ^ We add two additional explanatory variables, (allstar) percentage of years an all-star and (sbasesyr) stolen bases per year, to see if they explain our model better and decide if we need the variables to be a determinant of salary.

library(stargazer)
# Use the stargazer function to make a formal estimation result table to show the regression results for both models
stargazer(OLSA,OLSB, type="text", keep.stat=c("n","rsq"), title="Table 1. MLB Salary Determinants")
# We test the variables at the 5% significance level.
# For variables years (1.34e-06 < 0.05), gamesyr (2.45e-16 < 0.05), hrunsyr (0.000348 < 0.05), and allstar (0.000437 < 0.05), we reject the null hypothesis and say that they are statistically significant at the 5% level.
# However, for sbasesyr, we fail to reject the null hypothesis (0.746 > 0.05) and say that sbasesyr is not statistically significant at the 5% level.

# Access the "car" package to run linearHypothesis and determine joint statistical test.
library("car")
linearHypothesis(OLSB, c("allstar = 0","sbasesyr=0"))
# (0.00112 < 0.05) So we reject the null hypothesis and say that the two variables, allstar and sbasesyr, are jointly statistically significant at the 5% level. 
# Also, (0.001112<0.01) so we reject the null hypothesis and say that the two variables are jointly statistically significant at the 1% level. (0.001112<0.01) So we reject the null hypothesis.
# Yes we would keep the variables.

# Overall significance is 0.0000000000000002 < 0.05, so we reject the null hypothesis and state that overall significance is statistically significant at the 5% level. 

vif(OLSB)
# years  gamesyr  hrunsyr  allstar sbasesyr 
# 1.521628 2.676888 1.975786 1.627120 1.509690 
# We can see that the VIF shows us any possible variable inflation. However, there aren't any high correlation with the other variables. So we wouldn't need to drop any variables.

summary(OLSB)
# We can observe an adjusted R2 value of 0.6327 for model B showing that the two additional explanatory variables show greater explanatory power than Model A (0.6202)

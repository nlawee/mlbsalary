rm(list=ls())

# Analyze some sample data for Height(cm) and Weight(kg)
Height<-c(188,188,178,183,180,183,4,193)
Weight<-c(95,91,72,93,78,82,4,98)
heiweidata<-data.frame(Height,Weight)

# Draw a scatterplot and OLS regression (fitted values) line
plot(y=heiweidata$Weight, x=heiweidata$Height, xlab = "Height (cm)", ylab = "Weight (kg)")
abline(lm(heiweidata$Weight ~ heiweidata$Height), col="blue")

# Let's take a look at estimating the intercept and slope of a simple linear regression model.
lm(Weight ~ Height, data=heiweidata)
## weight = β0 + β1(Height) + u
## Our Sample Regression Function (SRF): y(weight) = 1.0519 + 0.4661(Height)
## b1 is just a hypothetical variable (no value), b1-hat has a value, i.e. 0.4661
## For every 1cm increase in height, weight will increase by 0.4661.

# Generate some new variables for predicted values and the residuals.
ols1<-lm(Weight ~ Height, data=heiweidata)
summary(ols1)

ols1$fitted.values

ols1$residuals

x1<-mean(heiweidata$Height)
x2<-mean(ols1$fitted.values)
x3<-mean(ols1$residuals)

# If an observation has a height of 175 cm, what is the best prediction for weight?

yhat<-ols1$coef[1]+(ols1$coef[2]*175)
yhat

# If the height is 175 cm then the best prediction for weight would be 82.63 kg. 
# You could calculate the residual for this prediction, if there was an actual value for a person whose height is 175 cm, but there is not. 
# Residual = actual - predicted

ols1<-lm(Weight ~ Height, data=heiweidata)

---------------------------------------------

# Import our bwght dataset, which contains data on the births of women in the United States/
# Dependent variable: Infant birth weight in ounces (bwght)
# Explanatory variable: Average number of cigarettes the mother smoked per day during pregnancy (cigs)

# Pulling the mean and standard deviation of both variables.
meanbwght <- mean(data$bwght)
stdevbwght <- sd(data$bwght)
meancigs <- mean(data$cigs)
stdevcigs <- sd(data$cigs)

# Let's run an OLS regression of bwght on cigs.
ols2<-lm(bwght ~ cigs, data=data)
summary(ols2)
## Our Sample Regression Function (SRF): y(bwght) = 119.77190 - 0.51377(cigs)

## The Intercept, 119.77190, is the average, starting birth weight for the infants in the sample.
## The slope, -0.51377, means that if the average number of cigs smoked per day by the mother goes up by 1, then the birth weight of the infant will decrease by 0.51377 oz.
## As we can see below, with our scatterplot with OLS regression line.
plot(y=data$bwght, x=data$cigs, xlab = "Cigs", ylab = "Birth Weight")
abline(lm(data$bwght ~ data$cigs), col="blue")


# What's the predicted birth weight when cigs = 0?
yhat2<-ols2$coef[1]+(ols2$coef[2]*0)
# y-hat = 119.77190-0.51377(0)̂
# y-hat = 119.7719

# How about when cigs = 20?
yhat3<-ols2$coef[1]+(ols2$coef[2]*20)
# y-hat = 119.77190-0.51377(20)
# y-hat = 109.4965

# The difference between the two is 10.27544. 
# This means that the infant’s weight decreased by 10.3 oz when the mother increases her average cigarettes a day by 20.

# Does this simple regression necessarily capture a causal relationship between the child's birth weight and the mother's smoking habits?
# In this instance, the smoking habits of the mother happens before the birth of the child so the relationship can be considered causal.



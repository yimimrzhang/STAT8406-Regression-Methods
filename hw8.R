# ===================== #
# R Code for Homework 8 #
# ===================== #

# ==================== #
# Problem 1 #
# ==================== #
warm <- read.csv("warming2014.txt",     # Reads in the warming data
  header=T)

# Part (a)
# ========
plot(warm$year,warm$temp,xlab="Year",ylab=    # Plot of temperature vs.
  "Temperature Difference",pch=16,cex=1.5,    #   year
  main="Scatterplot of Temperature vs. Year",
  cex.main=1.6,cex.axis=1.5,cex.lab=1.6,
  mgp=c(2.7,1,0))
reg1 <- lm(temp~year,data=warm)               # Regression of temp on year
summary(reg1)                                 # Regression summary

# Part (b)
# ========
qqnorm(reg1$resid,xlab="Normal Quantiles",    # Normal quantile plot of
  ylab="Residuals",cex.lab=1.6,cex.axis=1.5,  #   model residuals
  cex=1.5,pch=16,cex.main=1.8,
  main="Normal Quantile Plot",mgp=c(2.7,1,0))
qqline(reg1$resid)
library(nortest)                              # Loads "nortest" library
sf.test(reg1$resid)                           # Shapiro-Francia test

# Part (c)
# ============
plot(warm$year,reg1$resid,xlab="Year",ylab= # Partial residual plot of
  "Residuals",pch=16,cex=1.5,cex.main=1.6,  #   residuals vs. year
  main="Scatterplot of Residuals vs. Year",
  cex.axis=1.5,cex.lab=1.6,mgp=c(2.7,1,0))
abline(h=0,lwd=2,lty=2)                     # Plots horizontal 0-line
library(lmtest)                             # Loads "lmtest" library
dwtest(reg1, alternative="two.sided")       # 2-sided Durbin-Watson test


# ==================== #
#  Problem 2 #
# ==================== #
dat <- read.csv("mortality.txt",header=T)  # Reads in mortality data

# Part (a)
# ========
source("pairs.panels.r")      # Loads the "pairs.panels" function
pairs.panels(dat[,2:11])              # Panel plot of all variables

# Part (b)
# ========
lognox <- log(dat$nox)                # Log NOx variable
logso2 <- log(dat$so2)                # Log SO2 variable
dat2 <- data.frame(dat,lognox,logso2) # New data frame with logged variables

reg1<- lm(mortality~precip+jantemp+    # Regression of mortality on all 1st-
   house+educ+density+nonwhite+poor+   #   order terms 
   lognox+logso2,data=dat2)

# install.packages("car")
library(car)
avPlots(reg1, ~ .) # Partial Regression Plots

# Part (c)
# ========
library(olsrr)
back<-ols_step_backward_p(reg1, prem = 0.05, details = TRUE)    # Backward Elimination 
plot(back)

reg2<- lm(mortality~precip+jantemp+    # Final selected model
            educ+nonwhite+lognox,data=dat2)
summary(reg2)

# Part (d)
# ========
jack <- rstudent(reg2)    # Computes studentized deleted residuals
            
par(mfrow=c(2,2))         # 2x2 graphics window
qqnorm(jack,xlab="Normal Quantiles",ylab="Stddeleted Residuals",
  cex.axis=1.5,cex.lab=1.6,main="Normal Quantile Plot",
  cex.main=1.6,pch=16,cex=1.5,mgp=c(2.7,1,0))
plot(reg2$fitted,reg2$resid,xlab="Predicted Values",ylab="Residuals",
  cex.axis=1.5,cex.lab=1.6,main="Residual Plot",
  cex.main=1.6,pch=16,cex=1.5,mgp=c(2.7,1,0))
plot(dat2$jantemp,reg2$resid,xlab="Average January Temperature",
  ylab="Residuals",cex.axis=1.5,cex.lab=1.6,cex.main=1.6,main=
  "Partial Residual Plot",pch=16,cex=1.5,mgp=c(2.7,1,0))
plot(dat2$density,reg2$resid,xlab="Density",
  ylab="Residuals",cex.axis=1.5,cex.lab=1.6,cex.main=1.6,main=
  "Partial Residual Plot",pch=16,cex=1.5,mgp=c(2.7,1,0))

par(mfrow=c(1,1)) # Reset graphics window

# Part (e)
# ========
round(hatvalues(reg2),4)         # Levarage values
round(jack,4)                    # Studentized deleted residuals  
round(cooks.distance(reg2),4)    # Cook's D values
round(dffits(reg2),4)            # DfFits values
round(dfbetas(reg2),4)           # DfBetas values
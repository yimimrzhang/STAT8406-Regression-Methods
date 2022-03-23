# Scatterplot & Residual plot for Extinction data
# ===============================================
extinct <- read.csv("Data/extinct.txt",   # Reads in the extinction
  header=T)                               #   data
logtime <- log(extinct$exttime)           # Vector of log extinction times
logpairs <- log(extinct$numpairs)         # Vector of log num. pairs
reg1 <- lm(logtime~logpairs+extinct$size+ # Multiple regression of logtime
  extinct$migrant)                        #   on logpairs, size, migrant
plot(logpairs,logtime,pch=16,cex.lab=1.6, # Scatterplot of log pairs vs.
  ylab="Log Average Time to Extinction",  #   log nesting pairs
  xlab="Log Average Number of Nesting Pairs",cex.axis=1.5,
  cex=1.5,main="Log Extinction Time vs. Log Nesting Pairs",
  cex.main=1.6,mgp=c(2.7,1,0))
qqnorm(reg1$resid,pch=16,ylab="Residuals",# Normal quantile plot of model
  xlab="Standard Normal Quantiles",       #   residuals
  cex.lab=1.6,cex.axis=1.5,cex.main=1.8,
  main="Normal Quantile Plot",cex=1.5,mgp=c(2.7,1,0))

# Tests of Normality
# ==================
library(nortest)            # Loads nortest library
sf.test(reg1$resid)         # Conducts Shapiro-Francia test

# Dow Jones Indutrial Average Data
# ================================
DJIA <- c(17986,18140,18117,18209,18225, # Vector of the DJIA values
  18214,18133,18289,18204,18097,18136,   #   over a 24-day period.
  17857,17996,17663,17635,17895,17749,
  17977,17849,18076,17959,18128,18116,
  18011)
day <- 1:24                              # Vector of "day" variable values
par(mfrow=c(1,2))                        # Creates a 1x2 graphics window
plot(day,DJIA,pch=16,xlab="Day",ylab=    # Scatterplot of DJIA vs. day
  "Dow Jones Industrial Average (DJIA)",
  cex.lab=1.6,cex.axis=1.5,main=
  "Dow Jones Ind. Avg. vs. Day\n(2/19/2015 - 3/24/2015)",
  cex.main=1.6,cex=1.5,mgp=c(2.7,1,0))
reg2 <- lm(DJIA~day)                     # Regresses DJIA on day
abline(reg2,lwd=2)                       # Plots the regression line
plot(day,reg2$resid,pch=16,xlab="Day",   # Plots the residuals vs.
  ylab="Residuals",cex.lab=1.6,          #   time
  cex.axis=1.5,cex=1.5,main="Time Plot of Residuals",cex.main=1.8,
  mgp=c(2.7,1,0))
abline(h=0,lwd=2,lty=2)           # Places a dashed horizontal line at 0

library(lmtest)                      # Loads the lmtest library
dwtest(reg2,alternative="greater")   # Performs the Durbin-Watson test


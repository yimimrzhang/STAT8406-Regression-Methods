biom <- c(16.6,49.1,121.7,219.6,375.5,570.8,  # Vector of biomass values
          648.2,755.6)
solrad <- c(29.7,68.4,120.7,217.2,313.5,      # Vector of solar radiation values
            419.1,535.9,641.5)
gestat <- c(219,225,122,278,31,284,201,250,   # Vector of gestation values
            151,330,240,15,31,350)
longev <- c(18,25,5,15,6,15,8,15,8,20,12,1,   # Vector of longevities
            5,12)
plot(longev, gestat, pch=16, xlim=c(0,60),    # Plots gestation vs. longevity with
  xlab="Longevity in years (x)",ylim=c(0,800),#   axis labels and a title
  ylab="Gestation Period in days (y)",cex=1.5,
  cex.lab=1.6,cex.axis=1.5,cex.main=1.6,mgp=
  c(2.7,1,0),main="Gestation vs. Longevity with Confidence Bands")
reg.out <- lm(gestat~longev)                  # Regression of gestation on longevity
abline(reg.out)                               # Plots the fitted regression line
se <- sqrt(sum(reg.out$resid**2)/12)          # Residual SE
xbar <- mean(longev)                          # Mean of longevities (xbar)
sxx <- sum(longev**2) - 14*(xbar**2)          # Sxx = total variation in x's
tse <- (2.179)*se                             # tcrit times residual SE
x <- seq(0,60,0.5)                            # Vector of x's from 0 to 60 by 0.5
moe <- tse*sqrt((1/14)+((x-xbar)**2)/sxx)     # Confidence interval margin of error
yhat <- reg.out$coef[1] + reg.out$coef[2]*x   # Prediction values for x-vector
lower <- yhat - moe                           # Lower confidence band
upper <- yhat + moe                           # Upper confidence band
lines(x,lower)                                # Plots lower confidence band on plot
lines(x,upper)                                # Plots upper confidence band on plot

# Faster, Easier Way to calculuate/plot Confidence/Prediction Intervals/Bands
# ======================================================
plot(solrad,biom,xlab="Solar Radiation (x)",   # Plots biomass vs. solar radiation
  ylab="Plant Biomass (y)",cex.axis=1.5,       #   with axis labels and a title
  cex.lab=1.6,cex=1.5,pch=16,cex.main=1.8,
  main="Prediction & Confidence Bands",
  mgp=c(2.7,1,0))
reg.out <- lm(biom~solrad)                     # Regression of biomass on solar rad.
abline(reg.out$coef,lwd=2)                     # Plots the fittred regression line
xval <- data.frame(solrad=250)                 # Specify an x value in data.frame format
conf <- predict(reg.out,xval,interval=         # Computes confidence interval for E(y) at x
  "confidence")
pred <- predict(reg.out,xval,interval=         # Computes prediction interval for y at x
  "prediction")
xval <- data.frame(solrad=seq(0,650,25))       # Sequence of x-values from 0 to 650 by 25
conf <- predict(reg.out,xval,interval=         # Computes confidence bands
  "confidence")
pred <- predict(reg.out,xval,interval=         # Computes prediction bands
  "prediction")
matlines(xval,conf[,c("lwr","upr")],           # Plots confidence bands in red and
  col="red",lty=2,lwd=2)                       #   line type 2
matlines(xval,pred[,c("lwr","upr")],           # Plots prediction bands in red and
  col="red",lty=6,lwd=2)                       #   line type 6
legend(300,200,c("Regression Line",            # Places a legend on the plot at
  "Confidence Band","Prediction Band"),lty=    #   (300,200), with labels, distinguished
  c(1,2,6),lwd=c(2,2,2),col=c("black","red",   #   by line type and color
  "red"),cex=1.5)

# Compute Studentized Residuals
# ======================================================
stand<-rstandard(reg.out)               # Computes internally studentized residuals
stand
jack <- rstudent(reg.out)               # Computes studentized deleted residuals
jack


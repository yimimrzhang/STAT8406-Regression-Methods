
# Plots the E.O. Wilson species/area data
# =======================================
species <- c(100,108,45,53,16,11,7)      # Numbers of species values 
area <- c(44218,29371,4244,3435,32,5,1)  # Island areas
plot(area,species,pch=16,cex.axis=1.5,   # Scatterplot of species vs. area
  xlab="Area (Square Miles)",cex=1.5,    #   with axis labels and a title
  ylab="Number of Species",cex.lab=1.8,
  main="Number of Species vs. Area",
  mgp=c(2.7,1,0),cex.main=1.8)

lspec <- log10(species)                  # Log base 10 of species values
larea <- log10(area)                     # Log base 10 of island areas
plot(larea,lspec,pch=16,cex.axis=1.5,    # Scatterplot of log species vs.
  xlab="Log(Area (Square Miles))",cex=   #   log area with axis labels
  1.5,ylab="Log(Number of Species)",
  cex.lab=1.8,cex.main=1.7,mgp=c(2.7,1,0),
  main="Log(Number of Species) vs. Log(Area)")
reg <- lm(lspec~larea)                   # Regresses log species on log area
summary(reg)                             # Regression summary
abline(reg$coef)                         # Overlays the regression line

plot(reg$fitted,reg$resid,ylab="Residuals", 	# Residual plot
  xlab="Predicted Values",cex.lab=1.6,cex=1.5,
  pch=16,main="Residuals vs. Predicted Counts",
  cex.main=1.8,cex.axis=1.5,mgp=c(2.7,1,0))
abline(h=0,lty=2,lwd=2)                       	# Plots the y=0 line

qqnorm(reg$resid,pch=16,ylab="Residuals",	# Normal quantile plot of model
  xlab="Standard Normal Quantiles",       #   residuals
  cex.lab=1.6,cex.axis=1.5,cex.main=1.8,
  main="Normal Quantile Plot",cex=1.5,mgp=c(2.7,1,0))

# Scatterplot of heart disease mortality vs. wine consumption
# ===========================================================
hwdat <- read.csv("Data/heartwine.txt",  # Reads in the heart disease and
  header=T)                              #   mortality data
plot(hwdat$wine,hwdat$mortality,pch=16,  # Plots mortality vs. wine
  xlab="Wine Consumption (l/person/yr)", #   consumption with axis labels
  cex.lab=1.6,cex.axis=1.5,cex=1.5,      #   and a title
  ylab="Heart Disease Mortality (deaths/1000)",
  main="Heart Disease vs. Wine Consumption",
  cex.main=1.8,mgp=c(2.7,1,0))

# Scatterplot of heart disease mortality vs. wine consumption
# ===========================================================
par(mfrow=c(1,1),mar=c(5,4,4,2)+.1)      # Creates a 1x1 graphics window
logwine <- log10(hwdat$wine)             # Log wine consumptions
plot(logwine,hwdat$mortality,pch=16,xlab=# Plots mortality vs. log(wine)
  "Log Wine Consumption (l/person/yr)",  #   consumption with axis labels
  cex.lab=1.6,cex.axis=1.5,cex=1.5,      #   and a title
  ylab="Heart Disease Mortality (deaths/1000)",
  main="Heart Disease vs. Log Wine Consumption",
  cex.main=1.8,mgp=c(2.7,1,0))
abline(lm(hwdat$mortality~logwine),lwd=2)# Plots regression line


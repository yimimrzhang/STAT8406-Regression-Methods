# Code for Inference in Multiple Regression Handout
# =================================================
mfoam <- read.csv("Data/meadowfoam.txt",     # Reads in meadowfoam data
  header=T)
attach(mfoam)                                # Promotes the meadowfoam dataset

# Labeled Scatterplot of flowers vs. intensity by timing
# ======================================================
plot(intensity,flowers,cex=1.5,cex.axis=1.5, # Plots flowers vs. intensity
  pch=c(1,16)[unclass(timing)],xlab=         #   with a separate plotting
  "Light Intensity",ylab="Number of Flowers",#   symbol for each timing, and
  cex.lab=1.6,mgp=c(2.7,1,0))                #   axis labels
reg1.out <- lm(flowers[timing=="At PFI"]~    # Regression of flowers on
  intensity[timing=="At PFI"])               #   intensity for timing 1
reg2.out <- lm(flowers[timing==              # Regression of flowers on
  "24 Days before PFI"]~intensity[timing==   #   intensity for timing 2
  "24 Days before PFI"])
abline(reg1.out,lwd=2,lty=1)                 # Plots timing 1 regression line
abline(reg2.out,lwd=2,lty=8)                 # Plots timing 2 regression line
legend(150,45,c("At PFI","24 Days\nbefore PFI"),# Places a legend on the plot
  pch=c(16,1),lty=c(1,8),lwd=c(2,2),         #   at (150,45) for different
  cex=1.4)                                   #   plotting charaters, line types


# Correlation matrix for class size data
# ======================================
sizes <- read.csv("Data/sizes.txt",header=T) # Reads in the class data
cor(sizes[,1:3])                             # Correlations for 1st 3 var's

# Fits the interaction and no-interaction models for meadowfoam data
# ==================================================================
reg3.out <- lm(flowers~intensity+timing+intensity*timing) # Fits interaction model
summary(reg3.out)                             # Regression summary
anova(reg3.out)                               # Regression ANOVA table

reg4.out <- lm(flowers~intensity+timing)     # Fits first-order model
summary(reg4.out)                            # Regression summary
anova(reg4.out)                              # Regression ANOVA table

timing1<-factor(timing, levels=c("At PFI", 
                                 "24 Days before PFI")) # Modify level orders
reg5.out <- lm(flowers~intensity+timing1)               # Change baseline category
summary(reg5.out)                            


# Alternative way to fit the interaction model
# ============================================
reg3b.out <- lm(flowers~intensity*timing)  # Fits interaction model
summary(reg3b.out)                         # Regression summary
anova(reg3b.out)                           # Regression ANOVA table

# Confidence intervals for the model parameters
# =============================================
confint(reg4.out)

# Residual plot & Normal quantile plot
# ====================================
plot(reg4.out$fitted,reg4.out$resid,xlab=    # Creates a residual plot
  "Predicted Values",ylab="Residuals",pch=
  16,cex.lab=1.6,main="Residual Plot",
  cex.main=1.8,cex=1.5,cex.axis=1.5,
  mgp=c(2.7,1,0))
abline(h=0,lwd=2,lty=2)                      # Horizontal line at y=0
qqnorm(reg4.out$resid,ylab="Residuals",xlab= # Normal quantile plot
  "Normal Quantiles",cex.lab=1.6,cex.axis=
  1.5,cex=1.5,main="Normal Quantile Plot",
  cex.main=1.8,pch=16,mgp=c(2.7,1,0))

# Confidence & Prediction intervals for meadowfoam data
# =====================================================
predict(reg4.out,interval="confidence")   # Confidence intervals for data values
predict(reg4.out,interval="prediction")   # Prediction intervals for data values

# Confidence and prediction interval for a new observation
# ========================================================
xval <- data.frame(intensity=400,timing="At PFI")    # Data frame of x-values
conf <- predict(reg4.out,xval,interval="confidence") # Confidence interval for (400,1)
conf                                                 # Prints confidence interval
pred <- predict(reg4.out,xval,interval="prediction") # Prediction interval for (400,1)
pred                                                 # Prints prediction interval


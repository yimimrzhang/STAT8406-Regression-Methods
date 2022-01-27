# ========== #
# Homework 2 #
# ========== #

# ========= #
# Problem 1 #
# ========= #
city <- read.csv("citysize.txt",header=T) # Reads in the citysize data
attach(city)                              # Allows to access variable names 
                                          #   without calling the data frame
 
# Part (a): Scatterplot of Expenditure vs. City Size
# =====================================================
plot(size,expenditure,pch=16,cex.axis=1.5,      # Plots expenditure vs. city size
     xlab="City Size (1000s of People)",        # with x- and y-axis labels and a title
     ylab="Expenditure ($1000)", mgp=c(2.7,1,0),
     main="Expenditure vs. City Size",
     cex.lab=1.6,cex.main=1.8,cex=1.5)
lines(lowess(size,expenditure,f=0.6),lwd=2,     # Overlays lowess fit with f=0.6
      col=2,lty=8)


# Part (b): Compute Pearson's correlation
# ========================================
cor(size,expenditure)   # Pearson's correlation


# Part (c): Regression of Expenditure on City Size, plotted on scatterplot
# ==========================================================================
out <- lm(expenditure~size)  # Least squares regression of expenditure on city size
summary(out)                 # Regression summary
abline(out$coef,lwd=2)       # Plots the regression line


# Part (d): Residual Plot
# ========================
resid <- out$resid           # Puts the residuals in a vector called "resid"
plot(size,resid,pch=16,cex.axis=1.5,      # Plots the residuals vs. time
     xlab="City Size (1000s of People)",  # with x- and y-axis labels and a title
     ylab="Residuals",mgp=c(2.7,1,0),
     main="Residual Plot",
     cex.lab=1.6,cex.main=1.8,cex=1.5)
abline(h=0, lwd=2, lty=3)                 # Adds horizontal reference line at 0
lines(lowess(size,resid,f=0.5),lwd=2,     # Overlays lowess fit with f=0.6
      col=2,lty=8)

